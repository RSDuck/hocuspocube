import
    strformat, streams,
    ../util/bitstruct,
    exi

makeBitStruct uint8, CardStatus:
    ready[0]: bool
    programError[3]: bool
    eraseError[4]: bool
    sleep[5]: bool
    unlocked[6]: bool
    busy[7]: bool

type
    MemCardType* = enum
        memcard59
        memcard123
        memcard251
        memcard507
        memcard1019
        memcard2043

    MemCard = ref object of ExiDevice
        file: string
        data: seq[byte]

        cmd: byte

        enableInterrupt: bool
        interrupt: bool

        inputAdr: uint32
        status: CardStatus

const
    cmdGetId = 0x00'u8
    cmdMemCardId = 0x85'u8
    cmdGetStatus = 0x83'u8
    cmdClearStatus = 0x89'u8
    cmdReadBlock = 0x52'u8
    cmdEraseCard = 0xF4'u8
    cmdEraseSector = 0xF1'u8
    cmdWriteBlock = 0xF2'u8
    cmdSetInterrupt = 0x81'u8

    sectorSize = 0x2000'u32

proc flagInterrupt(dev: MemCard) =
    if dev.enableInterrupt and not dev.interrupt:
        dev.interrupt = true
        setExiPeripheralInt(dev)

method select(dev: MemCard, state: bool) =
    if state:
        procCall ExiDevice(dev).select(state)
    else:
        case dev.cmd
        of cmdEraseCard, cmdEraseSector, cmdWriteBlock:
            dev.flagInterrupt()
        else: discard

proc eraseCart(dev: MemCard) =
    for i in 0..<dev.data.len:
        dev.data[i] = 0xFF

template decodeAdr(done: untyped): untyped =
    if dev.transactionPos < 5:
        const
            shifts = [17'u8, 9, 7, 0]
            # for writes should the first be 0x3F?
            masks = [0x7F'u8, 0xFF, 3, 0x7F]
        response.writeByte i, 0xFF'u8
        if dev.transactionPos == 0:
            dev.inputAdr = 0
        else:
            dev.inputAdr = dev.inputAdr or
                (inData and masks[dev.transactionPos-1]) shl shifts[dev.transactionPos-1]
    else:
        done

proc flush(dev: Memcard) =
    let file = newFileStream(dev.file, fmWrite)
    file.writeData(addr dev.data[0], dev.data.len)
    file.close()

method exchange(dev: MemCard, response: var openArray[byte], input: openArray[byte]) =
    for i in 0..<inbytes():
        let inData = input.readByte(i)

        if dev.transactionPos == 0:
            dev.cmd = inData

        case dev.cmd
        of cmdGetId:
            let
                id = uint32(dev.data.len) shr 17
            response.writeByte i,
                (case dev.transactionPos
                of 0: 0xFF'u8
                of 1: 0x80'u8
                of 2: uint8(id shr 24)
                of 3: uint8(id shr 16)
                of 4: uint8(id shr 8)
                of 5: uint8(id)
                else: 0'u8)
            echo &"memcard get id {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdMemCardId:
            # TODO: check my own cart for ID instead of stealing it
            response.writeByte i,
                case dev.transactionPos
                of 0: 0xFF
                elif (dev.transactionPos and 1) == 1: 0xc2
                else: 0x21
            
            echo &"memcard get memcard id {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdGetStatus:
            response.writeByte i,
                if dev.transactionPos > 0: uint8(dev.status) else: 0xFF

            echo &"memcard get status {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdClearStatus:
            response.writeByte i, 0xFF'u8
            dev.status.ready = true
            dev.status.eraseError = false
            dev.status.programError = false
            dev.interrupt = false

            echo &"clear status {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdReadBlock:
            dev.status.unlocked = true
            decodeAdr:
                response.writeByte i,
                    if dev.transactionPos < 9:
                        0xFF'u8
                    else:
                        let oldAdr = dev.inputAdr
                        dev.inputAdr += 1
                        dev.data[oldAdr]
            echo &"readblock {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdEraseSector:
            response.writeByte i, 0xFF'u8
            case dev.transactionPos:
            of 1: dev.inputAdr = uint32(inData and 0x7F) shl 17
            of 2:
                dev.inputAdr = dev.inputAdr or (uint32(inData) shl 9)

                for i in 0'u32..<sectorSize:
                    dev.data[dev.inputAdr+i] = 0xFF

                dev.flush()
            else: discard
            echo &"erase sector {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdEraseCard:
            response.writeByte i, 0xFF'u8

            dev.eraseCart()
            dev.flush()
            echo &"erase card {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdWriteBlock:
            decodeAdr:
                response.writeByte i, 0xFF
                dev.data[dev.inputAdr] = dev.data[dev.inputAdr] and inData
                dev.inputAdr += 1
            echo &"write block {dev.transactionPos:02X} {inData:02X}"
            dev.flush()
        of cmdSetInterrupt:
            dev.enableInterrupt = bool(inData and 1)
            response.writeByte i, 0xFF
            echo &"set interrupt {inData}"
        else:
            echo &"unknown memcard cmd {dev.cmd:08X}, byte at position {dev.transactionPos} {inData:08X}"

        dev.transactionPos += 1

proc newMemcard*(file: string, kind: MemCardType): MemCard =
    result = MemCard(file: file,
        data: newSeq[byte](0x80000 shl ord(kind)))

    result.status.ready = true

    let inputStream = newFileStream(file, fmRead)
    if inputStream != nil:
        doAssert inputStream.readData(addr result.data[0], result.data.len) == result.data.len
        inputStream.close()
    else:
        result.eraseCart()
