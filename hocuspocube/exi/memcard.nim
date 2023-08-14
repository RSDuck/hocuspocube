import
    strformat, streams, stew/bitops2, bitops,
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

        unlockValue: uint32
        inputAdr: uint32
        status: CardStatus

        lfsrState: uint32

        unlockKeys: int
        unlockOnDeselect: bool

# credits go to Vincent Pelletier for actually figuring out what's behind the unlock sequence
# see: https://github.com/vpelletier/gc-memcard-adapter/blob/master/adapter.py 

proc lfsrAdvance(dev: Memcard): bool =
    # probably 31-bit LFSR
    result = dev.lfsrState.getBit(30)

    let feedback =
        not(dev.lfsrState.getBit(7) xor dev.lfsrState.getBit(15) xor
            dev.lfsrState.getBit(23) xor dev.lfsrState.getBit(30))

    dev.lfsrState = (dev.lfsrState shl 1) or uint32(feedback)

proc lfsrGetByte(dev: Memcard): uint8 =
    result = 0
    for i in 0..<8:
        result = (result shl 1) or uint8(dev.lfsrAdvance())

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
    #echo &"flag interrupt {dev.enableInterrupt} {dev.interrupt}"
    if dev.enableInterrupt and not dev.interrupt:
        dev.interrupt = true
        setExiPeripheralInt(dev)

proc flush(dev: Memcard) =
    let file = newFileStream(dev.file, fmWrite)
    file.writeData(addr dev.data[0], dev.data.len)
    file.close()

method select(dev: MemCard, state: bool) =
    if state:
        procCall ExiDevice(dev).select(state)
    else:
        if dev.unlockOnDeselect:
            dev.status.unlocked = true

        case dev.cmd
        of cmdEraseCard, cmdEraseSector, cmdWriteBlock:
            dev.flagInterrupt()
            dev.flush()                
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
            dev.unlockValue = 0
        else:
            dev.inputAdr = dev.inputAdr or
                (uint32(inData) and masks[dev.transactionPos-1]) shl shifts[dev.transactionPos-1]
            dev.unlockValue = dev.unlockValue or uint32(inData) shl ((3-(dev.transactionPos-1))*8)
    else:
        done

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
                elif (dev.transactionPos and 1) == 1: 0xC2
                else: 0x21

            #echo &"memcard get memcard id {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdGetStatus:
            response.writeByte i,
                if dev.transactionPos > 0: uint8(dev.status) else: 0xFF

            #echo &"memcard get status {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdClearStatus:
            response.writeByte i, 0xFF'u8
            dev.status.ready = true
            dev.status.eraseError = false
            dev.status.programError = false
            dev.interrupt = false

            #echo &"clear status {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdReadBlock:
            decodeAdr:
                if not dev.status.unlocked:
                    if (dev.inputAdr and not(0x7'u32)) == 0x7FEC8'u32:
                        if dev.transactionPos == 9:
                            echo &"initialising lfsr with {dev.inputAdr:08X}"
                            dev.lfsrState = reverseBits(dev.inputAdr shl 12) shr 1
                            discard dev.lfsrAdvance()
                        if dev.transactionPos >= 9:
                            discard dev.lfsrGetByte()

                        response.writeByte i, 0xFF
                    elif dev.inputAdr == 0:
                        response.writeByte i,
                            if dev.transactionPos >= 9: dev.lfsrGetByte()
                            else: 0xFF
                    else:
                        if dev.transactionPos == 5:
                            dev.unlockKeys += 1
                            echo &"this should be the unlock value: {dev.unlockValue:08X} {dev.inputAdr:08X}"
                        response.writeByte i, 0xFF

                        if dev.unlockKeys == 2:
                            dev.unlockOnDeselect = true
                else:
                    response.writeByte i,
                        if dev.transactionPos < 9:
                            0xFF'u8
                        else:
                            let oldAdr = dev.inputAdr
                            dev.inputAdr = (dev.inputAdr and not(0x1FF'u32)) or ((dev.inputAdr + 1) and 0x1FF'u32)
                            dev.data[oldAdr]
            #echo &"readblock {dev.transactionPos:02X} {inData:02X} {response[i]:02X} {dev.inputAdr:08X}"
        of cmdEraseSector:
            response.writeByte i, 0xFF'u8
            case dev.transactionPos:
            of 1: dev.inputAdr = uint32(inData and 0x7F) shl 17
            of 2:
                dev.inputAdr = dev.inputAdr or (uint32(inData) shl 9)

                for i in 0'u32..<sectorSize:
                    dev.data[dev.inputAdr+i] = 0xFF
            else: discard
            #echo &"erase sector {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdEraseCard:
            response.writeByte i, 0xFF'u8

            dev.eraseCart()
            #echo &"erase card {dev.transactionPos:02X} {inData:02X} {response[i]:02X}"
        of cmdWriteBlock:
            decodeAdr:
                if dev.transactionPos == 5:
                    echo &"write block {dev.transactionPos:02X} {dev.inputAdr:08X}"

                response.writeByte i, 0xFF
                if inData != 0xFF:
                    echo &"write block {dev.transactionPos:02X} {dev.inputAdr:08X} {inData:02X}"
                dev.data[dev.inputAdr] = dev.data[dev.inputAdr] and inData
                dev.inputAdr += 1

        of cmdSetInterrupt:
            dev.enableInterrupt = bool(inData and 1)
            response.writeByte i, 0xFF
            #echo &"set interrupt {inData:1X}"
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
