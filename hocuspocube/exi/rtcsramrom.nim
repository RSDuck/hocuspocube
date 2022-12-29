import
    times, streams, strformat,
    stew/endians2,
    ../util/bitstruct,
    exi

# welcome to the kitchen sink device
# where every piece of hw lands which didn't fit anywhere else
# but wasn't worth putting it somewhere on it's own

# boot rom descrambler
# Copyright 2008 Segher Boessenkool <segher@kernel.crashing.org>
# translated into Nim by me (original was in C)
proc descramble(output: var openArray[byte], input: openArray[byte]) =
    assert output.len == input.len

    var
        acc = 0'u8
        nacc = 0'u8

        t = 0x2953'u16
        u = 0xD9C2'u16
        v = 0x3FF1'u16
    
        x = 1'u8

        i = 0

    while i < output.len:
        let
            t0 = t and 1
            t1 = (t shr 1) and 1
            u0 = u and 1
            u1 = (u shr 1) and 1
            v0 = v and 1

        x = x xor uint8(t1 xor v0) xor
            uint8(u0 or u1) xor
            uint8((t0 xor u1 xor v0) and (t0 xor u0))

        if t0 == u0:
            v = v shr 1
            if v0 != 0:
                v = v xor 0xB3D0

        if t0 == 0:
            u = u shr 1
            if u0 != 0:
                u = u xor 0xFB10

        t = t shr 1
        if t0 != 0:
            t = t xor 0xA740

        nacc += 1
        acc = 2 * acc + x
        if nacc == 8:
            output[i] = input[i] xor acc
            nacc = 0
            i += 1

var
    ipl: array[2*1024*1024, byte]
    iplBios: array[0x1AFE00, byte]

    sram: array[512, byte]

    sramPath: string

let
    firstOfJanuary2000 = dateTime(2000, mJan, 1)

proc loadIplSram*(iplPath, inSramPath: string) =
    block:
        let file = newFileStream(iplPath, fmRead)
        assert file != nil

        let readBytes = file.readData(addr ipl[0], sizeof(ipl))
        assert readBytes == sizeof(ipl)
        file.close()

        descramble(iplBios, toOpenArray(ipl, 0x100, 0x100+0x1AFE00-1))
    #writeFile("ipl_descrambled.bin", iplBios)
    block:
        sramPath = inSramPath
        let file = newFileStream(sramPath, fmRead)
        if file != nil:
            let readBytes = file.readData(addr sram[0], sizeof(sram))
            assert readBytes == sizeof(sram)
            file.close()

proc iplRead*[T](adr: uint32): T =
    let
        adr = adr and 0x1F_FFF'u32
        descramble = descrambleBios()
    if descramble and adr >= 0x100 and adr < 0x1AFE00:
        # technically this is all still super wrong because the descrambling should run in parallel
        # with the code fetch
        # though that would require proper cache emulation, so we have to live with this
        cast[ptr T](addr iplBios[adr - 0x100])[]
    else:
        echo &"weird access to memory mapped IPL rom {adr} {descramble} (something's gone wrong!)"
        T(0)

makeBitStruct uint32, InputCmd:
    write[31]: bool
    offset[8..30]: uint32 # the lower 4 byte of the address are ignored

type
    RtcRamRom* = ref object of ExiDevice
        transactionPos: uint32
        inputCmd: InputCmd

        sramDirty: bool

method select(dev: RtcRamRom, status: bool) =
    if status:
        dev.inputCmd = InputCmd 0
        dev.transactionPos = 0
    elif dev.sramDirty:
        echo "flush sram to file"
        let file = newFileStream(sramPath, fmWrite)
        if file == nil:
            echo "failed to open file"
        else:
            file.writeData(addr sram[0], sizeof(sram))
            file.close()

method exchange(dev: RtcRamRom, response: var openArray[byte], input: openArray[byte]) =
    for i in 0..<inbytes():
        let data = input.readByte(i)

        #echo &"transaction {data:X} {uint32(dev.inputCmd):08X} {dev.transactionPos}"

        if dev.transactionPos < 4:
            dev.inputCmd = InputCmd(uint32(dev.inputCmd) or (uint32(data) shl ((3-dev.transactionPos)*8)))
            response.writeByte i, 0xFF
        else:
            case dev.inputCmd.offset*4
            of 0..0x1F_FFFF:
                let adr = (dev.inputCmd.offset*4+dev.transactionPos-4) and 0x1F_FFFF
                if not dev.inputCmd.write:
                    if descrambleBios() and adr >= 0x100 and adr < 0x1AFE00:
                        response.writeByte i, iplBios[adr - 0x100]
                    else:
                        response.writeByte i, ipl[adr]
                else:
                    echo &"writing to rom? {adr} {data}"
            of 0x800000..0x8001FF:
                let adr = (dev.inputCmd.offset*4+dev.transactionPos-4) and 0x1FF
                if dev.inputCmd.write:
                    dev.sramDirty = true
                    sram[adr] = data
                else:
                    if adr == 0:
                        let seconds = uint32((now() - firstOfJanuary2000).inSeconds())
                        cast[ptr uint32](addr sram[0])[] = toBE seconds

                    response.writeByte i, sram[adr]
            else:
                echo &"invalid RTC/ROM/SRAM read {dev.inputCmd.offset*4:X}"
                response.writeByte i, 0

        dev.transactionPos += 1

let devRtcsramrom* = RtcRamRom()
