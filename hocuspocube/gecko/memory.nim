import
    strformat,
    stew/endians2,

    gecko,
    ../dsp/dsp,
    ../vi,
    ../si/si,
    ../exi/[exi, rtcsramrom],
    ../flipper/[cp, pe, texturesetup],
    ../di

proc writeBus*[T](adr: uint32, val: T) =
    if adr < uint32 MainRAM.len:
        cast[ptr T](addr MainRAM[adr])[] = val
        if unlikely(MainRAMTagging[adr div 32] != memoryTagNone):
            case MainRAMTagging[adr div 32]
            of memoryTagNone: discard
            of memoryTagTexture: invalidateTexture(adr)
    elif (adr and 0xFFFF0000'u32) == 0xC000000:
        case adr and 0xFF00
        of 0x0000: cpWrite[T](adr, val)
        of 0x1000: peWrite[T](adr, val)
        of 0x2000: viWrite[T](adr, val)
        of 0x3000: piWrite[T](adr, val)
        of 0x5000: dspWrite[T](adr, val)
        of 0x6000: diWrite[T](adr, val)
        of 0x6400: siWrite[T](adr, val)
        of 0x6800: exiWrite[T](adr, val)
        of 0x6C00: aiWrite[T](adr, val)
        else: (let pc = geckoState.pc; echo &"unknown io register write {adr:X} {fromBE(val):X} {pc:08X}")
    else:
        let
            pc = geckoState.pc
            lr = geckoState.lr
        echo &"welp write to unknown memory access {adr:X} {val:X} pc: {pc:08X} lr: {lr:08X}"

proc readBus*[T](adr: uint32): T =
    if adr < uint32 MainRAM.len:
        cast[ptr T](addr MainRAM[adr])[]
    elif (adr and 0xFFFF0000'u32) == 0xC000000'u32:
        case adr and 0xFF00
        of 0x0000: cpRead[T](adr)
        of 0x1000: peRead[T](adr)
        of 0x2000: viRead[T](adr)
        of 0x3000: piRead[T](adr)
        of 0x5000: dspRead[T](adr)
        of 0x6000: diRead[T](adr)
        of 0x6400: siRead[T](adr)
        of 0x6800: exiRead[T](adr)
        of 0x6C00: aiRead[T](adr)
        else: (let pc = geckoState.pc; echo &"unknown io register read {adr:X} {pc:08X}"; T(0))
    elif (adr and 0xFFF00000'u32) == 0xFFF00000'u32:
        iplRead[T](adr)
    else:
        let
            pc = geckoState.pc
            lr = geckoState.lr
        echo &"welp read from unknown memory access {adr:X} pc: {pc:08X} lr: {lr:08X}"
        T(0)

proc burstBusWrite*(adr: uint32, data: openArray[uint32]) =
    if adr == 0xC008000:
        let writeAdr = updateFifo()
        if not cpNotifyFifoBurst(data):
            for i in 0..<data.len:
                writeBus[uint32](writeAdr + uint32(i) * 4, data[i])
    else:
        writeBus[uint32](adr, data[^1])