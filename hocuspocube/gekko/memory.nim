import
    strformat,
    stew/endians2,

    gekko, ppcstate,
    ../dsp/dsp,
    ../vi,
    ../si/si,
    ../exi/[exi, rtcsramrom],
    ../flipper/[cp, pe, texturesetup],
    ../di

proc writeBus*[T](adr: uint32, val: T) =
    if adr < uint32 mainRAM.len:
        cast[ptr T](addr mainRAM[adr])[] = val
        if unlikely(mainRAMTagging[adr div 32] != memoryTagNone):
            case mainRAMTagging[adr div 32]
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
        else: (let pc = gekkoState.pc; echo &"unknown io register write {adr:X} {fromBE(val):X} {pc:08X}")
    else:
        let
            pc = gekkoState.pc
            lr = gekkoState.lr
        echo &"welp write to unknown memory access {adr:X} {val:X} pc: {pc:08X} lr: {lr:08X}"

proc readBus*[T](adr: uint32): T =
    if adr < uint32 mainRAM.len:
        cast[ptr T](addr mainRAM[adr])[]
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
        else: (let pc = gekkoState.pc; echo &"unknown io register read {adr:X} {pc:08X}"; T(0))
    elif (adr and 0xFFF00000'u32) == 0xFFF00000'u32:
        iplRead[T](adr)
    else:
        let
            pc = gekkoState.pc
            lr = gekkoState.lr
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

proc flushGatherPipe(state: var PpcState) =
    let initialOffset = state.gatherPipeOffset
    var offset = 0'u32
    while state.gatherPipeOffset >= 32'u32:
        burstBusWrite(state.wpar.gbAddr, toOpenArray(cast[ptr UncheckedArray[uint32]](addr state.gatherpipe[offset]), 0, 7))
        state.gatherPipeOffset -= 32'u32
        offset += 32'u32

    if state.gatherPipeOffset > 0:
        # move the remaining state back to the beginning
        copyMem(addr state.gatherpipe[0], addr state.gatherpipe[initialOffset - state.gatherPipeOffset], state.gatherPipeOffset)

proc writeMemory*[T](state: var PpcState, adr: uint32, val: T) {.inline.} =
    # according to the manual gather pipe writes where the offset within the cache line
    # is not zeroe produce incorrect results
    # though for easier implementation of pair load/store we do it this way for now
    # it would probably be more correct to implement them via a 64-bit load/store
    if unlikely((adr and not(0x1F'u32)) == state.wpar.gbAddr and state.hid2.wpe):
        #echo "writing to gather pipe ", toHex(fromBE val), " ", toHex(state.pc), " ", toHex(state.lr)
        copyMem(addr state.gatherpipe[state.gatherpipeOffset], unsafeAddr val, sizeof(T))
        state.gatherpipeOffset += uint32 sizeof(T) 
        if state.gatherpipeOffset >= 32'u32:
            state.flushGatherPipe()
    elif (adr and 0xFFFFC000'u32) == 0xE0000000'u32:
        cast[ptr T](addr lockedCache[adr and 0x3FFF'u32])[] = val
    else:
        writeBus[T](adr, val)

# put processor and cache stuff into these procs:
proc readMemory*[T](state: var PpcState, adr: uint32): T {.inline.} =
    if (adr and 0xFFFFC000'u32) == 0xE0000000'u32:
        cast[ptr T](addr lockedCache[adr and 0x3FFF'u32])[]
    else:
        readBus[T](adr)
