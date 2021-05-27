import
    options,
    strformat, strutils,
    stew/endians2,

    gekko, ppcstate,
    ../dsp/dsp,
    ../vi,
    ../si/si,
    ../exi/[exi, rtcsramrom],
    ../flipper/[cp, pe, texturesetup],
    ../di,

    jit/blockcache

proc writeBus*[T](adr: uint32, val: T) =
    if adr < uint32 mainRAM.len:
        cast[ptr T](addr mainRAM[adr])[] = val
        if unlikely(mainRAMTagging[adr div 32] != memoryTagNone):
            case mainRAMTagging[adr div 32]
            of memoryTagNone: discard
            of memoryTagCode: discard
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

proc writeMemory*[T](state: var PpcState, adr: uint32, val: T) =
    # according to the manual gather pipe writes where the offset within the cache line
    # is not zeroe produce incorrect results
    # though for easier implementation of pair load/store we do it this way for now
    # it would probably be more correct to implement them via a 64-bit load/store
    if unlikely((adr and not(0x1F'u32)) == state.wpar.gbAddr and state.hid2.wpe):
        #let valstr = toHex(fromBE val)
        #echo &"writing to gather pipe {valstr} {sizeof(T)} {state.pc:08X} {state.lr:08X}"
        copyMem(addr state.gatherpipe[state.gatherpipeOffset], unsafeAddr val, sizeof(T))
        state.gatherpipeOffset += uint32 sizeof(T)
        if state.gatherpipeOffset >= 32'u32:
            state.flushGatherPipe()
    elif (adr and 0xFFFFC000'u32) == 0xE0000000'u32:
        cast[ptr T](addr lockedCache[adr and 0x3FFF'u32])[] = val
    else:
        writeBus[T](adr, val)

# put processor and cache stuff into these procs:
proc readMemory*[T](state: var PpcState, adr: uint32): T =
    if (adr and 0xFFFFC000'u32) == 0xE0000000'u32:
        cast[ptr T](addr lockedCache[adr and 0x3FFF'u32])[]
    else:
        readBus[T](adr)

proc readCode*(adr: uint32): uint32 =
    if adr < uint32 mainRAM.len:
        if unlikely(mainRAMTagging[adr div 32] != memoryTagCode):
            #assert mainRAMTagging[adr div 32] == memoryTagNone
            mainRAMTagging[adr div 32] = memoryTagCode
            copyMem(addr mainRAMFakeICache[adr and not(0x1F'u32)], addr mainRAM[adr and not(0x1F'u32)], 32)

        cast[ptr uint32](addr mainRAMFakeICache[adr])[]
    else:
        readBus[uint32](adr)

proc invalidateCode*(adr: uint32) =
    if adr < uint32 mainRAM.len:
        invalidateBlockCacheCode(adr and not(0x1F'u32))
        if mainRAMTagging[adr div 32] == memoryTagCode:
            mainRAMTagging[adr div 32] = memoryTagNone

proc flashInvalidateICache*() =
    for i, tag in mpairs mainRAMTagging:
        invalidateBlockCacheCode(uint32(i) * 32)
        if tag == memoryTagCode:
            tag = memoryTagNone

proc translateBat[T; U](batsLo: array[4, T], batsHi: array[4, U], adr: uint32): Option[uint32] {.inline.} =
    # TODO check privilege level
    for i in 0..<4:
        if (adr and (not(batsHi[i].bl) shl 17)) == batsHi[i].bepi:
            return some((adr and not(not(batsHi[i].bl) shl 17)) or batsLo[i].brpn)
    echo &"failed to translate addr {adr:X} {gekkoState.pc:08X} {batsLo.repr} {batsHi.repr}"
    none(uint32)

proc translateDataAddr*(state: PpcState, adr: uint32): Option[uint32] {.inline.} =
    if state.msr.dr:
        # TODO: page translation
        translateBat(state.dbatLo, state.dbatHi, adr)
    else:
        some(adr)

proc translateInstrAddr*(state: PpcState, adr: uint32): Option[uint32] {.inline.} =
    if state.msr.ir:
        # TODO: page translation
        translateBat(state.ibatLo, state.ibatHi, adr)
    else:
        some(adr)

proc jitReadMemory*[T](state: var PpcState, adr: uint32): T {.cdecl.} =
    fromBE state.readMemory[:T](state.translateDataAddr(adr).get)

proc jitWriteMemory*[T](state: var PpcState, adr: uint32, val: T) {.cdecl.} =
    state.writeMemory[:T](state.translateDataAddr(adr).get, toBE val)
