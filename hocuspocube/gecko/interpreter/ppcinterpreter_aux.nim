import
    ../ppcstate,
    ../memory,
    options, strformat, strutils,
    bitops, stew/bitops2, stew/endians2, math

proc updateSo*(state: var PpcState) =
    state.xer.so = state.xer.so or state.xer.ov

proc setCr*[T](state: var PpcState, n: int, a, b: T) =
    var cr = if state.xer.so: 1'u32 else: 0'u32
    if a < b:
        cr.setMask 0b1000
    elif a > b:
        cr.setMask 0b0100
    else:
        cr.setMask 0b0010
    state.cr.crf n, cr

proc translateBat[T; U](batsLo: array[4, T], batsHi: array[4, U], adr: uint32): Option[uint32] {.inline.} =
    # TODO check privilege level
    for i in 0..<4:
        if (adr and (not(batsHi[i].bl) shl 17)) == batsHi[i].bepi:
            return some((adr and not(not(batsHi[i].bl) shl 17)) or batsLo[i].brpn)
    echo &"failed to translate addr {adr:X}"
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

# put processor and cache stuff into these procs:
proc readMemory*[T](state: var PpcState, adr: uint32): T {.inline.} =
    result = readBus[T](adr)

proc makeFieldMask*(mask: uint32): uint32 =
    for i in 0..<8:
        if mask.getBit(i):
            result.setMask(0xF'u32 shl (i * 4))

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
    else:
        writeBus[T](adr, val)

template checkNan*(val: float32) =
    if isNan(val):
        let pos = instantiationInfo()
        echo "nan already here", toHex(state.pc), " ", pos
template checkNan*(val: float32, body: untyped) =
    if isNan(val):
        let pos = instantiationInfo()
        echo "nan already here", toHex(state.pc), " ", pos
        body