import
    ../ppcstate, ../gekko,
    options, strformat, strutils,
    bitops, stew/bitops2, math

proc updateOv*(state: var PpcState, ov: bool) {.inline.} =
    state.xer.ov = ov
    state.xer.so = state.xer.so or ov

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

proc makeFieldMask*(mask: uint32): uint32 =
    for i in 0..<8:
        if mask.getBit(i):
            result.setMask(0xF'u32 shl (i * 4))

template checkNan*(val: float64) =
    if isNan(val):
        let pos = instantiationInfo()
        echo "nan already here", toHex(state.pc), " ", pos
template checkNan*(val: float64, body: untyped) =
    if isNan(val):
        let pos = instantiationInfo()
        echo "nan already here", toHex(state.pc), " ", pos
        body

template handleFloatException*(instr) =
    if unlikely(not state.msr.fp):
        state.pc -= 4
        state.pendingExceptions.incl exceptionNoFloatPoint
    else:
        instr