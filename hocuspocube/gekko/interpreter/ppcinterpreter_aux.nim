import
    ../ppcstate,
    strutils,
    bitops, math

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