
template fetchFollowingImm*: uint16 {.dirty.} =
    state.pc += 1
    instrRead(state.pc)

template writeReg*(n: uint32, val: uint16) {.dirty.} =
    case n
    of 12: state.callStack.push(val)
    of 13: state.dataStack.push(val)
    of 14: state.loopAddrStack.push(val)
    of 15: state.loopCountstack.push(val)
    else: state.r[n] = val

template readReg*(n: uint32): uint16 {.dirty.} =
    case n
    of 12: state.callStack.pop()
    of 13: state.dataStack.pop()
    of 14: state.loopAddrStack.pop()
    of 15: state.loopCountstack.pop()
    else: state.r[n]

template midAccum*(n: uint32): var uint16 {.dirty.} =
    state.r[30 + n]

template lowAccum*(n: uint32): var uint16 {.dirty.} =
    state.r[28 + n]

template highAccum*(n: uint32): var uint16 {.dirty.} =
    state.r[16 + n]

template addrReg*(n: uint32): var uint16 {.dirty.} =
    state.r[n]

template idxReg*(n: uint32): var uint16 {.dirty.} =
    state.r[4 + n]

template writeAc*(n: uint32, val: int64) {.dirty.} =
    lowAccum(n) = uint16(val)
    midAccum(n) = uint16(val shr 16)
    highAccum(n) = uint16(val shr 32)

template setAcFlags*(val: int64) {.dirty.} =
    state.status.zr = val == 0
    state.status.mi = val < 0

    state.status.overs32 = int64(cast[int32](val)) != val
    state.status.top2bits = ((val and 0xC0000000'i64) == 0) or ((val and 0xC0000000'i64) == 0xC0000000)

template conditionHolds*(cond: uint32): bool {.dirty.} =
    template less: bool = state.status.ov != state.status.mi
    template condA: bool = (state.status.overs32 or state.status.top2bits) and not(state.status.zr)

    case range[0..15](cond)
    of 0: less()
    of 1: not less()
    of 2: not(less()) and not state.status.zr
    of 3: less() or state.status.zr
    of 4: not state.status.zr
    of 5: state.status.zr
    of 6: not state.status.ca
    of 7: state.status.ca
    of 8: not state.status.overs32
    of 9: state.status.overs32
    of 10: condA()
    of 11: not condA()
    of 12: not state.status.lz
    of 13: state.status.lz
    of 14: state.status.ov
    of 15: true