import
    ../ppcstate,
    options, strformat

proc updateSo*(state: var PpcState) =
    state.xer.so = state.xer.so or state.xer.ov

proc setCr*[T](state: var PpcState, n: int, a, b: T) =
    state.cr.so(n, state.xer.so)
    state.cr.eq(n, a == b)
    state.cr.gt(n, a > b)
    state.cr.lt(n, a < b)

proc translateBat[T; U](batsLo: array[4, T], batsHi: array[4, U], `addr`: uint32): Option[uint32] {.inline.} =
    # TODO check privilege level
    for i in 0..<4:
        if (`addr` and (not(batsHi[i].bl) shl 17)) == (batsHi[i].bepi shl 17):
            return some((`addr` and not(not(batsHi[i].bl) shl 17)) or (batsLo[i].brpn shl 17))
    echo &"failed to translate addr {`addr`:X}"
    none(uint32)

proc translateDataAddr*(state: PpcState, `addr`: uint32): Option[uint32] {.inline.} =
    if state.msr.dr:
        # TODO: page translation
        translateBat(state.dbatLo, state.dbatHi, `addr`)
    else:
        some(`addr`)

proc translateInstrAddr*(state: PpcState, `addr`: uint32): Option[uint32] {.inline.} =
    if state.msr.ir:
        # TODO: page translation
        translateBat(state.ibatLo, state.ibatHi, `addr`)
    else:
        some(`addr`)
