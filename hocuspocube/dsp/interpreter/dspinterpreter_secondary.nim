import
    ../dspstate, ../dspdef, ../dsp,
    dspinterpreter_aux,
    strformat

using state: var DspState

proc undefinedSecondary(state; instr: uint16) =
    discard

proc mr(state; m, r: uint16) =
    case range[0..3](m)
    of 0: discard # parallel nop
    of 1: state.writeReg dspRegAdr0.succ(int r), incAdr(adrReg(int r), wrapReg(int r))
    of 2: state.writeReg dspRegAdr0.succ(int r), decAdr(adrReg(int r), wrapReg(int r))
    of 3: state.writeReg dspRegAdr0.succ(int r), incAdr(adrReg(int r), wrapReg(int r), cast[int16](incReg(int r)))

proc incAdrReg(state; reg, m: int) =
    if m == 0:
        state.writeReg dspRegAdr0.succ(reg), incAdr(adrReg(reg), wrapReg(reg))
    else:
        state.writeReg dspRegAdr0.succ(reg), incAdr(adrReg(reg), wrapReg(reg), cast[int16](incReg(reg)))

proc mv(state; d, s: uint16) =
    state.writeReg dspRegX0.succ(int d),
        case range[0..3](s)
        of 0..1: state.readReg(dspRegA0.succ(int s))
        of 2..3: state.storeAccum(int s - 2)

proc st(state; s, m, r: uint16) =
    let
        val = case range[0..3](s)
            of 0..1: state.readReg(dspRegA0.succ(int s))
            of 2..3: state.storeAccum(int s - 2)
        adr = adrReg(int r)
    dataWrite(adr, val)

    state.incAdrReg(int r, int m)

proc ld(state; d, m, r: uint16) =
    let val = dataRead(adrReg(int r))

    state.incAdrReg(int r, int m)

    case range[0..7](d)
    of 0..5: state.writeReg dspRegX0.succ(int d), val
    of 6..7: state.loadAccum(int d - 6, val)

proc ls(state; d, m, n, k, s: uint16) =
    let
        (loadReg, storeReg) = if k == 0: (0, 3) else: (3, 0)
        storeVal = state.storeAccum(int s)

    state.writeReg(dspRegX0.succ(int d), dataRead(adrReg(loadReg)))
    dataWrite(adrReg(storeReg), storeVal)

    state.incAdrReg(0, int n)
    state.incAdrReg(3, int m)

proc ldd(state; d, m, n, r: uint16) =
    let
        (d1, d2, adr) =
            if r == 3:
                # ldd2
                if (d and 1) == 0:
                    (dspRegX1, dspRegX0, int(d shr 1))
                else:
                    (dspRegY1, dspRegY0, int(d shr 1))
            else:
                (dspRegX0.succ(int(d shr 1) * 2), dspRegY0.succ(int(d and 1) * 2), int(r))

    state.writeReg(d1, dataRead(adrReg(adr)))
    state.writeReg(d2, dataRead(adrReg(3)))

    state.incAdrReg(int adr, int n)
    state.incAdrReg(3, int m)

proc dispatchSecondary*(state; x: uint16) =
    dspSecondaryDispatch(x, state, undefinedSecondary)
