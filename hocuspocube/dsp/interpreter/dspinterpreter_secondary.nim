import
    ../dspstate, ../dspdef, ../dsp,
    dspinterpreter_aux,
    strformat

using state: var DspState

proc undefinedSecondary(state; instr: uint16) =
    raiseAssert(&"undefined secondary dsp instr {state.pc:02X} {instr:02X}")

proc mr*(state; m, r: uint16) =
    case range[0..3](m)
    of 0: discard # parallel nop
    of 1: state.adrReg[r] = incAdr(state.adrReg[r], state.wrapReg[r])
    of 2: state.adrReg[r] = decAdr(state.adrReg[r], state.wrapReg[r])
    of 3: state.adrReg[r] = incAdr(state.adrReg[r], state.wrapReg[r], cast[int16](state.incReg[r]))

proc incAdrReg(state; reg, m: int) =
    if m == 0:
        state.adrReg[reg] = incAdr(state.adrReg[reg], state.wrapReg[reg])
    else:
        state.adrReg[reg] = incAdr(state.adrReg[reg], state.wrapReg[reg], cast[int16](state.incReg[reg]))

proc mv*(state; d, s: uint16) =
    state.writeReg x0.succ(int d),
        case range[0..3](s)
        of 0..1: state.readReg(a0.succ(int s))
        of 2..3: state.storeAccum(int s - 2)

proc st*(state; s, m, r: uint16) =
    let
        val = case range[0..3](s)
            of 0..1: state.readReg(a0.succ(int s))
            of 2..3: state.storeAccum(int s - 2)
    dataWrite(state.adrReg[r], val)

    state.incAdrReg(int r, int m)

proc ld*(state; d, m, r: uint16) =
    let val = dataRead(state.adrReg[r])

    state.incAdrReg(int r, int m)

    case range[0..7](d)
    of 0..5: state.writeReg x0.succ(int d), val
    of 6..7: state.loadAccum(int d - 6, val)

proc ls*(state; d, m, n, k, s: uint16) =
    let
        (loadReg, storeReg) = if k == 0: (0, 3) else: (3, 0)
        storeVal = state.storeAccum(int s)

    state.writeReg(x0.succ(int d), dataRead(state.adrReg[loadReg]))
    dataWrite(state.adrReg[storeReg], storeVal)

    state.incAdrReg(0, int n)
    state.incAdrReg(3, int m)

proc ldd*(state; d, m, n, r: uint16) =
    let
        (d1, d2, adr) =
            if r == 3:
                # ldd2
                if (d and 1) == 0:
                    (x1, x0, int(d shr 1))
                else:
                    (y1, y0, int(d shr 1))
            else:
                (x0.succ(int(d shr 1) * 2), y0.succ(int(d and 1) * 2), int(r))

    state.writeReg(d1, dataRead(state.adrReg[adr]))
    state.writeReg(d2, dataRead(state.adrReg[3]))

    state.incAdrReg(int adr, int n)
    state.incAdrReg(3, int m)

proc dispatchSecondary*(state; x: uint16) =
    dspSecondaryDispatch(x, state, undefinedSecondary)
