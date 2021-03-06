import
    ../dspstate, ../dspdef,
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
proc mv(state; d, s: uint16) =
    raiseAssert "unimplemenetd dsp parallel instr"
proc st(state; s, m, r: uint16) =
    raiseAssert "unimplemenetd dsp parallel instr"
proc ld(state; d, m, r: uint16) =
    raiseAssert "unimplemenetd dsp parallel instr"
proc ls(state; d, m, n, k, s: uint16) =
    raiseAssert "unimplemenetd dsp parallel instr"
proc ldd(state; d, m, n, r: uint16) =
    raiseAssert &"unimplemenetd dsp parallel instr {state.pc:04X}"

proc dispatchSecondary*(state; x: uint16) =
    dspSecondaryDispatch(x, state, undefinedSecondary)
