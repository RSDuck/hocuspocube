import
    ../dspstate, ../dsp,
    dspinterpreter_aux

using state: var DspState

proc pld*(state; d, m, r: uint16) =
    state.loadAccum int d, instrRead(adrReg(int r))
    state.loadStoreAdrInc(m, int r)

proc ld*(state; m, r, d: uint16) =
    state.writeReg(DspReg d, dataRead(adrReg(int r)))
    state.loadStoreAdrInc(m, int r)

proc st*(state; m, r, s: uint16) =
    dataWrite(adrReg(int r), state.readReg(DspReg s))
    state.loadStoreAdrInc(m, int r)

proc ldsa*(state; d, a: uint16) =
    let val = dataRead(state.dppAdr(a))
    if d < 6:
        state.writeReg(dspRegX0.succ(int d), val)
    else:
        state.loadAccum(int d - 6, val)

proc stsa*(state; s, a: uint16) =
    let val = case range[0..7](s)
        of 0..1: state.readReg(dspRegA2.succ(int s))
        of 2..3: 0'u16
        of 4..5: state.readReg(dspRegA0.succ(int s - 4))
        of 6..7: state.storeAccum(int s - 6)
    dataWrite(state.dppAdr(a), val)

proc ldla*(state; d: uint16) =
    let adr = fetchFollowingImm
    state.writeReg(DspReg d, dataRead(adr))

proc stla*(state; s: uint16) =
    let adr = fetchFollowingImm
    dataWrite(adr, state.readReg(DspReg s))

proc stli*(state; a: uint16) =
    let imm = fetchFollowingImm
    dataWrite(a or 0xFF00'u16, imm)

