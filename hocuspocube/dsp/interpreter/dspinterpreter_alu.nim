import
    ../dspstate, ../dsp,
    dspinterpreter_aux

using state: var DspState

proc lri*(state; d: uint32) =
    writeReg d, fetchFollowingImm

proc andf*(state; acD: uint32) =
    state.status.lz = (midAccum(acD) and fetchFollowingImm) == 0

proc clr*(state; acR, extra: uint32) =
    writeAc acR, 0
    setAcFlags 0'i64

proc mrr*(state; d, s: uint32) =
    writeReg d, readReg(s)