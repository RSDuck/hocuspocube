import
    ../dspstate, ../dsp, ../../util/aluhelper,
    dspinterpreter_aux

using state: var DspState

proc lr*(state; d: uint32) =
    writeReg d, dataRead(fetchFollowingImm)

proc lrri*(state; s, d: uint32) =
    writeReg d, dataRead(addrReg s)
    (addrReg s) += 1

proc srri*(state; d, s: uint32) =
    dataWrite addrReg(d), readReg(s)
    (addrReg d) += 1

proc sr*(state; s: uint32) =
    dataWrite fetchFollowingImm, readReg(s)

proc si*(state; m: uint32) =
    dataWrite signExtend(uint16(m), 8), fetchFollowingImm

proc ilrr*(state; acD, arS: uint32) =
    midAccum(acD) = instrRead(addrReg arS)

proc ilrri*(state; acD, arS: uint32) =
    midAccum(acD) = instrRead(addrReg arS)
    (addrReg arS) += 1
