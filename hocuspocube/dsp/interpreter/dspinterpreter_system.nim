import
    ../dspstate, ../dsp

using state: var DspState

proc trap*(state) =
    raiseAssert "unimplemented dsp instr"

proc wait*(state) =
    dspCsr.halt = true
