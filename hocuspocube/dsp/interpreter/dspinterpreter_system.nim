import
    ../dspstate, ../dsp

using state: var DspState

proc halt*(state) =
    dspCsr.halt = true

proc nop*(state) =
    discard

proc sbclr*(state; i: uint32) =
    state.status = Status(uint16(state.status) and not(1'u16 shl (i + 6)))

proc sbset*(state; i: uint32) =
    state.status = Status(uint16(state.status) or (1'u16 shl (i + 6)))