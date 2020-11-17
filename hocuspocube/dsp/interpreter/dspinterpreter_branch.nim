import
    ../dspstate, ../dsp,
    dspinterpreter_aux

using state: var DspState

template setupLoop(count, lastInstr: uint16) {.dirty.} =
    if count > 0:
        state.callStack.push state.pc + 1
        state.loopAddrStack.push lastInstr
        state.loopCountStack.push count
    else:
        state.pc = lastInstr

proc loop*(state; r: uint32) =
    let count = readReg(r)
    setupLoop count, state.pc + 1

proc bloop*(state; r: uint32) =
    let
        count = readReg(r)
        lastInstr = fetchFollowingImm
    setupLoop count, lastInstr

proc jmp*(state; cc: uint32) =
    let dst = fetchFollowingImm
    if conditionHolds(cc):
        state.pc = dst - 1