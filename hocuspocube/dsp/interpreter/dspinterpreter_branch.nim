import
    ../dspstate, ../dsp,
    dspinterpreter_aux,
    strformat

using state: var DspState

proc setupLoop(state; count, lastInstr: uint16) =
    if count > 0:
        state.callStack.push state.pc + 1
        state.loopAddrStack.push lastInstr
        state.loopCountStack.push count
    else:
        state.pc = lastInstr

proc jmp*(state; cc: uint16) =
    let dst = fetchFollowingImm
    if state.conditionHolds(cc):
        state.pc = dst - 1

proc jmpr*(state; r, cc: uint16) =
    if state.conditionHolds(cc):
        echo &"jump by address to {adrReg(int r):04X}"
        state.pc = adrReg(int r) - 1

proc call*(state; cc: uint16) =
    let dst = fetchFollowingImm
    if state.conditionHolds(cc):
        echo &"calling {dst:04X} {state.pc:04X}"
        state.callStack.push(state.pc + 1)
        state.pc = dst - 1

proc callr*(state; r, cc: uint16) =
    raiseAssert "unimplemented dsp instr"

proc rets*(state; cc: uint16) =
    if state.conditionHolds(cc):
        state.pc = state.callStack.pop() - 1
        echo &"returning to {state.pc+1:04X}"
    else:
        echo &"return skipped! {state.pc:04X} {cc}"

proc reti*(state; cc: uint16) =
    raiseAssert &"unimplemented dsp instr {state.pc:04X}"

proc exec*(state; cc: uint16) =
    raiseAssert "unimplemented dsp instr"

proc loopi*(state; c: uint16) =
    let lastInstr = fetchFollowingImm
    state.setupLoop c, lastInstr

proc loop*(state; r: uint16) =
    let
        count = state.readReg(DspReg r)
        lastInstr = fetchFollowingImm
    state.setupLoop count, lastInstr

proc repi*(state; c: uint16) =
    state.setupLoop c, state.pc + 1

proc rep*(state; r: uint16) =
    let count = state.readReg(DspReg r)
    state.setupLoop count, state.pc + 1
