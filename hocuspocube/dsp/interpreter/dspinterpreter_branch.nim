import
    ".."/[dspstate, dsp],
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
        #echo &"jump by address to {adrReg(int r):04X}"
        state.pc = state.adrReg[r] - 1

proc call*(state; cc: uint16) =
    let dst = fetchFollowingImm
    if state.conditionHolds(cc):
        #echo &"calling {dst:04X} {state.pc:04X}"
        state.callStack.push(state.pc + 1)
        state.pc = dst - 1
    else:
        discard
        #echo &"call skipped {state.pc:04X}"

proc callr*(state; r, cc: uint16) =
    if state.conditionHolds(cc):
        #echo &"calling (indirectly) {state.adrReg[r]:04X} {state.pc:04X}"
        state.callStack.push(state.pc + 1)
        state.pc = state.adrReg[r] - 1

proc rets*(state; cc: uint16) =
    if state.conditionHolds(cc):
        state.pc = state.callStack.pop() - 1
        #echo &"returning to {state.pc+1:04X}"
    else:
        #echo &"return skipped! {state.pc:04X} {cc}"
        discard

proc reti*(state; cc: uint16) =
    if state.conditionHolds(cc):
        state.pc = state.callStack.pop() - 1
        state.status = Status state.statusStack.pop()

proc exec*(state; cc: uint16) =
    if not state.conditionHolds(cc):
        state.pc += 1

proc loopi*(state; c: uint16) =
    let lastInstr = fetchFollowingImm
    if c > 0:
        state.setupLoop c, lastInstr
    else:
        state.pc = lastInstr

proc loop*(state; r: uint16) =
    let
        count = state.readReg(DspReg r)
        lastInstr = fetchFollowingImm

    if count > 0:
        state.setupLoop count, lastInstr
    else:
        state.pc = lastInstr

proc repi*(state; c: uint16) =
    if c > 0:
        state.setupLoop c, state.pc + 1
    else:
        state.pc += 1

proc rep*(state; r: uint16) =
    let count = state.readReg(DspReg r)
    if count > 0:
        state.setupLoop count, state.pc + 1
    else:
        state.pc += 1

# those two don't belong here 100%
# but it's not worth to have an entire module just for them
proc trap*(state) =
    raiseAssert "unimplemented dsp instr trap"

proc wait*(state) =
    dspCsr.halt = true
    if state.negativeCycles < 0: state.negativeCycles = 0
