import
    ../dsp, ../dspdef, ../dspstate,

    ../../cycletiming,

    dspinterpreter_alu,
    dspinterpreter_branch,
    dspinterpreter_loadstore,
    dspinterpreter_system,

    strformat

proc undefinedInstr(state: var DspState, instr: uint16) =
    echo &"undefined dsp instr {instr:04X} at {state.pc:X}"

proc dspRun*(timestamp: var int64, target: int64) =
    runPeripherals()

    # is reset affected by global interrupt enable?
    # also does a reset reset any registers (including the stack)?
    if dspCsr.reset:
        dspCsr.reset = false
        echo "resetting to rom init vector"
        mDspState.pc = IRomStartAdr

    if dspCsr.halt or dspCsr.busyCopying:
        return

    while true:
        {.computedGoto.}

        #[block triggeredInt:
            if theDspState.status.te:
                if theDspState.status.te3:
                    if dspCsr.piint:
                        theDspState.pc = ]#

        let prevPc = mDspState.pc

        let instr = instrRead(mDspState.pc)
        dspMainDispatch instr, mDspState, undefinedInstr

        while mDspState.loopAddrStack.sp > 0 and
            mDspState.loopAddrStack.peek() == mDspState.pc:

            mDspState.loopCountStack.peek() -= 1

            if mDspState.loopCountStack.peek() == 0:
                discard mDspState.callStack.pop()
                discard mDspState.loopAddrStack.pop()
                discard mDspState.loopCountStack.pop()
            else:
                mDspState.pc = mDspState.callStack.peek() - 1

        mDspState.pc += 1
        timestamp += geckoCyclesPerDspCycle

        #if mDspState.pc < 0x8000 and mDspState.pc > 0x0ba4:
        #    echo &"bad pc from {prevPc:04X}"

        if timestamp >= target or dspCsr.halt:
            #echo &"dsp slice, halted: {dspCsr.halt} pc: {mDspState.pc:04X}"
            return