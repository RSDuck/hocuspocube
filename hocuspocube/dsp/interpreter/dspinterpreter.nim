import
    ../dsp, ../dspdef, ../dspstate,
    ../dspperipherals,

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

    if dspCsr.halt:
        return

    while true:
        {.computedGoto.}

        let instr = instrRead(theDspState.pc)
        dspMainDispatch instr, theDspState, undefinedInstr

        while theDspState.loopAddrStack.sp > 0 and
            theDspState.loopAddrStack.peek() == theDspState.pc:

            theDspState.loopCountStack.peek() -= 1

            if theDspState.loopCountStack.peek() == 0:
                discard theDspState.callStack.pop()
                discard theDspState.loopAddrStack.pop()
                discard theDspState.loopCountStack.pop()
            else:
                theDspState.pc = theDspState.callStack.peek() - 1

        theDspState.pc += 1
        timestamp += geckoCyclesPerDspCycle

        if timestamp >= target or dspCsr.halt:
            echo "dsp slice, halted:", dspCsr.halt
            return