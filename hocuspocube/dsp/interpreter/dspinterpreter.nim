import
    ".."/[dsp, dspdef, dspstate, dspcommon],

    dspinterpreter_alu,
    dspinterpreter_branch,
    dspinterpreter_loadstore,

    ../../cycletiming,

    strformat

proc undefinedInstr(state: var DspState, instr: uint16) =
    echo &"undefined dsp instr {instr:04X} at {state.pc:X}"

proc dspRun*() =
    runPeripherals()
    handleReset()
    handleExceptions()

    if dspCsr.halt or dspCsr.busyCopying:
        if mDspState.negativeCycles < 0:
            mDspState.negativeCycles = 0
        return

    while true:
        {.computedGoto.}

        handleExceptions()

        let instr = instrRead(mDspState.pc)

        dspMainDispatch instr, mDspState, undefinedInstr

        handleLoopStack()

        mDspState.pc += 1
        mDspState.negativeCycles += int32 gekkoCyclesPerDspCycle

        if mDspState.negativeCycles >= 0:
            #echo &"dsp slice, halted: {dspCsr.halt} pc: {mDspState.pc:04X}"
            return
