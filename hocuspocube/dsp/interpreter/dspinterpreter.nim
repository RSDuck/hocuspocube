import
    ".."/[dsp, dspdef, dspstate, dspcommon],

    ../../cycletiming,

    dspinterpreter_alu,
    dspinterpreter_branch,
    dspinterpreter_loadstore,

    strformat

proc undefinedInstr(state: var DspState, instr: uint16) =
    echo &"undefined dsp instr {instr:04X} at {state.pc:X}"

proc dspRun*(timestamp: var int64, target: int64) =
    runPeripherals()
    handleReset()
    handleExceptions()

    if dspCsr.halt or dspCsr.busyCopying:
        timestamp = target
        return

    while true:
        {.computedGoto.}

        handleExceptions()

        let instr = instrRead(mDspState.pc)

        dspMainDispatch instr, mDspState, undefinedInstr

        handleLoopStack()

        mDspState.pc += 1
        timestamp += gekkoCyclesPerDspCycle

        if timestamp >= target or dspCsr.halt:
            #echo &"dsp slice, halted: {dspCsr.halt} pc: {mDspState.pc:04X}"
            return