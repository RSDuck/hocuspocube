import
    ".."/[dsp, dspdef, dspstate, dspcommon],

    ../../cycletiming,

    dspinterpreter_alu,
    dspinterpreter_branch,
    dspinterpreter_loadstore,

    strformat, stew/endians2

proc undefinedInstr(state: var DspState, instr: uint16) =
    echo &"undefined dsp instr {instr:04X} at {state.pc:X}"

var logstuff = false

proc dspRun*(timestamp: var int64, target: int64) =
    runPeripherals()
    handleReset()
    handleExceptions()

    if dspCsr.halt or dspCsr.busyCopying:
        timestamp = target
        #echo &"skipping dsp slice {dspCsr.halt} {dspCsr.busyCopying}"
        return

    while true:
        {.computedGoto.}

        handleExceptions()

        #let prevPc = mDspState.pc

        let instr = instrRead(mDspState.pc)

#[        if mDspState.pc == 0x05e3'u16 and instr == 0x00d8'u16:
            logstuff = true
        if mDspState.pc == 0x05fd'u16:
            logstuff = false

        if logStuff:
            echo &"dspstate {mDspState.pc:02X} {uint16(mDspState.status):02X} {instr:04X}"
            for i in 0..<32:
                echo &"r {i}: {mDspState.r[DspReg(i)]:02X}"]#

        dspMainDispatch instr, mDspState, undefinedInstr

        handleLoopStack()

        mDspState.pc += 1
        timestamp += gekkoCyclesPerDspCycle

        #if mDspState.pc < 0x8000 and mDspState.pc > 0x0ba4:
        #    echo &"bad pc from {prevPc:04X}"

        if timestamp >= target or dspCsr.halt:
            #echo &"dsp slice, halted: {dspCsr.halt} pc: {mDspState.pc:04X}"
            return