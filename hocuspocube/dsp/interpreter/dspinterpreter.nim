import
    ../dsp, ../dspdef, ../dspstate,

    ../../cycletiming,

    dspinterpreter_alu,
    dspinterpreter_branch,
    dspinterpreter_loadstore,
    dspinterpreter_system,

    strformat, stew/endians2

proc undefinedInstr(state: var DspState, instr: uint16) =
    echo &"undefined dsp instr {instr:04X} at {state.pc:X}"

var logstuff = false

proc handleExceptions() =
    if mDspState.status.et:
        if mDspState.status.te3:
            if dspCsr.piint:
                mDspState.callStack.push(mDspState.pc)
                mDspState.statusStack.push(uint16 mDspState.status)
                mDspState.pc = 0x000E'u16

                #echo "piint!"

                dspCsr.piint = false
                dspCsr.halt = false

            # put accelerator overflow etc. here

proc dspRun*(timestamp: var int64, target: int64) =
    runPeripherals()

    # is reset affected by global interrupt enable?
    # also does a reset reset any registers (including the stack)?
    # apparently it does, otherwise the stack overflows (because there's still stuff on it)
    if dspCsr.reset:
        dspCsr.reset = false
        echo "resetting to rom init vector"
        mDspState = default(DspState)
        # necessary for the for the AX ucode to work
        # Dolphin does this too, needs to be tested
        for i in 0..<4:
            mDspState.r[dspRegWrap0.succ(i)] = 0xFFFF'u16
        mDspState.pc = IRomStartAdr

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
        timestamp += gekkoCyclesPerDspCycle

        #if mDspState.pc < 0x8000 and mDspState.pc > 0x0ba4:
        #    echo &"bad pc from {prevPc:04X}"

        if timestamp >= target or dspCsr.halt:
            #echo &"dsp slice, halted: {dspCsr.halt} pc: {mDspState.pc:04X}"
            return