import
    dspstate, dsp

proc handleExceptions*() =
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

proc handleReset*() =
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
            mDspState.wrapReg[i] = 0xFFFF'u16
        mDspState.pc = IRomStartAdr

proc handleLoopStack*(offset: uint16 = 0) =
    while mDspState.loopAddrStack.sp > 0 and
        mDspState.loopAddrStack.peek() == (mDspState.pc + offset):

        mDspState.loopCountStack.peek() -= 1

        if mDspState.loopCountStack.peek() == 0:
            discard mDspState.callStack.pop()
            discard mDspState.loopAddrStack.pop()
            discard mDspState.loopCountStack.pop()
        else:
            mDspState.pc = mDspState.callStack.peek() - 1 - offset

proc postBlock*() =
    if dspCsr.halt:
        if mDspState.negativeCycles < 0:
            mDspState.negativeCycles = 0

    handleLoopStack(0xFFFF'u16)
    handleExceptions()
