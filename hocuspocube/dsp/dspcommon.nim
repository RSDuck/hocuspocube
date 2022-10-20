import
    strformat,
    dspstate, dsp, jit/dspblockcache

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

proc nextPc*(pc: uint16): uint16 =
    result = pc
    while mDspState.loopAddrStack.sp > 0 and
            mDspState.loopAddrStack.peek() == result:

        mDspState.loopCountStack.peek() -= 1

        if mDspState.loopCountStack.peek() == 0:
            discard mDspState.callStack.pop()
            discard mDspState.loopAddrStack.pop()
            discard mDspState.loopCountStack.pop()
        else:
            result = mDspState.callStack.peek() - 1

    result += 1

proc compileBlockDsp(adr: uint16): dspblockcache.BlockEntryFunc {.importc: "compileBlockDsp".}

proc nextBlock*(pc: uint16): BlockEntryFunc =
    let pc = nextPc(pc-1)
    result = lookupBlock(pc)
    if unlikely(result == nil):
        result = compileBlockDsp(pc)
