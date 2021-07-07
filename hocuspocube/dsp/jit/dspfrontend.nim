import
    strformat, sets,

    ../../cycletiming,
    ../../util/jit/[ir, codegenx64],
    dspfrontendcommon,
    dspblockcache,

    ".."/[dsp, dspdef, dspcommon],

    dspjit_alu,
    dspjit_branch,
    dspjit_loadstore

proc undefinedInstr(state: var IrBlockBuilder[DspIrState], instr: uint16) =
    raiseAssert(&"undefined dsp instr {instr:02X}")

proc compileBlock(): BlockEntryFunc =
    var
        builder: IrBlockBuilder[DspIrState]
        cycles = 0'i32
        blockAdr = mDspState.pc

        numInstrs = 0

    builder.regs.pc = blockAdr
    builder.blk = IrBasicBlock()

    echo &"compiling dsp block {blockAdr:04X}"

    while not builder.regs.branch:
        builder.regs.instr = instrRead(builder.regs.pc)

        echo &"adding instr {builder.regs.instr:04X}"

        dspMainDispatch(builder.regs.instr, builder, undefinedInstr)

        cycles += int32 gekkoCyclesPerDspCycle

        if builder.regs.pc in loopEnds:
            break

        builder.regs.pc += 1
        numInstrs += 1

    #echo builder.blk

    result = cast[BlockEntryFunc](genCode(builder.blk, cycles, false, false))
    blockEntries[mapBlockEntryAdr(blockAdr)] = result

proc dspRun*(timestamp: var int64, target: int64) =
    runPeripherals()
    handleReset()
    handleExceptions()

    if dspCsr.halt or dspCsr.busyCopying:
        timestamp = target
        #echo &"skipping dsp slice {dspCsr.halt} {dspCsr.busyCopying}"
        return

    while timestamp < target:
        handleExceptions()

        let entryPoint = blockEntries[mapBlockEntryAdr(mDspState.pc)]

        #echo &"dsp iteration {repr(entryPoint)} {mDspState.pc:04X} {mapBlockEntryAdr(mDspState.pc):04X} {repr(mDspState)}"

        let cycles =
            if likely(entryPoint != nil):
                entryPoint(addr mDspState)
            else:
                compileBlock()(addr mDspState)

        if likely(cycles != -1):
            timestamp += cycles
        else:
            #echo "skipping idle loop!"
            timestamp = target

        handleLoopStack(0xFFFF'u16)

        if timestamp >= target or dspCsr.halt:
            #echo &"dsp slice, halted: {dspCsr.halt} pc: {mDspState.pc:04X} {repr(mDspState)}"
            return