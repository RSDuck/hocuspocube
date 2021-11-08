import
    strformat, sets,

    ../../cycletiming,
    ../../util/jit/[ir, codegenx64, iropt],
    dspfrontendcommon,
    dspblockcache,

    ".."/[dsp, dspdef, dspcommon, dspstate],

    dspjit_alu,
    dspjit_branch,
    dspjit_loadstore

proc undefinedInstr(state: var IrBlockBuilder[DspIrState], instr: uint16) =
    raiseAssert(&"undefined dsp instr {instr:02X}")

proc compileBlock(): BlockEntryFunc =
    let blockAdr = mDspState.pc
    var
        builder: IrBlockBuilder[DspIrState]

        numInstrs = 0

        instrReadWrites: seq[tuple[read, write: set[DspReg]]]

    builder.regs.pc = blockAdr
    builder.blk = IrBasicBlock()

    while not builder.regs.branch:
        builder.regs.instr = instrRead(builder.regs.pc)

        dspMainDispatch(builder.regs.instr, builder, undefinedInstr)

        instrReadWrites.add (builder.regs.regsRead, builder.regs.regsWritten)
        reset builder.regs.regsRead
        reset builder.regs.regsWritten

        builder.regs.cycles += int32 gekkoCyclesPerDspCycle

        builder.regs.pc += 1
        numInstrs += 1

        if (builder.regs.pc-1 in loopEnds) or builder.regs.cycles >= 128:
            discard builder.triop(dspBranch, builder.imm(true), builder.imm(builder.regs.pc), builder.imm(0))
            break

    let isIdleLoop = builder.blk.checkIdleLoopDsp(instrReadWrites, blockAdr)
    #echo &"dsp block {blockAdr:04X} (is idle loop: {isIdleLoop}): \n", builder.blk
    builder.blk.ctxLoadStoreEliminiate()
    builder.blk.removeIdentities()
    builder.blk.mergeExtractEliminate()
    builder.blk.removeIdentities()
    builder.blk.dspOpts()
    builder.blk.foldConstants()
    builder.blk.removeIdentities()
    builder.blk.removeDeadCode()
    #echo "dsp block (after op): \n", builder.blk
    builder.blk.calcLiveIntervals()
    builder.blk.verify()

    result = cast[BlockEntryFunc](genCode(builder.blk, builder.regs.cycles, false, isIdleLoop))
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