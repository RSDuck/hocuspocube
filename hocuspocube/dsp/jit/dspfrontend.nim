import
    strformat,

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

proc compileBlock(): BlockEntryFunc {.exportc: "compileBlockDsp".} =
    let
        blockAdr = mDspState.pc
        fn = IrFunc()
    var
        builder = IrBlockBuilder[DspIrState](fn: fn)

        numInstrs = 0

        instrReadWrites: seq[tuple[read, write: set[DspReg]]]

    builder.regs.pc = blockAdr

    while not builder.regs.branch:
        builder.regs.instr = instrRead(builder.regs.pc)
        #echo &"dsp instr {builder.regs.instr:04X}"

        dspMainDispatch(builder.regs.instr, builder, undefinedInstr)

        instrReadWrites.add (builder.regs.regsRead, builder.regs.regsWritten)
        reset builder.regs.regsRead
        reset builder.regs.regsWritten

        builder.regs.cycles += int32 gekkoCyclesPerDspCycle

        builder.regs.pc += 1
        numInstrs += 1

        if isLoopEnd(builder.regs.pc-1) or builder.regs.cycles >= 128 and not builder.regs.branch:
            discard builder.triop(dspBranch, builder.imm(true), builder.imm(builder.regs.pc), builder.imm(0))
            break

    let blk = IrBasicBlock(instrs: move builder.instrs)
    fn.blocks.add(blk)

    var flags = {GenCodeFlags.dsp}
    if fn.checkIdleLoopDsp(blk, instrReadWrites, blockAdr):
        flags.incl idleLoop
    #echo &"dsp block {blockAdr:04X} (is idle loop: {idleLoop in flags})"
    fn.ctxLoadStoreEliminiate()
    fn.removeIdentities()
    fn.mergeExtractEliminate()
    fn.removeIdentities()
    fn.dspOpts()
    fn.foldConstants()
    fn.removeIdentities()
    fn.removeDeadCode()
    #echo "dsp block (after op): \n", builder.blk
    fn.calcLiveIntervals()
    fn.verify()

    result = cast[BlockEntryFunc](genCode(fn, builder.regs.cycles, flags))
    setBlock blockAdr, result

proc dspRun*() =
    runPeripherals()
    handleReset()
    handleExceptions()

    if dspCsr.halt or dspCsr.busyCopying:
        if mDspState.negativeCycles < 0:
            mDspState.negativeCycles = 0
        #echo &"skipping dsp slice {dspCsr.halt} {dspCsr.busyCopying}"
        return

    codegenx64.dspRun(mDspState)
