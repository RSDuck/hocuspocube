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

    let
        leaveBlock = IrBasicBlock()
        entryPointBlock = IrBasicBlock()
        mainBlock = IrBasicBlock()


    #echo &"dsp block {builder.regs.pc:04X}"
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
            #echo &"loop end/too many cycles {builder.regs.cycles}"
            discard builder.unop(dispatchExternalDsp, builder.imm(builder.regs.pc))
            break

    # this is not ideal because it might hide errors
    # but it should work
    if (let instr = fn.getInstr(builder.instrs[^1]); instr.kind == dspCallInterpreter):
        discard builder.unop(dispatchExternalDsp, builder.loadctx(ctxLoad16, uint32 offsetof(DspState, pc)))

    let
        lastInstr = builder.instrs.pop()
        oldCycles = builder.loadctx(ctxLoadU32, uint32 offsetof(DspState, negativeCycles))
        newCycles = builder.biop(iAdd, oldCycles, builder.imm(uint32(numInstrs)))
    builder.storectx(ctxStore32, uint32 offsetof(DspState, negativeCycles), newCycles)
    builder.instrs.add lastInstr

    mainBlock.instrs = move builder.instrs

    block:
        builder.storectx(ctxStore16, uint32 offsetof(DspState, pc), builder.imm(blockAdr))
        discard builder.zeroop(leaveJitDsp)

        leaveBlock.instrs = move builder.instrs

    block:
        let
            oldCycles = builder.loadctx(ctxLoadU32, uint32 offsetof(DspState, negativeCycles))
            sliceNotDone = builder.biop(iCmpLessS, oldCycles, builder.imm(0))
        builder.funcInternBranch(sliceNotDone, mainBlock, leaveBlock)

        entryPointBlock.instrs = move builder.instrs

    fn.blocks.add leaveBlock
    fn.blocks.add entryPointBlock
    fn.blocks.add mainBlock

    fn.transformIdleLoopDsp(mainBlock, instrReadWrites, blockAdr)
    fn.ctxLoadStoreEliminiate()
    fn.removeIdentities()
    fn.mergeExtractEliminate()
    fn.removeIdentities()
    fn.dspOpts()
    fn.foldConstants()
    fn.removeIdentities()
    fn.removeDeadCode()
    #echo "dsp block (after op): \n", prettify(mainBlock, fn)
    fn.calcLiveIntervals()
    fn.verify()

    var entryPoints: seq[pointer]
    genCode(fn, entryPoints = entryPoints)
    result = cast[BlockEntryFunc](entryPoints[1])
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

    #echo &"dsp run {mDspState.pc:04X}"
    codegenx64.dspRun(mDspState)
