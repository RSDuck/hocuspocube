
import
    stew/endians2, strformat,
    ".."/[gekko, ppcdef, memory, ppccommon],
    ../../util/jit/[ir, codegenx64, iropt],
    ../../util/instrdecoding,
    ppcfrontendcommon, gekkoblockcache,
    ../ppcstate,
    ../fastmem,

    ppcjit_int,
    ppcjit_loadstore,
    ppcjit_float,
    ppcjit_branch,
    ppcjit_system

proc undefinedInstr(builder: var IrBlockBuilder[PpcIrRegState], instr: uint32) =
    raiseAssert(&"undefined instruction {toBE(instr):08X} at {builder.regs.pc:08X}")

proc compileBlock(): BlockEntryFunc {.exportc: "compileBlockPpc", used.} =
    let
        blockAdr = gekkoState.translateInstrAddr(gekkoState.pc)
        dataTranslation = gekkoState.msr.dr
        fn = IrFunc()
    var
        builder = IrBlockBuilder[PpcIrRegState](fn: fn)
        cycles = 0'i32

    builder.regs.pc = gekkoState.pc

    while not builder.regs.branch:
        let instr = fromBE readCode(gekkoState.translateInstrAddr(builder.regs.pc))
        builder.regs.instr = instr

        #echo &"instr {toBE(instr):08X}"

        dispatchPpc(instr, builder, undefinedInstr)

        builder.regs.pc += 4
        cycles += 1

        if cycles >= 64 and not builder.regs.branch:
            discard builder.triop(ppcBranch, builder.imm(true), builder.imm(builder.regs.pc), builder.imm(0))
            break

    let blk = IrBasicBlock(instrs: move builder.instrs)
    fn.blocks.add blk

    var flags: set[GenCodeFlags]
    if fn.checkIdleLoopPpc(blk, gekkoState.pc):
        flags.incl idleLoop
    if builder.regs.floatInstr:
        flags.incl fexception

    #echo &"block {gekkoState.pc:08X} {cycles} is idle loop: {idleLoop in flags}"
    #echo "preopt\n", builder.blk

    fn.ctxLoadStoreEliminiate()
    fn.removeIdentities()
    fn.mergeExtractEliminate()
    fn.removeIdentities()
    fn.floatOpts()
    fn.foldConstants()
    fn.removeIdentities()
    #fn.globalValueEnumeration()
    fn.removeDeadCode()
    fn.calcLiveIntervals()
    fn.verify()

    #echo "postopt\n", builder.blk

    result = cast[BlockEntryFunc](genCode(fn,
        cycles,
        flags,
        if dataTranslation: translatedAdrSpace else: physicalAdrSpace))

    setBlock blockAdr, result

proc gekkoRun*() =
    handleExceptions(gekkoState)
    ppcRun(gekkoState)
