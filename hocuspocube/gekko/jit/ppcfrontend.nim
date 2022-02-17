
import
    options, stew/endians2, strformat,
    ".."/[gekko, ppcdef, memory, ppccommon],
    ../../util/jit/[ir, codegenx64, iropt],
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

proc compileBlock(): BlockEntryFunc =
    let
        blockAdr = gekkoState.translateInstrAddr(gekkoState.pc)
        dataTranslation = gekkoState.msr.dr
    var
        builder: IrBlockBuilder[PpcIrRegState]
        cycles = 0'i32

    builder.blk = IrBasicBlock()
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

    let isIdleLoop = builder.blk.checkIdleLoopPpc(gekkoState.pc)

    #echo &"block {gekkoState.pc:08X} is idle loop: {isIdleLoop}"
    #echo "preopt\n", builder.blk

    builder.blk.ctxLoadStoreEliminiate()
    builder.blk.removeIdentities()
    builder.blk.mergeExtractEliminate()
    builder.blk.removeIdentities()
    builder.blk.floatOpts()
    builder.blk.foldConstants()
    builder.blk.removeIdentities()
    #builder.blk.globalValueEnumeration()
    builder.blk.removeDeadCode()
    builder.blk.calcLiveIntervals()
    builder.blk.verify()

    #echo "postopt\n", builder.blk

    result = cast[BlockEntryFunc](genCode(builder.blk,
        cycles,
        builder.regs.floatInstr,
        isIdleLoop,
        if dataTranslation: translatedAdrSpace else: physicalAdrSpace))

    blockEntries[mapBlockEntryAdr(blockAdr)] = result

proc gekkoRun*(timestamp: var int64, target: var int64) =
    while timestamp < target:
        if gekkoState.pendingExceptions != {}:
            handleExceptions()

        let entryPoint = blockEntries[mapBlockEntryAdr(gekkoState.translateInstrAddr(gekkoState.pc))]

        let cycles =
            if likely(entryPoint != nil):
                entryPoint(addr gekkoState)
            else:
                compileBlock()(addr gekkoState)

        if likely(cycles != -1):
            timestamp += cycles
        else:
            #echo "skipping idle loop!"
            timestamp = target

    #echo &"slice finished {gekkoState.pc:08X}"
