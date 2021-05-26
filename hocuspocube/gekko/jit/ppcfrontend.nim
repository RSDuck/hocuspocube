
import
    options, stew/endians2, strformat,
    ../gekko, ../ppcdef, ../ppcstate, ../memory,
    ir, ppcfrontendcommon, ../ppccommon,
    codegenx64,

    ppcjit_int,
    ppcjit_loadstore,
    ppcjit_float,
    ppcjit_branch,
    ppcjit_system

var
    blockEntries: array[(0x1800000 + 0x900) div 4, BlockEntryFunc]

proc undefinedInstr(builder: var IrBlockBuilder[PpcIrRegState], instr: uint32) =
    raiseAssert(&"undefined instruction {toBE(instr):08X} at {builder.regs.pc:08X}")

proc mapBlockEntryAdr(adr: uint32): uint32 =
    if adr >= 0xFFF00000'u32:
        (adr - 0xFFF00000'u32 + 0x1800000) div 4
    else:
        adr div 4

proc compileBlock(): BlockEntryFunc =
    var
        builder: IrBlockBuilder[PpcIrRegState]
        cycles = 0'i32

        blockAdr = gekkoState.translateInstrAddr(gekkoState.pc).get

        instrIndexes: seq[int32]
    builder.blk = IrBasicBlock()
    builder.regs.pc = gekkoState.pc

    while not builder.regs.branch:
        let instr = fromBE readCode(gekkoState.translateInstrAddr(builder.regs.pc).get)
        builder.regs.instr = instr

        #echo &"instr {toBE(instr):08X}"

        instrIndexes.add int32(builder.blk.instrs.len)
        dispatchPpc(instr, builder, undefinedInstr)

        builder.regs.pc += 4
        cycles += 1

        if cycles >= 64:
            discard builder.triop(irInstrBranch, builder.imm(true), builder.imm(builder.regs.pc), builder.imm(0))
            break

    let isIdleLoop = builder.blk.checkIdleLoop(instrIndexes, gekkoState.pc, builder.regs.pc)

    #echo &"block {gekkoState.pc:08X} is idle loop: {isIdleLoop}"
    #echo "preopt\n", builder.blk

    builder.blk.ctxLoadStoreEliminiate()
    builder.blk.removeIdentities()
    builder.blk.foldConstants()
    builder.blk.removeIdentities()
    builder.blk.removeDeadCode()
    builder.blk.calcLiveIntervals()
    builder.blk.verify()

    #echo "postopt\n", builder.blk

    result = genCode(builder.blk, cycles, builder.regs.floatInstr, isIdleLoop)

    blockEntries[mapBlockEntryAdr(blockAdr)] = result

proc gekkoRun*(timestamp: var int64, target: var int64) =
    while timestamp < target:
        if gekkoState.pendingExceptions != {}:
            handleExceptions()

        let entryPoint = blockEntries[mapBlockEntryAdr(gekkoState.translateInstrAddr(gekkoState.pc).get)]

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

        if timestamp >= target:
            return
