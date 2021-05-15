
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
    blockEntries: array[0x1800000 div 4, BlockEntryFunc]

proc undefinedInstr(builder: var IrBlockBuilder[PpcIrRegState], instr: uint32) =
    raiseAssert(&"undefined instruction {toBE(instr):08X} at {builder.regs.pc:08X}")

proc compileBlock(): BlockEntryFunc =
    var
        builder: IrBlockBuilder[PpcIrRegState]
        cycles = 0'i32

        blockAdr = gekkoState.translateInstrAddr(gekkoState.pc).get
    builder.blk = IrBasicBlock()
    builder.regs.pc = gekkoState.pc

    #echo &"pc: {uint32(gekkoState.pc):08X}"

    while not builder.regs.branch:
        let instr = fromBE readCode(gekkoState.translateInstrAddr(builder.regs.pc).get)

        #echo &"instr {toBE(instr):08X}"

        dispatchPpc(instr, builder, undefinedInstr)

        builder.regs.pc += 4
        cycles += 1

        if cycles >= 64#[or builder.regs.pc == 0x8000cc64'u32]#:
            discard builder.triop(irInstrBranch, builder.imm(true), builder.imm(builder.regs.pc), builder.imm(0))
            break

    builder.blk.calcLiveIntervals()

    #echo "made block: \n", builder.blk

    result = genCode(builder.blk, cycles)

    blockEntries[blockAdr] = result

proc gekkoRun*(timestamp: var int64, target: var int64) =
    while timestamp < target:
        if gekkoState.pendingExceptions != {}:
            handleExceptions()

        let entryPoint = blockEntries[gekkoState.translateInstrAddr(gekkoState.pc).get]

        if likely(entryPoint != nil):
            timestamp += entryPoint(addr gekkoState)
        else:
            timestamp += compileBlock()(addr gekkoState)

        if timestamp >= target:
            return
