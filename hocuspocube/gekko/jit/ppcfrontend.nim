
import
    stew/endians2, strformat, algorithm,
    ../../util/aluhelper,
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

type
    PreliminaryBlock = object
        startAdr: uint32
        instrs: seq[uint32]
        branches: bool

proc endAdr(blk: PreliminaryBlock): uint32 =
    blk.startAdr + uint32(blk.instrs.len) * 4

proc `$`(blk: PreliminaryBlock): string =
    result = &"block ({blk.startAdr:08X}):\n"
    for instr in blk.instrs:
        result &= &"  {fromBE(instr):08X}\n"

proc compileBlock(): BlockEntryFunc {.exportc: "compileBlockPpc", used.} =
    #[
        we assume a function:
            - has a single entry point
            - lies continously in memory
                (no constants/variables in between)

        Kind of "inspired" by:
        https://github.com/Ryujinx/Ryujinx/blob/master/ARMeilleure/Decoders/Decoder.cs
    ]#

    var
        looseEnds = @[gekkoState.pc]
        blocks: seq[PreliminaryBlock]

    while looseEnds.len > 0:
        var
            pc = looseEnds.pop()
            blk = PreliminaryBlock(startAdr: pc)
            nearestBlockStart = high(uint32)

        block withinExistingBlock:
            for oldBlk in mitems blocks:
                if pc == oldBlk.startAdr:
                    # just two branches to the same address
                    break withinExistingBlock
                elif pc > oldBlk.startAdr and pc < oldBlk.endAdr:
                    # split block
                    let instrOffset = int(pc-oldBlk.startAdr) div 4
                    blk.instrs.setLen oldBlk.instrs.len-instrOffset
                    for i in 0..<blk.instrs.len:
                        blk.instrs[i] = oldBlk.instrs[instrOffset+i]
                    oldBlk.instrs.setLen instrOffset
                    blk.branches = oldBlk.branches
                    oldBlk.branches = false
                    blocks.add blk

                    break withinExistingBlock
                elif oldBlk.startAdr > pc:
                    nearestBlockStart = min(nearestBlockStart, oldBlk.startAdr)

            while not blk.branches and pc < nearestBlockStart:
                let
                    instr = fromBE readCode(gekkoState.translateInstrAddr(pc))
                    nia = pc + 4

                template branchesAlways(val: uint32): bool =
                    (val and 0b10100) == 0b10100

                matchSparseInstrs(instr, PpcPatternsTbl):
                of "bx", li, aa, lk:
                    if lk == 0:
                        let targetAdr = (if aa == 0: pc else: 0'u32) + (signExtend(li, 24) shl 2)
                        # if it's jumping above the start of the function it's a tail call
                        if targetAdr >= gekkoState.pc:
                            looseEnds.add(targetAdr)
                    else:
                        looseEnds.add(nia)
                    blk.branches = true
                of "bcx", bo, bi, bd, aa, lk:
                    if not branchesAlways(bo) or lk != 0:
                        looseEnds.add(pc + 4)
                    if lk == 0:
                        let targetAdr = (if aa == 0: pc else: 0'u32) + signExtend(bd, 14) shl 2
                        if targetAdr >= gekkoState.pc:
                            looseEnds.add(targetAdr)
                    blk.branches = true
                of "bcctrx", bo, bi, lk:
                    if not branchesAlways(bo) or lk != 0:
                        looseEnds.add(nia)
                    blk.branches = true
                of "bclrx", bo, bi, lk:
                    #echo "bclrx ", branchesAlways(bo), " ", lk
                    if not branchesAlways(bo) or lk != 0:
                        looseEnds.add(nia)
                    blk.branches = true
                of "isync":
                    blk.branches = true
                of "sync":
                    blk.branches = true
                of "rfi":
                    blk.branches = true
                of "sc":
                    blk.branches = true

                blk.instrs.add instr
                pc = nia

            blocks.add(blk)

    #echo "function before tail call prunning:"
    #echo blocks

    # eliminate tail calls
    block:
        blocks.sort() do (a, b: PreliminaryBlock) -> int:
            cmp(a.startAdr, b.startAdr)

        for i in 1..<blocks.len:
            if blocks[i].startAdr != blocks[i-1].endAdr:
                blocks.setLen i
                break


    #echo "after:"
    #echo blocks

    for preblk in blocks:
        let
            blockAdr = gekkoState.translateInstrAddr(preblk.startAdr)
            dataTranslation = gekkoState.msr.dr
            fn = IrFunc()
        var builder = IrBlockBuilder[PpcIrRegState](fn: fn)
        builder.regs.pc = preblk.startAdr

        for instr in preblk.instrs:
            #echo &"instr {toBE(instr):08X}"
            builder.regs.instr = instr
            dispatchPpc(instr, builder, undefinedInstr)
            builder.regs.pc += 4

        if not preblk.branches:
            discard builder.triop(ppcBranch, builder.imm(true), builder.imm(builder.regs.pc), builder.imm(0))

        let blk = IrBasicBlock(instrs: move builder.instrs)
        fn.blocks.add blk

        var flags: set[GenCodeFlags]
        if fn.checkIdleLoopPpc(blk, preblk.startAdr):
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

        #echo "postopt\n", prettify(blk, fn)

        let entryPoint = cast[BlockEntryFunc](genCode(fn,
            int32 preblk.instrs.len,
            flags,
            if dataTranslation: translatedAdrSpace else: physicalAdrSpace))
        if preblk.startAdr == gekkoState.pc:
            result = entryPoint

        setBlock blockAdr, entryPoint

proc gekkoRun*() =
    handleExceptions(gekkoState)
    ppcRun(gekkoState)
