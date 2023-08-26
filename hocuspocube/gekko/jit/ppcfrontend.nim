
import
    stew/endians2, strformat, algorithm, tables, std/packedsets,
    ../../util/[aluhelper, bitstruct],
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

#[
    some thoughts on the function JIT as it's starting to take form:

    - See below for what constitutes as a function while scanning to the PPC code.

    - It is easier to put the cycle check followed by increasing
        the cycle counter at the beginning of blocks than at the end where control
        might diverge through conditional branches etc.

        Since the generated function can exit at any block, it has to be eable to resume
        from any block too. Once we get into optimisations such as lifting loads and stores
        across blocks or checking skipping float exceptions based on control flow dominance,
        we need to generate an external block entry point which loads in the appropriate
        variables/checks for FP exception.
]#

proc undefinedInstr(builder: var IrBlockBuilder[PpcIrRegState], instr: uint32) =
    raiseAssert(&"undefined instruction {toBE(instr):08X} at {builder.regs.pc:08X}")

type
    PreliminaryBlock = object
        startAdr: uint32
        instrs: seq[uint32]
        branches, isLoopHead: bool

proc endAdr(blk: PreliminaryBlock): uint32 =
    blk.startAdr + uint32(blk.instrs.len) * 4

proc `$`(blk: PreliminaryBlock): string =
    result = &"block ({blk.startAdr:08X}, branches: {blk.branches}, isLoopHead: {blk.isLoopHead}):\n"
    for instr in blk.instrs:
        result &= &"  {fromBE(instr):08X}\n"

proc compileBlock(funcAdr: uint32): BlockEntryFunc {.exportc: "compileBlockPpc", used.} =
    #[
        we assume a function:
            - has a single entry point
            - lies continously in memory
                (no constants/variables in between)

        Kind of "inspired" by:
        https://github.com/Ryujinx/Ryujinx/blob/master/ARMeilleure/Decoders/Decoder.cs
    ]#

    #echo &"starting func at {funcAdr:08X}"

    var
        looseEnds = @[funcAdr]
        blocks: seq[PreliminaryBlock]

    while looseEnds.len > 0:
        var
            pc = looseEnds.pop()
            blk = PreliminaryBlock(startAdr: pc)
            nearestBlockStart = high(uint32)
            nearestBlockIdx = -1

        block withinExistingBlock:
            for blkIdx, oldBlk in mpairs blocks:
                if pc == oldBlk.startAdr:
                    oldBlk.isLoopHead = true
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
                    blk.isLoopHead = true
                    oldBlk.branches = false
                    blocks.add blk

                    break withinExistingBlock
                elif oldBlk.startAdr > pc:
                    nearestBlockStart = min(nearestBlockStart, oldBlk.startAdr)
                    nearestBlockIdx = blkIdx

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
                        if targetAdr >= funcAdr:
                            looseEnds.add(targetAdr)
                    else:
                        looseEnds.add(nia)
                    blk.branches = true
                of "bcx", bo, bi, bd, aa, lk:
                    if not branchesAlways(bo) or lk != 0:
                        looseEnds.add(pc + 4)
                    if lk == 0:
                        let targetAdr = (if aa == 0: pc else: 0'u32) + signExtend(bd, 14) shl 2
                        if targetAdr >= funcAdr:
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
                else:
                    if nia == nearestBlockStart:
                        blocks[nearestBlockIdx].isLoopHead = true

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

    # always set the first block as loop head, so that there will be atleast one exit per function
    # TODO: calculate dominance hierachy (necessary for various things) and also use that to make this check optional
    blocks[0].isLoopHead = true

    let fn = IrFunc()
    var entryPoints: seq[(uint32, IrBasicBlock)]

    for preblk in blocks:
        var builder = IrBlockBuilder[PpcIrRegState](fn: fn)
        builder.regs.pc = preblk.startAdr

        #[
            What code we generate for each basic block:

                cyclesDone:
                    state.pc = #[block constant]#
                    return
                handleFpException:
                    handleFpException()

                headerBlk:
                    jmp cyclesDone, state.negativeCycles >= 0
                    state.negativeCycles += #[per block constant]#
                    jmp handleFpException, not state.msr.fp

                    #[actual block]#

            Code generation for syscall:
                handleSyscall()
                dispatchPpc state.pc
        ]#

        let mainBlk = IrBasicBlock()
        var
            checkFpExceptionBlk, doFpExceptionBlk: IrBasicBlock
            leaveBlock, cycleCheckBlk: IrBasicBlock

        # main block
        block:
            # add the cycles of the block
            let
                oldCycles = builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, negativeCycles))
                newCycles = builder.biop(iAdd, oldCycles, builder.imm(uint32(preblk.instrs.len)))
            builder.storectx(ctxStore32, uint32 offsetof(PpcState, negativeCycles), newCycles)

            # all the instructions
            for instr in preblk.instrs:
                #echo &"instr {toBE(instr):08X}"
                builder.regs.instr = instr
                dispatchPpc(instr, builder, undefinedInstr)
                builder.regs.pc += 4

            if not preblk.branches:
                builder.branchUncond(builder.imm(builder.regs.pc))

            mainBlk.instrs = move builder.instrs

        # leave block
        if preblk.isLoopHead:
            leaveBlock = IrBasicBlock()

            builder.storectx(ctxStore32, uint32 offsetof(PpcState, pc), builder.imm(preblk.startAdr))
            discard builder.zeroop(leaveJitPpc)

            leaveBlock.instrs = move builder.instrs

        if builder.regs.floatInstr:
            checkFpExceptionBlk = IrBasicBlock()
            doFpExceptionBlk = IrBasicBlock()
            # handle fp exception
            block:
                discard builder.unop(fpExceptionPpc, builder.imm(preblk.startAdr))
                doFpExceptionBlk.instrs = move builder.instrs
            # check for float exception
            block:
                let fpNotEnabled = builder.biop(iCmpEqual,
                        builder.biop(bitAnd,
                            builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, msr)),
                            builder.imm(getFieldMask[Msr](fp))),
                        builder.imm(0))

                builder.funcInternBranch(fpNotEnabled, doFpExceptionBlk, mainBlk)
                checkFpExceptionBlk.instrs = move builder.instrs

        # entry point
        if preblk.isLoopHead:
            cycleCheckBlk = IrBasicBlock()

            let
                oldCycles = builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, negativeCycles))
                sliceNotDone = builder.biop(iCmpLessS, oldCycles, builder.imm(0))

            builder.funcInternBranch(sliceNotDone,
                if checkFpExceptionBlk == nil:
                    mainBlk
                else:
                    checkFpExceptionBlk,
                leaveBlock)

            cycleCheckBlk.instrs = move builder.instrs

        if preblk.isLoopHead: fn.blocks.add leaveBlock
        if builder.regs.floatInstr: fn.blocks.add doFpExceptionBlk
        if preblk.isLoopHead: fn.blocks.add cycleCheckBlk
        if builder.regs.floatInstr: fn.blocks.add checkFpExceptionBlk
        fn.blocks.add mainBlk

        fn.transformIdleLoopsPpc(mainBlk, preblk.startAdr)

        entryPoints.add (preblk.startAdr,
            if preblk.isLoopHead: cycleCheckBlk
            elif builder.regs.floatInstr: checkFpExceptionBlk
            else: mainBlk)

    #echo &"block {funcAdr:08X}"
    #echo "preopt"
    #for blk in fn.blocks:
    #    echo repr(cast[pointer](blk))
    #    echo prettify(blk, fn)

    fn.resolveInnerFuncBranches(entryPoints)
    fn.ctxLoadStoreEliminiate()
    fn.removeIdentities()
    fn.mergeExtractEliminate()
    fn.removeIdentities()
    fn.floatOpts()
    fn.foldConstants()
    fn.removeIdentities()
#    fn.globalValueEnumeration()
    fn.removeDeadCode()
    fn.calcLiveIntervals()
    fn.verify()

    #echo "postopt"
    #for blk in fn.blocks:
    #    echo repr(cast[pointer](blk))
    #    echo prettify(blk, fn)

    let dataTranslation = gekkoState.msr.dr
    var entryPointAddrs: seq[pointer]
    genCode(fn,
        if dataTranslation: translatedAdrSpace else: physicalAdrSpace,
        entryPointAddrs)

    block:
        # this assumes we don't reorder the basic blocks
        # which for now is ok to do
        var
            i = 0
            j = 0
        while i < entryPoints.len:
            while fn.blocks[j] != entryPoints[i][1]:
                j += 1

            setBlock gekkoState.translateInstrAddr(entryPoints[i][0]), cast[BlockEntryFunc](entryPointAddrs[j])
            if entryPoints[i][0] == funcAdr:
                result = cast[BlockEntryFunc](entryPointAddrs[j])

            i += 1

proc gekkoRun*() =
    handleExceptions(gekkoState)
    ppcRun(gekkoState)
