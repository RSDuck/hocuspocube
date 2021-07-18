import
    options, bitops, algorithm, strformat,
    stew/bitseqs,
    ir

proc ctxLoadStoreEliminiate*(blk: IrBasicBlock) =
    # lift registers
    type RegState = object
        curVal, lastStore: IrInstrRef
    var
        regs: array[32, RegState]
        crs: array[32, RegState]
        fprs: array[32, RegState]
    for i in 0..<32:
        regs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
        crs[i] = regs[i]
        fprs[i] = regs[i]

    proc doLoad(states: var RegState, blk: IrBasicBlock, iref: IrInstrRef) =
        if states.curVal == InvalidIrInstrRef:
            states.curVal = iref
        else:
            blk.getInstr(iref) = makeIdentity(states.curVal)
    proc doStore(states: var RegState, blk: IrBasicBlock, iref, val: IrInstrRef) =
        if states.lastStore != InvalidIrInstrRef:
            blk.getInstr(states.lastStore) = makeIdentity(InvalidIrInstrRef)
        states = RegState(curVal: val, lastStore: iref)

    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)
        case instr.kind
        of irInstrCallInterpreterPpc:
            for i in 0..<32:
                regs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
                crs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
                fprs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
        of irInstrLoadReg:
            doLoad(regs[instr.ctxLoadIdx], blk, iref)
        of irInstrLoadCrBit:
            doLoad(crs[instr.ctxLoadIdx], blk, iref)
        of irInstrLoadFprPair:
            doLoad(fprs[instr.ctxLoadIdx], blk, iref)
        of irInstrStoreReg:
            doStore(regs[instr.ctxStoreIdx], blk, iref, instr.source(0))
        of irInstrStoreCrBit:
            doStore(crs[instr.ctxStoreIdx], blk, iref, instr.source(0))
        of irInstrStoreFprPair:
            doStore(fprs[instr.ctxStoreIdx], blk, iref, instr.source(0))
        of irInstrLoadSpr:
            if instr.ctxLoadIdx == irSprNumCr.uint32:
                for i in 0..<32:
                    crs[i].lastStore = InvalidIrInstrRef
        of irInstrStoreSpr:
            if instr.ctxStoreIdx == irSprNumCr.uint32:
                for i in 0..<32:
                    if crs[i].lastStore != InvalidIrInstrRef:
                        blk.getInstr(crs[i].lastStore) = makeIdentity(InvalidIrInstrRef)
                        crs[i].lastStore = InvalidIrInstrRef
                    crs[i].curVal = InvalidIrInstrRef
        else: discard

    var
        newInstrs: seq[IrInstrRef]
        cr = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)

    # lower loadCrBit/storeCrBit and simultaenously lift cr reads/writes
    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)
        case instr.kind
        of irInstrLoadCrBit:
            if cr.curVal == InvalidIrInstrRef:
                cr.curVal = blk.loadctx(irInstrLoadSpr, irSprNumCr.uint32)
                newInstrs.add cr.curVal

            let
                shiftAmount = blk.imm(31-instr.ctxLoadIdx)
                shift = blk.biop(irInstrShrLogic, cr.curVal, shiftAmount)
                mask = blk.imm(1)
            blk.getInstr(iref) = makeBiop(irInstrBitAnd, shift, mask)
            newInstrs.add [shiftAmount, shift, mask, iref]
        of irInstrStoreCrBit:
            if cr.curVal == InvalidIrInstrRef:
                cr.curVal = blk.loadctx(irInstrLoadSpr, irSprNumCr.uint32)
                newInstrs.add cr.curVal

            let
                mask = blk.imm(not(1'u32 shl (31-instr.ctxStoreIdx)))
                andOp = blk.biop(irInstrBitAnd, cr.curVal, mask)
                shiftAmount = blk.imm(31'u32-instr.ctxStoreIdx)
                shiftOp = blk.biop(irInstrShl, instr.source(0), shiftAmount)
                orrOp = blk.biop(irInstrBitOr, shiftOp, andOp)

            blk.getInstr(iref) = makeStorectx(irInstrStoreSpr, irSprNumCr.uint32, orrOp)
            newInstrs.add [mask, andOp, shiftAmount, shiftOp, orrOp, iref]

            if cr.lastStore != InvalidIrInstrRef:
                blk.getInstr(cr.lastStore) = makeIdentity(InvalidIrInstrRef)
            cr = RegState(curVal: orrOp, lastStore: iref)
        of irInstrLoadSpr:
            newInstrs.add iref
            if instr.ctxLoadIdx == irSprNumCr.uint32:
                doLoad(cr, blk, iref)
        of irInstrStoreSpr:
            newInstrs.add iref
            if instr.ctxStoreIdx == irSprNumCr.uint32:
                doStore(cr, blk, iref, instr.source(0))
        else:
            newInstrs.add iref

    blk.instrs = newInstrs

    #echo blk

proc floatOpts*(blk: IrBasicBlock) =
    #[
        currently performs the following optimisations:
            - if only ever the lower part of a irInstrLoadFprPair is used
                it's replaced by an irInstrLoadFpr instruction

            - PPC scalar instructions either replicate the result
                across the pair (floats) or merge in the previous register value (doubles).
                But if the following instructions only use the lower part of the instruction
                anyway this operation in unecessary.

                Thus for cases like this we change the source of scalar operations to directly
                use the unreplicated/unmerged scalar value.

                If no instruction depends on the upper part at all the swizzle/merge will be
                be removed by dead code elimination.

            - if a value is stored as a pair and merged with the previous value in memory,
                we can also just use a non-pair store
    ]#
    var
        pairLoads: seq[IrInstrRef]
        pairLoadUpperUsed = init(BitSeq, blk.instrPool.len)
    for i in 0..<blk.instrs.len:
        let iref = blk.instrs[i]
        case blk.getInstr(iref).kind
        of FpScalarOps:
            for source in msources blk.getInstr(iref):
                while blk.getInstr(source).kind in {irInstrFSwizzleD00, irInstrFMergeD00, irInstrFMergeD01}:
                    source = blk.getInstr(source).source(0)
        of irInstrLoadFprPair:
            pairLoads.add iref
        of irInstrStoreFprPair:
            block replaced:
                let
                    instr = blk.getInstr(iref)
                    srcInstr = blk.getInstr(instr.source(0)) 
                if srcInstr.kind == irInstrFMergeD01:
                    let srcSrcInstr = blk.getInstr(srcInstr.source(1))
                    if srcSrcInstr.kind == irInstrLoadFprPair and srcSrcInstr.ctxLoadIdx == instr.ctxStoreIdx:
                        blk.getInstr(iref) = makeStorectx(irInstrStoreFpr, instr.ctxStoreIdx, instr.source(0))
                        break replaced

                pairLoadUpperUsed.setBit(int(instr.source(0)))
        of FpPairOps:
            for source in sources blk.getInstr(iref):
                pairLoadUpperUsed.setBit(int(source))
        of irInstrFMergeD01:
            pairLoadUpperUsed.setBit(int(blk.getInstr(iref).source(1)))
        of irInstrFMergeD10:
            pairLoadUpperUsed.setBit(int(blk.getInstr(iref).source(0)))
        else: discard
#[
        let instr = blk.getInstr(iref)
        if instr.kind in {irInstrCvtsd2ss, irInstrCvtps2pd}:
            let
                arithref = instr.source(0)
                arithinstr = blk.getInstr(arithref)

            block isNotSingle:
                for src in sources arithinstr:
                    let srcInstr = blk.getInstr(src)
                    if srcInstr.kind notin {irInstrCvtss2sd, irInstrCvtps2pd}:
                        break isNotSingle

                # replace operation by a single operation]#

    for pairLoad in pairLoads:
        if not pairLoadUpperUsed[int(pairLoad)]:
            blk.getInstr(pairLoad) = IrInstr(kind: irInstrLoadFpr, ctxLoadIdx: blk.getInstr(pairLoad).ctxLoadIdx)

proc foldConstants*(blk: IrBasicBlock) =
    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)
        case instr.kind
        of irInstrIAdd:
            let
                a = blk.isImmValI(instr.source(0))
                b = blk.isImmValI(instr.source(1))
            if a.isSome and b.isSome:
                blk.getInstr(iref) = makeImm(a.get + b.get)
            elif a.isSome and a.get == 0:
                blk.getInstr(iref) = makeIdentity(instr.source(1))
            elif b.isSome and b.get == 0:
                blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrISub:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValI(instr.source(0))
                    b = blk.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get - b.get)
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrRol, irInstrShl, irInstrShrArith, irInstrShrLogic:
            let b = blk.isImmValI(instr.source(1))
            if b.isSome and (b.get and 0x3F'u32) == 0:
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            elif b.isSome and instr.kind == irInstrShl and (b.get and 0x3F'u32) >= 32:
                blk.getInstr(iref) = makeImm(0'u32)
            elif (let a = blk.isImmValI(instr.source(0)); a.isSome and b.isSome):
                let imm =
                    case instr.kind
                    of irInstrRol: rotateLeftBits(a.get, b.get)
                    of irInstrShl: a.get shl b.get
                    of irInstrShrArith: cast[uint32](cast[int32](a.get) shr b.get)
                    of irInstrShrLogic: a.get shr b.get
                    else: raiseAssert("shouldn't happen")
                blk.getInstr(iref) = makeImm(imm)
        of irInstrBitOr:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            else:
                let
                    a = blk.isImmValI(instr.source(0))
                    b = blk.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get or b.get)
                elif (a.isSome and a.get == 0xFFFF_FFFF'u32) or
                    (b.isSome and b.get == 0xFFFF_FFFF'u32):
                    blk.getInstr(iref) = makeImm(0xFFFF_FFFF'u32)
                elif a.isSome and a.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrBitAnd:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            else:
                let
                    a = blk.isImmValI(instr.source(0))
                    b = blk.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get and b.get)
                elif (a.isSome and a.get == 0'u32) or
                    (b.isSome and b.get == 0'u32):
                    blk.getInstr(iref) = makeImm(0'u32)
                elif a.isSome and a.get == 0xFFFF_FFFF'u32:
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0xFFFF_FFFF'u32:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrBitXor:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValI(instr.source(0))
                    b = blk.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get xor b.get)
                elif a.isSome and a.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrBitNot:
            if (let a = blk.isImmValI(instr.source(0)); a.isSome):
                blk.getInstr(iref) = makeImm(not a.get)
        of irInstrCondXor:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValB(instr.source(0))
                    b = blk.isImmValB(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get xor b.get)
        of irInstrCondOr:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            else:
                let
                    a = blk.isImmValB(instr.source(0))
                    b = blk.isImmValB(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get or b.get)
                elif (a.isSome and a.get) or (b.isSome and b.get):
                    blk.getInstr(iref) = makeImm(true)
                elif (a.isSome and not a.get):
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif (b.isSome and not b.get):
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrCondAnd:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            else:
                let
                    a = blk.isImmValB(instr.source(0))
                    b = blk.isImmValB(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get and b.get)
                elif (a.isSome and not a.get) or (b.isSome and not b.get):
                    blk.getInstr(iref) = makeImm(false)
                elif (a.isSome and a.get):
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif (b.isSome and b.get):
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of irInstrCondNot:
            if (let a = blk.isImmValB(instr.source(0)); a.isSome):
                blk.getInstr(iref) = makeImm(not a.get)
        of irInstrCsel:
            let c = blk.isImmValB(instr.source(2))
            if c.isSome:
                blk.getInstr(iref) = makeIdentity(if c.get: instr.source(0) else: instr.source(1))
        else: discard # no optimisations for you :(

proc removeIdentities*(blk: IrBasicBlock) =
    var newInstrs: seq[IrInstrRef]
    for i in 0..<blk.instrs.len:
        let iref = blk.instrs[i]
        if blk.getInstr(iref).kind != irInstrIdentity:
            for source in msources blk.getInstr(iref):
                while blk.getInstr(source).kind == irInstrIdentity:
                    source = blk.getInstr(source).source(0)

            newInstrs.add iref
        else:
            blk.freeInstrs.add iref
    blk.instrs = newInstrs

proc removeDeadCode*(blk: IrBasicBlock) =
    # calculate initial uses
    for i in 0..<blk.instrs.len:
        let iref = blk.instrs[i]
        for source in msources blk.getInstr(iref):
            blk.getInstr(source).numUses += 1

    var newInstrs: seq[IrInstrRef]
    for i in countdown(blk.instrs.len-1, 0):
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)
        if instr.numUses == 0 and instr.kind notin SideEffectOps:
            for source in sources instr:
                blk.getInstr(source).numUses -= 1

            blk.freeInstrs.add iref
        else:
            newInstrs.add iref

    assert newInstrs.len <= blk.instrs.len

    newInstrs.reverse()
    blk.instrs = newInstrs

proc verify*(blk: IrBasicBlock) =
    var instrsEncountered = init(BitSeq, blk.instrPool.len)
    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)

        for source in sources instr:
            assert blk.getInstr(source).kind notin ResultlessOps
            assert instrsEncountered[int(source)], &"instr {i} (${int(iref)}) has invalid source ${int(source)}\n{blk}"

        instrsEncountered.setBit(int(iref))

        if i == blk.instrs.len - 1:
            assert instr.kind in {irInstrBranchPpc, irInstrSyscallPpc, irInstrCallInterpreterPpc}

proc checkIdleLoop*(blk: IrBasicBlock, instrIndexes: seq[int32], startAdr, endAdr: uint32): bool =
    let lastInstr = blk.getInstr(blk.instrs[^1])
    if lastInstr.kind == irInstrBranchPpc and
        (let target = blk.isImmValI(lastInstr.source(1));
        target.isSome and target.get >= startAdr and target.get < endAdr):

        var
            regsWritten, regsRead: set[0..31]
        for i in instrIndexes[(target.get - startAdr) div 4]..<blk.instrs.len:
            let instr = blk.getInstr(blk.instrs[i])
            case instr.kind
            of irInstrSyscallPpc, irInstrStore8, irInstrStore16, irInstrStore32,
                irInstrStoreFss, irInstrStoreFsd, irInstrStoreFsq, irInstrStoreFpq,
                irInstrLoadSpr, irInstrStoreSpr, irInstrCallInterpreterPpc:
                return false
            of irInstrLoadReg:
                regsRead.incl instr.ctxLoadIdx
            of irInstrStoreReg:
                if instr.ctxStoreIdx in regsRead:
                    return false
                regsWritten.incl instr.ctxStoreIdx
            else: discard

        return true
    return false

proc calcLiveIntervals*(blk: IrBasicBlock) =
    for i in 0..<blk.instrs.len:
        let instr = blk.getInstr(blk.instrs[i])
        for src in instr.sources:
            blk.getInstr(src).lastRead = int32 i
