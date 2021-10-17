import
    options, bitops, algorithm, strformat,
    tables,
    stew/bitseqs,
    ../aluhelper,
    ../../dsp/dspstate,
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
        of ppcCallInterpreter:
            for i in 0..<32:
                regs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
                crs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
                fprs[i] = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)
        of loadPpcReg:
            doLoad(regs[instr.ctxLoadIdx], blk, iref)
        of loadCrBit:
            doLoad(crs[instr.ctxLoadIdx], blk, iref)
        of loadFprPair:
            doLoad(fprs[instr.ctxLoadIdx], blk, iref)
        of storePpcReg:
            doStore(regs[instr.ctxStoreIdx], blk, iref, instr.source(0))
        of storeCrBit:
            doStore(crs[instr.ctxStoreIdx], blk, iref, instr.source(0))
        of storeFprPair:
            doStore(fprs[instr.ctxStoreIdx], blk, iref, instr.source(0))
        of loadSpr:
            if instr.ctxLoadIdx == irSprNumCr.uint32:
                for i in 0..<32:
                    crs[i].lastStore = InvalidIrInstrRef
        of storeSpr:
            if instr.ctxStoreIdx == irSprNumCr.uint32:
                for i in 0..<32:
                    if crs[i].lastStore != InvalidIrInstrRef:
                        blk.getInstr(crs[i].lastStore) = makeIdentity(InvalidIrInstrRef)
                        crs[i].lastStore = InvalidIrInstrRef
                    crs[i].curVal = InvalidIrInstrRef
        else: discard

    let oldInstrs = move blk.instrs
    var cr = RegState(curVal: InvalidIrInstrRef, lastStore: InvalidIrInstrRef)

    assert blk.instrs.len == 0

    # lower loadCrBit/storeCrBit and simultaenously lift cr reads/writes
    for iref in oldInstrs:
        let instr = blk.getInstr(iref)
        case instr.kind
        of loadCrBit:
            if cr.curVal == InvalidIrInstrRef:
                cr.curVal = blk.loadctx(loadSpr, irSprNumCr.uint32)

            blk.getInstr(iref) = makeBiop(bitAnd,
                blk.biop(lsr, cr.curVal, blk.imm(31-instr.ctxLoadIdx)),
                blk.imm(1))
        of storeCrBit:
            if cr.curVal == InvalidIrInstrRef:
                cr.curVal = blk.loadctx(loadSpr, irSprNumCr.uint32)

            let newCr = blk.biop(bitOr,
                blk.biop(lsl, instr.source(0), blk.imm(31'u32-instr.ctxStoreIdx)),
                blk.biop(bitAnd, cr.curVal, blk.imm(not(1'u32 shl (31-instr.ctxStoreIdx)))))
            blk.getInstr(iref) = makeStorectx(storeSpr, irSprNumCr.uint32, newCr)

            if cr.lastStore != InvalidIrInstrRef:
                blk.getInstr(cr.lastStore) = makeIdentity(InvalidIrInstrRef)
            cr = RegState(curVal: newCr, lastStore: iref)
        of loadSpr:
            if instr.ctxLoadIdx == irSprNumCr.uint32:
                doLoad(cr, blk, iref)
        of storeSpr:
            if instr.ctxStoreIdx == irSprNumCr.uint32:
                doStore(cr, blk, iref, instr.source(0))
        else: discard

        blk.instrs.add iref

proc floatOpts*(blk: IrBasicBlock) =
    #[
        currently performs the following optimisations:
            - if only ever the lower part of a loadFprPair is used
                it's replaced by an loadFpr instruction

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
                while blk.getInstr(source).kind in {fSwizzleD00, fMergeD00, fMergeD01}:
                    source = blk.getInstr(source).source(0)
        of loadFprPair:
            pairLoads.add iref
        of storeFprPair:
            block replaced:
                let
                    instr = blk.getInstr(iref)
                    srcInstr = blk.getInstr(instr.source(0)) 
                if srcInstr.kind == fMergeD01:
                    let srcSrcInstr = blk.getInstr(srcInstr.source(1))
                    if srcSrcInstr.kind == loadFprPair and srcSrcInstr.ctxLoadIdx == instr.ctxStoreIdx:
                        blk.getInstr(iref) = makeStorectx(storeFpr, instr.ctxStoreIdx, instr.source(0))
                        break replaced

                pairLoadUpperUsed.setBit(int(instr.source(0)))
        of FpPairOps:
            for source in sources blk.getInstr(iref):
                pairLoadUpperUsed.setBit(int(source))
        of fMergeD01:
            pairLoadUpperUsed.setBit(int(blk.getInstr(iref).source(1)))
        of fMergeD10:
            pairLoadUpperUsed.setBit(int(blk.getInstr(iref).source(0)))
        else: discard
#[
        let instr = blk.getInstr(iref)
        if instr.kind in {cvtsd2ss, cvtps2pd}:
            let
                arithref = instr.source(0)
                arithinstr = blk.getInstr(arithref)

            block isNotSingle:
                for src in sources arithinstr:
                    let srcInstr = blk.getInstr(src)
                    if srcInstr.kind notin {cvtss2sd, cvtps2pd}:
                        break isNotSingle

                # replace operation by a single operation]#

    for pairLoad in pairLoads:
        if not pairLoadUpperUsed[int(pairLoad)]:
            blk.getInstr(pairLoad) = IrInstr(kind: loadFpr, ctxLoadIdx: blk.getInstr(pairLoad).ctxLoadIdx)

proc dspOpts*(blk: IrBasicBlock) =
    let oldInstrs = move blk.instrs

    # lower dsp specific instructions
    for iref in oldInstrs:
        let instr = blk.getInstr(iref)
        case instr.kind
        of extractMid, extractHi:
            blk.getInstr(iref) = makeBiop(bitAndX,
                blk.biop(lsrX, instr.source(0), blk.imm(if instr.kind == extractMid: 16 else: 32)),
                blk.imm(0xFFFF))
        of extractLo:
            blk.getInstr(iref) = makeBiop(bitAndX, instr.source(0), blk.imm(0xFFFF))
        of mergeLo:
            blk.getInstr(iref) = makeBiop(bitOrX,
                blk.biop(bitAndX, instr.source(0), blk.imm(not 0xFFFF'u64)),
                blk.biop(bitAndX, instr.source(1), blk.imm(0xFFFF)))
        of mergeMid:
            blk.getInstr(iref) = makeBiop(bitOrX,
                blk.biop(bitAndX, instr.source(0), blk.imm(not 0xFFFF_0000'u64)),
                blk.biop(lslX, blk.biop(bitAndX, instr.source(1), blk.imm(0xFFFF)), blk.imm(16)))
        of mergeHi:
            blk.getInstr(iref) = makeBiop(bitOrX,
                blk.biop(bitAndX, instr.source(0), blk.imm(not 0xFFFF_0000_0000'u64)),
                blk.biop(lslX, blk.unop(extsb, instr.source(1)), blk.imm(32)))
        of loadStatusBit:
            blk.getInstr(iref) = makeBiop(bitAnd,
                blk.biop(lsr, blk.loadctx(loadDspReg, psr.uint32), blk.imm(instr.ctxLoadIdx)),
                blk.imm(1))
        of storeStatusBit:
            blk.getInstr(iref) = makeStorectx(storeDspReg, psr.uint32,
                blk.biop(bitOr,
                    blk.biop(bitAnd, blk.loadctx(loadDspReg, psr.uint32), blk.imm(not(1'u32 shl instr.ctxStoreIdx))),
                    blk.biop(lsl, blk.biop(bitAnd, instr.source(0), blk.imm(1)), blk.imm(instr.ctxStoreIdx))))
        else:
            discard

        blk.instrs.add iref

proc foldConstants*(blk: IrBasicBlock) =
    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)
        case instr.kind
        of iAdd:
            let
                a = blk.isImmValI(instr.source(0))
                b = blk.isImmValI(instr.source(1))
            if a.isSome and b.isSome:
                blk.getInstr(iref) = makeImm(a.get + b.get)
            elif a.isSome and a.get == 0:
                blk.getInstr(iref) = blk.narrowIdentity(instr.source(1))
            elif b.isSome and b.get == 0:
                blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
        of iAddX:
            let
                a = blk.isImmValIX(instr.source(0))
                b = blk.isImmValIX(instr.source(1))
            if a.isSome and b.isSome:
                blk.getInstr(iref) = makeImm(a.get + b.get)
            elif a.isSome and a.get == 0:
                blk.getInstr(iref) = makeIdentity(instr.source(1))
            elif b.isSome and b.get == 0:
                blk.getInstr(iref) = makeIdentity(instr.source(0))
        of iSub:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValI(instr.source(0))
                    b = blk.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get - b.get)
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
        of iSubX:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValIX(instr.source(0))
                    b = blk.isImmValIX(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get - b.get)
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of rol, lsl, asr, lsr:
            let b = blk.isImmValI(instr.source(1))
            if b.isSome and (b.get and 0x1F'u32) == 0:
                blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
            elif (let a = blk.isImmValI(instr.source(0)); a.isSome and b.isSome):
                let imm =
                    case instr.kind
                    of rol: rotateLeftBits(a.get, b.get)
                    of lsl: a.get shl b.get
                    of asr: cast[uint32](cast[int32](a.get) shr b.get)
                    of lsr: a.get shr b.get
                    else: raiseAssert("shouldn't happen")
                blk.getInstr(iref) = makeImm(imm)
        of lslX, asrX, lsrX:
            let b = blk.isImmValI(instr.source(1))
            if b.isSome and (b.get and 0x3F'u32) == 0:
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            elif (let a = blk.isImmValIX(instr.source(0)); a.isSome and b.isSome):
                let imm =
                    case instr.kind
                    of lslX: a.get shl b.get
                    of asrX: cast[uint64](cast[int64](a.get) shr b.get)
                    of lsrX: a.get shr b.get
                    else: raiseAssert("shouldn't happen")
                blk.getInstr(iref) = makeImm(imm)
        of InstrKind.bitOr:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
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
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
        of bitOrX:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            else:
                let
                    a = blk.isImmValIX(instr.source(0))
                    b = blk.isImmValIX(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get or b.get)
                elif (a.isSome and a.get == 0xFFFF_FFFF_FFFF_FFFF'u64) or
                    (b.isSome and b.get == 0xFFFF_FFFF_FFFF_FFFF'u64):
                    blk.getInstr(iref) = makeImm(0xFFFF_FFFF_FFFF_FFFF'u64)
                elif a.isSome and a.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of InstrKind.bitAnd:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
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
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(1))
                elif b.isSome and b.get == 0xFFFF_FFFF'u32:
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
        of bitAndX:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeIdentity(instr.source(0))
            else:
                let
                    a = blk.isImmValIX(instr.source(0))
                    b = blk.isImmValIX(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get and b.get)
                elif (a.isSome and a.get == 0'u32) or
                    (b.isSome and b.get == 0'u32):
                    blk.getInstr(iref) = makeImm(0'u32)
                elif a.isSome and a.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
        of InstrKind.bitXor:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValI(instr.source(0))
                    b = blk.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get xor b.get)
                elif a.isSome and a.get == 0:
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = blk.narrowIdentity(instr.source(0))
                elif a.isSome and a.get == 0xFFFF_FFFF'u32:
                    blk.getInstr(iref) = makeUnop(bitNot, instr.source(1))
                elif b.isSome and b.get == 0xFFFF_FFFF'u32:
                    blk.getInstr(iref) = makeUnop(bitNot, instr.source(0))
        of bitXorX:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValIX(instr.source(0))
                    b = blk.isImmValIX(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get xor b.get)
                elif a.isSome and a.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    blk.getInstr(iref) = makeIdentity(instr.source(0))
                elif a.isSome and a.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                    blk.getInstr(iref) = makeUnop(bitNot, instr.source(1))
                elif b.isSome and b.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                    blk.getInstr(iref) = makeUnop(bitNot, instr.source(0))
        of InstrKind.bitNot:
            if (let a = blk.isImmValI(instr.source(0)); a.isSome):
                blk.getInstr(iref) = makeImm(not a.get)
        of bitNotX:
            if (let a = blk.isImmValIX(instr.source(0)); a.isSome):
                blk.getInstr(iref) = makeImm(not a.get)
        of condXor:
            if instr.source(0) == instr.source(1):
                blk.getInstr(iref) = makeImm(0)
            else:
                let
                    a = blk.isImmValB(instr.source(0))
                    b = blk.isImmValB(instr.source(1))
                if a.isSome and b.isSome:
                    blk.getInstr(iref) = makeImm(a.get xor b.get)
        of condOr:
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
        of condAnd:
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
        of condNot:
            if (let a = blk.isImmValB(instr.source(0)); a.isSome):
                blk.getInstr(iref) = makeImm(not a.get)
        of csel:
            let c = blk.isImmValB(instr.source(2))
            if c.isSome:
                blk.getInstr(iref) = blk.narrowIdentity(if c.get: instr.source(0) else: instr.source(1))
        of cselX:
            let c = blk.isImmValB(instr.source(2))
            if c.isSome:
                blk.getInstr(iref) = makeIdentity(if c.get: instr.source(0) else: instr.source(1))
        of extsb, extsh, extzw:
            let val = blk.isImmValI(instr.source(0))
            if val.isSome:
                blk.getInstr(iref) = makeImm(case instr.kind
                    of extsh: signExtend(val.get, 16)
                    of extsb: signExtend(val.get, 8)
                    of extzw: val.get
                    else: raiseAssert("should not happen"))
        of extsw:
            let val = blk.isImmValI(instr.source(0))
            if val.isSome:
                blk.getInstr(iref) = makeImm(signExtend(val.get, 32))
        else: discard # no optimisations for you :(

proc removeIdentities*(blk: IrBasicBlock) =
    var newInstrs: seq[IrInstrRef]
    for i in 0..<blk.instrs.len:
        let iref = blk.instrs[i]
        if blk.getInstr(iref).kind != identity:
            for source in msources blk.getInstr(iref):
                while blk.getInstr(source).kind == identity:
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

proc globalValueEnumeration*(blk: IrBasicBlock) =
    var
        knownValues: Table[IrInstr, IrInstrRef]
        replacedValues = newSeq[IrInstrRef](blk.instrPool.len)

    for i in 0..<blk.instrs.len:
        let iref = blk.instrs[i]
        if blk.getInstr(iref).kind notin StrictSideEffectOps:
            for source in msources blk.getInstr(iref):
                let replacedVal = IrInstrRef(int(replacedValues[int(source)]) - 1)
                if replacedVal != InvalidIrInstrRef:
                    source = replacedVal

            knownValues.withValue(blk.getInstr(iref), knownValue) do:
                replacedValues[int(iref)] = IrInstrRef(int(knownValue[]) + 1)
            do:
                knownValues[blk.getInstr(iref)] = iref

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
            assert instr.kind in {ppcBranch, ppcSyscall, ppcCallInterpreter,
                dspBranch, dspCallInterpreter}, &"last instruction of IR block not valid\n{blk}"

proc checkIdleLoop*(blk: IrBasicBlock, instrIndexes: seq[int32], startAdr, endAdr: uint32): bool =
    let lastInstr = blk.getInstr(blk.instrs[^1])
    if lastInstr.kind == ppcBranch and
        (let target = blk.isImmValI(lastInstr.source(1));
        target.isSome and target.get >= startAdr and target.get < endAdr):

        var
            regsWritten, regsRead: set[0..31]
        for i in instrIndexes[(target.get - startAdr) div 4]..<blk.instrs.len:
            let instr = blk.getInstr(blk.instrs[i])
            case instr.kind
            of ppcSyscall, ppcStore8, ppcStore16, ppcStore32,
                ppcStoreFss, ppcStoreFsd, ppcStoreFsq, ppcStoreFpq,
                loadSpr, storeSpr, ppcCallInterpreter:
                return false
            of loadPpcReg:
                regsRead.incl instr.ctxLoadIdx
            of storePpcReg:
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
