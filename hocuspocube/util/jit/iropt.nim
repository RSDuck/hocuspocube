import
    options, bitops, algorithm, strformat,
    tables, std/setutils,
    stew/bitseqs,
    ../aluhelper,
    ../../gekko/ppcstate, ../../dsp/dspstate,
    ir

proc ctxLoadStoreEliminiate*(fn: IrFunc) =
    # context loads/stores may not alias until after this pass
    type RegState = object
        curVal, lastStore: IrInstrRef

    var regState: Table[uint32, RegState]

    for blk in fn.blocks:
        regState.clear()

        for i in 0..<blk.instrs.len:
            let
                iref = blk.instrs[i]
                instr = fn.getInstr(iref)

            case instr.kind
            of CtxLoadInstrs:
                let val = regState.mgetOrPut(instr.ctxOffset, RegState(curVal: iref, lastStore: InvalidIrInstrRef))
                if val.curVal != iref:
                    fn.getInstr(iref) = makeIdentity(val.curVal)
            of CtxStoreInstrs:
                let
                    val = addr regState.mgetOrPut(instr.ctxOffset, RegState())
                    storeVal = instr.source(0)
                if val.lastStore != InvalidIrInstrRef:
                    fn.getInstr(val.lastStore) = makeIdentity(InvalidIrInstrRef)
                val.lastStore = iref
                val.curVal = storeVal
            of ppcCallInterpreter, dspCallInterpreter:
                regState.clear()
            else: discard

proc floatOpts*(fn: IrFunc) =
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

            - single operations on PPC are weird as in that they practically don't exist.
                Instead there are double operations which convert to single afterwards.

                But if all of the sources of a floating point op yielding a single are singles
                we can replace this operation with a single precision one.

                We transform this pattern:

                float(op_double(double(x)))

                into

                op_single(x)

                What is problematic are operations which only work on the sign, so negation and absolute.

                This also needs optimisations across basic blocks very badly, because all registers are of course
                first assumed to contain doubles.
    ]#
    var
        pairLoads: seq[IrInstrRef]
        pairLoadUpperUsed = init(BitSeq, fn.instrPool.len)
    for blk in fn.blocks:
        for i in 0..<blk.instrs.len:
            let iref = blk.instrs[i]
            case fn.getInstr(iref).kind
            of FpScalarOps:
                for source in msources fn.getInstr(iref):
                    while fn.getInstr(source).kind in {fSwizzleD00, fMergeD00, fMergeD01}:
                        source = fn.getInstr(source).source(0)
            of ctxLoadFprPair:
                pairLoads.add iref
            of ctxStoreFprPair:
                block replaced:
                    let
                        instr = fn.getInstr(iref)
                        srcInstr = fn.getInstr(instr.source(0)) 
                    if srcInstr.kind == fMergeD01:
                        let srcSrcInstr = fn.getInstr(srcInstr.source(1))
                        if srcSrcInstr.kind == ctxLoadFprPair and srcSrcInstr.ctxOffset == instr.ctxOffset:
                            fn.getInstr(iref) = makeStorectx(ctxStoreFpr, instr.ctxOffset, instr.source(0))
                            break replaced

                    pairLoadUpperUsed.setBit(int(instr.source(0)))
            of FpAllSrcsReadPair:
                for source in sources fn.getInstr(iref):
                    pairLoadUpperUsed.setBit(int(source))
            of fMergeD01:
                pairLoadUpperUsed.setBit(int(fn.getInstr(iref).source(1)))
            of fMergeD10:
                pairLoadUpperUsed.setBit(int(fn.getInstr(iref).source(0)))
            else: discard

            let instr = fn.getInstr(iref)
            if instr.kind in {cvtsd2ss, cvtpd2ps}:
                let
                    arithref = instr.source(0)
                    arithinstr = fn.getInstr(arithref)

                const
                    fpDoubleUnops = {fRessd, fRsqrtsd, fRespd, fRsqrtpd}
                    fpDoubleBiops = {fAddsd, fSubsd, fMulsd, fDivsd,
                        fAddpd, fSubpd, fMulpd, fDivpd}
                    fpDoubleTriops = {fMaddsd, fMsubsd, fNmaddsd, fNmsubsd,
                        fMaddpd, fMsubpd, fNmaddpd, fNmsubpd}
                    fpDoubleArith = fpDoubleUnops + fpDoubleBiops + fpDoubleTriops

                if arithinstr.kind notin fpDoubleArith:
                    continue
                
                const fpExpandOps = {cvtss2sd, cvtps2pd}

                case arithinstr.kind
                of fpDoubleUnops:
                    let source0 = fn.getInstr(arithinstr.source(0))
                    if source0.kind in fpExpandOps:
                        fn.getInstr(iref) = makeUnop(succ(arithinstr.kind), source0.source(0))
                of fpDoubleBiops:
                    let
                        source0 = fn.getInstr(arithinstr.source(0))
                        source1 = fn.getInstr(arithinstr.source(1))
                    if source0.kind in fpExpandOps and source1.kind in fpExpandOps:
                        fn.getInstr(iref) = makeBiop(succ(arithinstr.kind), source0.source(0), source1.source(0))
                of fpDoubleTriops:
                    let
                        source0 = fn.getInstr(arithinstr.source(0))
                        source1 = fn.getInstr(arithinstr.source(1))
                        source2 = fn.getInstr(arithinstr.source(2))
                    if source0.kind in fpExpandOps and source1.kind in fpExpandOps and
                            source2.kind in fpExpandOps:
                        fn.getInstr(iref) = makeTriop(succ(arithinstr.kind), source0.source(0), source1.source(0), source2.source(0))
                else: discard

    for pairLoad in pairLoads:
        if not pairLoadUpperUsed[int(pairLoad)]:
            fn.getInstr(pairLoad) = makeLoadctx(ctxLoadFpr, fn.getInstr(pairLoad).ctxOffset)

proc mergeExtractEliminate*(fn: IrFunc) =
    #[
        this pass is more or less a glorified lowering + optimisation at once
        of mergeBit and extractBit instructions.

        - extractBit instructions which extracts a value which was merged in previously
            are replaced by just that value. Otherwise it's just straightforwardly lowered.
        - every single mergeBit instruction in a mergeBit chain is lowered to
            independent constructions of the current value of the chain at that point.

            Example:
                $2 = mergebit $1, $..., 28
                $3 = mergebit $2, $..., 29

            is turned into:
                $2 = ($1 & ~((1<<28))) | ($... << 28)
                $3 = ($1 & ~((1<<28)|(1<<29))) | ($... << 28) | ($... << 29)

            this produces a lot of garbage which will be left to dead code removal,
                but has two advantages:
            - if two merges in a chain merge in at the same bit position, but in the end
                only a value containing the bit from the last merge is read,
                this way the other merge is completely optimised away.
            - the lowering is more optimised as it considers multiple merges at once
                (just a single and instead of one per merge).
    ]#
    type
        PartialBitValue = object
            bits: array[32, IrInstrRef]
            chainStart: IrInstrRef

    for blk in fn.blocks:
        var
            partialBits: Table[IrInstrRef, PartialBitValue]
            builder = IrBlockBuilder[void](fn: fn)

        for iref in blk.instrs:
            let instr = fn.getInstr(iref)
            case instr.kind
            of mergeBit:
                var newVal = partialBits.getOrDefault(instr.source(0), PartialBitValue(chainStart: instr.source(0)))
                newVal.bits[instr.bit] = instr.source(1)
                partialBits[iref] = newVal

                var
                    mask = 0xFFFF_FFFF'u32
                    op = InvalidIrInstrRef
                for i in 0..<32:
                    if newVal.bits[i] != InvalidIrInstrRef:
                        mask.clearBit(i)
                        let shiftedBit = builder.biop(lsl, newVal.bits[i], builder.imm(uint64 i))
                        op =
                            if op != InvalidIrInstrRef: 
                                builder.biop(bitOr, op, shiftedBit)
                            else:
                                shiftedBit
                fn.getInstr(iref) = makeBiop(bitOr, op, builder.biop(bitAnd, newVal.chainStart, builder.imm(mask)))
            of extractBit:
                block replaced:
                    partialBits.withValue(instr.source(0), val):
                        if (let bit = val.bits[instr.bit]; bit != InvalidIrInstrRef):
                            fn.getInstr(iref) = makeIdentity(bit)
                            break replaced

                    fn.getInstr(iref) = makeBiop(bitAnd, builder.biop(lsr, instr.source(0), builder.imm(instr.bit)), builder.imm(1))
            else: discard

            builder.instrs.add iref

        blk.instrs = move builder.instrs

    #[      for iref in oldInstrs:
            let instr = fn.getInstr(iref)
            case instr.kind
            of mergeBit:
                fn.getInstr(iref) = makeBiop(bitOr, blk.biop(lsl, instr.source(1), blk.imm(instr.bit)), blk.biop(bitAnd, instr.source(0), blk.imm(not(1'u32 shl instr.bit))))
            of extractBit:
                fn.getInstr(iref) = makeBiop(bitAnd, blk.biop(lsr, instr.source(0), blk.imm(instr.bit)), blk.imm(1))
            else: discard

            blk.instrs.add iref]#

proc dspOpts*(fn: IrFunc) =
    for blk in fn.blocks:
        var builder = IrBlockBuilder[void](fn: fn)

        # lower dsp specific instructions
        for iref in blk.instrs:
            let instr = fn.getInstr(iref)
            case instr.kind
            of extractMid, extractHi:
                fn.getInstr(iref) = makeBiop(bitAndX,
                    builder.biop(lsrX, instr.source(0), builder.imm(if instr.kind == extractMid: 16 else: 32)),
                    builder.imm(0xFFFF))
            of extractLo:
                fn.getInstr(iref) = makeBiop(bitAndX, instr.source(0), builder.imm(0xFFFF))
            of mergeLo:
                fn.getInstr(iref) = makeBiop(bitOrX,
                    builder.biop(bitAndX, instr.source(0), builder.imm(not 0xFFFF'u64)),
                    builder.biop(bitAndX, instr.source(1), builder.imm(0xFFFF)))
            of mergeMid:
                fn.getInstr(iref) = makeBiop(bitOrX,
                    builder.biop(bitAndX, instr.source(0), builder.imm(not 0xFFFF_0000'u64)),
                    builder.biop(lslX, builder.biop(bitAndX, instr.source(1), builder.imm(0xFFFF)), builder.imm(16)))
            of mergeHi:
                fn.getInstr(iref) = makeBiop(bitOrX,
                    builder.biop(bitAndX, instr.source(0), builder.imm(not 0xFFFF_0000_0000'u64)),
                    builder.biop(lslX, builder.unop(extsb, instr.source(1)), builder.imm(32)))
            of mergeMidHi:
                fn.getInstr(iref) = makeBiop(bitOrX,
                    builder.biop(bitAndX, instr.source(0), builder.imm(0xFFFF'u64)),
                    builder.biop(lslX, builder.unop(extshX, instr.source(1)), builder.imm(16)))
            #[of mergeBit:
                fn.getInstr(iref) = makeBiop(bitOr, builder.biop(lsl, instr.source(1), builder.imm(instr.bit)), builder.biop(bitAnd, instr.source(0), builder.imm(not(1'u32 shl instr.bit))))
            of extractBit:
                fn.getInstr(iref) = makeBiop(bitAnd, builder.biop(lsr, instr.source(0), builder.imm(instr.bit)), builder.imm(1))]#
            else:
                discard

            builder.instrs.add iref
        

        blk.instrs = move builder.instrs

proc foldConstants*(fn: IrFunc) =
    for blk in fn.blocks:
        for i in 0..<blk.instrs.len:
            let
                iref = blk.instrs[i]
                instr = fn.getInstr(iref)
            case instr.kind
            of iAdd:
                let
                    a = fn.isImmValI(instr.source(0))
                    b = fn.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    fn.getInstr(iref) = makeImm(a.get + b.get)
                elif a.isSome and a.get == 0:
                    fn.getInstr(iref) = fn.narrowIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
            of iAddX:
                let
                    a = fn.isImmValIX(instr.source(0))
                    b = fn.isImmValIX(instr.source(1))
                if a.isSome and b.isSome:
                    fn.getInstr(iref) = makeImm(a.get + b.get)
                elif a.isSome and a.get == 0:
                    fn.getInstr(iref) = makeIdentity(instr.source(1))
                elif b.isSome and b.get == 0:
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
            of iSub:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeImm(0)
                else:
                    let
                        a = fn.isImmValI(instr.source(0))
                        b = fn.isImmValI(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get - b.get)
                    elif b.isSome and b.get == 0:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
            of iSubX:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeImm(0)
                else:
                    let
                        a = fn.isImmValIX(instr.source(0))
                        b = fn.isImmValIX(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get - b.get)
                    elif b.isSome and b.get == 0:
                        fn.getInstr(iref) = makeIdentity(instr.source(0))
            of rol, lsl, asr, lsr:
                let b = fn.isImmValI(instr.source(1))
                if b.isSome and (b.get and 0x1F'u32) == 0:
                    fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
                elif (let a = fn.isImmValI(instr.source(0)); a.isSome and b.isSome):
                    let imm =
                        case instr.kind
                        of rol: rotateLeftBits(a.get, b.get)
                        of lsl: a.get shl b.get
                        of asr: cast[uint32](cast[int32](a.get) shr b.get)
                        of lsr: a.get shr b.get
                        else: raiseAssert("shouldn't happen")
                    fn.getInstr(iref) = makeImm(imm)
            of lslX, asrX, lsrX:
                let b = fn.isImmValI(instr.source(1))
                if b.isSome and (b.get and 0x3F'u32) == 0:
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
                elif (let a = fn.isImmValIX(instr.source(0)); a.isSome and b.isSome):
                    let imm =
                        case instr.kind
                        of lslX: a.get shl b.get
                        of asrX: cast[uint64](cast[int64](a.get) shr b.get)
                        of lsrX: a.get shr b.get
                        else: raiseAssert("shouldn't happen")
                    fn.getInstr(iref) = makeImm(imm)
            of InstrKind.bitOr:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
                else:
                    let
                        a = fn.isImmValI(instr.source(0))
                        b = fn.isImmValI(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get or b.get)
                    elif (a.isSome and a.get == 0xFFFF_FFFF'u32) or
                        (b.isSome and b.get == 0xFFFF_FFFF'u32):
                        fn.getInstr(iref) = makeImm(0xFFFF_FFFF'u32)
                    elif a.isSome and a.get == 0:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(1))
                    elif b.isSome and b.get == 0:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
            of bitOrX:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
                else:
                    let
                        a = fn.isImmValIX(instr.source(0))
                        b = fn.isImmValIX(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get or b.get)
                    elif (a.isSome and a.get == 0xFFFF_FFFF_FFFF_FFFF'u64) or
                        (b.isSome and b.get == 0xFFFF_FFFF_FFFF_FFFF'u64):
                        fn.getInstr(iref) = makeImm(0xFFFF_FFFF_FFFF_FFFF'u64)
                    elif a.isSome and a.get == 0:
                        fn.getInstr(iref) = makeIdentity(instr.source(1))
                    elif b.isSome and b.get == 0:
                        fn.getInstr(iref) = makeIdentity(instr.source(0))
            of InstrKind.bitAnd:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
                else:
                    let
                        a = fn.isImmValI(instr.source(0))
                        b = fn.isImmValI(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get and b.get)
                    elif (a.isSome and a.get == 0'u32) or
                        (b.isSome and b.get == 0'u32):
                        fn.getInstr(iref) = makeImm(0'u32)
                    elif a.isSome and a.get == 0xFFFF_FFFF'u32:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(1))
                    elif b.isSome and b.get == 0xFFFF_FFFF'u32:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
            of bitAndX:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
                else:
                    let
                        a = fn.isImmValIX(instr.source(0))
                        b = fn.isImmValIX(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get and b.get)
                    elif (a.isSome and a.get == 0'u32) or
                        (b.isSome and b.get == 0'u32):
                        fn.getInstr(iref) = makeImm(0'u32)
                    elif a.isSome and a.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                        fn.getInstr(iref) = makeIdentity(instr.source(1))
                    elif b.isSome and b.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                        fn.getInstr(iref) = makeIdentity(instr.source(0))
            of InstrKind.bitXor:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeImm(0)
                else:
                    let
                        a = fn.isImmValI(instr.source(0))
                        b = fn.isImmValI(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get xor b.get)
                    elif a.isSome and a.get == 0:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(1))
                    elif b.isSome and b.get == 0:
                        fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
                    elif a.isSome and a.get == 0xFFFF_FFFF'u32:
                        fn.getInstr(iref) = makeUnop(bitNot, instr.source(1))
                    elif b.isSome and b.get == 0xFFFF_FFFF'u32:
                        fn.getInstr(iref) = makeUnop(bitNot, instr.source(0))
            of bitXorX:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeImm(0)
                else:
                    let
                        a = fn.isImmValIX(instr.source(0))
                        b = fn.isImmValIX(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get xor b.get)
                    elif a.isSome and a.get == 0:
                        fn.getInstr(iref) = makeIdentity(instr.source(1))
                    elif b.isSome and b.get == 0:
                        fn.getInstr(iref) = makeIdentity(instr.source(0))
                    elif a.isSome and a.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                        fn.getInstr(iref) = makeUnop(bitNot, instr.source(1))
                    elif b.isSome and b.get == 0xFFFF_FFFF_FFFF_FFFF'u64:
                        fn.getInstr(iref) = makeUnop(bitNot, instr.source(0))
            of InstrKind.bitNot:
                if (let a = fn.isImmValI(instr.source(0)); a.isSome):
                    fn.getInstr(iref) = makeImm(not a.get)
            of bitNotX:
                if (let a = fn.isImmValIX(instr.source(0)); a.isSome):
                    fn.getInstr(iref) = makeImm(not a.get)
            of condXor:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeImm(0)
                else:
                    let
                        a = fn.isImmValB(instr.source(0))
                        b = fn.isImmValB(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get xor b.get)
            of condOr:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
                else:
                    let
                        a = fn.isImmValB(instr.source(0))
                        b = fn.isImmValB(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get or b.get)
                    elif (a.isSome and a.get) or (b.isSome and b.get):
                        fn.getInstr(iref) = makeImm(true)
                    elif (a.isSome and not a.get):
                        fn.getInstr(iref) = makeIdentity(instr.source(1))
                    elif (b.isSome and not b.get):
                        fn.getInstr(iref) = makeIdentity(instr.source(0))
            of condAnd:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
                else:
                    let
                        a = fn.isImmValB(instr.source(0))
                        b = fn.isImmValB(instr.source(1))
                    if a.isSome and b.isSome:
                        fn.getInstr(iref) = makeImm(a.get and b.get)
                    elif (a.isSome and not a.get) or (b.isSome and not b.get):
                        fn.getInstr(iref) = makeImm(false)
                    elif (a.isSome and a.get):
                        fn.getInstr(iref) = makeIdentity(instr.source(1))
                    elif (b.isSome and b.get):
                        fn.getInstr(iref) = makeIdentity(instr.source(0))
            of condNot:
                if (let a = fn.isImmValB(instr.source(0)); a.isSome):
                    fn.getInstr(iref) = makeImm(not a.get)
            of csel:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = fn.narrowIdentity(instr.source(0))
                else:
                    let c = fn.isImmValB(instr.source(2))
                    if c.isSome:
                        fn.getInstr(iref) = fn.narrowIdentity(if c.get: instr.source(0) else: instr.source(1))
            of cselX:
                if instr.source(0) == instr.source(1):
                    fn.getInstr(iref) = makeIdentity(instr.source(0))
                else:
                    let c = fn.isImmValB(instr.source(2))
                    if c.isSome:
                        fn.getInstr(iref) = makeIdentity(if c.get: instr.source(0) else: instr.source(1))
            of extsb, extsh, extzwX, extswX:
                let val = fn.isImmValI(instr.source(0))
                if val.isSome:
                    fn.getInstr(iref) = makeImm(case instr.kind
                        of extsh: uint64 signExtend(val.get, 16)
                        of extsb: uint64 signExtend(val.get, 8)
                        of extshX: signExtend(uint64 val.get, 16)
                        of extsbX: signExtend(uint64 val.get, 8)
                        of extzwX: uint64 val.get
                        of extswX: signExtend(uint64(val.get), 32)
                        else: raiseAssert("should not happen"))
            of iCmpEqual, iCmpNequal,
                    iCmpGreaterU, iCmpGequalU, iCmpLessU, iCmpLequalU,
                    iCmpGreaterS, iCmpGequalS, iCmpLessS, iCmpLequalS:
                let
                    a = fn.isImmValI(instr.source(0))
                    b = fn.isImmValI(instr.source(1))
                if a.isSome and b.isSome:
                    fn.getInstr(iref) = makeImm(case instr.kind
                        of iCmpEqual: a.get == b.get
                        of iCmpNequal: a.get != b.get
                        of iCmpGreaterU: a.get > b.get
                        of iCmpGequalU: a.get >= b.get
                        of iCmpLessU: a.get < b.get
                        of iCmpLequalU: a.get <= b.get
                        of iCmpGreaterS: cast[int32](a.get) > cast[int32](b.get)
                        of iCmpGequalS: cast[int32](a.get) >= cast[int32](b.get)
                        of iCmpLessS: cast[int32](a.get) < cast[int32](b.get)
                        of iCmpLequalS: cast[int32](a.get) <= cast[int32](b.get)
                        else: raiseAssert("C compiler, please optimise me away"))
            of iCmpEqualX, iCmpNequalX,
                    iCmpGreaterUX, iCmpGequalUX, iCmpLessUX, iCmpLequalUX,
                    iCmpGreaterSX, iCmpGequalSX, iCmpLessSX, iCmpLequalSX:
                let
                    a = fn.isImmValIX(instr.source(0))
                    b = fn.isImmValIX(instr.source(1))
                if a.isSome and b.isSome:
                    fn.getInstr(iref) = makeImm(case instr.kind
                        of iCmpEqualX: a.get == b.get
                        of iCmpNequalX: a.get != b.get
                        of iCmpGreaterUX: a.get > b.get
                        of iCmpGequalUX: a.get >= b.get
                        of iCmpLessUX: a.get < b.get
                        of iCmpLequalUX: a.get <= b.get
                        of iCmpGreaterSX: cast[int64](a.get) > cast[int64](b.get)
                        of iCmpGequalSX: cast[int64](a.get) >= cast[int64](b.get)
                        of iCmpLessSX: cast[int64](a.get) < cast[int64](b.get)
                        of iCmpLequalSX: cast[int64](a.get) <= cast[int64](b.get)
                        else: raiseAssert("C compiler, please optimise me away"))
            else: discard # no optimisations for you :(

proc removeIdentities*(fn: IrFunc) =
    for blk in fn.blocks:
        var newInstrs: seq[IrInstrRef]
        for i in 0..<blk.instrs.len:
            let iref = blk.instrs[i]
            if fn.getInstr(iref).kind != identity:
                for source in msources fn.getInstr(iref):
                    while fn.getInstr(source).kind == identity:
                        source = fn.getInstr(source).source(0)
                        assert source != InvalidIrInstrRef

                newInstrs.add iref
            else:
                fn.freeInstrs.add iref
        blk.instrs = newInstrs

proc removeDeadCode*(fn: IrFunc) =
    for blk in fn.blocks:
        # calculate initial uses
        for i in 0..<blk.instrs.len:
            let iref = blk.instrs[i]
            for source in msources fn.getInstr(iref):
                fn.getInstr(source).numUses += 1

        var newInstrs: seq[IrInstrRef]
        for i in countdown(blk.instrs.len-1, 0):
            let
                iref = blk.instrs[i]
                instr = fn.getInstr(iref)
            if instr.numUses == 0 and instr.kind notin SideEffectOps:
                for source in sources instr:
                    fn.getInstr(source).numUses -= 1

                fn.freeInstrs.add iref
            else:
                newInstrs.add iref

        assert newInstrs.len <= blk.instrs.len

        newInstrs.reverse()
        blk.instrs = newInstrs

proc globalValueEnumeration*(fn: IrFunc) =
    var
        knownValues: Table[IrInstr, IrInstrRef]
        replacedValues = newSeq[IrInstrRef](fn.instrPool.len)
    for blk in fn.blocks:
        knownValues.clear()
        for i in 0..<blk.instrs.len:
            let iref = blk.instrs[i]
            if fn.getInstr(iref).kind notin SideEffectOps:
                for source in msources fn.getInstr(iref):
                    let replacedVal = IrInstrRef(int(replacedValues[int(source)]) - 1)
                    if replacedVal != InvalidIrInstrRef:
                        source = replacedVal

                knownValues.withValue(fn.getInstr(iref), knownValue) do:
                    replacedValues[int(iref)] = IrInstrRef(int(knownValue[]) + 1)
                do:
                    knownValues[fn.getInstr(iref)] = iref

proc verify*(fn: IrFunc) =
    for blk in fn.blocks:
        var instrsEncountered = init(BitSeq, fn.instrPool.len)
        for i in 0..<blk.instrs.len:
            let
                iref = blk.instrs[i]
                instr = fn.getInstr(iref)

            for source in sources instr:
                assert fn.getInstr(source).kind notin ResultlessOps
                assert instrsEncountered[int(source)-1], &"instr {i} (${int(iref)}) has invalid source ${int(source)}\n{prettify(blk, fn)}"

            instrsEncountered.setBit(int(iref)-1)

            if i == blk.instrs.len - 1:
                assert instr.kind in BlockEndingInstrs,
                    &"last instruction of IR block not valid\n{prettify(blk, fn)}"
            else:
                assert instr.kind notin BlockEndingInstrs,
                    &"block ending instruction inside block\n{prettify(blk, fn)}"

proc deconstructImmBranch*(fn: IrFunc, branchInstr: IrInstr): (IrInstrRef, Option[uint32], Option[uint32]) =
    let target = fn.getInstr(branchInstr.source(0))
    if target.kind == loadImmI:
        (InvalidIrInstrRef, some(uint32 target.immValI), none(uint32))
    elif target.kind == csel:
        (target.source(2), fn.isImmValI(target.source(0)), fn.isImmValI(target.source(1)))
    else:
        (InvalidIrInstrRef, none(uint32), none(uint32))

proc transformIdleLoop(cond: IrInstrRef, targetTaken, targetNotTaken: Option[uint32],
        fn: IrFunc, blk: IrBasicBlock, dispatchInstrKind, leaveInstrKind, pcStoreKind: InstrKind, startAdr, pcOffset, cyclesOffset: uint32) =

    proc writeIdleLoopExit(builder: var IrBlockBuilder[void],
            fn: IrFunc, blk: IrBasicBlock,
            exitAdr, pcOffset, cyclesOffset: uint32,
            pcStoreKind: InstrKind) =
        let cycles = builder.loadctx(ctxLoadU32, cyclesOffset)
        builder.storectx(ctxStore32, uint32 cyclesOffset,
            builder.triop(csel, builder.imm(0), cycles, builder.biop(iCmpLessS, cycles, builder.imm(0))))

        builder.storectx(pcStoreKind, uint32 pcOffset, builder.imm(exitAdr))
        discard builder.zeroop(leaveInstrKind)

    var
        builder = IrBlockBuilder[void](fn: fn)
    if cond != InvalidIrInstrRef:
        let
            takenBlock = IrBasicBlock()
            notTakenBlock = IrBasicBlock()

        fn.getInstr(blk.instrs[^1]) = makeFuncInternBranch(cond, takenBlock, notTakenBlock)

        for (blk, target) in [(takenBlock, targetTaken), (notTakenBlock, targetNotTaken)]:
            if target == some(startAdr):
                builder.writeIdleLoopExit(fn, blk, target.get, pcOffset, cyclesOffset, pcStoreKind)
            else:
                discard builder.unop(dispatchInstrKind, builder.imm(target.get))
            blk.instrs = move builder.instrs

            fn.blocks.add blk

        #echo "transforming cond idle loop"
    else:
        #echo "transforming uncond idle loop ", prettify(blk, fn)
        builder.instrs = move blk.instrs
        builder.instrs.setLen builder.instrs.len-1
        builder.writeIdleLoopExit(fn, blk, targetTaken.get, pcOffset, cyclesOffset, pcStoreKind)
        blk.instrs = move builder.instrs

proc transformIdleLoopsPpc*(fn: IrFunc, blk: IrBasicBlock, startAdr: uint32) =
    let lastInstr = fn.getInstr(blk.instrs[^1])
    if lastInstr.kind == dispatchExternalPpc:
        # check whether this is a back jump to the start of the block
        let (cond, targetTaken, targetNotTaken) = fn.deconstructImmBranch(lastInstr)
        if (cond != InvalidIrInstrRef and targetNotTaken.isNone) or
                not(targetTaken == some(startAdr) or targetNotTaken == some(startAdr)):
            return

        var
            regsReadEarlier: set[0..31]
            regsWrittenInLoop: set[0..31]
        for i in 0..<blk.instrs.len-1:
            let instr = fn.getInstr(blk.instrs[i])
            case instr.kind
            of ppcStore8, ppcStore16, ppcStore32,
                    ppcStoreFss, ppcStoreFsd, ppcStoreFsq, sprStore32,
                    ppcCallInterpreter:
                return
            of CtxLoadInstrs:
                if instr.ctxOffset-uint32(offsetof(PpcState, r)) < 32*4:
                    let reg = (instr.ctxOffset-uint32(offsetof(PpcState, r))) div 4

                    if reg notin regsWrittenInLoop:
                        regsReadEarlier.incl reg
            of CtxStoreInstrs:
                if instr.ctxOffset-uint32(offsetof(PpcState, r)) >= 32*4:
                    continue
                let reg = (instr.ctxOffset-uint32(offsetof(PpcState, r))) div 4

                if reg in regsReadEarlier:
                    return

                regsWrittenInLoop.incl reg
            else: discard

        #echo &"found ppc idle loop {startAdr:08X}"

        transformIdleLoop(cond, targetTaken, targetNotTaken,
            fn, blk,
            dispatchExternalPpc, leaveJitPpc, ctxStore32,
            startAdr,
            uint32 offsetof(PpcState, pc), uint32 offsetof(PpcState, negativeCycles))

proc transformIdleLoopDsp*(fn: IrFunc, blk: IrBasicBlock, instrs: seq[tuple[read, write: set[DspReg]]], startAdr: uint16) =
    let lastInstr = fn.getInstr(blk.instrs[^1])
    if lastInstr.kind == dispatchExternalDsp:
        let (cond, targetTaken, targetNotTaken) = fn.deconstructImmBranch(lastInstr)
        if (cond != InvalidIrInstrRef and targetNotTaken.isNone) or
                not(targetTaken == some(uint32 startAdr) or targetNotTaken == some(uint32 startAdr)):
            return

        var regsLocked, regsWritten: set[DspReg]
        for instr in instrs:
            regsLocked.incl instr.read * complement(regsWritten)

            if regsLocked * instr.write != {}:
                return

            regsWritten.incl instr.write

        #echo &"found dsp idle loop {startAdr:04X}"
        transformIdleLoop(cond, targetTaken, targetNotTaken,
            fn, blk,
            dispatchExternalDsp, leaveJitDsp, ctxStore16,
            startAdr,
            uint32 offsetof(DspState, pc), uint32 offsetof(DspState, negativeCycles))

proc calcLiveIntervals*(fn: IrFunc) =
    for blk in fn.blocks:
        for i in 0..<blk.instrs.len:
            let instr = fn.getInstr(blk.instrs[i])
            for src in instr.sources:
                fn.getInstr(src).lastRead = int32 i

proc findTarget(targets: seq[(uint32, IrBasicBlock)], target: uint32): IrBasicBlock =
    #let target = targets.binarySearch(target) do (a: (uint32, IrBasicBlock), b: uint32) -> int:
    #    cmp(a[0], b)
    
    #if target != -1:
    #    targets[target][1]
    #else:
    #    nil
    for (adr, blk) in targets:
        if adr == target:
            return blk
    nil

proc resolveInnerFuncBranches*(fn: IrFunc, targets: seq[(uint32, IrBasicBlock)]) =
    for blk in fn.blocks:
        let lastInstr = fn.getInstr(blk.instrs[^1])
        if lastInstr.kind == dispatchExternalPpc:
            let (cond, immTaken, immNotTaken) = fn.deconstructImmBranch(lastInstr)
            if cond == InvalidIrInstrRef and immTaken.isSome:
                let target = targets.findTarget(immTaken.get)
                if target != nil:
                    #echo "replaced branch uncond"
                    fn.getInstr(blk.instrs[^1]) = makeFuncInternBranch(target)
            elif cond != InvalidIrInstrRef and immTaken.isSome and immNotTaken.isSome:
                let
                    takenTarget = targets.findTarget(immTaken.get)
                    notTakenTarget = targets.findTarget(immNotTaken.get)
                if takenTarget != nil and notTakenTarget != nil:
                    #echo "replaced cond branch"
                    fn.getInstr(blk.instrs[^1]) = makeFuncInternBranch(cond, takenTarget, notTakenTarget)
