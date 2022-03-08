import
    strformat, options, hashes

#[
    Some notes on this weird IR I've come up with.

    It isn't directly based on anything, for the most part it's just
    stuff I liked I picked up from other projects (dynarmic, xenia, ...).

    The entire IR is implicitly typed and a reference to an IR instruction
    also refers to it's value. Every value is either an integer or a
    floating point value. Booleans are just integers which have to be
    either 0 or 1.

    Unless marked with an X in the name integer instructions use only the lower
    32-bit of the source values. If they produce an integer value as a result,
    the upper 32-bit of it are going to be zero (similarly to how 32-bit instructions
    work on x64/aarch64).
]#

type
    InstrKind* = enum
        identity
        loadImmI

        ctxLoad8
        ctxLoad16
        ctxLoadU32
        ctxLoadS32
        ctxLoad64
        ctxLoadFpr
        ctxLoadFprPair
        ctxStore8
        ctxStore16
        ctxStore32
        ctxStore64
        ctxStoreFpr
        ctxStoreFprPair

        # for registers with a special side effects
        sprLoad32
        sprStore32

        extractBit
        mergeBit
        # used for DSP partial loads/stores to dsp accumulators
        extractLo
        extractMid
        extractHi
        mergeLo
        mergeMid
        mergeHi
        mergeMidHi

        csel
        cselX

        iAdd
        iSub

        iAddX
        iSubX

        iAddExtended
        iSubExtended

        iMul
        iMulhS
        iMulhU
        iMulX

        iDivS
        iDivU

        bitAnd
        bitOr
        bitXor
        bitNot

        bitAndX
        bitOrX
        bitXorX
        bitNotX

        lsl
        lsr
        asr
        rol

        lslX
        lsrX
        asrX

        clz

        extsb
        extsh

        extsbX
        extshX

        extzwX
        extswX

        condAnd
        condOr
        condXor
        condNot

        overflowAdd
        overflowSub

        overflowAddX
        overflowSubX

        carryAdd
        carrySub

        carryAddX
        carrySubX

        overflowAddExtended
        overflowSubExtended

        carryAddExtended
        carrySubExtended

        iCmpEqual
        iCmpGreaterU
        iCmpLessU
        iCmpGreaterS
        iCmpLessS

        iCmpEqualX
        iCmpGreaterUX
        iCmpLessUX
        iCmpGreaterSX
        iCmpLessSX

        ppcLoadU8
        ppcLoadU16
        ppcLoad32
        ppcLoadFss
        ppcLoadFsd
        ppcLoadFsq
        ppcLoadFpq

        ppcStore8
        ppcStore16
        ppcStore32
        ppcStoreFss
        ppcStoreFsd
        ppcStoreFsq
        ppcStoreFpq

        dspLoadIMem
        dspLoadDMem
        dspStoreDMem

        ppcBranch
        ppcSyscall

        dspBranch

        fSwizzleD00
        fSwizzleD11

        fSwizzleS00
        fSwizzleS11

        fMergeD00
        fMergeD01
        fMergeD10
        fMergeD11

        fMergeS00
        fMergeS01
        fMergeS10
        fMergeS11

        # the upper bits of scalar operations are undefined
        cvtsd2ss
        cvtss2sd

        cvtpd2ps
        cvtps2pd

        cvtsd2intTrunc
        cvtss2intTrunc

        fRessd
        fRsqrtsd

        fResss
        fRsqrtss

        fRespd
        fRsqrtpd

        fResps
        fRsqrtps

        fNegsd
        fAbssd

        fNegss
        fAbsss

        fNegpd
        fAbspd

        fNegps
        fAbsps

        fAddsd
        fSubsd
        fMulsd
        fDivsd

        fAddss
        fSubss
        fMulss
        fDivss

        fAddpd
        fSubpd
        fMulpd
        fDivpd

        fAddps
        fSubps
        fMulps
        fDivps

        fMaddsd
        fMsubsd
        fNmaddsd
        fNmsubsd

        fMaddss
        fMsubss
        fNmaddss
        fNmsubss

        fMaddpd
        fMsubpd
        fNmaddpd
        fNmsubpd

        fMaddps
        fMsubps
        fNmaddps
        fNmsubps

        fCmpEqualsd
        fCmpGreatersd
        fCmpLesssd
        fUnorderedsd

        ppcCallInterpreter
        dspCallInterpreter

const
    CtxLoadInstrs* = {ctxLoad8..ctxLoadFprPair}
    CtxStoreInstrs* = {ctxStore8..ctxStoreFprPair}
    UnOpInstrs* = {
        identity,

        extractLo, extractMid, extractHi,

        bitNot, bitNotX, condNot,

        clz,

        extsb, extsh,
        extsbX, extshX,
        extswX, extzwX,

        ppcLoadU8, ppcLoadU16, ppcLoad32,
        ppcLoadFss, ppcLoadFsd,

        dspLoadIMem, dspLoadDMem,

        ppcSyscall,

        fSwizzleD00,
        fSwizzleD11,

        fSwizzleS00,
        fSwizzleS11,

        cvtsd2ss, cvtss2sd,
        cvtpd2ps, cvtps2pd,

        cvtsd2intTrunc,
        cvtss2intTrunc,

        fRessd, fRsqrtsd,
        fRespd, fRsqrtpd,
        fResss, fRsqrtss,
        fResps, fRsqrtps,

        fNegsd, fAbssd,
        fNegpd, fAbspd,

        fNegss, fAbsss,
        fNegps, fAbsps}
    BiOpInstrs* = {
        mergeLo, mergeMid, mergeHi, mergeMidHi,

        iAdd, iSub,
        iAddX, iSubX,

        iMul, iMulhS, iMulhU, iMulX,

        iDivS, iDivU,

        bitAnd, bitOr, bitXor,
        bitAndX, bitOrX, bitXorX,

        lsl, lsr, asr, rol,
        lslX, lsrX, asrX,

        overflowAdd, overflowSub,
        overflowAddX, overflowSubX,
        carryAdd, carrySub,
        carryAddX, carrySubX,

        iCmpEqual,
        iCmpGreaterU, iCmpLessU,
        iCmpGreaterS, iCmpLessS,
        iCmpEqualX,
        iCmpGreaterUX,
        iCmpLessUX,
        iCmpGreaterSX,
        iCmpLessSX,

        condAnd, condOr, condXor,

        ppcStore8, ppcStore16, ppcStore32,
        ppcStoreFss, ppcStoreFsd,

        ppcLoadFsq, ppcLoadFpq,

        dspStoreDMem,

        fMergeD00, fMergeD01, fMergeD10, fMergeD11,
        fMergeS00, fMergeS01, fMergeS10, fMergeS11,

        fAddsd, fSubsd, fMulsd, fDivsd,
        fAddpd, fSubpd, fMulpd, fDivpd,
        fAddss, fSubss, fMulss, fDivss,
        fAddps, fSubps, fMulps, fDivps,

        fCmpEqualsd, fCmpGreatersd, fCmpLesssd, fUnorderedsd}
    TriOpInstrs* = {
        csel, cselX,
        iAddExtended, iSubExtended,
        overflowAddExtended, overflowSubExtended,
        carryAddExtended, carrySubExtended,

        ppcStoreFsq, ppcStoreFpq,

        ppcBranch, dspBranch,

        fMaddsd, fMsubsd, fNmaddsd, fNmsubsd,
        fMaddpd, fMsubpd, fNmaddpd, fNmsubpd,

        fMaddss, fMsubss, fNmaddss, fNmsubss,
        fMaddps, fMsubps, fNmaddps, fNmsubps}

    ResultlessOps* = {
        ctxStore8..ctxStoreFprPair,
        sprStore32,

        ppcStore8..ppcStoreFpq,
        dspStoreDMem,

        ppcBranch,
        ppcSyscall,

        dspBranch,
        
        ppcCallInterpreter,
        dspCallInterpreter}

    SideEffectOps* = CtxStoreInstrs + {
        sprStore32,

        ppcLoadU8..dspStoreDMem,

        ppcBranch, ppcSyscall,
        dspBranch,
        ppcCallInterpreter, dspCallInterpreter}
    StrictSideEffectOps* = SideEffectOps + CtxLoadInstrs + {sprLoad32}

    FpScalarOps* = {
        ppcStoreFsd, ppcStoreFss,
        fSwizzleD00, fMergeD00,
        fSwizzleS00, fMergeS00,
        fRessd, fRsqrtsd,
        fResss, fRsqrtss,
        cvtsd2ss, cvtss2sd,
        cvtsd2intTrunc, cvtss2intTrunc,
        fNegsd, fAbssd,
        fNegss, fAbsss,
        fAddsd, fSubsd, fMulsd, fDivsd,
        fAddss, fSubss, fMulss, fDivss,
        fMaddsd, fMsubsd, fNmaddsd, fNmsubsd,
        fMaddss, fMsubss, fNmaddss, fNmsubss,
        fCmpEqualsd, fCmpGreatersd, fCmpLesssd, fUnorderedsd}
    FpAllSrcsReadPair* = {
        fSwizzleD11, fMergeD11,
        cvtpd2ps, cvtps2pd,
        fRespd, fRsqrtpd,
        fNegpd, fAbspd,
        fAddpd, fSubpd, fMulpd, fDivpd,
        fMaddpd, fMsubpd, fNmaddpd, fNmsubpd}

    HasWideResult* = {
        loadImmI,
        ctxLoad64,
        mergeLo, mergeMid, mergeHi, mergeMidHi,
        iAddX, iSubX,
        bitAndX, bitOrX, bitXorX, bitNotX,
        lslX, lsrX, asrX,
        extsbX, extshX,
        extswX}

type
    IrInstrRef* = distinct uint32

    IrInstr* = object
        case kind*: InstrKind
        of loadImmI:
            immValI*: uint64
        of ppcCallInterpreter, dspCallInterpreter:
            instr*, pc*: uint32
            target*: pointer
        of CtxLoadInstrs, CtxStoreInstrs:
            ctxOffset*: uint32
        of extractBit, mergeBit:
            bit*: uint32
        of sprStore32, sprLoad32:
            spr*: Spr
        else: discard

        srcRegular: array[3, IrInstrRef]

        lastRead*, numUses*: int32

    IrFunc* = ref object
        blocks*: seq[IrBasicBlock]

        instrPool*: seq[IrInstr]
        freeInstrs*: seq[IrInstrRef]

    IrBasicBlock* = ref object
        instrs*: seq[IrInstrRef]

    IrBlockBuilder*[T] = object
        instrs*: seq[IrInstrRef]
        fn*: IrFunc

        regs*: T

    Spr* = enum
        tbL
        tbU
        decrementer
        dmaL
        wpar
        hid0
        iBatLo0
        iBatLo1
        iBatLo2
        iBatLo3
        iBatHi0
        iBatHi1
        iBatHi2
        iBatHi3
        dBatLo0
        dBatLo1
        dBatLo2
        dBatLo3
        dBatHi0
        dBatHi1
        dBatHi2
        dBatHi3
        pcs
        pss
        eas
        lcs

const
    InvalidIrInstrRef* = IrInstrRef(0)

proc numSources*(instr: IrInstr): int =
    case instr.kind
    of loadImmI, sprLoad32, CtxLoadInstrs, ppcCallInterpreter, dspCallInterpreter:
        0
    of CtxStoreInstrs, sprStore32, extractBit, UnopInstrs:
        1
    of BiOpInstrs, mergeBit:
        2
    of TriOpInstrs:
        3

proc source*(instr: var IrInstr, i: int): var IrInstrRef =
    assert i < instr.numSources
    return instr.srcRegular[i]

proc source*(instr: IrInstr, i: int): IrInstrRef =
    assert i < instr.numSources
    return instr.srcRegular[i]

iterator sources*(instr: IrInstr): IrInstrRef =
    var i = 0
    while i < instr.numSources:
        yield instr.source(i)
        i += 1

proc `==`*(a, b: IrInstrRef): bool {.borrow.}
proc `$`*(r: IrInstrRef): string =
    result = "$"
    result.addInt int(r)
proc hash*(iref: IrInstrRef): Hash =
    hash(int(iref))

func `==`*(a, b: IrInstr): bool =
    if a.kind == b.kind:
        case a.kind
        of loadImmI:
            a.immValI == b.immValI
        of sprLoad32:
            a.spr == b.spr
        of sprStore32:
            a.spr == b.spr and a.srcRegular[0] == b.srcRegular[0]
        of ppcCallInterpreter, dspCallInterpreter:
            a.instr == b.instr and a.pc == b.pc and
                a.target == b.target
        of CtxLoadInstrs:
            a.ctxOffset == b.ctxOffset
        of CtxStoreInstrs:
            a.ctxOffset == b.ctxOffset and
                a.srcRegular[0] == b.srcRegular[0]
        of extractBit:
            a.bit == b.bit and a.srcRegular[0] == b.srcRegular[0]
        of mergeBit:
            a.bit == b.bit and a.srcRegular[0] == b.srcRegular[0] and
                a.srcRegular[1] == b.srcRegular[1]
        of UnopInstrs:
            a.srcRegular[0] == b.srcRegular[0]
        of BiOpInstrs:
            a.srcRegular[0] == b.srcRegular[0] and
                a.srcRegular[1] == b.srcRegular[1]
        of TriOpInstrs:
            a.srcRegular[0] == b.srcRegular[0] and
                a.srcRegular[1] == b.srcRegular[1] and
                a.srcRegular[2] == b.srcRegular[2]
    else:
        false

func hash*(instr: IrInstr): Hash =
    result = hash(instr.kind)
    case instr.kind
    of loadImmI:
        result = result !& hash(instr.immValI)
    of sprLoad32:
        result = result !& hash(instr.spr)
    of sprStore32:
        result = result !& hash(instr.spr)
        result = result !& hash(instr.srcRegular[0])
    of ppcCallInterpreter, dspCallInterpreter:
        result = result !& hash(instr.instr)
        result = result !& hash(instr.pc)
        result = result !& hash(instr.target)
    of CtxLoadInstrs:
        result = result !& hash(instr.ctxOffset)
    of CtxStoreInstrs:
        result = result !& hash(instr.ctxOffset)
        result = result !& hash(instr.srcRegular[0])
    of extractBit:
        result = result !& hash(instr.bit)
        result = result !& hash(instr.srcRegular[0])
    of mergeBit:
        result = result !& hash(instr.bit)
        result = result !& hash(instr.srcRegular[0])
        result = result !& hash(instr.srcRegular[1])
    of UnopInstrs:
        result = result !& hash(instr.srcRegular[0])
    of BiOpInstrs:
        for i in 0..<2:
            result = result !& hash(instr.srcRegular[i])
    of TriOpInstrs:
        for i in 0..<3:
            result = result !& hash(instr.srcRegular[i])
    result = !$ result

iterator msources*(instr: var IrInstr): var IrInstrRef =
    case instr.kind
    of CtxLoadInstrs, loadImmI:
        discard
    else:
        var i = 0
        while i < instr.numSources:
            yield instr.source(i)
            i += 1

proc allocInstr(fn: IrFunc): IrInstrRef =
    if fn.freeInstrs.len > 0:
        fn.freeInstrs.pop()
    else:
        let instr = IrInstrRef(fn.instrPool.len + 1)
        fn.instrPool.setLen(fn.instrPool.len + 1)
        instr

# this used to be a proc, but that caused some problems
# where the pointer returned by this proc would be calculated first
# but then then the left side is evaluated, resizing the seq
# and thus potentially invalidating it.
#
# see https://github.com/nim-lang/Nim/issues/18683
template getInstr*(f: IrFunc, iref: IrInstrRef): IrInstr =
    assert iref != InvalidIrInstrRef
    f.instrPool[int(iref)-1]

proc makeImm*(val: uint64): IrInstr {.inline.} =
    IrInstr(kind: loadImmI, immValI: val)

proc makeImm*(val: bool): IrInstr {.inline.} =
    IrInstr(kind: loadImmI, immValI: uint64(val))

proc makeLoadctx*(kind: InstrKind, offset: uint32): IrInstr {.inline.} =
    case kind
    of CtxLoadInstrs:
        IrInstr(kind: kind, ctxOffset: offset)
    else:
        raiseAssert(&"invalid context load kind {kind}")

proc makeStorectx*(kind: InstrKind, offset: uint32, val: IrInstrRef): IrInstr {.inline.} =
    case kind
    of CtxStoreInstrs:
        assert val != InvalidIrInstrRef
        IrInstr(kind: kind, ctxOffset: offset, srcRegular: [val, InvalidIrInstrRef, InvalidIrInstrRef])
    else:
        raiseAssert(&"invalid context store kind {kind}")

proc makeUnop*(kind: InstrKind, operand: IrInstrRef): IrInstr {.inline.} =
    case kind
    of UnOpInstrs:
        IrInstr(kind: kind, srcRegular: [operand, InvalidIrInstrRef, InvalidIrInstrRef])
    else:
        raiseAssert(&"invalid unop kind {kind}")

proc makeBiop*(kind: InstrKind, a, b: IrInstrRef): IrInstr {.inline.} =
    case kind
    of BiOpInstrs:
        IrInstr(kind: kind, srcRegular: [a, b, InvalidIrInstrRef])
    else:
        raiseAssert(&"invalid biop kind {kind}")

proc makeTriop*(kind: InstrKind, a, b, c: IrInstrRef): IrInstr {.inline.} =
    case kind
    of TriOpInstrs:
        IrInstr(kind: kind, srcRegular: [a, b, c])
    else:
        raiseAssert(&"invalid triop kind {kind}")

proc makeExtractBit*(val: IrInstrRef, bit: uint32): IrInstr {.inline.} =
    IrInstr(kind: extractBit, srcRegular: [val, InvalidIrInstrRef, InvalidIrInstrRef], bit: bit)

proc makeMergeBit*(val, mergeval: IrInstrRef, bit: uint32): IrInstr {.inline.} =
    IrInstr(kind: mergeBit, srcRegular: [val, mergeval, InvalidIrInstrRef], bit: bit)

proc makeLoadSpr*(spr: Spr): IrInstr {.inline.} =
    IrInstr(kind: sprLoad32, spr: spr)

proc makeStoreSpr*(spr: Spr, val: IrInstrRef): IrInstr {.inline.} =
    IrInstr(kind: sprStore32, spr: spr, srcRegular: [val, InvalidIrInstrRef, InvalidIrInstrRef])

proc makeIdentity*(iref: IrInstrRef): IrInstr {.inline.} =
    makeUnop(identity, iref)

proc narrowIdentity*(fn: IrFunc, iref: IrInstrRef): IrInstr {.inline.} =
    let kind = fn.getInstr(iref).kind
    if kind == loadImmI:
        makeImm(uint32 fn.getInstr(iref).immValI)
    elif kind in HasWideResult:
        makeUnop(extzwX, iref)
    else:
        makeIdentity(iref)

# block building helpers
proc imm*[T](builder: var IrBlockBuilder[T], val: uint64): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeImm(val)
    add(builder.instrs, result)

proc imm*[T](builder: var IrBlockBuilder[T], val: bool): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeImm(val)
    add(builder.instrs, result)

proc loadctx*[T](builder: var IrBlockBuilder[T], kind: InstrKind, idx: uint32): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeLoadctx(kind, idx)
    add(builder.instrs, result)

proc storectx*[T](builder: var IrBlockBuilder[T], kind: InstrKind, idx: uint32, val: IrInstrRef) =
    let result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeStorectx(kind, idx, val)
    add(builder.instrs, result)

proc unop*[T](builder: var IrBlockBuilder[T], kind: InstrKind, val: IrInstrRef): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeUnop(kind, val)
    add(builder.instrs, result)

proc biop*[T](builder: var IrBlockBuilder[T], kind: InstrKind, a, b: IrInstrRef): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeBiop(kind, a, b)
    add(builder.instrs, result)

proc triop*[T](builder: var IrBlockBuilder[T], kind: InstrKind, a, b, c: IrInstrRef): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeTriop(kind, a, b, c)
    add(builder.instrs, result)

proc interpretppc*[T](builder: var IrBlockBuilder[T], instrcode, pc: uint32, target: pointer) =
    let result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = IrInstr(kind: ppcCallInterpreter, target: target, instr: instrcode, pc: pc)
    add(builder.instrs, result)

proc interpretdsp*[T](builder: var IrBlockBuilder[T], instrcode, pc: uint32, target: pointer) =
    let result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = IrInstr(kind: dspCallInterpreter, target: target, instr: instrcode, pc: pc)
    add(builder.instrs, result)

proc extractBit*[T](builder: var IrBlockBuilder[T], val: IrInstrRef, bit: uint32): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeExtractBit(val, bit)
    add(builder.instrs, result)

proc mergeBit*[T](builder: var IrBlockBuilder[T], val, mergeVal: IrInstrRef, bit: uint32): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeMergeBit(val, mergeVal, bit)
    add(builder.instrs, result)

proc loadSpr*[T](builder: var IrBlockBuilder[T], spr: Spr): IrInstrRef =
    result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeLoadSpr(spr)
    add(builder.instrs, result)

proc storeSpr*[T](builder: var IrBlockBuilder[T], spr: Spr, val: IrInstrRef) =
    let result = builder.fn.allocInstr()
    builder.fn.getInstr(result) = makeStoreSpr(spr, val)
    add(builder.instrs, result)

proc isImmVal*(fn: IrFunc, iref: IrInstrRef, imm: bool): bool =
    let instr = fn.getInstr(iref)
    if instr.kind == loadImmI:
        bool(instr.immValI)
    else:
        false

proc isImmVal*(fn: IrFunc, iref: IrInstrRef, imm: uint32): bool =
    let instr = fn.getInstr(iref)
    instr.kind == loadImmI and uint32(instr.immValI) == imm

proc isImmValX*(fn: IrFunc, iref: IrInstrRef, imm: uint64): bool =
    let instr = fn.getInstr(iref)
    instr.kind == loadImmI and instr.immValI == imm

proc isImmValI*(fn: IrFunc, iref: IrInstrRef): Option[uint32] =
    let instr = fn.getInstr(iref)
    if instr.kind == loadImmI:
        some(uint32(instr.immValI))
    else:
        none(uint32)

proc isImmValIX*(fn: IrFunc, iref: IrInstrRef): Option[uint64] =
    let instr = fn.getInstr(iref)
    if instr.kind == loadImmI:
        some(instr.immValI)
    else:
        none(uint64)

proc isImmValB*(fn: IrFunc, iref: IrInstrRef): Option[bool] =
    let instr = fn.getInstr(iref)
    if instr.kind == loadImmI:
        some(bool instr.immValI)
    else:
        none(bool)

proc isEitherImmI*(fn: IrFunc, a, b: IrInstrRef): Option[(IrInstrRef, uint32)] =
    if (let instr = fn.getInstr(a); instr.kind == loadImmI):
        some((b, uint32(instr.immValI)))
    elif (let instr = fn.getInstr(b); instr.kind == loadImmI):
        some((a, uint32(instr.immValI)))
    else:
        none((IrInstrRef, uint32))

proc isEitherImmIX*(fn: IrFunc, a, b: IrInstrRef): Option[(IrInstrRef, uint64)] =
    if (let instr = fn.getInstr(a); instr.kind == loadImmI):
        some((b, instr.immValI))
    elif (let instr = fn.getInstr(b); instr.kind == loadImmI):
        some((a, instr.immValI))
    else:
        none((IrInstrRef, uint64))

proc prettify*(blk: IrBasicBlock, fn: IrFunc): string =
    for i, iref in pairs blk.instrs:
        if iref == InvalidIrInstrRef:
            result &= "<invalid instr ref>"
        else:
            let instr = fn.getInstr(iref)

            let lineStartLen = result.len
            result &= &"{i:03}: "
            if instr.kind notin ResultlessOps:
                result &= &"${int(iref)}"

            const maxInstrNameLen = (proc(): int =
                    var maxLen = 0
                    for kind in InstrKind:
                        maxLen = max(maxLen, len($kind))
                    maxLen)()

            while (result.len-lineStartLen) < maxInstrNameLen+1:
                result &= ' '

            if instr.kind in ResultlessOps:
                result &= "   "
            else:
                result &= " = "
            result &= &"{instr.kind} "
            case instr.kind
            of loadImmI:
                result &= &"{instr.immValI:016X}"
            of CtxLoadInstrs:
                result &= &"{instr.ctxOffset}"
            of CtxStoreInstrs:
                result &= &"{instr.ctxOffset}, ${int(instr.source(0))}"
            of extractBit:
                result &= &"{instr.bit}"
            of mergeBit:
                result &= &"{instr.bit}, {instr.source(0)}, {instr.source(1)}"
            of sprLoad32:
                result &= &"{instr.spr}"
            of sprStore32:
                result &= &"{instr.spr}, {instr.source(0)}"
            of dspCallInterpreter, ppcCallInterpreter:
                result &= &"{instr.instr:08X}"
            else:
                for i in 0..<instr.numSources:
                    if i > 0:
                        result &= ", "
                    result &= '$'
                    result &= $int(instr.source(i))
            if instr.kind in ResultlessOps:
                result &= '\n'
            else:
                result &= &" (last read: {instr.lastRead})\n"
