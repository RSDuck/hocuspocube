import
    strutils, strformat, options, hashes

#[
    Some notes on this weird IR I've come up with.

    It isn't directly based on anything, for the most part it's just
    stuff I liked I picked up from other projects (dynarmic, xenia, ...).

    The entire IR is implicitly typed and a reference to an IR instruction
    also refers to it's value. Every value is either an integer, a boolean value
    or a floating point value. Booleans are just integers which are either 0
    or 1.

    Unless marked with an X in the name integer instructions use only the lower
    32-bit of the source values. If they produce an integer value as a result,
    the upper 32-bit of it are going to be zero (similarly to how 32-bit instructions
    work on x64/aarch64).
]#

type
    InstrKind* = enum
        identity = "ident"

        loadImmI = "loadi"
        loadImmB = "loadb"

        # PowerPC reg load/store instructions
        loadPpcReg = "ldrreg_ppc"
        storePpcReg = "strreg_ppc"
        loadCrBit = "ldrcr"
        storeCrBit = "strcr"
        loadXer = "ldrxer"
        storeXer = "strxer"
        loadSpr = "ldrspr"
        storeSpr = "strspr"
        loadFpr = "ldrfreg"
        storeFpr = "strfreg"
        loadFprPair = "ldrfregp"
        storeFprPair = "strfregp"

        # DSP reg load/store instructions
        loadAccum = "ldraccum"
        storeAccum = "straccum"
        loadStatusBit = "ldrstat"
        storeStatusBit = "strstat"
        loadDspReg = "ldrreg_dsp"
        storeDspReg = "strreg_dsp"

        # used for DSP partial loads/stores to dsp accumulators
        extractLo = "extrlo"
        extractMid = "extrmid"
        extractHi = "extrhi"
        mergeLo = "mergelo"
        mergeMid = "mergemid"
        mergeHi = "mergehi"

        csel = "csel"
        cselX = "cselx"

        iAdd = "iadd"
        iSub = "isub"

        iAddX = "iaddx"
        iSubX = "isubx"

        iAddExtended = "iaddext"
        iSubExtended = "isubext"

        imul = "mul"
        iMulhS = "mulhs"
        iMulhU = "mulhu"

        iDivS = "divs"
        iDivU = "divu"

        bitAnd = "bitand"
        bitOr = "bitor"
        bitXor = "bitxor"
        bitNot = "bitnot"

        bitAndX = "bitandx"
        bitOrX = "bitorx"
        bitXorX = "bitxorx"
        bitNotX = "bitnotx"

        lsl = "lsl"
        lsr = "lsr"
        asr = "asr"
        rol = "rol"

        lslX = "lslx"
        lsrX = "lsrx"
        asrX = "asrx"

        clz = "clz"

        extsb = "extsb"
        extsh = "extsh"

        extzw = "extzw"
        extsw = "extsw"

        condAnd = "condand"
        condOr = "condor"
        condXor = "condxor"
        condNot = "condnot"

        overflowAdd = "addov"
        overflowSub = "subov"

        overflowAddX = "addovx"
        overflowSubX = "subovx"

        carryAdd = "addca"
        carrySub = "subca"

        carryAddX = "addcax"
        carrySubX = "subcax"

        overflowAddExtended = "addextov"
        overflowSubExtended = "subextov"

        carryAddExtended = "addextca"
        carrySubExtended = "subextca"

        iCmpEqual = "cmpieq"
        iCmpGreaterU = "cmpigtu"
        iCmpLessU = "cmpiltu"
        iCmpGreaterS = "cmpgts"
        iCmpLessS = "cmplts"

        iCmpEqualX = "cmpieqx"
        iCmpGreaterUX = "cmpigtux"
        iCmpLessUX = "cmpiltux"
        iCmpGreaterSX = "cmpgtsx"
        iCmpLessSX = "cmpltsx"

        ppcLoadU8 = "ldrb"
        ppcLoadU16 = "ldrh"
        ppcLoadS16 = "ldrsh"
        ppcLoad32 = "ldr"
        ppcLoadFss = "ldrfss"
        ppcLoadFsd = "ldrfsd"
        ppcLoadFsq = "ldrfsq"
        ppcLoadFpq = "ldrfpq"

        ppcStore8 = "strb"
        ppcStore16 = "strh"
        ppcStore32 = "str"
        ppcStoreFss = "strfss"
        ppcStoreFsd = "strfsd"
        ppcStoreFsq = "strfsq"
        ppcStoreFpq = "strfpq"

        ppcBranch = "b_ppc"
        ppcSyscall = "sc_ppc"

        dspBranch = "b_dsp"

        fSwizzleD00 = "swizzlefd00"
        fSwizzleD11 = "swizzlefd11"

        fSwizzleS00 = "swizzlefs00"
        fSwizzleS11 = "swizzlefs11"

        fMergeD00 = "mergefd00"
        fMergeD01 = "mergefd01"
        fMergeD10 = "mergefd10"
        fMergeD11 = "mergefd11"

        fMergeS00 = "mergefs00"
        fMergeS01 = "mergefs01"
        fMergeS10 = "mergefs10"
        fMergeS11 = "mergefs11"

        cvtsd2ss = "cvtsd2ss"
        cvtss2sd = "cvtss2sd"

        cvtpd2ps = "cvtpd2ps"
        cvtps2pd = "cvtps2pd"

        cvtsd2intTrunc = "cvtsd2intTrunc"
        cvtss2intTrunc = "cvtss2intTrunc"

        fRessd = "fressd"
        fRsqrtsd = "frsqrtsd"

        fResss = "fresss"
        fRsqrtss = "frsqrtss"

        fRespd = "frespd"
        fRsqrtpd = "frsqrtpd"

        fResps = "fresps"
        fRsqrtps = "frsqrtps"

        fNegsd = "fnegsd"
        fAbssd = "fabssd"

        fNegss = "fnegss"
        fAbsss = "fabsss"

        fNegpd = "fnegpd"
        fAbspd = "fabspd"

        fNegps = "fnegps"
        fAbsps = "fabsps"

        fAddsd = "faddsd"
        fSubsd = "fsubsd"
        fMulsd = "fmulsd"
        fDivsd = "fdivsd"

        fAddss = "faddss"
        fSubss = "fsubss"
        fMulss = "fmulss"
        fDivss = "fdivss"

        fAddpd = "faddpd"
        fSubpd = "fsubpd"
        fMulpd = "fmulpd"
        fDivpd = "fdivpd"

        fAddps = "faddpds"
        fSubps = "fsubpds"
        fMulps = "fmulpds"
        fDivps = "fdivpds"

        fMaddsd = "fmaddsd"
        fMsubsd = "fmsubsd"
        fNmaddsd = "fnmaddsd"
        fNmsubsd = "fnmsubsd"

        fMaddss = "fmaddss"
        fMsubss = "fmsubss"
        fNmaddss = "fnmaddss"
        fNmsubss = "fnmsubss"

        fMaddpd = "fmaddpd"
        fMsubpd = "fmsubpd"
        fNmaddpd = "fnmaddpd"
        fNmsubpd = "fnmsubpd"

        fMaddps = "fmaddps"
        fMsubps = "fmsubps"
        fNmaddps = "fnmaddps"
        fNmsubps = "fnmsubps"

        fCmpEqualsd = "cmpfeq"
        fCmpGreatersd = "cmpfgt"
        fCmpLesssd = "cmpflt"
        fUnorderedsd = "cmpfunord"

        ppcCallInterpreter = "interpretppc"
        dspCallInterpreter = "interpretdsp"

const
    LoadImmInstrs* = {
        loadImmI, loadImmB}
    CtxLoadInstrs* = {
        loadPpcReg, loadCrBit, loadXer, loadSpr, loadFpr, loadFprPair,
        loadAccum, loadStatusBit, loadDspReg}
    CtxStoreInstrs* = {
        storePpcReg, storeCrBit, storeXer, storeSpr, storeFpr, storeFprPair,
        storeAccum, storeStatusBit, storeDspReg}
    UnOpInstrs* = {
        identity,

        extractLo, extractMid, extractHi,

        bitNot, bitNotX, condNot,

        clz,

        extsb, extsh,
        extsw, extzw,

        ppcLoadU8, ppcLoadU16, ppcLoadS16, ppcLoad32,
        ppcLoadFss, ppcLoadFsd,

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
        mergeLo, mergeMid, mergeHi,

        iAdd, iSub,
        iAddX, iSubX,

        imul, iMulhS, iMulhU,

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
        storePpcReg,
        storeCrBit, storeXer, storeSpr,
        storeFpr, storeFprPair,
        ppcStore8, ppcStore16, ppcStore32,
        ppcStoreFss, ppcStoreFsd, ppcStoreFsq, ppcStoreFpq,

        ppcBranch,
        ppcSyscall,

        dspBranch,
        
        ppcCallInterpreter,
        dspCallInterpreter}

    SideEffectOps* = {
        storePpcReg, storeCrBit, storeXer,
        storeSpr, storeFpr, storeFprPair,
        storeAccum, storeStatusBit, storeDspReg,

        ppcLoadU8, ppcLoadU16, ppcLoadS16, ppcLoad32,
        ppcLoadFss, ppcLoadFsd, ppcLoadFsq, ppcLoadFpq,

        ppcStore8, ppcStore16, ppcStore32,
        ppcStoreFss, ppcStoreFsd, ppcStoreFsq, ppcStoreFpq,

        ppcBranch, ppcSyscall,
        dspBranch,
        ppcCallInterpreter, dspCallInterpreter}
    StrictSideEffectOps* = SideEffectOps + {
        loadPpcReg, loadCrBit, loadXer, loadSpr,
        loadFpr, loadFprPair,
        loadAccum, loadStatusBit, loadDspReg}

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
    FpPairOps* = {
        fSwizzleD11, fMergeD11,
        cvtpd2ps, cvtps2pd,
        fRespd, fRsqrtpd,
        fNegpd, fAbspd,
        fAddpd, fSubpd, fMulpd, fDivpd,
        fMaddpd, fMsubpd, fNmaddpd, fNmsubpd}

    HasWideResult* = {
        loadAccum,
        mergeLo, mergeMid, mergeHi,
        iAddX, iSubX,
        bitAndX, bitOrX, bitXorX, bitNotX,
        lslX, lsrX, asrX,
        extsw}

type
    IrInstrRef* = distinct int32

    IrInstr* = object
        case kind*: InstrKind
        of loadImmI:
            immValI*: uint64
        of loadImmB:
            immValB*: bool
        of ppcCallInterpreter, dspCallInterpreter:
            instr*, pc*: uint32
            target*: pointer
        of CtxLoadInstrs:
            ctxLoadIdx*: uint32
        of CtxStoreInstrs:
            ctxStoreIdx*: uint32
        else: discard

        srcRegular: array[3, IrInstrRef]

        lastRead*, numUses*: int32

    IrBasicBlock* = ref object
        instrs*: seq[IrInstrRef]

        instrPool*: seq[IrInstr]
        freeInstrs*: seq[IrInstrRef]

    IrBlockBuilder*[T] = object
        blk*: IrBasicBlock

        regs*: T

    IrXerNum* = enum
        irXerNumOv
        irXerNumCa
        irXerNumSo

    IrSprNum* = enum
        irSprNumCr
        irSprNumXer
        irSprNumLr
        irSprNumCtr

        irSprNumMsr

        irSprNumSrr0
        irSprNumSrr1

        irSprNumDsisr
        irSprNumDar

        irSprNumSprg0
        irSprNumSprg1
        irSprNumSprg2
        irSprNumSprg3

        irSprNumHid0
        irSprNumHid1
        irSprNumHid2

        irSprNumTbU
        irSprNumTbL

        irSprNumDec

        irSprNumL2cr

        irSprNumMmcr0
        irSprNumMmcr1

        irSprNumPmc0
        irSprNumPmc1
        irSprNumPmc2
        irSprNumPmc3

        irSprNumWpar
        irSprNumDmaL
        irSprNumDmaU

        irSprNumGqr0
        irSprNumGqr1
        irSprNumGqr2
        irSprNumGqr3
        irSprNumGqr4
        irSprNumGqr5
        irSprNumGqr6
        irSprNumGqr7

        irSprNumIBatL0
        irSprNumIBatL1
        irSprNumIBatL2
        irSprNumIBatL3
        irSprNumIBatU0
        irSprNumIBatU1
        irSprNumIBatU2
        irSprNumIBatU3
        irSprNumDBatL0
        irSprNumDBatL1
        irSprNumDBatL2
        irSprNumDBatL3
        irSprNumDBatU0
        irSprNumDBatU1
        irSprNumDBatU2
        irSprNumDBatU3

    DspAccum* = enum
        dspAccumA
        dspAccumB
        dspAccumX
        dspAccumY
        dspAccumProd

    DspStatusBit* = enum
        dspStatusBitCa
        dspStatusBitOv
        dspStatusBitZr
        dspStatusBitMi
        dspStatusBitExt
        dspStatusBitUnnorm
        dspStatusBitTb
        dspStatusBitSv
        dspStatusBitTe0
        dspStatusBitTe1
        dspStatusBitTe2
        dspStatusBitTe3
        dspStatusBitEt
        dspStatusBitIm
        dspStatusBitXl
        dspStatusBitDp

const
    InvalidIrInstrRef* = IrInstrRef(-1)

proc numSources*(instr: IrInstr): int =
    case instr.kind
    of LoadImmInstrs, CtxLoadInstrs, ppcCallInterpreter, dspCallInterpreter:
        0
    of CtxStoreInstrs, UnopInstrs:
        1
    of BiOpInstrs:
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
        of loadImmB:
            a.immValB == b.immValB
        of ppcCallInterpreter, dspCallInterpreter:
            a.instr == b.instr and a.pc == b.pc and
                a.target == b.target
        of CtxLoadInstrs:
            a.ctxLoadIdx == b.ctxLoadIdx
        of CtxStoreInstrs:
            a.ctxStoreIdx == b.ctxStoreIdx and
                a.srcRegular[0] == b.srcRegular[0]
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
    of loadImmB:
        result = result !& hash(instr.immValB)
    of ppcCallInterpreter, dspCallInterpreter:
        result = result !& hash(instr.instr)
        result = result !& hash(instr.pc)
        result = result !& hash(instr.target)
    of CtxLoadInstrs:
        result = result !& hash(instr.ctxLoadIdx)
    of CtxStoreInstrs:
        result = result !& hash(instr.ctxStoreIdx)
        result = result !& hash(instr.srcRegular[0])
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
    of CtxLoadInstrs, LoadImmInstrs:
        discard
    else:
        var i = 0
        while i < instr.numSources:
            yield instr.source(i)
            i += 1

proc allocInstr(blk: IrBasicBlock): IrInstrRef =
    if blk.freeInstrs.len > 0:
        blk.freeInstrs.pop()
    else:
        let instr = IrInstrRef blk.instrPool.len
        blk.instrPool.setLen(blk.instrPool.len + 1)
        instr

# this used to be a proc, but that caused some problems
# where the pointer returned by this proc would be calculated first
# but then then the left side is evaluated, resizing the seq
# and thus potentially invalidating it.
#
# see https://github.com/nim-lang/Nim/issues/18683
template getInstr*(blk: IrBasicBlock, iref: IrInstrRef): IrInstr =
    blk.instrPool[int iref]

proc makeImm*(val: uint64): IrInstr {.inline.} =
    IrInstr(kind: loadImmI, immValI: val)

proc makeImm*(val: bool): IrInstr {.inline.} =
    IrInstr(kind: loadImmB, immValB: val)

proc makeLoadctx*(kind: InstrKind, idx: uint32): IrInstr {.inline.} =
    case kind
    of CtxLoadInstrs:
        IrInstr(kind: kind, ctxLoadIdx: idx)
    else:
        raiseAssert(&"invalid context load kind {kind}")

proc makeStorectx*(kind: InstrKind, idx: uint32, val: IrInstrRef): IrInstr {.inline.} =
    case kind
    of CtxStoreInstrs:
        IrInstr(kind: kind, ctxStoreIdx: idx, srcRegular: [val, InvalidIrInstrRef, InvalidIrInstrRef])
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

proc makeIdentity*(iref: IrInstrRef): IrInstr {.inline.} =
    makeUnop(identity, iref)

proc narrowIdentity*(blk: IrBasicBlock, iref: IrInstrRef): IrInstr {.inline.} =
    if blk.getInstr(iref).kind in HasWideResult:
        makeUnop(extzw, iref)
    else:
        makeIdentity(iref)

# block building helpers
proc imm*(blk: IrBasicBlock, val: uint64): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeImm(val)
    add(blk.instrs, result)

template imm*[T](builder: IrBlockBuilder[T], val: uint64): IrInstrRef =
    imm(builder.blk, val)

proc imm*(blk: IrBasicBlock, val: bool): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeImm(val)
    add(blk.instrs, result)

template imm*[T](builder: IrBlockBuilder[T], val: bool): IrInstrRef =
    imm(builder.blk, val)

proc loadctx*(blk: IrBasicBlock, kind: InstrKind, idx: uint32): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeLoadctx(kind, idx)
    add(blk.instrs, result)

template loadctx*[T](builder: IrBlockBuilder[T], kind: InstrKind, idx: uint32): IrInstrRef =
    loadctx(builder.blk, kind, idx)

proc storectx*(blk: IrBasicBlock, kind: InstrKind, idx: uint32, val: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeStorectx(kind, idx, val)
    add(blk.instrs, result)

template storectx*[T](builder: IrBlockBuilder[T], kind: InstrKind, idx: uint32, val: IrInstrRef): IrInstrRef =
    storectx(builder.blk, kind, idx, val)

proc unop*(blk: IrBasicBlock, kind: InstrKind, val: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeUnop(kind, val)
    add(blk.instrs, result)

template unop*[T](builder: IrBlockBuilder[T], kind: InstrKind, val: IrInstrRef): IrInstrRef =
    unop(builder.blk, kind, val)

proc biop*(blk: IrBasicBlock, kind: InstrKind, a, b: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeBiop(kind, a, b)
    add(blk.instrs, result)

template biop*[T](builder: IrBlockBuilder[T], kind: InstrKind, a, b: IrInstrRef): IrInstrRef =
    biop(builder.blk, kind, a, b)

proc triop*(blk: IrBasicBlock, kind: InstrKind, a, b, c: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeTriop(kind, a, b, c)
    add(blk.instrs, result)

template triop*[T](builder: IrBlockBuilder[T], kind: InstrKind, a, b, c: IrInstrRef): IrInstrRef =
    triop(builder.blk, kind, a, b, c)

proc interpretppc*(blk: IrBasicBlock, instrcode, pc: uint32, target: pointer): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = IrInstr(kind: ppcCallInterpreter, target: target, instr: instrcode, pc: pc)
    add(blk.instrs, result)

template interpreter*[T](builder: IrBlockBuilder[T], instrcode, pc: uint32, target: pointer): untyped =
    discard interpretppc(builder.blk, instrcode, pc, target)

proc interpretdsp*(blk: IrBasicBlock, instrcode, pc: uint32, target: pointer): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = IrInstr(kind: dspCallInterpreter, target: target, instr: instrcode, pc: pc)
    add(blk.instrs, result)

template interpretdsp*[T](builder: IrBlockBuilder[T], instrcode, pc: uint32, target: pointer): untyped =
    discard interpretdsp(builder.blk, instrcode, pc, target)

proc isImmVal*(blk: IrBasicBlock, iref: IrInstrRef, imm: bool): bool =
    let instr = blk.getInstr(iref)
    instr.kind == loadImmB and instr.immValB == imm

proc isImmVal*(blk: IrBasicBlock, iref: IrInstrRef, imm: uint32): bool =
    let instr = blk.getInstr(iref)
    instr.kind == loadImmI and uint32(instr.immValI) == imm

proc isImmValX*(blk: IrBasicBlock, iref: IrInstrRef, imm: uint64): bool =
    let instr = blk.getInstr(iref)
    instr.kind == loadImmI and instr.immValI == imm

proc isImmValI*(blk: IrBasicBlock, iref: IrInstrRef): Option[uint32] =
    let instr = blk.getInstr(iref)
    if instr.kind == loadImmI:
        some(uint32(instr.immValI))
    else:
        none(uint32)

proc isImmValIX*(blk: IrBasicBlock, iref: IrInstrRef): Option[uint64] =
    let instr = blk.getInstr(iref)
    if instr.kind == loadImmI:
        some(instr.immValI)
    else:
        none(uint64)

proc isImmValB*(blk: IrBasicBlock, iref: IrInstrRef): Option[bool] =
    let instr = blk.getInstr(iref)
    if instr.kind == loadImmB:
        some(instr.immValB)
    else:
        none(bool)

proc isEitherImmI*(blk: IrBasicBlock, a, b: IrInstrRef): Option[(IrInstrRef, uint32)] =
    if (let instr = blk.getInstr(a); instr.kind == loadImmI):
        some((b, uint32(instr.immValI)))
    elif (let instr = blk.getInstr(b); instr.kind == loadImmI):
        some((a, uint32(instr.immValI)))
    else:
        none((IrInstrRef, uint32))

proc isEitherImmIX*(blk: IrBasicBlock, a, b: IrInstrRef): Option[(IrInstrRef, uint64)] =
    if (let instr = blk.getInstr(a); instr.kind == loadImmI):
        some((b, instr.immValI))
    elif (let instr = blk.getInstr(b); instr.kind == loadImmI):
        some((a, instr.immValI))
    else:
        none((IrInstrRef, uint64))

proc `$`*(blk: IrBasicBlock): string =
    for i, iref in pairs blk.instrs:
        let instr = blk.getInstr(iref)

        let lineStartLen = result.len
        result &= &"{i:03}: "
        if instr.kind notin ResultlessOps:
            result &= &"${int(iref)}"

        while (result.len-lineStartLen) < 9:
            result &= ' '

        if instr.kind in ResultlessOps:
            result &= "   "
        else:
            result &= " = "
        result &= &"{instr.kind} "
        case instr.kind
        of loadImmI:
            result &= &"{instr.immValI:016X}"
        of loadImmB:
            result &= $instr.immValB
        of CtxLoadInstrs:
            result &= &"{instr.ctxLoadIdx}"
        of CtxStoreInstrs:
            result &= &"{instr.ctxStoreIdx}, ${int(instr.source(0))}"
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
