import
    strutils, strformat, options, hashes

type
    IrInstrKind* = enum
        irInstrIdentity = "ident"

        irInstrLoadImmI = "loadi"
        irInstrLoadImmB = "loadb"

        # PowerPC reg load/store instructions
        irInstrLoadReg = "ldrreg"
        irInstrStoreReg = "strreg"
        irInstrLoadCrBit = "ldrcr"
        irInstrStoreCrBit = "strcr"
        irInstrLoadXer = "ldrxer"
        irInstrStoreXer = "strxer"
        irInstrLoadSpr = "ldrspr"
        irInstrStoreSpr = "strspr"
        irInstrLoadFpr = "ldrfreg"
        irInstrStoreFpr = "strfreg"
        irInstrLoadFprPair = "ldrfregp"
        irInstrStoreFprPair = "strfregp"

        irInstrCsel = "csel"

        irInstrIAdd = "iadd"
        irInstrISub = "isub"

        irInstrIAddExtended = "iaddext"
        irInstrISubExtended = "isubext"

        irInstrMul = "mul"
        irInstrMulhS = "mulhs"
        irInstrMulhU = "mulhu"

        irInstrDivS = "divs"
        irInstrDivU = "divu"

        irInstrBitAnd = "bitand"
        irInstrBitOr = "bitor"
        irInstrBitXor = "bitxor"
        irInstrBitNot = "bitnot"

        irInstrShl = "shl"
        irInstrShrLogic = "shrl"
        irInstrShrArith = "shra"
        irInstrRol = "rol"

        irInstrClz = "clz"

        irInstrExtsb = "extsb"
        irInstrExtsh = "extsh"

        irInstrCondAnd = "condand"
        irInstrCondOr = "condor"
        irInstrCondXor = "condxor"
        irInstrCondNot = "condnot"

        irInstrOverflowAdd = "addov"
        irInstrOverflowSub = "subov"

        irInstrCarryAdd = "addca"
        irInstrCarrySub = "subca"

        irInstrOverflowAddExtended = "addextov"
        irInstrOverflowSubExtended = "subextov"

        irInstrCarryAddExtended = "addextca"
        irInstrCarrySubExtended = "subextca"

        irInstrCmpEqualI = "cmpieq"
        irInstrCmpGreaterUI = "cmpigtu"
        irInstrCmpLessUI = "cmpiltu"
        irInstrCmpGreaterSI = "cmpgts"
        irInstrCmpLessSI = "cmplts"

        irInstrLoadU8 = "ldrb"
        irInstrLoadU16 = "ldrh"
        irInstrLoadS16 = "ldrsh"
        irInstrLoad32 = "ldr"
        irInstrLoadFss = "ldrfss"
        irInstrLoadFsd = "ldrfsd"
        irInstrLoadFsq = "ldrfsq"
        irInstrLoadFpq = "ldrfpq"

        irInstrStore8 = "strb"
        irInstrStore16 = "strh"
        irInstrStore32 = "str"
        irInstrStoreFss = "strfss"
        irInstrStoreFsd = "strfsd"
        irInstrStoreFsq = "strfsq"
        irInstrStoreFpq = "strfpq"

        irInstrBranchPpc = "bppc"
        irInstrSyscallPpc = "scppc"

        irInstrBranchDsp = "bdsp"

        irInstrFSwizzleD00 = "swizzlefd00"
        irInstrFSwizzleD11 = "swizzlefd11"

        irInstrFSwizzleS00 = "swizzlefs00"
        irInstrFSwizzleS11 = "swizzlefs11"

        irInstrFMergeD00 = "mergefd00"
        irInstrFMergeD01 = "mergefd01"
        irInstrFMergeD10 = "mergefd10"
        irInstrFMergeD11 = "mergefd11"

        irInstrFMergeS00 = "mergefs00"
        irInstrFMergeS01 = "mergefs01"
        irInstrFMergeS10 = "mergefs10"
        irInstrFMergeS11 = "mergefs11"

        irInstrCvtsd2ss = "cvtsd2ss"
        irInstrCvtss2sd = "cvtss2sd"

        irInstrCvtpd2ps = "cvtpd2ps"
        irInstrCvtps2pd = "cvtps2pd"

        irInstrCvtsd2intTrunc = "cvtsd2intTrunc"
        irInstrCvtss2intTrunc = "cvtss2intTrunc"

        irInstrFRessd = "fressd"
        irInstrFRsqrtsd = "frsqrtsd"

        irInstrFResss = "fresss"
        irInstrFRsqrtss = "frsqrtss"

        irInstrFRespd = "frespd"
        irInstrFRsqrtpd = "frsqrtpd"

        irInstrFResps = "fresps"
        irInstrFRsqrtps = "frsqrtps"

        irInstrFNegsd = "fnegsd"
        irInstrFAbssd = "fabssd"

        irInstrFNegss = "fnegss"
        irInstrFAbsss = "fabsss"

        irInstrFNegpd = "fnegpd"
        irInstrFAbspd = "fabspd"

        irInstrFNegps = "fnegps"
        irInstrFAbsps = "fabsps"

        irInstrFAddsd = "faddsd"
        irInstrFSubsd = "fsubsd"
        irInstrFMulsd = "fmulsd"
        irInstrFDivsd = "fdivsd"

        irInstrFAddss = "faddss"
        irInstrFSubss = "fsubss"
        irInstrFMulss = "fmulss"
        irInstrFDivss = "fdivss"

        irInstrFAddpd = "faddpd"
        irInstrFSubpd = "fsubpd"
        irInstrFMulpd = "fmulpd"
        irInstrFDivpd = "fdivpd"

        irInstrFAddps = "faddpds"
        irInstrFSubps = "fsubpds"
        irInstrFMulps = "fmulpds"
        irInstrFDivps = "fdivpds"

        irInstrFMaddsd = "fmaddsd"
        irInstrFMsubsd = "fmsubsd"
        irInstrFNmaddsd = "fnmaddsd"
        irInstrFNmsubsd = "fnmsubsd"

        irInstrFMaddss = "fmaddss"
        irInstrFMsubss = "fmsubss"
        irInstrFNmaddss = "fnmaddss"
        irInstrFNmsubss = "fnmsubss"

        irInstrFMaddpd = "fmaddpd"
        irInstrFMsubpd = "fmsubpd"
        irInstrFNmaddpd = "fnmaddpd"
        irInstrFNmsubpd = "fnmsubpd"

        irInstrFMaddps = "fmaddps"
        irInstrFMsubps = "fmsubps"
        irInstrFNmaddps = "fnmaddps"
        irInstrFNmsubps = "fnmsubps"

        irInstrCmpEqualFsd = "cmpfeq"
        irInstrCmpGreaterFsd = "cmpfgt"
        irInstrCmpLessFsd = "cmpflt"
        irInstrCmpUnorderedSd = "cmpfunord"

        irInstrCallInterpreterPpc = "interpretppc"
        irInstrCallInterpreterDsp = "interpretdsp"

const
    LoadImmInstrs* = {
        irInstrLoadImmI, irInstrLoadImmB}
    CtxLoadInstrs* = {
        irInstrLoadReg, irInstrLoadCrBit, irInstrLoadXer, irInstrLoadSpr, irInstrLoadFpr, irInstrLoadFprPair}
    CtxStoreInstrs* = {
        irInstrStoreReg, irInstrStoreCrBit, irInstrStoreXer, irInstrStoreSpr, irInstrStoreFpr, irInstrStoreFprPair}
    UnOpInstrs* = {
        irInstrIdentity,

        irInstrBitNot, irInstrCondNot,

        irInstrClz,

        irInstrExtSb,
        irInstrExtSh,

        irInstrLoadU8, irInstrLoadU16, irInstrLoadS16, irInstrLoad32,
        irInstrLoadFss, irInstrLoadFsd,

        irInstrSyscallPpc,

        irInstrFSwizzleD00,
        irInstrFSwizzleD11,

        irInstrFSwizzleS00,
        irInstrFSwizzleS11,

        irInstrCvtsd2ss, irInstrCvtss2sd,
        irInstrCvtpd2ps, irInstrCvtps2pd,

        irInstrCvtsd2intTrunc,
        irInstrCvtss2intTrunc,

        irInstrFRessd, irInstrFRsqrtsd,
        irInstrFRespd, irInstrFRsqrtpd,
        irInstrFResss, irInstrFRsqrtss,
        irInstrFResps, irInstrFRsqrtps,

        irInstrFNegsd, irInstrFAbssd,
        irInstrFNegpd, irInstrFAbspd,

        irInstrFNegss, irInstrFAbsss,
        irInstrFNegps, irInstrFAbsps}
    BiOpInstrs* = {
        irInstrIAdd, irInstrISub,

        irInstrMul, irInstrMulhS, irInstrMulhU,

        irInstrDivS, irInstrDivU,

        irInstrBitAnd, irInstrBitOr, irInstrBitXor,

        irInstrShl, irInstrShrLogic, irInstrShrArith, irInstrRol,

        irInstrOverflowAdd, irInstrOverflowSub,
        irInstrCarryAdd, irInstrCarrySub,

        irInstrCmpEqualI,
        irInstrCmpGreaterUI, irInstrCmpLessUI,
        irInstrCmpGreaterSI, irInstrCmpLessSI,

        irInstrCondAnd, irInstrCondOr, irInstrCondXor,

        irInstrStore8, irInstrStore16, irInstrStore32,
        irInstrStoreFss, irInstrStoreFsd,

        irInstrLoadFsq, irInstrLoadFpq,

        irInstrFMergeD00, irInstrFMergeD01, irInstrFMergeD10, irInstrFMergeD11,
        irInstrFMergeS00, irInstrFMergeS01, irInstrFMergeS10, irInstrFMergeS11,

        irInstrFAddsd, irInstrFSubsd, irInstrFMulsd, irInstrFDivsd,
        irInstrFAddpd, irInstrFSubpd, irInstrFMulpd, irInstrFDivpd,
        irInstrFAddss, irInstrFSubss, irInstrFMulss, irInstrFDivss,
        irInstrFAddps, irInstrFSubps, irInstrFMulps, irInstrFDivps,

        irInstrCmpEqualFsd, irInstrCmpGreaterFsd, irInstrCmpLessFsd, irInstrCmpUnorderedsd}
    TriOpInstrs* = {
        irInstrCsel,
        irInstrIAddExtended, irInstrISubExtended,
        irInstrOverflowAddExtended, irInstrOverflowSubExtended,
        irInstrCarryAddExtended, irInstrCarrySubExtended,

        irInstrStoreFsq, irInstrStoreFpq,

        irInstrBranchPpc, irInstrBranchDsp,

        irInstrFMaddsd, irInstrFMsubsd, irInstrFNmaddsd, irInstrFNmsubsd,
        irInstrFMaddpd, irInstrFMsubpd, irInstrFNmaddpd, irInstrFNmsubpd,

        irInstrFMaddss, irInstrFMsubss, irInstrFNmaddss, irInstrFNmsubss,
        irInstrFMaddps, irInstrFMsubps, irInstrFNmaddps, irInstrFNmsubps}

    ResultlessOps* = {
        irInstrStoreReg,
        irInstrStoreCrBit, irInstrStoreXer, irInstrStoreSpr,
        irInstrStoreFpr, irInstrStoreFprPair,
        irInstrStore8, irInstrStore16, irInstrStore32,
        irInstrStoreFss, irInstrStoreFsd, irInstrStoreFsq, irInstrStoreFpq,

        irInstrBranchPpc,
        irInstrSyscallPpc,

        irInstrBranchDsp,
        
        irInstrCallInterpreterPpc,
        irInstrCallInterpreterDsp}

    SideEffectOps* = {
        irInstrStoreReg, irInstrStoreCrBit, irInstrStoreXer,
        irInstrStoreSpr, irInstrStoreFpr, irInstrStoreFprPair,

        irInstrLoadU8, irInstrLoadU16, irInstrLoadS16, irInstrLoad32,
        irInstrLoadFss, irInstrLoadFsd, irInstrLoadFsq, irInstrLoadFpq,

        irInstrStore8, irInstrStore16, irInstrStore32,
        irInstrStoreFss, irInstrStoreFsd, irInstrStoreFsq, irInstrStoreFpq,

        irInstrBranchPpc, irInstrSyscallPpc,
        irInstrBranchDsp,
        irInstrCallInterpreterPpc, irInstrCallInterpreterDsp}
    StrictSideEffectOps* = SideEffectOps + {
        irInstrLoadReg, irInstrLoadCrBit, irInstrLoadXer, irInstrLoadSpr,
        irInstrLoadFpr, irInstrLoadFprPair}

    FpScalarOps* = {
        irInstrStoreFsd, irInstrStoreFss,
        irInstrFSwizzleD00, irInstrFMergeD00,
        irInstrFSwizzleS00, irInstrFMergeS00,
        irInstrFRessd, irInstrFRsqrtsd,
        irInstrFResss, irInstrFRsqrtss,
        irInstrCvtsd2ss, irInstrCvtss2sd,
        irInstrCvtsd2intTrunc, irInstrCvtss2intTrunc,
        irInstrFNegsd, irInstrFAbssd,
        irInstrFNegss, irInstrFAbsss,
        irInstrFAddsd, irInstrFSubsd, irInstrFMulsd, irInstrFDivsd,
        irInstrFAddss, irInstrFSubss, irInstrFMulss, irInstrFDivss,
        irInstrFMaddsd, irInstrFMsubsd, irInstrFNmaddsd, irInstrFNmsubsd,
        irInstrFMaddss, irInstrFMsubss, irInstrFNmaddss, irInstrFNmsubss,
        irInstrCmpEqualFsd, irInstrCmpGreaterFsd, irInstrCmpLessFsd, irInstrCmpUnorderedSd}
    FpPairOps* = {
        irInstrFSwizzleD11, irInstrFMergeD11,
        irInstrCvtpd2ps, irInstrCvtps2pd,
        irInstrFRespd, irInstrFRsqrtpd,
        irInstrFNegpd, irInstrFAbspd,
        irInstrFAddpd, irInstrFSubpd, irInstrFMulpd, irInstrFDivpd,
        irInstrFMaddpd, irInstrFMsubpd, irInstrFNmaddpd, irInstrFNmsubpd}

type
    IrInstrRef* = distinct int32

    IrInstr* = object
        case kind*: IrInstrKind
        of irInstrLoadImmI:
            immValI*: uint32
        of irInstrLoadImmB:
            immValB*: bool
        of irInstrCallInterpreterPpc, irInstrCallInterpreterDsp:
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

const
    InvalidIrInstrRef* = IrInstrRef(-1)

proc numSources*(instr: IrInstr): int =
    case instr.kind
    of LoadImmInstrs, CtxLoadInstrs, irInstrCallInterpreterPpc, irInstrCallInterpreterDsp:
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
    result &= $int(r)
proc hash*(iref: IrInstrRef): Hash =
    hash(int(iref))

func `==`*(a, b: IrInstr): bool =
    if a.kind == b.kind:
        case a.kind
        of irInstrLoadImmI:
            a.immValI == b.immValI
        of irInstrLoadImmB:
            a.immValB == b.immValB
        of irInstrCallInterpreterPpc, irInstrCallInterpreterDsp:
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
    of irInstrLoadImmI:
        result = result !& hash(instr.immValI)
    of irInstrLoadImmB:
        result = result !& hash(instr.immValB)
    of irInstrCallInterpreterPpc, irInstrCallInterpreterDsp:
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

proc getInstr*(blk: IrBasicBlock, iref: IrInstrRef): var IrInstr =
    blk.instrPool[int iref]

proc makeImm*(val: uint32): IrInstr {.inline.} =
    IrInstr(kind: irInstrLoadImmI, immValI: val)

proc makeImm*(val: bool): IrInstr {.inline.} =
    IrInstr(kind: irInstrLoadImmB, immValB: val)

proc makeLoadctx*(kind: IrInstrKind, idx: uint32): IrInstr {.inline.} =
    case kind
    of CtxLoadInstrs:
        IrInstr(kind: kind, ctxLoadIdx: idx)
    else:
        raiseAssert(&"invalid context load kind {kind}")

proc makeStorectx*(kind: IrInstrKind, idx: uint32, val: IrInstrRef): IrInstr {.inline.} =
    case kind
    of CtxStoreInstrs:
        IrInstr(kind: kind, ctxStoreIdx: idx, srcRegular: [val, InvalidIrInstrRef, InvalidIrInstrRef])
    else:
        raiseAssert(&"invalid context store kind {kind}")

proc makeUnop*(kind: IrInstrKind, operand: IrInstrRef): IrInstr {.inline.} =
    case kind
    of UnOpInstrs:
        IrInstr(kind: kind, srcRegular: [operand, InvalidIrInstrRef, InvalidIrInstrRef])
    else:
        raiseAssert(&"invalid unop kind {kind}")

proc makeBiop*(kind: IrInstrKind, a, b: IrInstrRef): IrInstr {.inline.} =
    case kind
    of BiOpInstrs:
        IrInstr(kind: kind, srcRegular: [a, b, InvalidIrInstrRef])
    else:
        raiseAssert(&"invalid biop kind {kind}")

proc makeTriop*(kind: IrInstrKind, a, b, c: IrInstrRef): IrInstr {.inline.} =
    case kind
    of TriOpInstrs:
        IrInstr(kind: kind, srcRegular: [a, b, c])
    else:
        raiseAssert(&"invalid triop kind {kind}")

proc makeIdentity*(iref: IrInstrRef): IrInstr {.inline.} =
    makeUnop(irInstrIdentity, iref)

proc imm*(blk: IrBasicBlock, val: uint32): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeImm(val)

proc imm*(blk: IrBasicBlock, val: bool): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeImm(val)

proc loadctx*(blk: IrBasicBlock, kind: IrInstrKind, idx: uint32): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeLoadctx(kind, idx)

proc storectx*(blk: IrBasicBlock, kind: IrInstrKind, idx: uint32, val: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeStorectx(kind, idx, val)

proc unop*(blk: IrBasicBlock, kind: IrInstrKind, val: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeUnop(kind, val)

proc biop*(blk: IrBasicBlock, kind: IrInstrKind, a, b: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeBiop(kind, a, b)

proc triop*(blk: IrBasicBlock, kind: IrInstrKind, a, b, c: IrInstrRef): IrInstrRef =
    result = blk.allocInstr()
    blk.getInstr(result) = makeTriop(kind, a, b, c)

proc interpreter*(blk: IrBasicBlock, kind: IrInstrKind, instrcode, pc: uint32, target: pointer): IrInstrRef =
    case kind
    of irInstrCallInterpreterDsp, irInstrCallInterpreterPpc:
        result = blk.allocInstr()
        blk.getInstr(result) = IrInstr(kind: kind, target: target, instr: instrcode, pc: pc)
    else:
        raiseAssert(&"{kind} is not an interpreter instr")

proc interpreter*(blk: IrBasicBlock, instrcode, pc: uint32, target: pointer): IrInstrRef =
    blk.interpreter(irInstrCallInterpreterPpc, instrcode, pc, target)

proc interpretdsp*(blk: IrBasicBlock, instrcode, pc: uint32, target: pointer): IrInstrRef =
    blk.interpreter(irInstrCallInterpreterDsp, instrcode, pc, target)

# block building helpers
proc imm*[T](builder: IrBlockBuilder[T], val: uint32): IrInstrRef =
    result = builder.blk.imm(val)
    add(builder.blk.instrs, result)

proc imm*[T](builder: IrBlockBuilder[T], val: bool): IrInstrRef =
    result = builder.blk.imm(val)
    add(builder.blk.instrs, result)

proc loadctx*[T](builder: IrBlockBuilder[T], kind: IrInstrKind, idx: uint32): IrInstrRef =
    result = builder.blk.loadctx(kind, idx)
    add(builder.blk.instrs, result)

proc storectx*[T](builder: IrBlockBuilder[T], kind: IrInstrKind, idx: uint32, val: IrInstrRef): IrInstrRef =
    result = builder.blk.storectx(kind, idx, val)
    add(builder.blk.instrs, result)

proc unop*[T](builder: IrBlockBuilder[T], kind: IrInstrKind, val: IrInstrRef): IrInstrRef =
    result = builder.blk.unop(kind, val)
    add(builder.blk.instrs, result)

proc biop*[T](builder: IrBlockBuilder[T], kind: IrInstrKind, a, b: IrInstrRef): IrInstrRef =
    result = builder.blk.biop(kind, a, b)
    add(builder.blk.instrs, result)

proc triop*[T](builder: IrBlockBuilder[T], kind: IrInstrKind, a, b, c: IrInstrRef): IrInstrRef =
    result = builder.blk.triop(kind, a, b, c)
    add(builder.blk.instrs, result)

proc interpreter*[T](builder: IrBlockBuilder[T], instrcode, pc: uint32, target: pointer) =
    add(builder.blk.instrs, builder.blk.interpreter(instrcode, pc, target))

proc interpretdsp*[T](builder: IrBlockBuilder[T], instrcode, pc: uint16, target: pointer) =
    add(builder.blk.instrs, builder.blk.interpretdsp(instrcode, pc, target))

proc isImmVal*(blk: IrBasicBlock, iref: IrInstrRef, imm: bool): bool =
    let instr = blk.getInstr(iref)
    instr.kind == irInstrLoadImmB and instr.immValB == imm

proc isImmVal*(blk: IrBasicBlock, iref: IrInstrRef, imm: uint32): bool =
    let instr = blk.getInstr(iref)
    instr.kind == irInstrLoadImmI and instr.immValI == imm

proc isImmValI*(blk: IrBasicBlock, iref: IrInstrRef): Option[uint32] =
    let instr = blk.getInstr(iref)
    if instr.kind == irInstrLoadImmI:
        some(instr.immValI)
    else:
        none(uint32)

proc isImmValB*(blk: IrBasicBlock, iref: IrInstrRef): Option[bool] =
    let instr = blk.getInstr(iref)
    if instr.kind == irInstrLoadImmB:
        some(instr.immValB)
    else:
        none(bool)

proc isEitherImmI*(blk: IrBasicBlock, a, b: IrInstrRef): Option[(IrInstrRef, uint32)] =
    if (let instr = blk.getInstr(a); instr.kind == irInstrLoadImmI):
        some((b, instr.immValI))
    elif (let instr = blk.getInstr(b); instr.kind == irInstrLoadImmI):
        some((a, instr.immValI))
    else:
        none((IrInstrRef, uint32))

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
        of irInstrLoadImmI:
            result &= &"{instr.immValI:08X}"
        of irInstrLoadImmB:
            result &= $instr.immValB
        of CtxLoadInstrs:
            result &= &"{instr.ctxLoadIdx}"
        of CtxStoreInstrs:
            result &= &"{instr.ctxStoreIdx}, ${int(instr.source(0))}"
        of irInstrCallInterpreterDsp, irInstrCallInterpreterPpc:
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
