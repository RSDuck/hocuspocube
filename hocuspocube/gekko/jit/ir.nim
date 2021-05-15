import
    strutils, strformat, options

type
    IrInstrKind* = enum
        irInstrLoadImmI = "loadi"
        irInstrLoadImmB = "loadb"

        irInstrLoadReg = "ldrreg"
        irInstrStoreReg = "strreg"
        irInstrLoadCrBit = "ldrcr"
        irInstrStoreCrBit = "strcr"
        irInstrLoadXer = "ldrxer"
        irInstrStoreXer = "strxer"
        irInstrLoadSpr = "ldrspr"
        irInstrStoreSpr = "strspr"

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

        irInstrStore8 = "strb"
        irInstrStore16 = "strh"
        irInstrStore32 = "str"

        irInstrBranch = "b"

        irInstrSyscall = "sc"

const
    LoadImmInstrs* = {
        irInstrLoadImmI, irInstrLoadImmB}
    CtxLoadInstrs* = {
        irInstrLoadReg, irInstrLoadCrBit, irInstrLoadXer, irInstrLoadSpr}
    CtxStoreInstrs* = {
        irInstrStoreReg, irInstrStoreCrBit, irInstrStoreXer, irInstrStoreSpr}
    UnOpInstrs* = {
        irInstrBitNot, irInstrCondNot,

        irInstrClz,

        irInstrExtSb,
        irInstrExtSh,

        irInstrLoadU8, irInstrLoadU16, irInstrLoadS16, irInstrLoad32,

        irInstrSyscall}
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

        irInstrStore8, irInstrStore16, irInstrStore32}
    TriOpInstrs* = {
        irInstrCsel,
        irInstrIAddExtended, irInstrISubExtended,
        irInstrOverflowAddExtended, irInstrOverflowSubExtended,
        irInstrCarryAddExtended, irInstrCarrySubExtended,

        irInstrBranch}

    ResultlessOps* = {
        irInstrStoreReg, irInstrStoreCrBit, irInstrStoreXer, irInstrStoreSpr,
        irInstrStore8, irInstrStore16, irInstrStore32,
        
        irInstrBranch,
        irInstrSyscall}

type
    IrInstrRef* = distinct int32

    IrInstr* = object
        case kind*: IrInstrKind
        of irInstrLoadImmI:
            immValI*: uint32
        of irInstrLoadImmB:
            immValB*: bool
        of CtxLoadInstrs:
            ctxLoadIdx*: uint32
        of CtxStoreInstrs:
            ctxStoreIdx*: uint32
            ctxStoreSrc*: IrInstrRef
        else:
            srcRegular: array[3, IrInstrRef]

        lastRead*: int32

    IrBasicBlock* = ref object
        instrs*: seq[IrInstrRef]

        instrPool: seq[IrInstr]
        freeInstrs: seq[IrInstrRef]

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
    of LoadImmInstrs:
        0
    of CtxLoadInstrs:
        0
    of CtxStoreInstrs:
        1
    of UnopInstrs:
        1
    of BiOpInstrs:
        2
    of TriOpInstrs:
        3

proc source*(instr: var IrInstr, i: int): var IrInstrRef =
    assert i < instr.numSources
    if instr.kind in CtxStoreInstrs:
        return instr.ctxStoreSrc
    else:
        return instr.srcRegular[i]

proc source*(instr: IrInstr, i: int): IrInstrRef =
    assert i < instr.numSources
    if instr.kind in CtxStoreInstrs:
        return instr.ctxStoreSrc
    else:
        return instr.srcRegular[i]

iterator sources*(instr: IrInstr): IrInstrRef =
    case instr.kind
    of CtxLoadInstrs, LoadImmInstrs:
        discard
    of CtxStoreInstrs:
        yield instr.ctxStoreSrc
    else:
        var i = 0
        while i < instr.numSources:
            yield instr.source(i)
            i += 1

proc `==`*(a, b: IrInstrRef): bool {.borrow.}
proc `$`*(r: IrInstrRef): string =
    result = "$"
    result &= $int(r)

proc allocInstr(blk: IrBasicBlock): IrInstrRef =
    if blk.freeInstrs.len > 0:
        blk.freeInstrs.pop()
    else:
        let instr = IrInstrRef blk.instrs.len
        blk.instrPool.setLen(blk.instrPool.len + 1)
        instr

proc getInstr*(blk: IrBasicBlock, iref: IrInstrRef): var IrInstr =
    blk.instrPool[int iref]

proc imm*[T](builder: var IrBlockBuilder[T], val: uint32): IrInstrRef =
    result = builder.blk.allocInstr()
    builder.blk.instrs.add result
    builder.blk.getInstr(result) = IrInstr(kind: irInstrLoadImmI, immValI: val)

proc imm*[T](builder: var IrBlockBuilder[T], val: bool): IrInstrRef =
    result = builder.blk.allocInstr()
    builder.blk.instrs.add result
    builder.blk.getInstr(result) = IrInstr(kind: irInstrLoadImmB, immValB: val)

proc loadctx*[T](builder: var IrBlockBuilder[T], kind: IrInstrKind, idx: uint32): IrInstrRef =
    case kind
    of CtxLoadInstrs:
        result = builder.blk.allocInstr()
        builder.blk.instrs.add result
        builder.blk.getInstr(result) = IrInstr(kind: kind, ctxLoadIdx: idx)
    else:
        raiseAssert(&"{kind} is not a context load")

proc storectx*[T](builder: var IrBlockBuilder[T], kind: IrInstrKind, idx: uint32, val: IrInstrRef): IrInstrRef =
    case kind
    of CtxStoreInstrs:
        result = builder.blk.allocInstr()
        builder.blk.instrs.add result
        builder.blk.getInstr(result) = IrInstr(kind: kind, ctxStoreIdx: idx, ctxStoreSrc: val)
    else:
        raiseAssert(&"{kind} is not a context store")

proc biop*[T](builder: var IrBlockBuilder[T], kind: IrInstrKind, a, b: IrInstrRef): IrInstrRef =
    case kind
    of BiOpInstrs:
        result = builder.blk.allocInstr()
        builder.blk.instrs.add result
        builder.blk.getInstr(result) = IrInstr(kind: kind, srcRegular: [a, b, InvalidIrInstrRef])
    else:
        raiseAssert(&"{kind} is not a biop")

proc unop*[T](builder: var IrBlockBuilder[T], kind: IrInstrKind, val: IrInstrRef): IrInstrRef =
    case kind
    of UnOpInstrs:
        result = builder.blk.allocInstr()
        builder.blk.instrs.add result
        builder.blk.getInstr(result) = IrInstr(kind: kind, srcRegular: [val, InvalidIrInstrRef, InvalidIrInstrRef])
    else:
        raiseAssert("{kind} is not an unop")

proc triop*[T](builder: var IrBlockBuilder[T], kind: IrInstrKind, a, b, c: IrInstrRef): IrInstrRef =
    case kind
    of TriOpInstrs:
        result = builder.blk.allocInstr()
        builder.blk.instrs.add result
        builder.blk.getInstr(result) = IrInstr(kind: kind, srcRegular: [a, b, c])
    else:
        raiseAssert("{kind} is not an unop")

proc eliminateMemAccess*(blk: IrBasicBlock) =
    discard

proc calcLiveIntervals*(blk: IrBasicBlock) =
    for i in 0..<blk.instrs.len:
        let instr = blk.getInstr(blk.instrs[i])
        for src in instr.sources:
            blk.getInstr(src).lastRead = int32 i

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
            result &= &"{instr.ctxStoreIdx}, ${int(instr.ctxStoreSrc)}"
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

proc isImmVal*(blk: IrBasicBlock, iref: IrInstrRef, imm: bool): bool =
    let instr = blk.getInstr(iref)
    instr.kind == irInstrLoadImmB and instr.immValB == imm

proc isImmValI*(blk: IrBasicBlock, iref: IrInstrRef): Option[uint32] =
    let instr = blk.getInstr(iref)
    if instr.kind == irInstrLoadImmI:
        some(instr.immValI)
    else:
        none(uint32)

proc isEitherImmI*(blk: IrBasicBlock, a, b: IrInstrRef): Option[(IrInstrRef, uint32)] =
    if (let instr = blk.getInstr(a); instr.kind == irInstrLoadImmI):
        some((b, instr.immValI))
    elif (let instr = blk.getInstr(b); instr.kind == irInstrLoadImmI):
        some((a, instr.immValI))
    else:
        none((IrInstrRef, uint32))
