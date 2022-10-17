import
    tables, options,
    ../ppcstate,
    ../../util/jit/ir


type
    PpcIrRegState* = object
        pc*, instr*: uint32
        floatInstr*: bool

using builder: var IrBlockBuilder[PpcIrRegState]

proc branchUncond*(builder; target: IrInstrRef) =
    discard builder.unop(dispatchExternalPpc, target)

proc branchCond*(builder; cond, taken, notTaken: IrInstrRef) =
    discard builder.unop(dispatchExternalPpc, builder.triop(csel, taken, notTaken, cond))

proc loadreg*(builder; a: uint32): IrInstrRef =
    builder.loadctx(ctxLoadU32, uint32(offsetof(PpcState, r)) + a*4)

proc storereg*(builder; d: uint32, val: IrInstrRef) =
    builder.storectx(ctxStore32, uint32(offsetof(PpcState, r)) + d*4, val)

proc loadfreg*(builder; a: uint32): IrInstrRef =
    builder.loadctx(ctxLoadFprPair, uint32(offsetof(PpcState, fr)) + a*16)

proc storefregp*(builder; d: uint32, val: IrInstrRef) =
    builder.storectx(ctxStoreFprPair, uint32(offsetof(PpcState, fr)) + d*16, val)

proc storefregLowOnly*(builder; d: uint32, val: IrInstrRef) =
    builder.storefregp(d, builder.biop(fMergeD01, val, builder.loadfreg(d)))

proc storefregReplicate*(builder; d: uint32, val: IrInstrRef) =
    builder.storefregp(d, builder.unop(fSwizzleD00, val))

proc loadregs*(builder; a, b: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadreg(a), builder.loadreg(b))

proc loadLr*(builder): IrInstrRef =
    builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, lr))

proc loadCtr*(builder): IrInstrRef =
    builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, ctr))

proc loadCr*(builder): IrInstrRef =
    builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, cr))

proc loadXer*(builder): IrInstrRef =
    builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, xer))

proc storeLr*(builder; val: IrInstrRef) =
    builder.storectx(ctxStore32, uint32 offsetof(PpcState, lr), val)

proc storeCtr*(builder; val: IrInstrRef) =
    builder.storectx(ctxStore32, uint32 offsetof(PpcState, ctr), val)

proc storeCr*(builder; val: IrInstrRef) =
    builder.storectx(ctxStore32, uint32 offsetof(PpcState, cr), val)

proc storeXer*(builder; val: IrInstrRef) =
    builder.storectx(ctxStore32, uint32 offsetof(PpcState, xer), val)

proc loadCrBit*(builder; num: uint32): IrInstrRef =
    builder.extractBit(builder.loadCr(), 31-num)

proc storeCrBit*(builder; num: uint32, val: IrInstrRef) =
    builder.storeCr(builder.mergeBit(builder.loadCr(), val, 31-num))

proc storeCrBits*(builder; num: uint32, lt, gt, eq, so: IrInstrRef) =
    var cr = builder.loadCr()
    cr = builder.mergeBit(cr, lt, 31-(num*4+0))
    cr = builder.mergeBit(cr, gt, 31-(num*4+1))
    cr = builder.mergeBit(cr, eq, 31-(num*4+2))
    cr = builder.mergeBit(cr, so, 31-(num*4+3))
    builder.storeCr(cr)

proc loadSo*(builder): IrInstrRef =
    builder.extractBit(builder.loadXer(), 31)

proc loadOv*(builder): IrInstrRef =
    builder.extractBit(builder.loadXer(), 30)

proc loadCa*(builder): IrInstrRef =
    builder.extractBit(builder.loadXer(), 29)

proc storeSo*(builder; val: IrInstrRef) =
    builder.storeXer(builder.mergeBit(builder.loadXer(), val, 31))

proc storeOv*(builder; val: IrInstrRef) =
    builder.storeXer(builder.mergeBit(builder.loadXer(), val, 30))

proc storeCa*(builder; val: IrInstrRef) =
    builder.storeXer(builder.mergeBit(builder.loadXer(), val, 29))

proc loadregcarry*(builder; a: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadreg(a), builder.loadCa())

proc loadregscarry*(builder; a, b: uint32): (IrInstrRef, IrInstrRef, IrInstrRef) =
    (builder.loadreg(a), builder.loadreg(b), builder.loadCa())

proc postSingleOp*(builder; val: IrInstrRef): IrInstrRef =
    builder.unop(cvtss2sd, builder.unop(cvtsd2ss, val))

proc postSingleOpPair*(builder; val: IrInstrRef): IrInstrRef =
    builder.unop(cvtps2pd, builder.unop(cvtpd2ps, val))
