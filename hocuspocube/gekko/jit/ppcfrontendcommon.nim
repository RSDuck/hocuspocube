import
    ../ppcstate,
    ir

type
    PpcIrRegState* = object
        pc*, instr*: uint32
        branch*, floatInstr*: bool

    BlockEntryFunc* = proc(ppcstate: ptr PpcState): int32 {.cdecl.}

proc loadreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadReg, a)

proc storereg*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreReg, d, val)

proc loadfreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadFpr, a)

proc storefreg*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFpr, d, val)

proc loadfregp*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadFprPair, a)

proc storefregp*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFprPair, d, val)

proc loadregs*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadReg, a), builder.loadctx(irInstrLoadReg, b))

proc loadregcarry*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadReg, a),
        builder.loadctx(irInstrLoadXer, irXerNumCa.uint32))

proc loadregscarry*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadReg, a),
        builder.loadctx(irInstrLoadReg, b),
        builder.loadctx(irInstrLoadXer, irXerNumCa.uint32))

proc postSingleOp*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(irInstrFSwizzleD00,
        builder.unop(irInstrCvtss2sd, builder.unop(irInstrCvtsd2ss, val)))

proc postSingleOpPair*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(irInstrCvtss2sd, builder.unop(irInstrCvtsd2ss, val))
