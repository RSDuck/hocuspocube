import
    ../ppcstate,
    ../../util/jit/ir

type
    PpcIrRegState* = object
        pc*, instr*: uint32
        branch*, floatInstr*: bool

proc loadreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadPpcReg, a)

proc storereg*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStorePpcReg, d, val)

proc loadfreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadFprPair, a)

proc storefregLowOnly*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFprPair, d, builder.biop(irInstrFMergeD01, val, builder.loadfreg(d)))

proc storefregReplicate*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFprPair, d, builder.unop(irInstrFSwizzleD00, val))

proc storefregp*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFprPair, d, val)

proc loadregs*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadPpcReg, a), builder.loadctx(irInstrLoadPpcReg, b))

proc loadregcarry*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadPpcReg, a),
        builder.loadctx(irInstrLoadXer, irXerNumCa.uint32))

proc loadregscarry*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadPpcReg, a),
        builder.loadctx(irInstrLoadPpcReg, b),
        builder.loadctx(irInstrLoadXer, irXerNumCa.uint32))

proc postSingleOp*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(irInstrCvtss2sd, builder.unop(irInstrCvtsd2ss, val))

proc postSingleOpPair*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(irInstrCvtps2pd, builder.unop(irInstrCvtpd2ps, val))
