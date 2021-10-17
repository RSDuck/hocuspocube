import
    ../ppcstate,
    ../../util/jit/ir

type
    PpcIrRegState* = object
        pc*, instr*: uint32
        branch*, floatInstr*: bool

proc loadreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(loadPpcReg, a)

proc storereg*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(storePpcReg, d, val)

proc loadfreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(loadFprPair, a)

proc storefregLowOnly*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(storeFprPair, d, builder.biop(fMergeD01, val, builder.loadfreg(d)))

proc storefregReplicate*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(storeFprPair, d, builder.unop(fSwizzleD00, val))

proc storefregp*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(storeFprPair, d, val)

proc loadregs*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(loadPpcReg, a), builder.loadctx(loadPpcReg, b))

proc loadregcarry*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(loadPpcReg, a),
        builder.loadctx(loadXer, irXerNumCa.uint32))

proc loadregscarry*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef, IrInstrRef) =
    (builder.loadctx(loadPpcReg, a),
        builder.loadctx(loadPpcReg, b),
        builder.loadctx(loadXer, irXerNumCa.uint32))

proc postSingleOp*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(cvtss2sd, builder.unop(cvtsd2ss, val))

proc postSingleOpPair*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(cvtps2pd, builder.unop(cvtpd2ps, val))
