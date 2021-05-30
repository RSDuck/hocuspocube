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
    builder.loadctx(irInstrLoadFprPair, a)

proc storefregLowOnly*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFprPair, d, builder.biop(irInstrFMergeD, builder.loadfreg(d), val))

proc storefregReplicate*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreFprPair, d, builder.unop(irInstrFSwizzleD00, val))

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
    builder.unop(irInstrCvtss2sd, builder.unop(irInstrCvtsd2ss, val))

proc postSingleOpPair*(builder: var IrBlockBuilder[PpcIrRegState], val: IrInstrRef): IrInstrRef =
    builder.unop(irInstrCvtps2pd, builder.unop(irInstrCvtpd2ps, val))
