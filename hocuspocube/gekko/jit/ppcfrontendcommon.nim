import
    ../ppcstate,
    ir

type
    PpcIrRegState* = object
        pc*: uint32
        branch*: bool

    BlockEntryFunc* = proc(ppcstate: ptr PpcState): int32 {.cdecl.}

proc loadreg*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadReg, a)

proc storereg*(builder: var IrBlockBuilder[PpcIrRegState], d: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreReg, d, val)

proc loadregs*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadReg, a), builder.loadctx(irInstrLoadReg, b))

proc loadregcarry*(builder: var IrBlockBuilder[PpcIrRegState], a: uint32): (IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadReg, a),
        builder.loadctx(irInstrLoadXer, irXerNumCa.uint32))

proc loadregscarry*(builder: var IrBlockBuilder[PpcIrRegState], a, b: uint32): (IrInstrRef, IrInstrRef, IrInstrRef) =
    (builder.loadctx(irInstrLoadReg, a),
        builder.loadctx(irInstrLoadReg, b),
        builder.loadctx(irInstrLoadXer, irXerNumCa.uint32))
