import
    strformat,
    ../../util/jit/ir,
    ".."/[dsp, dspstate],
    ../../cycletiming

type
    DspIrState* = object
        pc*, instr*: uint16
        cycles*: int32
        branch*: bool

using builder: var IrBlockBuilder[DspIrState]

proc fetchFollowingImm*(builder): uint16 =
    builder.regs.pc += 1
    builder.regs.cycles += int32 gekkoCyclesPerDspCycle
    instrRead(builder.regs.pc)

proc readAccum*(builder; num: uint32): IrInstrRef =
    builder.loadctx(loadAccum, dspAccumA.uint32 + num.uint32)

proc readAuxAccum*(builder; num: uint32): IrInstrRef =
    builder.loadctx(loadAccum, dspAccumX.uint32 + num.uint32)

proc readProd*(builder): IrInstrRef =
    builder.loadctx(loadAccum, dspAccumProd.uint32)

proc writeAccum*(builder; num: uint32, val: IrInstrRef) =
    discard builder.storectx(storeAccum, dspAccumA.uint32 + num.uint32, val)

proc writeAuxAccum*(builder; num: uint32, val: IrInstrRef) =
    discard builder.storectx(storeAccum, dspAccumX.uint32 + num.uint32, val)

proc writeProd*(builder; val: IrInstrRef) =
    discard builder.storectx(storeAccum, dspAccumProd.uint32, val)

proc readStatus*(builder; bit: DspStatusBit): IrInstrRef =
    builder.loadctx(loadStatusBit, bit.uint32)

proc writeStatus*(builder; bit: DspStatusBit, val: IrInstrRef) =
    discard builder.storectx(storeStatusBit, bit.uint32, val)

proc readReg*(builder; reg: DspReg): IrInstrRef =
    case reg
    of r0..lcs, dpp, psr, pc1:
        builder.loadctx(loadDspReg, uint32(reg))
    of a0..b0:
        builder.unop(extractLo, builder.loadctx(loadAccum, dspAccumA.uint32 + reg.uint32 - a0.uint32))
    of a1..b1:
        builder.unop(extractMid, builder.loadctx(loadAccum, dspAccumA.uint32 + reg.uint32 - a1.uint32))
    of a2..b2:
        builder.unop(extractHi, builder.loadctx(loadAccum, dspAccumA.uint32 + reg.uint32 - a2.uint32))
    of x0..y0:
        builder.unop(extractLo, builder.loadctx(loadAccum, dspAccumX.uint32 + reg.uint32 - x0.uint32))
    of x1..y1:
        builder.unop(extractMid, builder.loadctx(loadAccum, dspAccumX.uint32 + reg.uint32 - x1.uint32))
    of ps0:
        builder.unop(extractLo, builder.loadctx(loadAccum, dspAccumProd.uint32))
    of ps1:
        builder.unop(extractMid, builder.loadctx(loadAccum, dspAccumProd.uint32))
    of ps2:
        builder.unop(extractHi, builder.loadctx(loadAccum, dspAccumProd.uint32))

proc writeReg*(builder; reg: DspReg, val: IrInstrRef) =
    case reg
    of r0..lcs, dpp, psr, pc1:
        discard builder.storectx(storeDspReg, uint32(reg), val)
    of a0..b0:
        let accum = dspAccumA.uint32 + reg.uint32 - a0.uint32
        discard builder.storectx(storeAccum, accum,
            builder.biop(mergeLo, builder.loadctx(loadAccum, accum), val))
    of a1..b1:
        let accum = dspAccumA.uint32 + reg.uint32 - a1.uint32
        discard builder.storectx(storeAccum, accum,
            builder.biop(mergeMid, builder.loadctx(loadAccum, accum), val))
    of a2..b2:
        let accum = dspAccumA.uint32 + reg.uint32 - a2.uint32
        discard builder.storectx(storeAccum, accum,
            builder.biop(mergeHi, builder.loadctx(loadAccum, accum),
            builder.unop(extsb, val)))
    of x0..y0:
        let accum = dspAccumX.uint32 + reg.uint32 - x0.uint32
        discard builder.storectx(storeAccum, accum,
            builder.biop(mergeLo, builder.loadctx(loadAccum, accum), val))
    of x1..y1:
        let accum = dspAccumX.uint32 + reg.uint32 - x1.uint32
        discard builder.storectx(storeAccum, accum,
            builder.biop(mergeMid, builder.loadctx(loadAccum, accum), val))
    of ps0:
        discard builder.storectx(storeAccum, dspAccumProd.uint32,
            builder.biop(mergeLo, builder.loadctx(loadAccum, dspAccumProd.uint32), val))
    of ps1:
        discard builder.storectx(storeAccum, dspAccumProd.uint32,
            builder.biop(mergeMid, builder.loadctx(loadAccum, dspAccumProd.uint32), val))
    of ps2:
        discard builder.storectx(storeAccum, dspAccumProd.uint32,
            builder.biop(mergeHi, builder.loadctx(loadAccum, dspAccumProd.uint32),
            builder.unop(extsb, val)))

proc incAdr*(builder; adr: IrInstrRef, n: uint32): IrInstrRef =
    let
        wrap = builder.readReg(l0.succ int(n))

        nextAdr = builder.biop(iAdd, adr, builder.imm(1))
        altAdr = builder.biop(iSub, adr, wrap)

        leftHand = builder.biop(bitXor, adr, nextAdr)
        rightHand = builder.biop(lsl, builder.biop(bitOr, wrap, builder.imm(1)), builder.imm(1))

    builder.biop(bitAnd,
        builder.triop(csel,
            altAdr, nextAdr, builder.biop(iCmpGreaterU, leftHand, rightHand)),
        builder.imm(0xFFFF))

proc decAdr*(builder; adr: IrInstrRef, n: uint32): IrInstrRef =
    let
        wrap = builder.readReg(l0.succ int(n))

        nextAdr = builder.biop(iAdd, adr, wrap)
        altAdr = builder.biop(iSub, adr, builder.imm(1))

        leftHand = builder.biop(bitXor, adr, nextAdr)
        rightHand = builder.biop(lsl, builder.biop(bitOr, wrap, builder.imm(1)), builder.imm(1))

    builder.biop(bitAnd,
        builder.triop(csel,
            altAdr, nextAdr, builder.biop(iCmpGreaterU, builder.biop(bitOr, leftHand, rightHand), wrap)),
        builder.imm(0xFFFF))

proc loadAccum*(builder; num: uint32, val: IrInstrRef) =
    let
        xl = builder.readStatus(dspStatusBitXl)

        mergeVal = builder.biop(mergeMid, builder.readAccum(num), val)
        signExtendedVal = builder.biop(lslX, builder.unop(extsh, val), builder.imm(16))

    builder.writeAccum(num, builder.triop(cselX, signExtendedVal, mergeVal, xl))

proc writeAccumSignExtend*(builder; num: uint32, val: IrInstrRef) =
    builder.writeAccum(num, builder.biop(asrX, builder.biop(lslX, val, builder.imm(24)), builder.imm(24)))
