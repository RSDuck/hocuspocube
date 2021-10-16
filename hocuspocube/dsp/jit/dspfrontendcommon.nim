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
    builder.loadctx(irInstrLoadAccum, dspAccumA.uint32 + num.uint32)

proc readAuxAccum*(builder; num: uint32): IrInstrRef =
    builder.loadctx(irInstrLoadAccum, dspAccumX.uint32 + num.uint32)

proc readProd*(builder): IrInstrRef =
    builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32)

proc writeAccum*(builder; num: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreAccum, dspAccumA.uint32 + num.uint32, val)

proc writeAuxAccum*(builder; num: uint32, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreAccum, dspAccumX.uint32 + num.uint32, val)

proc writeProd*(builder; val: IrInstrRef) =
    discard builder.storectx(irInstrStoreAccum, dspAccumProd.uint32, val)

proc readStatus*(builder; bit: DspStatusBit): IrInstrRef =
    builder.loadctx(irInstrLoadStatusBit, bit.uint32)

proc writeStatus*(builder; bit: DspStatusBit, val: IrInstrRef) =
    discard builder.storectx(irInstrStoreStatusBit, bit.uint32, val)

proc readReg*(builder; reg: DspReg): IrInstrRef =
    case reg
    of dspRegAdr0..dspRegLoopCountStack, dspRegDpp, dspRegStatus, dspRegPc1:
        builder.loadctx(irInstrLoadDspReg, uint32(reg))
    of dspRegA0..dspRegB0:
        builder.unop(irInstrExtractLo, builder.loadctx(irInstrLoadAccum, dspAccumA.uint32 + reg.uint32 - dspRegA0.uint32))
    of dspRegA1..dspRegB1:
        builder.unop(irInstrExtractMid, builder.loadctx(irInstrLoadAccum, dspAccumA.uint32 + reg.uint32 - dspRegA1.uint32))
    of dspRegA2..dspRegB2:
        builder.unop(irInstrExtractHi, builder.loadctx(irInstrLoadAccum, dspAccumA.uint32 + reg.uint32 - dspRegA2.uint32))
    of dspRegX0..dspRegY0:
        builder.unop(irInstrExtractLo, builder.loadctx(irInstrLoadAccum, dspAccumX.uint32 + reg.uint32 - dspRegX0.uint32))
    of dspRegX1..dspRegY1:
        builder.unop(irInstrExtractMid, builder.loadctx(irInstrLoadAccum, dspAccumX.uint32 + reg.uint32 - dspRegX1.uint32))
    of dspRegPs0:
        builder.unop(irInstrExtractLo, builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32))
    of dspRegPs1:
        builder.unop(irInstrExtractMid, builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32))
    of dspRegPs2:
        builder.unop(irInstrExtractHi, builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32))

proc writeReg*(builder; reg: DspReg, val: IrInstrRef) =
    case reg
    of dspRegAdr0..dspRegLoopCountStack, dspRegDpp, dspRegStatus, dspRegPc1:
        discard builder.storectx(irInstrStoreDspReg, uint32(reg), val)
    of dspRegA0..dspRegB0:
        let accum = dspAccumA.uint32 + reg.uint32 - dspRegA0.uint32
        discard builder.storectx(irInstrStoreAccum, accum,
            builder.biop(irInstrMergeLo, builder.loadctx(irInstrLoadAccum, accum), val))
    of dspRegA1..dspRegB1:
        let accum = dspAccumA.uint32 + reg.uint32 - dspRegA1.uint32
        discard builder.storectx(irInstrStoreAccum, accum,
            builder.biop(irInstrMergeMid, builder.loadctx(irInstrLoadAccum, accum), val))
    of dspRegA2..dspRegB2:
        let accum = dspAccumA.uint32 + reg.uint32 - dspRegA2.uint32
        discard builder.storectx(irInstrStoreAccum, accum,
            builder.biop(irInstrMergeHi, builder.loadctx(irInstrLoadAccum, accum),
            builder.unop(irInstrExtsb, val)))
    of dspRegX0..dspRegY0:
        let accum = dspAccumX.uint32 + reg.uint32 - dspRegX0.uint32
        discard builder.storectx(irInstrStoreAccum, accum,
            builder.biop(irInstrMergeLo, builder.loadctx(irInstrLoadAccum, accum), val))
    of dspRegX1..dspRegY1:
        let accum = dspAccumX.uint32 + reg.uint32 - dspRegX1.uint32
        discard builder.storectx(irInstrStoreAccum, accum,
            builder.biop(irInstrMergeMid, builder.loadctx(irInstrLoadAccum, accum), val))
    of dspRegPs0:
        discard builder.storectx(irInstrStoreAccum, dspAccumProd.uint32,
            builder.biop(irInstrMergeLo, builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32), val))
    of dspRegPs1:
        discard builder.storectx(irInstrStoreAccum, dspAccumProd.uint32,
            builder.biop(irInstrMergeMid, builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32), val))
    of dspRegPs2:
        discard builder.storectx(irInstrStoreAccum, dspAccumProd.uint32,
            builder.biop(irInstrMergeHi, builder.loadctx(irInstrLoadAccum, dspAccumProd.uint32),
            builder.unop(irInstrExtsb, val)))

proc incAdr*(builder; adr: IrInstrRef, n: uint32): IrInstrRef =
    let
        wrap = builder.readReg(dspRegWrap0.succ int(n))

        nextAdr = builder.biop(irInstrIAdd, adr, builder.imm(1))
        altAdr = builder.biop(irInstrISub, adr, wrap)

        leftHand = builder.biop(irInstrBitXor, adr, nextAdr)
        rightHand = builder.biop(irInstrShl, builder.biop(irInstrBitOr, wrap, builder.imm(1)), builder.imm(1))

    builder.biop(irInstrBitAnd,
        builder.triop(irInstrCsel,
            altAdr, nextAdr, builder.biop(irInstrCmpGreaterUI, leftHand, rightHand)),
        builder.imm(0xFFFF))

proc decAdr*(builder; adr: IrInstrRef, n: uint32): IrInstrRef =
    let
        wrap = builder.readReg(dspRegWrap0.succ int(n))

        nextAdr = builder.biop(irInstrIAdd, adr, wrap)
        altAdr = builder.biop(irInstrISub, adr, builder.imm(1))

        leftHand = builder.biop(irInstrBitXor, adr, nextAdr)
        rightHand = builder.biop(irInstrShl, builder.biop(irInstrBitOr, wrap, builder.imm(1)), builder.imm(1))

    builder.biop(irInstrBitAnd,
        builder.triop(irInstrCsel,
            altAdr, nextAdr, builder.biop(irInstrCmpGreaterUI, builder.biop(irInstrBitOr, leftHand, rightHand), wrap)),
        builder.imm(0xFFFF))

proc loadAccum*(builder; num: uint32, val: IrInstrRef) =
    let
        xl = builder.readStatus(dspStatusBitXl)

        mergeVal = builder.biop(irInstrMergeMid, builder.readAccum(num), val)
        signExtendedVal = builder.biop(irInstrShlX, builder.unop(irInstrExtsh, val), builder.imm(16))

    builder.writeAccum(num, builder.triop(irInstrCselX, signExtendedVal, mergeVal, xl))

proc writeAccumSignExtend*(builder; num: uint32, val: IrInstrRef) =
    builder.writeAccum(num, builder.biop(irInstrShrArithX, builder.biop(irInstrShlX, val, builder.imm(24)), builder.imm(24)))
