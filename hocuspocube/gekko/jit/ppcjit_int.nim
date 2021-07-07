import
    bitops,
    ../ppccommon,
    ../../util/aluhelper,
    ../../util/jit/ir, ppcfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[PpcIrRegState]

const
    interpretInt = false
    interpretAdd = false
    interpretSub = false
    interpretMulDiv = false

proc setCr(builder; signed: bool, cond: uint32, a, b: IrInstrRef) =
    let
        (lt, gt) =
            if signed:
                (builder.biop(irInstrCmpLessSI, a, b), builder.biop(irInstrCmpGreaterSI, a, b))
            else:
                (builder.biop(irInstrCmpLessUI, a, b), builder.biop(irInstrCmpGreaterUI, a, b))
        eq = builder.biop(irInstrCmpEqualI, a, b)
    discard builder.storectx(irInstrStoreCrBit, cond*4+0, lt)
    discard builder.storectx(irInstrStoreCrBit, cond*4+1, gt)
    discard builder.storectx(irInstrStoreCrBit, cond*4+2, eq)
    discard builder.storectx(irInstrStoreCrBit, cond*4+3, builder.loadctx(irInstrLoadXer, irXerNumSo.uint32))

proc handleRc(builder; val: IrInstrRef, rc: uint32) =
    if rc != 0:
        builder.setCr(true, 0, val, builder.imm(0))

template updateOv(builder; val: IrInstrRef, oe: uint32) =
    if oe != 0:
        discard builder.storectx(irInstrStoreXer, irXerNumOv.uint32, val)
        discard builder.storectx(irInstrStoreXer, irXerNumSo.uint32,
            builder.biop(irInstrCondOr, builder.loadctx(irInstrLoadXer, irXerNumSo.uint32), val))

proc addx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrIAdd, ra, rb)

        builder.updateOv(builder.biop(irInstrOverflowAdd, ra, rb), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc addcx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addcx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrIAdd, ra, rb)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.biop(irInstrCarryAdd, ra, rb))
        builder.updateOv(builder.biop(irInstrOverflowAdd, rb, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc addex*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addex)
    else:
        let
            (ra, rb, carry) = builder.loadregscarry(a, b)
            val = builder.triop(irInstrIAddExtended, ra, rb, carry)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.triop(irInstrCarryAddExtended, ra, rb, carry))
        builder.updateOv(builder.triop(irInstrOverflowAddExtended, ra, rb, carry), oe)

        builder.storereg(d, val)

proc addi*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addi)
    else:
        let imm = builder.imm(signExtend[uint32](imm, 16))
        if a == 0:
            builder.storereg(d, imm)
        else:
            builder.storereg(d, builder.biop(irInstrIAdd, builder.loadreg(a), imm))

proc addic*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addic)
    else:
        let
            imm = builder.imm(signExtend[uint32](imm, 16))
            ra = builder.loadreg(a)
            val = builder.biop(irInstrIAdd, ra, imm)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.biop(irInstrCarryAdd, ra, imm))

        builder.storereg d, val

proc addicdot*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addicdot)
    else:
        let
            imm = builder.imm(signExtend[uint32](imm, 16))
            ra = builder.loadreg(a)
            val = builder.biop(irInstrIAdd, ra, imm)

        builder.handleRc(val, 1)
        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.biop(irInstrCarryAdd, ra, imm))

        builder.storereg d, val

proc addis*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addis)
    else:
        let imm = builder.imm(imm shl 16)
        if a == 0:
            builder.storereg(d, imm)
        else:
            builder.storereg(d, builder.biop(irInstrIAdd, builder.loadreg(a), imm))

proc addmex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addmex)
    else:
        let
            (ra, carry) = builder.loadregcarry(a)
            minusOne = builder.imm(0xFFFF_FFFF'u32)
            val = builder.triop(irInstrIAddExtended, ra, minusOne, carry)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.triop(irInstrCarryAddExtended, ra, minusOne, carry))
        builder.updateOv(builder.triop(irInstrOverflowAddExtended, ra, minusOne, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc addzex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addzex)
    else:
        let
            (ra, carry) = builder.loadregcarry(a)
            val = builder.biop(irInstrIAdd, ra, carry)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.biop(irInstrCarryAdd, ra, carry))
        builder.updateOv(builder.biop(irInstrOverflowAdd, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc divwx*(builder; d, a, b, oe, rc: uint32) =
    #[builder.updateOv(builder.biop(irInstrCondOr,
            builder.biop(irInstrCondAnd,
                builder.biop(irInstrCmpEqualI, builder.r(a), builder.imm(0x8000_0000'u32)),
                builder.biop(irInstrCmpEqualI, builder.r(b), builder.imm(0xFFFF_FFFF'u32))),
            builder.biop(irInstrCmpEqualI, builder.r(b), builder.imm(0))),
        oe)]#
    assert oe == 0
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.divwx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrDivS, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc divwux*(builder; d, a, b, oe, rc: uint32) =
    assert oe == 0
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.divwux)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrDivU, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc mulhwx*(builder; d, a, b, rc: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mulhwx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrMulhS, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc mulhwux*(builder; d, a, b, rc: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mulhwux)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrMulhU, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc mulli*(builder; d, a, imm: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mulli)
    else:
        builder.storereg(d, builder.biop(irInstrMul, builder.loadreg(a), builder.imm(signExtend[uint32](imm, 16))))

proc mullwx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mullwx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrMul, ra, rb)

        builder.updateOv(block:
            let upperResult = builder.biop(irInstrMulhS, ra, rb)
            builder.unop(irInstrCondNot, builder.biop(irInstrCondOr,
                builder.biop(irInstrCmpEqualI, upperResult, builder.imm(0)),
                builder.biop(irInstrCmpEqualI, upperResult, builder.imm(0xFFFFFFFF'u32)))),
            oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc negx*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.negx)
    else:
        let
            ra = builder.loadreg(a)
            zero = builder.imm(0)
            val = builder.biop(irInstrISub, zero, ra)

        builder.updateOv(builder.biop(irInstrOverflowSub, zero, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrISub, rb, ra)

        builder.updateOv(builder.biop(irInstrOverflowSub, rb, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfcx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfcx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(irInstrISub, rb, ra)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.biop(irInstrCarrySub, rb, ra))
        builder.updateOv(builder.biop(irInstrOverflowSub, rb, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfex*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfex)
    else:
        let
            (ra, rb, carry) = builder.loadregscarry(a, b)
            val = builder.triop(irInstrISubExtended, rb, ra, carry)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.triop(irInstrCarrySubExtended, rb, ra, carry))
        builder.updateOv(builder.triop(irInstrOverflowSubExtended, rb, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfic*(builder; d, a, imm: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfic)
    else:
        let
            imm = builder.imm(signExtend[uint32](imm, 16))
            ra = builder.loadreg(a)
            val = builder.biop(irInstrISub, imm, ra)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.biop(irInstrCarrySub, imm, ra))

        builder.storereg d, val

proc subfmex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfmex)
    else:
        let
            minusOne = builder.imm(0xFFFF_FFFF'u32)
            (ra, carry) = builder.loadregcarry(a)
            val = builder.triop(irInstrISubExtended, minusOne, ra, carry)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.triop(irInstrCarrySubExtended, minusOne, ra, carry))
        builder.updateOv(builder.triop(irInstrOverflowSubExtended, minusOne, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfzex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfzex)
    else:
        let
            zero = builder.imm(0'u32)
            (ra, carry) = builder.loadregcarry(a)
            val = builder.triop(irInstrISubExtended, zero, ra, carry)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32, builder.triop(irInstrCarrySubExtended, zero, ra, carry))
        builder.updateOv(builder.triop(irInstrOverflowSubExtended, zero, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc cmp*(builder; crfD, l, a, b: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cmp)
    else:
        let (ra, rb) = builder.loadregs(a, b)
        builder.setCr(true, crfD, ra, rb)

proc cmpi*(builder; crfD, l, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cmpi)
    else:
        let ra = builder.loadreg(a)
        builder.setCr(true, crfD, ra, builder.imm(signExtend[uint32](imm, 16)))

proc cmpl*(builder; crfD, l, a, b: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cmpl)
    else:
        let (ra, rb) = builder.loadregs(a, b)
        builder.setCr(false, crfD, ra, rb)

proc cmpli*(builder; crfD, l, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cmpli)
    else:
        let ra = builder.loadreg(a)
        builder.setCr(false, crfD, ra, builder.imm(imm))

template logicOp(genOp: untyped): untyped =
    let
        (rs {.inject.}, rb {.inject.}) = builder.loadregs(s, b)
        val = genOp

    builder.handleRc(val, rc)

    builder.storereg a, val

template logicOpImm(genOp, rc): untyped =
    let
        rs {.inject.} = builder.loadreg(s)
        val = genOp

    builder.handleRc(val, rc)

    builder.storereg a, val

proc andx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andx)
    else:
        logicOp(builder.biop(irInstrBitAnd, rs, rb))

proc andcx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andcx)
    else:
        logicOp(builder.biop(irInstrBitAnd, rs, builder.unop(irInstrBitNot, rb)))

proc andidot*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andidot)
    else:
        logicOpImm builder.biop(irInstrBitAnd, rs, builder.imm(imm)), 1

proc andisdot*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andisdot)
    else:
        logicOpImm builder.biop(irInstrBitAnd, rs, builder.imm(imm shl 16)), 1

template simpleUnop(genOp: untyped): untyped =
    let
        rs {.inject.} = builder.loadreg(s)
        val = genOp

    builder.handleRc(val, rc)

    builder.storereg a, val

proc cntlzwx*(builder; s, a, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cntlzwx)
    else:
        simpleUnop builder.unop(irInstrClz, rs)

proc eqvx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.eqvx)
    else:
        logicOp builder.unop(irInstrBitNot, builder.biop(irInstrBitXor, rs, rb))

proc extsbx*(builder; s, a, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.extsbx)
    else:
        simpleUnop builder.unop(irInstrExtsb, rs)

proc extshx*(builder; s, a, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.extshx)
    else:
        simpleUnop builder.unop(irInstrExtsh, rs)

proc nandx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.nandx)
    else:
        logicOp builder.unop(irInstrBitNot, builder.biop(irInstrBitAnd, rs, rb))

proc norx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.norx)
    else:
        logicOp builder.unop(irInstrBitNot, builder.biop(irInstrBitOr, rs, rb))

proc orx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.orx)
    else:
        logicOp builder.biop(irInstrBitOr, rs, rb)

proc orcx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.orcx)
    else:
        logicOp builder.biop(irInstrBitOr, rs, builder.unop(irInstrBitNot, rb))

proc ori*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ori)
    else:
        logicOpImm builder.biop(irInstrBitOr, rs, builder.imm(imm)), 0

proc oris*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.oris)
    else:
        logicOpImm builder.biop(irInstrBitOr, rs, builder.imm(imm shl 16)), 0

proc xorx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.xorx)
    else:
        logicOp builder.biop(irInstrBitXor, rs, rb)

proc xori*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.xori)
    else:
        logicOpImm builder.biop(irInstrBitXor, rs, builder.imm(imm)), 0

proc xoris*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.xoris)
    else:
        logicOpImm builder.biop(irInstrBitXor, rs, builder.imm(imm shl 16)), 0

proc rlwimix*(builder; s, a, sh, mb, me, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rlwimix)
    else:
        let
            (rs, ra) = builder.loadregs(s, a)
            val =
                builder.biop(irInstrBitOr,
                    builder.biop(irInstrBitAnd,
                        builder.biop(irInstrRol, rs, builder.imm(sh)),
                        builder.imm(ppcmask(mb, me))),
                    builder.biop(irInstrBitAnd, ra, builder.imm(not ppcmask(mb, me))))

        builder.handleRc(val, rc)

        builder.storereg a, val

proc rlwinmx*(builder; s, a, sh, mb, me, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rlwinmx)
    else:
        let
            rs = builder.loadreg(s)
            val =
                builder.biop(irInstrBitAnd,
                        builder.biop(irInstrRol, rs, builder.imm(sh)),
                        builder.imm(ppcmask(mb, me)))

        builder.handleRc(val, rc)

        builder.storereg a, val

proc rlwnmx*(builder; s, a, b, mb, me, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rlwnmx)
    else:
        let
            (rs, rb) = builder.loadregs(s, b)
            val =
                builder.biop(irInstrBitAnd,
                        builder.biop(irInstrRol, rs, rb),
                        builder.imm(ppcmask(mb, me)))

        builder.handleRc(val, rc)

        builder.storereg a, val

proc slwx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.slwx)
    else:
        let
            (rs, rb) = builder.loadregs(s, b)
            val = builder.biop(irInstrShl, rs, rb)

        builder.handleRc(val, rc)

        builder.storereg a, val

proc srawx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.srawx)
    else:
        let
            (rs, rb) = builder.loadregs(s, b)
            val = builder.biop(irInstrShrArith, rs, rb)

        builder.handleRc(val, rc)
        
        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32,
            builder.biop(irInstrCondAnd,
                builder.biop(irInstrCmpLessSI, rs, builder.imm(0)),
                builder.unop(irInstrCondNot,
                    builder.biop(irInstrCmpEqualI,
                        builder.biop(irInstrBitAnd,
                            rs,
                            builder.unop(irInstrBitNot, builder.biop(irInstrShl, builder.imm(0xFFFFFFFF'u32), rb))),
                        builder.imm(0)))))

        builder.storereg a, val

proc srawix*(builder; s, a, sh, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.srawix)
    else:
        let
            rs = builder.loadreg(s)
            val = builder.biop(irInstrShrArith, rs, builder.imm(sh))

        builder.handleRc(val, rc)

        discard builder.storectx(irInstrStoreXer, irXerNumCa.uint32,
            builder.biop(irInstrCondAnd,
                builder.biop(irInstrCmpLessSI, rs, builder.imm(0)),
                builder.unop(irInstrCondNot,
                    builder.biop(irInstrCmpEqualI,
                        builder.biop(irInstrBitAnd,
                            builder.imm((0..int(sh)-1).toMask[:uint32]()),
                            rs),
                        builder.imm(0)))))


        builder.storereg a, val

proc srwx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.srwx)
    else:
        let
            (rs, rb) = builder.loadregs(s, b)
            val = builder.biop(irInstrShrLogic, rs, rb)

        builder.handleRc(val, rc)

        builder.storereg a, val

template crOp(genOp: untyped): untyped =
    let
        crbA {.inject.} = builder.loadctx(irInstrLoadCrBit, crbA)
        crbB {.inject.} = builder.loadctx(irInstrLoadCrBit, crbB)
    discard builder.storectx(irInstrStoreCrBit, crbD, genOp)

proc crand*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crand)
    else:
        crOp builder.biop(irInstrCondAnd, crbA, crbB)

proc crandc*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crandc)
    else:
        crOp builder.biop(irInstrCondAnd, crbA, builder.unop(irInstrCondNot, crbB))

proc creqv*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.creqv)
    else:
        crOp builder.unop(irInstrCondNot, builder.biop(irInstrCondXor, crbA, crbB))

proc crnand*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crnand)
    else:
        crOp builder.unop(irInstrCondNot, builder.biop(irInstrCondAnd, crbA, crbB))

proc crnor*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crnor)
    else:
        crOp builder.unop(irInstrCondNot, builder.biop(irInstrCondOr, crbA, crbB))

proc cror*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cror)
    else:
        crOp builder.biop(irInstrCondOr, crbA, crbB)

proc crorc*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crorc)
    else:
        crOp builder.biop(irInstrCondOr, crbA, builder.unop(irInstrCondNot, crbB))

proc crxor*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crxor)
    else:
        crOp builder.biop(irInstrCondXor, crbA, crbB)

proc mcrf*(builder; crfD, crfS: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mcrf)
    else:
        discard builder.storectx(irInstrStoreCrBit, crfD*4+0, builder.loadctx(irInstrLoadCrBit, crfS*4+0))
        discard builder.storectx(irInstrStoreCrBit, crfD*4+1, builder.loadctx(irInstrLoadCrBit, crfS*4+1))
        discard builder.storectx(irInstrStoreCrBit, crfD*4+2, builder.loadctx(irInstrLoadCrBit, crfS*4+2))
        discard builder.storectx(irInstrStoreCrBit, crfD*4+3, builder.loadctx(irInstrLoadCrBit, crfS*4+3))
