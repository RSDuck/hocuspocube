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
                (builder.biop(iCmpLessS, a, b), builder.biop(iCmpGreaterS, a, b))
            else:
                (builder.biop(iCmpLessU, a, b), builder.biop(iCmpGreaterU, a, b))
        eq = builder.biop(iCmpEqual, a, b)
    discard builder.storectx(storeCrBit, cond*4+0, lt)
    discard builder.storectx(storeCrBit, cond*4+1, gt)
    discard builder.storectx(storeCrBit, cond*4+2, eq)
    discard builder.storectx(storeCrBit, cond*4+3, builder.loadctx(loadXer, irXerNumSo.uint32))

proc handleRc(builder; val: IrInstrRef, rc: uint32) =
    if rc != 0:
        builder.setCr(true, 0, val, builder.imm(0))

template updateOv(builder; val: IrInstrRef, oe: uint32) =
    if oe != 0:
        discard builder.storectx(storeXer, irXerNumOv.uint32, val)
        discard builder.storectx(storeXer, irXerNumSo.uint32,
            builder.biop(condOr, builder.loadctx(loadXer, irXerNumSo.uint32), val))

proc addx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iAdd, ra, rb)

        builder.updateOv(builder.biop(overflowAdd, ra, rb), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc addcx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addcx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iAdd, ra, rb)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.biop(carryAdd, ra, rb))
        builder.updateOv(builder.biop(overflowAdd, rb, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc addex*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addex)
    else:
        let
            (ra, rb, carry) = builder.loadregscarry(a, b)
            val = builder.triop(iAddExtended, ra, rb, carry)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.triop(carryAddExtended, ra, rb, carry))
        builder.updateOv(builder.triop(overflowAddExtended, ra, rb, carry), oe)

        builder.storereg(d, val)

proc addi*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addi)
    else:
        let imm = builder.imm(signExtend[uint32](imm, 16))
        if a == 0:
            builder.storereg(d, imm)
        else:
            builder.storereg(d, builder.biop(iAdd, builder.loadreg(a), imm))

proc addic*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addic)
    else:
        let
            imm = builder.imm(signExtend[uint32](imm, 16))
            ra = builder.loadreg(a)
            val = builder.biop(iAdd, ra, imm)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.biop(carryAdd, ra, imm))

        builder.storereg d, val

proc addicdot*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addicdot)
    else:
        let
            imm = builder.imm(signExtend[uint32](imm, 16))
            ra = builder.loadreg(a)
            val = builder.biop(iAdd, ra, imm)

        builder.handleRc(val, 1)
        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.biop(carryAdd, ra, imm))

        builder.storereg d, val

proc addis*(builder; d, a, imm: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addis)
    else:
        let imm = builder.imm(imm shl 16)
        if a == 0:
            builder.storereg(d, imm)
        else:
            builder.storereg(d, builder.biop(iAdd, builder.loadreg(a), imm))

proc addmex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addmex)
    else:
        let
            (ra, carry) = builder.loadregcarry(a)
            minusOne = builder.imm(0xFFFF_FFFF'u32)
            val = builder.triop(iAddExtended, ra, minusOne, carry)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.triop(carryAddExtended, ra, minusOne, carry))
        builder.updateOv(builder.triop(overflowAddExtended, ra, minusOne, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc addzex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretAdd:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.addzex)
    else:
        let
            (ra, carry) = builder.loadregcarry(a)
            val = builder.biop(iAdd, ra, carry)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.biop(carryAdd, ra, carry))
        builder.updateOv(builder.biop(overflowAdd, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg(d, val)

proc divwx*(builder; d, a, b, oe, rc: uint32) =
    #[builder.updateOv(builder.biop(condOr,
            builder.biop(condAnd,
                builder.biop(iCmpEqual, builder.r(a), builder.imm(0x8000_0000'u32)),
                builder.biop(iCmpEqual, builder.r(b), builder.imm(0xFFFF_FFFF'u32))),
            builder.biop(iCmpEqual, builder.r(b), builder.imm(0))),
        oe)]#
    assert oe == 0
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.divwx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iDivS, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc divwux*(builder; d, a, b, oe, rc: uint32) =
    assert oe == 0
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.divwux)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iDivU, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc mulhwx*(builder; d, a, b, rc: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mulhwx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iMulhS, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc mulhwux*(builder; d, a, b, rc: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mulhwux)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iMulhU, ra, rb)

        builder.storereg(d, val)

        builder.handleRc(val, rc)

proc mulli*(builder; d, a, imm: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mulli)
    else:
        builder.storereg(d, builder.biop(imul, builder.loadreg(a), builder.imm(signExtend[uint32](imm, 16))))

proc mullwx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretMulDiv:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mullwx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(imul, ra, rb)

        builder.updateOv(block:
            let upperResult = builder.biop(iMulhS, ra, rb)
            builder.unop(condNot, builder.biop(condOr,
                builder.biop(iCmpEqual, upperResult, builder.imm(0)),
                builder.biop(iCmpEqual, upperResult, builder.imm(0xFFFFFFFF'u32)))),
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
            val = builder.biop(iSub, zero, ra)

        builder.updateOv(builder.biop(overflowSub, zero, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iSub, rb, ra)

        builder.updateOv(builder.biop(overflowSub, rb, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfcx*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfcx)
    else:
        let
            (ra, rb) = builder.loadregs(a, b)
            val = builder.biop(iSub, rb, ra)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.biop(carrySub, rb, ra))
        builder.updateOv(builder.biop(overflowSub, rb, ra), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfex*(builder; d, a, b, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfex)
    else:
        let
            (ra, rb, carry) = builder.loadregscarry(a, b)
            val = builder.triop(iSubExtended, rb, ra, carry)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.triop(carrySubExtended, rb, ra, carry))
        builder.updateOv(builder.triop(overflowSubExtended, rb, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfic*(builder; d, a, imm: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfic)
    else:
        let
            imm = builder.imm(signExtend[uint32](imm, 16))
            ra = builder.loadreg(a)
            val = builder.biop(iSub, imm, ra)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.biop(carrySub, imm, ra))

        builder.storereg d, val

proc subfmex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfmex)
    else:
        let
            minusOne = builder.imm(0xFFFF_FFFF'u32)
            (ra, carry) = builder.loadregcarry(a)
            val = builder.triop(iSubExtended, minusOne, ra, carry)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.triop(carrySubExtended, minusOne, ra, carry))
        builder.updateOv(builder.triop(overflowSubExtended, minusOne, ra, carry), oe)
        builder.handleRc(val, rc)

        builder.storereg d, val

proc subfzex*(builder; d, a, oe, rc: uint32) =
    when interpretInt or interpretSub:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.subfzex)
    else:
        let
            zero = builder.imm(0'u32)
            (ra, carry) = builder.loadregcarry(a)
            val = builder.triop(iSubExtended, zero, ra, carry)

        discard builder.storectx(storeXer, irXerNumCa.uint32, builder.triop(carrySubExtended, zero, ra, carry))
        builder.updateOv(builder.triop(overflowSubExtended, zero, ra, carry), oe)
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
        logicOp(builder.biop(bitAnd, rs, rb))

proc andcx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andcx)
    else:
        logicOp(builder.biop(bitAnd, rs, builder.unop(bitNot, rb)))

proc andidot*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andidot)
    else:
        logicOpImm builder.biop(bitAnd, rs, builder.imm(imm)), 1

proc andisdot*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.andisdot)
    else:
        logicOpImm builder.biop(bitAnd, rs, builder.imm(imm shl 16)), 1

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
        simpleUnop builder.unop(clz, rs)

proc eqvx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.eqvx)
    else:
        logicOp builder.unop(bitNot, builder.biop(bitXor, rs, rb))

proc extsbx*(builder; s, a, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.extsbx)
    else:
        simpleUnop builder.unop(extsb, rs)

proc extshx*(builder; s, a, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.extshx)
    else:
        simpleUnop builder.unop(extsh, rs)

proc nandx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.nandx)
    else:
        logicOp builder.unop(bitNot, builder.biop(bitAnd, rs, rb))

proc norx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.norx)
    else:
        logicOp builder.unop(bitNot, builder.biop(bitOr, rs, rb))

proc orx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.orx)
    else:
        logicOp builder.biop(bitOr, rs, rb)

proc orcx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.orcx)
    else:
        logicOp builder.biop(bitOr, rs, builder.unop(bitNot, rb))

proc ori*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ori)
    else:
        logicOpImm builder.biop(bitOr, rs, builder.imm(imm)), 0

proc oris*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.oris)
    else:
        logicOpImm builder.biop(bitOr, rs, builder.imm(imm shl 16)), 0

proc xorx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.xorx)
    else:
        logicOp builder.biop(bitXor, rs, rb)

proc xori*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.xori)
    else:
        logicOpImm builder.biop(bitXor, rs, builder.imm(imm)), 0

proc xoris*(builder; s, a, imm: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.xoris)
    else:
        logicOpImm builder.biop(bitXor, rs, builder.imm(imm shl 16)), 0

proc rlwimix*(builder; s, a, sh, mb, me, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rlwimix)
    else:
        let
            (rs, ra) = builder.loadregs(s, a)
            val =
                builder.biop(bitOr,
                    builder.biop(bitAnd,
                        builder.biop(rol, rs, builder.imm(sh)),
                        builder.imm(ppcmask(mb, me))),
                    builder.biop(bitAnd, ra, builder.imm(not ppcmask(mb, me))))

        builder.handleRc(val, rc)

        builder.storereg a, val

proc rlwinmx*(builder; s, a, sh, mb, me, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rlwinmx)
    else:
        let
            rs = builder.loadreg(s)
            val =
                if mb == 0 and sh == 31-me:
                    builder.biop(lsl, rs, builder.imm(sh))
                elif sh == 32-mb and me == 31:
                    builder.biop(lsr, rs, builder.imm(mb))
                else:
                    builder.biop(bitAnd,
                            builder.biop(rol, rs, builder.imm(sh)),
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
                builder.biop(bitAnd,
                        builder.biop(rol, rs, rb),
                        builder.imm(ppcmask(mb, me)))

        builder.handleRc(val, rc)

        builder.storereg a, val

proc slwx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.slwx)
    else:
        let
            (rs, rb) = builder.loadregs(s, b)
            val = builder.unop(extzw, builder.biop(lslX, rs, rb))

        builder.handleRc(val, rc)

        builder.storereg a, val

proc srawx*(builder; s, a, b, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.srawx)
    else:
        let
            (rs, rb) = builder.loadregs(s, b)
            val = builder.unop(extzw, builder.biop(asrX, builder.unop(extsw, rs), rb))

        builder.handleRc(val, rc)
        
        discard builder.storectx(storeXer, irXerNumCa.uint32,
            builder.biop(condAnd,
                builder.biop(iCmpLessS, rs, builder.imm(0)),
                builder.unop(condNot,
                    builder.biop(iCmpEqual,
                        builder.biop(bitAnd,
                            rs,
                            builder.unop(bitNot, builder.biop(lsl, builder.imm(0xFFFFFFFF'u32), rb))),
                        builder.imm(0)))))

        builder.storereg a, val

proc srawix*(builder; s, a, sh, rc: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.srawix)
    else:
        let
            rs = builder.loadreg(s)
            val = builder.biop(asr, rs, builder.imm(sh))

        builder.handleRc(val, rc)

        discard builder.storectx(storeXer, irXerNumCa.uint32,
            builder.biop(condAnd,
                builder.biop(iCmpLessS, rs, builder.imm(0)),
                builder.unop(condNot,
                    builder.biop(iCmpEqual,
                        builder.biop(bitAnd,
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
            val = builder.unop(extzw, builder.biop(lsrX, rs, rb))

        builder.handleRc(val, rc)

        builder.storereg a, val

template crOp(genOp: untyped): untyped =
    let
        crbA {.inject.} = builder.loadctx(loadCrBit, crbA)
        crbB {.inject.} = builder.loadctx(loadCrBit, crbB)
    discard builder.storectx(storeCrBit, crbD, genOp)

proc crand*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crand)
    else:
        crOp builder.biop(condAnd, crbA, crbB)

proc crandc*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crandc)
    else:
        crOp builder.biop(condAnd, crbA, builder.unop(condNot, crbB))

proc creqv*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.creqv)
    else:
        crOp builder.unop(condNot, builder.biop(condXor, crbA, crbB))

proc crnand*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crnand)
    else:
        crOp builder.unop(condNot, builder.biop(condAnd, crbA, crbB))

proc crnor*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crnor)
    else:
        crOp builder.unop(condNot, builder.biop(condOr, crbA, crbB))

proc cror*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.cror)
    else:
        crOp builder.biop(condOr, crbA, crbB)

proc crorc*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crorc)
    else:
        crOp builder.biop(condOr, crbA, builder.unop(condNot, crbB))

proc crxor*(builder; crbD, crbA, crbB: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.crxor)
    else:
        crOp builder.biop(condXor, crbA, crbB)

proc mcrf*(builder; crfD, crfS: uint32) =
    when interpretInt:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mcrf)
    else:
        discard builder.storectx(storeCrBit, crfD*4+0, builder.loadctx(loadCrBit, crfS*4+0))
        discard builder.storectx(storeCrBit, crfD*4+1, builder.loadctx(loadCrBit, crfS*4+1))
        discard builder.storectx(storeCrBit, crfD*4+2, builder.loadctx(loadCrBit, crfS*4+2))
        discard builder.storectx(storeCrBit, crfD*4+3, builder.loadctx(loadCrBit, crfS*4+3))
