import
    ir, ppcfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[PpcIrRegState]

const
    interpretFloat = false
    interpretPs = false

proc faddx*(builder; d, a, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.faddx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFAddSd, fra, frb)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc faddsx*(builder; d, a, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.faddsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.postSingleOp(builder.biop(irInstrFAddSd, fra, frb))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fdivx*(builder; d, a, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fdivx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFDivSd, fra, frb)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fdivsx*(builder; d, a, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fdivsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.postSingleOp(builder.biop(irInstrFDivSd, fra, frb))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fmulx*(builder; d, a, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmulx)
    else:
        let
            fra = builder.loadfreg(a)
            frc = builder.loadfreg(c)

            val = builder.biop(irInstrFMulSd, fra, frc)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fmulsx*(builder; d, a, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmulsx)
    else:
        let
            fra = builder.loadfreg(a)
            frc = builder.loadfreg(c)

            val = builder.postSingleOp(builder.biop(irInstrFMulSd, fra, frc))

        builder.storefregReplicate d, val

proc fresx*(builder; d, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fresx)
    else:
        let
            frb = builder.loadfreg(b)

            val = builder.postSingleOp(builder.unop(irInstrFResSd, frb))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc frsqrtex*(builder; d, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.frsqrtex)
    else:
        let
            frb = builder.loadfreg(b)

            val = builder.postSingleOp(builder.unop(irInstrFRsqrtSd, frb))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fsubx*(builder; d, a, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fsubx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFSubSd, fra, frb)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fsubsx*(builder; d, a, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fsubsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.postSingleOp(builder.biop(irInstrFSubSd, fra, frb))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fselx*(builder; d, a, b, c, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fselx)
    builder.regs.floatInstr = true

proc fmaddx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmaddx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.triop(irInstrFMaddSd, fra, frb, frс)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fmaddsx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmaddsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.postSingleOp(builder.triop(irInstrFMaddSd, fra, frb, frс))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fmsubx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmsubx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.triop(irInstrFMsubSd, fra, frb, frс)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fmsubsx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmsubsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.postSingleOp(builder.triop(irInstrFMsubSd, fra, frb, frс))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fnmaddx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fnmaddx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.triop(irInstrFNmaddSd, fra, frb, frс)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fnmaddsx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fnmaddsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.postSingleOp(builder.triop(irInstrFNmaddSd, fra, frb, frс))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fnmsubx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fnmsubx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.triop(irInstrFNmsubSd, fra, frb, frс)

        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fnmsubsx*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fnmsubsx)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frс = builder.loadfreg(c)

            val = builder.postSingleOp(builder.triop(irInstrFNmsubSd, fra, frb, frс))

        builder.storefregReplicate d, val
    builder.regs.floatInstr = true

proc fctiwx*(builder; d, b, rc: uint32) =
    raiseAssert "instr not implemented fctiwx"

proc fctiwzx*(builder; d, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fctiwzx)
    else:
        let
            frb = builder.loadfreg(b)
            val = builder.unop(irInstrCvtsd2intTrunc, frb)
        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc frspx*(builder; d, b, rc: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.frspx)
    else:
        let
            frb = builder.loadfreg(b)
            val = builder.postSingleOp(frb)
        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fcmpo*(builder; crfD, a, b: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fcmpo)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

        let
            lt = builder.biop(irInstrCmpLessFsd, fra, frb)
            gt = builder.biop(irInstrCmpGreaterFsd, fra, frb)
            eq = builder.biop(irInstrCmpEqualFsd, fra, frb)
            unordered = builder.biop(irInstrCmpUnorderedsd, fra, frb)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+0, lt)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+1, gt)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+2, eq)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+3, unordered)
    builder.regs.floatInstr = true

proc fcmpu*(builder; crfD, a, b: uint32) =
    when interpretFloat:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fcmpu)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

        let
            lt = builder.biop(irInstrCmpLessFsd, fra, frb)
            gt = builder.biop(irInstrCmpGreaterFsd, fra, frb)
            eq = builder.biop(irInstrCmpEqualFsd, fra, frb)
            unordered = builder.biop(irInstrCmpUnorderedsd, fra, frb)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+0, lt)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+1, gt)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+2, eq)
        discard builder.storectx(irInstrStoreCrBit, crfD*4+3, unordered)
    builder.regs.floatInstr = true

proc mffsx*(builder; d, rc: uint32) =
    discard

proc mtfsb0x*(builder; crbD, rc: uint32) =
    raiseAssert("unimplemented instr mtfsb0x")

proc mtfsb1x*(builder; crbD, rc: uint32) =
    builder.regs.floatInstr = true

proc mtfsfx*(builder; fm, b, rc: uint32) =
    builder.regs.floatInstr = true

proc mtfsfix*(builder; crfD, imm, rc: uint32) =
    raiseAssert("unimplemented instr mtfsfix")

proc fabsx*(builder; d, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fabsx)
    else:
        let
            frb = builder.loadfreg(b)
            val = builder.unop(irInstrFAbssd, frb)
        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc fmrx*(builder; d, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fmrx)
    else:
        builder.storefregLowOnly d, builder.loadfreg(b)
    builder.regs.floatInstr = true

proc fnabsx*(builder; d, b, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fnabsx)
    builder.regs.floatInstr = true

proc fnegx*(builder; d, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.fnegx)
    else:
        let
            frb = builder.loadfreg(b)
            val = builder.unop(irInstrFNegsd, frb)
        builder.storefregLowOnly d, val
    builder.regs.floatInstr = true

proc ps_div*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_div)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.postSingleOpPair(builder.biop(irInstrFDivpd, fra, frb))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_sub*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_sub)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.postSingleOpPair(builder.biop(irInstrFSubpd, fra, frb))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_add*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_add)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.postSingleOpPair(builder.biop(irInstrFAddpd, fra, frb))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_sel*(builder; d, a, b, c, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_sel)
    builder.regs.floatInstr = true

proc ps_res*(builder; d, b, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_res)
    builder.regs.floatInstr = true

proc ps_mul*(builder; d, a, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_mul)
    else:
        let
            fra = builder.loadfreg(a)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.biop(irInstrFMulpd, fra, frc))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_rsqrte*(builder; d, b, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_rsqrte)
    builder.regs.floatInstr = true

proc ps_msub*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_msub)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.triop(irInstrFMsubpd, fra, frb, frc))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_madd*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_madd)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.triop(irInstrFMaddpd, fra, frb, frc))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_nmsub*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_nmsub)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.triop(irInstrFNmsubpd, fra, frb, frc))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_nmadd*(builder; d, a, b, c, rc : uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_nmadd)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.triop(irInstrFNmaddpd, fra, frb, frc))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_neg*(builder; d, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_neg)
    else:
        let
            frb = builder.loadfreg(b)
            val = builder.unop(irInstrFNegpd, frb)
        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_mr*(builder; d, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_mr)
    else:
        builder.storefregp d, builder.loadfreg(b)
    builder.regs.floatInstr = true

proc ps_nabs*(builder; d, b, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_nabs)
    builder.regs.floatInstr = true

proc ps_abs*(builder; d, b, rc: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_abs)
    builder.regs.floatInstr = true

proc ps_sum0*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_sum0)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            sumpart = builder.biop(irInstrFAddsd, fra, builder.unop(irInstrFSwizzleD11, frb))
            merged = builder.biop(irInstrFMergeD, frc, sumpart)

        builder.storefregp d, merged
    builder.regs.floatInstr = true

proc ps_sum1*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_sum1)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            sumpart = builder.biop(irInstrFAddpd, builder.unop(irInstrFSwizzleD00, fra), frb)
            merged = builder.biop(irInstrFMergeD, sumpart, frc)

        builder.storefregp d, merged
    builder.regs.floatInstr = true

proc ps_muls0*(builder; d, a, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_muls0)
    else:
        let
            fra = builder.loadfreg(a)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.biop(irInstrFMulpd, fra, builder.unop(irInstrFSwizzleD00, frc)))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_muls1*(builder; d, a, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_muls1)
    else:
        let
            fra = builder.loadfreg(a)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.biop(irInstrFMulpd, fra, builder.unop(irInstrFSwizzleD11, frc)))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_madds0*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_madds0)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.triop(irInstrFMaddpd, fra, frb, builder.unop(irInstrFSwizzleD00, frc)))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_madds1*(builder; d, a, b, c, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_madds1)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)
            frc = builder.loadfreg(c)

            val = builder.postSingleOpPair(builder.triop(irInstrFMaddpd, fra, frb, builder.unop(irInstrFSwizzleD11, frc)))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_cmpu0*(builder; crfD, a, b: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_cmpu0)
    builder.regs.floatInstr = true

proc ps_cmpo0*(builder; crfD, a, b: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_cmpo0)
    builder.regs.floatInstr = true

proc ps_cmpu1*(builder; crfD, a, b: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_cmpu1)
    builder.regs.floatInstr = true

proc ps_cmpo1*(builder; crfD, a, b: uint32) =
    builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_cmpo1)
    builder.regs.floatInstr = true

proc ps_merge00*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_merge00)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFMergeD,
                builder.unop(irInstrFSwizzleD00, frb),
                fra)

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_merge01*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_merge01)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFMergeD, frb, fra)

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_merge10*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_merge10)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFMergeD,
                builder.unop(irInstrFSwizzleD00, frb),
                builder.unop(irInstrFSwizzleD11, fra))

        builder.storefregp d, val
    builder.regs.floatInstr = true

proc ps_merge11*(builder; d, a, b, rc: uint32) =
    when interpretFloat or interpretPs:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.ps_merge11)
    else:
        let
            fra = builder.loadfreg(a)
            frb = builder.loadfreg(b)

            val = builder.biop(irInstrFMergeD,
                frb,
                builder.unop(irInstrFSwizzleD11, fra))

        builder.storefregp d, val
    builder.regs.floatInstr = true