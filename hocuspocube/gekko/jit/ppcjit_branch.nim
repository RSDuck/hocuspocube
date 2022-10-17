import
    ../../util/aluhelper, stew/bitops2,
    ../../util/jit/ir, ppcfrontendcommon,
    fallbacks


using builder: var IrBlockBuilder[PpcIrRegState]

const interpretBranch = false

proc bx*(builder; li, aa, lk: uint32) =
    when interpretBranch:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.bx)
    else:
        if lk == 1:
            builder.storeLr(builder.imm(builder.regs.pc + 4))

        let target =
            (if aa == 1:
                (signExtend(li, 24) shl 2)
            else:
                builder.regs.pc + (signExtend(li, 24) shl 2))

        builder.branchUncond(builder.imm(target))

proc handleCondAndCtr(builder; bo, bi: uint32): IrInstrRef =
    let
        ctrOk =
            (if bo.getBit(2):
                builder.imm(true)
            else:
                let ctr = builder.biop(iSub, builder.loadCtr(), builder.imm(1))
                builder.storeCtr(ctr)
                let isZero = builder.biop(iCmpEqual, ctr, builder.imm(0))
                if bo.getBit(1):
                    isZero
                else:
                    builder.unop(condNot, isZero))
        condOk =
            (if bo.getBit(4):
                builder.imm(true)
            else:
                let crBit = builder.loadCrBit(bi)
                if bo.getBit(3):
                    crBit
                else:
                    builder.unop(condNot, crBit))

    builder.biop(condAnd, ctrOk, condOk)

proc bcx*(builder; bo, bi, bd, aa, lk: uint32) =
    when interpretBranch:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.bcx)
    else:
        let
            target =
                (if aa == 1:
                    signExtend(bd, 14) shl 2
                else:
                    builder.regs.pc + (signExtend(bd, 14) shl 2))
            cond = builder.handleCondAndCtr(bo, bi)
            prevNia = builder.imm(builder.regs.pc + 4)

        if lk == 1:
            builder.storeLr(builder.triop(csel, prevNia, builder.loadLr(), cond))

        builder.branchCond(cond, builder.imm(target), prevNia)

proc bcctrx*(builder; bo, bi, lk: uint32) =
    when interpretBranch:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.bcctrx)
    else:
        let
            cond = builder.handleCondAndCtr(bo or 0x10, bi)
            prevNia = builder.imm(builder.regs.pc + 4)

        if lk == 1:
            builder.storeLr(builder.triop(csel, prevNia, builder.loadLr(), cond))

        builder.branchCond(cond, builder.loadCtr(), prevNia)

proc bclrx*(builder; bo, bi, lk: uint32) =
    when interpretBranch:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.bclrx)
    else:
        let
            cond = builder.handleCondAndCtr(bo, bi)
            lr = builder.loadLr()
            prevNia = builder.imm(builder.regs.pc + 4)

        if lk == 1:
            builder.storeLr(builder.triop(csel, prevNia, lr, cond))

        builder.branchCond(cond, lr, prevNia)
