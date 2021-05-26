import
    ../../util/aluhelper, stew/bitops2,
    ir, ppcfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[PpcIrRegState]

const interpretBranch = false

proc bx*(builder; li, aa, lk: uint32) =
    when interpretBranch:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.bx)
    else:
        if lk == 1:
            discard builder.storectx(irInstrStoreSpr, irSprNumLr.uint32, builder.imm(builder.regs.pc + 4))

        let target =
            (if aa == 1:
                (signExtend(li, 24) shl 2)
            else:
                builder.regs.pc + (signExtend(li, 24) shl 2))

        discard builder.triop(irInstrBranch, builder.imm(true), builder.imm(target), builder.imm(0))

    builder.regs.branch = true

proc handleCondAndCtr(builder; bo, bi: uint32): IrInstrRef =
    let
        ctrOk =
            (if bo.getBit(2):
                builder.imm(true)
            else:
                let ctr = builder.biop(irInstrISub, builder.loadctx(irInstrLoadSpr, irSprNumCtr.uint32), builder.imm(1))
                discard builder.storectx(irInstrStoreSpr, irSprNumCtr.uint32, ctr)
                let isZero = builder.biop(irInstrCmpEqualI, ctr, builder.imm(0))
                if bo.getBit(1):
                    isZero
                else:
                    builder.unop(irInstrCondNot, isZero))
        condOk =
            (if bo.getBit(4):
                builder.imm(true)
            else:
                let crBit = builder.loadctx(irInstrLoadCrBit, bi)
                if bo.getBit(3):
                    crBit
                else:
                    builder.unop(irInstrCondNot, crBit))

    builder.biop(irInstrCondAnd, ctrOk, condOk)

proc bcx*(builder; bo, bi, bd, aa, lk: uint32) =
    when interpretBranch:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.bcx)
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
            discard builder.storectx(irInstrStoreSpr, irSprNumLr.uint32, 
                builder.triop(irInstrCsel, prevNia, builder.loadctx(irInstrLoadSpr, irSprNumLr.uint32), cond))

        discard builder.triop(irInstrBranch, cond, builder.imm(target), prevNia)

    builder.regs.branch = true

proc bcctrx*(builder; bo, bi, lk: uint32) =
    when interpretBranch:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.bcctrx)
    else:
        let
            cond = builder.handleCondAndCtr(bo or 0x10, bi)
            prevNia = builder.imm(builder.regs.pc + 4)

        if lk == 1:
            discard builder.storectx(irInstrStoreSpr, irSprNumLr.uint32,
                builder.triop(irInstrCsel, prevNia, builder.loadctx(irInstrLoadSpr, irSprNumLr.uint32), cond))

        discard builder.triop(irInstrBranch, cond, builder.loadctx(irInstrLoadSpr, irSprNumCtr.uint32), prevNia)

    builder.regs.branch = true

proc bclrx*(builder; bo, bi, lk: uint32) =
    when interpretBranch:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.bclrx)
    else:
        let
            cond = builder.handleCondAndCtr(bo, bi)
            lr = builder.loadctx(irInstrLoadSpr, irSprNumLr.uint32)
            prevNia = builder.imm(builder.regs.pc + 4)

        if lk == 1:
            discard builder.storectx(irInstrStoreSpr, irSprNumLr.uint32,
                builder.triop(irInstrCsel, prevNia, lr, cond))

        discard builder.triop(irInstrBranch, cond, lr, prevNia)

    builder.regs.branch = true