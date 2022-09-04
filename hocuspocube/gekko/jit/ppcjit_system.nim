import
    strformat,
    ../../util/bitstruct,
    ../ppcstate, ../ppccommon,
    ../../util/jit/ir, ppcfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[PpcIrRegState]

const interpretSystem = false

proc eieio*(builder) =
    raiseAssert "instr not implemented eieio"

proc isync*(builder) =
    discard builder.triop(ppcBranch, builder.imm(true), builder.imm(builder.regs.pc + 4), builder.imm(0))

proc lwarx*(builder; d, a, b: uint32) =
    raiseAssert "instr not implemented lwarx"

proc stwcxdot*(builder; s, a, b: uint32) =
    raiseAssert "instr not implemented stwcxdot"

proc sync*(builder) =
    discard builder.triop(ppcBranch, builder.imm(true), builder.imm(builder.regs.pc + 4), builder.imm(0))

const
    exceptionSavedMask = Msr(0xFFFFFFFF'u32).exceptionSaved
    powMask = getFieldMask[Msr](pow)

proc rfi*(builder) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.rfi)
    else:
        let
            msr = builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, msr))
            srr0 = builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, srr0))
            srr1 = builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, srr1))

            newMsr = builder.biop(bitOr,
                builder.biop(bitAnd, msr, builder.imm(not(exceptionSavedMask or powMask))),
                builder.biop(bitAnd, srr1, builder.imm(exceptionSavedMask)))

        builder.storectx(ctxStore32, uint32 offsetof(PpcState, msr), newMsr)
        discard builder.triop(ppcBranch, builder.imm(true), srr0, builder.imm(0))

proc sc*(builder) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.sc)
    else:
        discard builder.unop(ppcSyscall, builder.imm(builder.regs.pc + 4))

proc tw*(builder; to, a, b: uint32) =
    builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.tw)

proc twi*(builder; to, a, imm: uint32) =
    builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.twi)

proc mcrxr*(builder; crfS: uint32) =
    raiseAssert "instr not implemented mcrxr"

proc mfcr*(builder; d: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mfcr)
    else:
        builder.storereg(d, builder.loadCr())

proc mfmsr*(builder; d: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mfmsr)
    else:
        builder.storereg(d, builder.loadctx(ctxLoadU32, uint32 offsetof(PpcState, msr)))

proc mfspr*(builder; d, spr: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mfspr)
    else:
        let n = decodeSplitSpr spr

        builder.storereg(d,
            if n == 22:
                builder.loadSpr(decrementer)
            else:
                builder.loadctx(ctxLoadU32,
                    case n
                    of 1: uint32 offsetof(PpcState, xer)
                    of 8: uint32 offsetof(PpcState, lr)
                    of 9: uint32 offsetof(PpcState, ctr)
                    of 18: uint32 offsetof(PpcState, dsisr)
                    of 19: uint32 offsetof(PpcState, dar)
                    of 26: uint32 offsetof(PpcState, srr0)
                    of 27: uint32 offsetof(PpcState, srr1)
                    of 272..275: uint32(offsetof(PpcState, sprg)) + 4*(n-272)
                    of 528..535:
                        let n = n - 528
                        (uint32(if (n and 1) == 0: offsetof(PpcState, ibatHi) else: offsetof(PpcState, ibatLo)) + (n div 2)*4)
                    of 536..543:
                        let n = n - 536
                        (uint32(if (n and 1) == 0: offsetof(PpcState, dbatHi) else: offsetof(PpcState, dbatLo)) + (n div 2)*4)
                    of 912..919: uint32(offsetof(PpcState, gqr)) + (n-912)*4
                    of 920: uint32 offsetof(PpcState, hid2)
                    of 921: uint32 offsetof(PpcState, wpar)
                    of 922: uint32 offsetof(PpcState, dmaU)
                    of 923: uint32 offsetof(PpcState, dmaL)
                    of 936, 952: uint32 offsetof(PpcState, mmcr0)
                    of 940, 956: uint32 offsetof(PpcState, mmcr1)
                    of 953..954: uint32(offsetof(PpcState, pmc)) + 4*(n-953)
                    of 957..958: uint32(offsetof(PpcState, pmc)) + 4*(n-957+2)
                    of 1008: uint32 offsetof(PpcState, hid0)
                    of 1009: uint32 offsetof(PpcState, hid1)
                    of 1017: uint32 offsetof(PpcState, l2cr)
                    else: raiseAssert(&"unimplemented spr read {n}")))

proc mftb*(builder; d, tpr: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mftb)
    else:
        let n = decodeSplitSpr(tpr)

        builder.storereg(d,
            case n
            of 268: builder.loadSpr(tbL)
            of 269: builder.loadSpr(tbU)
            else:
                raiseAssert &"unknown mftb register {n}")

proc mtcrf*(builder; s, crm: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mtcrf)
    else:
        let
            mask = makeFieldMask(crm)
            rs = builder.loadreg(s)
            cr = builder.loadCr()

        builder.storeCr(builder.biop(bitOr,
            builder.biop(bitAnd, cr, builder.imm(not mask)),
            builder.biop(bitAnd, rs, builder.imm(mask))))

proc mtmsr*(builder; s: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mtmsr)
    else:
        builder.storectx(ctxStore32, uint32 offsetof(PpcState, msr), builder.loadreg(s))

proc mcrfs*(builder; crfD, crfS: uint32) =
    raiseAssert "unimplemented instr mcrf"

proc mtspr*(builder; d, spr: uint32) =
    when interpretSystem:
        builder.interpretppc(builder.regs.instr, builder.regs.pc, fallbacks.mtspr)
    else:
        let n = decodeSplitSpr spr

        let rd = builder.loadreg(d)

        case n
        of 22: builder.storeSpr(decrementer, rd)
        of 284: builder.storeSpr(tbL, rd)
        of 285: builder.storeSpr(tbU, rd)
        of 921: builder.storeSpr(wpar, rd)
        of 923: builder.storeSpr(dmaL, rd)
        of 1008: builder.storeSpr(hid0, rd)
        of 528..535:
            let n = int(n - 528)
            builder.storeSpr(
                if (n and 1) == 0:
                    ibatHi0.succ(n shr 1)
                else:
                    iBatLo0.succ(n shr 1), rd)
        of 536..543:
            let n = int(n - 536)
            builder.storeSpr(
                if (n and 1) == 0:
                    dbatHi0.succ(n shr 1)
                else:
                    dBatLo0.succ(n shr 1), rd)
        else:
            builder.storectx(ctxStore32, (case n
                of 1: uint32 offsetof(PpcState, xer)
                of 8: uint32 offsetof(PpcState, lr)
                of 9: uint32 offsetof(PpcState, ctr)
                of 18: uint32 offsetof(PpcState, dsisr)
                of 19: uint32 offsetof(PpcState, dar)
                of 26: uint32 offsetof(PpcState, srr0)
                of 27: uint32 offsetof(PpcState, srr1)
                of 272..275: uint32(offsetof(PpcState, sprg)) + 4*(n-272)
                of 912..919: uint32(offsetof(PpcState, gqr)) + (n-912)*4
                of 920: uint32 offsetof(PpcState, hid2)
                of 922: uint32 offsetof(PpcState, dmaU)
                of 936, 952: uint32 offsetof(PpcState, mmcr0)
                of 940, 956: uint32 offsetof(PpcState, mmcr1)
                of 953..954: uint32(offsetof(PpcState, pmc)) + (n-953)*4
                of 957..958: uint32(offsetof(PpcState, pmc)) + (n-957+2)*4
                of 1009: uint32 offsetof(PpcState, hid1)
                of 1017: uint32 offsetof(PpcState, l2cr)
                else: raiseAssert(&"unimplemented spr write {n}")), rd)

proc mfsr*(builder; d, sr: uint32) =
    raiseAssert "instr not implemented mfsr"

proc mfsrin*(builder; d, b: uint32) =
    raiseAssert "instr not implemented mfsrin"

proc mtsr*(builder; s, sr: uint32) =
    discard

proc mtsrin*(builder; s, b: uint32) =
    raiseAssert "instr not implemented mtsrin"

proc tlbie*(builder; b: uint32) =
    raiseAssert "instr not implemented tlbie"

proc tlbsync*(builder) =
    raiseAssert "instr not implemented tlbsync"

proc eciwx*(builder; d, a, b: uint32) =
    raiseAssert "instr not implemented eciwx"

proc ecowx*(builder; s, a, b: uint32) =
    raiseAssert "instr not implemented ecowx"
