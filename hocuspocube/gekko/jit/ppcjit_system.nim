import
    strformat,
    ../../util/bitstruct,
    ../ppcstate, ../ppccommon,
    ir, ppcfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[PpcIrRegState]

const interpretSystem = false

proc eieio*(builder) =
    raiseAssert "instr not implemented eieio"

proc isync*(builder) =
    discard builder.triop(irInstrBranch, builder.imm(true), builder.imm(builder.regs.pc + 4), builder.imm(0))
    builder.regs.branch = true

proc lwarx*(builder; d, a, b: uint32) =
    raiseAssert "instr not implemented lwarx"

proc stwcxdot*(builder; s, a, b: uint32) =
    raiseAssert "instr not implemented stwcxdot"

proc sync*(builder) =
    discard builder.triop(irInstrBranch, builder.imm(true), builder.imm(builder.regs.pc + 4), builder.imm(0))
    builder.regs.branch = true

const
    exceptionSavedMask = Msr(0xFFFFFFFF'u32).exceptionSaved
    powMask = getFieldMask[Msr](pow)

proc rfi*(builder) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rfi)
    else:
        let
            msr = builder.loadctx(irInstrLoadSpr, irSprNumMsr.uint32)
            srr0 = builder.loadctx(irInstrLoadSpr, irSprNumSrr0.uint32)
            srr1 = builder.loadctx(irInstrLoadSpr, irSprNumSrr1.uint32)

            newMsr = builder.biop(irInstrBitOr,
                builder.biop(irInstrBitAnd, msr, builder.imm(not(exceptionSavedMask or powMask))),
                builder.biop(irInstrBitAnd, srr1, builder.imm(exceptionSavedMask)))

        discard builder.storectx(irInstrStoreSpr, irSprNumMsr.uint32, newMsr)
        discard builder.triop(irInstrBranch, builder.imm(true), srr0, builder.imm(0))
    builder.regs.branch = true

proc sc*(builder) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.sc)
    else:
        discard builder.unop(irInstrSyscall, builder.imm(builder.regs.pc + 4))
    builder.regs.branch = true

proc tw*(builder; to, a, b: uint32) =
    raiseAssert "instr not implemented tw"

proc twi*(builder; to, a, imm: uint32) =
    raiseAssert "unimplemented instr twi"

proc mcrxr*(builder; crfS: uint32) =
    raiseAssert "instr not implemented mcrxr"

proc mfcr*(builder; d: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mfcr)
    else:
        builder.storereg(d, builder.loadctx(irInstrLoadSpr, irSprNumCr.uint32))

proc mfmsr*(builder; d: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mfmsr)
    else:
        builder.storereg(d, builder.loadctx(irInstrLoadSpr, irSprNumMsr.uint32))

proc mfspr*(builder; d, spr: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mfspr)
    else:
        let n = decodeSplitSpr spr

        builder.storereg(d, builder.loadctx(irInstrLoadSpr,
            case n
            of 1: irSprNumXer.uint32
            of 8: irSprNumLr.uint32
            of 9: irSprNumCtr.uint32
            of 18: irSprNumDsisr.uint32
            of 19: irSprNumDar.uint32
            of 22: irSprNumDec.uint32
            of 26: irSprNumSrr0.uint32
            of 27: irSprNumSrr1.uint32
            of 272..275: irSprNumSprg0.succ(int n - 272).uint32
            of 528..535:
                let n = n - 528
                (if (n and 1) == 0: irSprNumIBatU0 else: irSprNumIBatL0).succ(int(n shr 1)).uint32
            of 536..543:
                let n = n - 536
                (if (n and 1) == 0: irSprNumDBatU0 else: irSprNumDBatL0).succ(int(n shr 1)).uint32
            of 912..919: irSprNumGqr0.succ(int n - 912).uint32
            of 936, 952: irSprNumMmcr0.uint32
            of 940, 956: irSprNumMmcr1.uint32
            of 953..954: irSprNumPmc0.succ(int n - 953).uint32
            of 957..958: irSprNumPmc2.succ(int n - 957).uint32
            of 937..938: irSprNumPmc0.succ(int n - 953).uint32
            of 941..942: irSprNumPmc2.succ(int n - 957).uint32
            of 1008: irSprNumHid0.uint32
            of 1009: irSprNumHid1.uint32
            of 920: irSprNumHid2.uint32
            of 921: irSprNumWpar.uint32
            of 1017: irSprNumL2cr.uint32
            else: raiseAssert(&"unimplemented {n}")))

proc mftb*(builder; d, tpr: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mftb)
    else:
        let n = decodeSplitSpr(tpr)

        builder.storereg(d,
            case n
            of 268: builder.loadctx(irInstrLoadSpr, irSprNumTbL.uint32)
            of 269: builder.loadctx(irInstrLoadSpr, irSprNumTbU.uint32)
            else:
                raiseAssert &"unknown mftb register {n}")

proc mtcrf*(builder; s, crm: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mtcrf)
    else:
        let
            mask = makeFieldMask(crm)
            rs = builder.loadreg(s)
            cr = builder.loadctx(irInstrLoadSpr, irSprNumCr.uint32)

        discard builder.storectx(irInstrStoreSpr, irSprNumCr.uint32,
            builder.biop(irInstrBitOr,
                builder.biop(irInstrBitAnd, cr, builder.imm(not mask)),
                builder.biop(irInstrBitAnd, rs, builder.imm(mask))))

proc mtmsr*(builder; s: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mtmsr)
    else:
        discard builder.storectx(irInstrStoreSpr, irSprNumMsr.uint32, builder.loadreg(s))

proc mcrfs*(builder; crfD, crfS: uint32) =
    raiseAssert "unimplemented instr mcrf"

proc mtspr*(builder; d, spr: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mtspr)
    else:
        let n = decodeSplitSpr spr

        let rd = builder.loadreg(d)

        case n
        of 1: discard builder.storectx(irInstrStoreSpr, irSprNumXer.uint32, rd)
        of 8: discard builder.storectx(irInstrStoreSpr, irSprNumLr.uint32, rd)
        of 9: discard builder.storectx(irInstrStoreSpr, irSprNumCtr.uint32, rd)
        of 18: discard builder.storectx(irInstrStoreSpr, irSprNumDsisr.uint32, rd)
        of 19: discard builder.storectx(irInstrStoreSpr, irSprNumDar.uint32, rd)
        of 22: discard builder.storectx(irInstrStoreSpr, irSprNumDec.uint32, rd)
        of 26: discard builder.storectx(irInstrStoreSpr, irSprNumSrr0.uint32, rd)
        of 27: discard builder.storectx(irInstrStoreSpr, irSprNumSrr1.uint32, rd)
        of 272..275: discard builder.storectx(irInstrStoreSpr, irSprNumSprg0.succ(int n - 272).uint32, rd)
        of 284: discard builder.storectx(irInstrStoreSpr, irSprNumTbL.uint32, rd)
        of 285: discard builder.storectx(irInstrStoreSpr, irSprNumTbU.uint32, rd)
        of 528..535:
            let n = n - 528
            discard builder.storectx(irInstrStoreSpr,
                (if (n and 1) == 0: irSprNumIBatU0 else: irSprNumIBatL0).succ(int(n shr 1)).uint32, rd)
        of 536..543:
            let n = n - 536
            discard builder.storectx(irInstrStoreSpr,
                (if (n and 1) == 0: irSprNumDBatU0 else: irSprNumDBatL0).succ(int(n shr 1)).uint32, rd)
        of 912..919: discard builder.storectx(irInstrStoreSpr, irSprNumGqr0.succ(int n - 912).uint32, rd)
        of 952: discard builder.storectx(irInstrStoreSpr, irSprNumMmcr0.uint32, rd)
        of 956: discard builder.storectx(irInstrStoreSpr, irSprNumMmcr1.uint32, rd)
        of 953..954: discard builder.storectx(irInstrStoreSpr, irSprNumPmc0.succ(int n - 953).uint32, rd)
        of 957..958: discard builder.storectx(irInstrStoreSpr, irSprNumPmc2.succ(int n - 957).uint32, rd)
        of 1008: discard builder.storectx(irInstrStoreSpr, irSprNumHid0.uint32, rd)
        of 1009: discard builder.storectx(irInstrStoreSpr, irSprNumHid1.uint32, rd)
        of 920: discard builder.storectx(irInstrStoreSpr, irSprNumHid2.uint32, rd)
        of 921: discard builder.storectx(irInstrStoreSpr, irSprNumWpar.uint32, rd)
        of 1017: discard builder.storectx(irInstrStoreSpr, irSprNumL2cr.uint32, rd)
        else: raiseAssert(&"unknown spr num {n}")

proc dcbf*(builder; a, b: uint32) =
    discard

proc dcbi*(builder; a, b: uint32) =
    discard

proc dcbst*(builder; a, b: uint32) =
    discard

proc dcbt*(builder; a, b: uint32) =
    discard

proc dcbtst*(builder; a, b: uint32) =
    discard

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
