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
    builder.regs.branch = true

proc lwarx*(builder; d, a, b: uint32) =
    raiseAssert "instr not implemented lwarx"

proc stwcxdot*(builder; s, a, b: uint32) =
    raiseAssert "instr not implemented stwcxdot"

proc sync*(builder) =
    discard builder.triop(ppcBranch, builder.imm(true), builder.imm(builder.regs.pc + 4), builder.imm(0))
    builder.regs.branch = true

const
    exceptionSavedMask = Msr(0xFFFFFFFF'u32).exceptionSaved
    powMask = getFieldMask[Msr](pow)

proc rfi*(builder) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.rfi)
    else:
        let
            msr = builder.loadctx(loadSpr, irSprNumMsr.uint32)
            srr0 = builder.loadctx(loadSpr, irSprNumSrr0.uint32)
            srr1 = builder.loadctx(loadSpr, irSprNumSrr1.uint32)

            newMsr = builder.biop(bitOr,
                builder.biop(bitAnd, msr, builder.imm(not(exceptionSavedMask or powMask))),
                builder.biop(bitAnd, srr1, builder.imm(exceptionSavedMask)))

        discard builder.storectx(storeSpr, irSprNumMsr.uint32, newMsr)
        discard builder.triop(ppcBranch, builder.imm(true), srr0, builder.imm(0))
    builder.regs.branch = true

proc sc*(builder) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.sc)
    else:
        discard builder.unop(ppcSyscall, builder.imm(builder.regs.pc + 4))
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
        builder.storereg(d, builder.loadctx(loadSpr, irSprNumCr.uint32))

proc mfmsr*(builder; d: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mfmsr)
    else:
        builder.storereg(d, builder.loadctx(loadSpr, irSprNumMsr.uint32))

proc mfspr*(builder; d, spr: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mfspr)
    else:
        let n = decodeSplitSpr spr

        builder.storereg(d, builder.loadctx(loadSpr,
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
            of 920: irSprNumHid2.uint32
            of 921: irSprNumWpar.uint32
            of 922: irSprNumDmaU.uint32
            of 923: irSprNumDmaL.uint32
            of 936, 952: irSprNumMmcr0.uint32
            of 940, 956: irSprNumMmcr1.uint32
            of 953..954: irSprNumPmc0.succ(int n - 953).uint32
            of 957..958: irSprNumPmc2.succ(int n - 957).uint32
            of 937..938: irSprNumPmc0.succ(int n - 953).uint32
            of 941..942: irSprNumPmc2.succ(int n - 957).uint32
            of 1008: irSprNumHid0.uint32
            of 1009: irSprNumHid1.uint32
            of 1017: irSprNumL2cr.uint32
            else: raiseAssert(&"unimplemented {n}")))

proc mftb*(builder; d, tpr: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mftb)
    else:
        let n = decodeSplitSpr(tpr)

        builder.storereg(d,
            case n
            of 268: builder.loadctx(loadSpr, irSprNumTbL.uint32)
            of 269: builder.loadctx(loadSpr, irSprNumTbU.uint32)
            else:
                raiseAssert &"unknown mftb register {n}")

proc mtcrf*(builder; s, crm: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mtcrf)
    else:
        let
            mask = makeFieldMask(crm)
            rs = builder.loadreg(s)
            cr = builder.loadctx(loadSpr, irSprNumCr.uint32)

        discard builder.storectx(storeSpr, irSprNumCr.uint32,
            builder.biop(bitOr,
                builder.biop(bitAnd, cr, builder.imm(not mask)),
                builder.biop(bitAnd, rs, builder.imm(mask))))

proc mtmsr*(builder; s: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mtmsr)
    else:
        discard builder.storectx(storeSpr, irSprNumMsr.uint32, builder.loadreg(s))

proc mcrfs*(builder; crfD, crfS: uint32) =
    raiseAssert "unimplemented instr mcrf"

proc mtspr*(builder; d, spr: uint32) =
    when interpretSystem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.mtspr)
    else:
        let n = decodeSplitSpr spr

        let rd = builder.loadreg(d)

        case n
        of 1: discard builder.storectx(storeSpr, irSprNumXer.uint32, rd)
        of 8: discard builder.storectx(storeSpr, irSprNumLr.uint32, rd)
        of 9: discard builder.storectx(storeSpr, irSprNumCtr.uint32, rd)
        of 18: discard builder.storectx(storeSpr, irSprNumDsisr.uint32, rd)
        of 19: discard builder.storectx(storeSpr, irSprNumDar.uint32, rd)
        of 22: discard builder.storectx(storeSpr, irSprNumDec.uint32, rd)
        of 26: discard builder.storectx(storeSpr, irSprNumSrr0.uint32, rd)
        of 27: discard builder.storectx(storeSpr, irSprNumSrr1.uint32, rd)
        of 272..275: discard builder.storectx(storeSpr, irSprNumSprg0.succ(int n - 272).uint32, rd)
        of 284: discard builder.storectx(storeSpr, irSprNumTbL.uint32, rd)
        of 285: discard builder.storectx(storeSpr, irSprNumTbU.uint32, rd)
        of 528..535:
            let n = n - 528
            discard builder.storectx(storeSpr,
                (if (n and 1) == 0: irSprNumIBatU0 else: irSprNumIBatL0).succ(int(n shr 1)).uint32, rd)
        of 536..543:
            let n = n - 536
            discard builder.storectx(storeSpr,
                (if (n and 1) == 0: irSprNumDBatU0 else: irSprNumDBatL0).succ(int(n shr 1)).uint32, rd)
        of 912..919: discard builder.storectx(storeSpr, irSprNumGqr0.succ(int n - 912).uint32, rd)
        of 920: discard builder.storectx(storeSpr, irSprNumHid2.uint32, rd)
        of 921: discard builder.storectx(storeSpr, irSprNumWpar.uint32, rd)
        of 922: discard builder.storectx(storeSpr, irSprNumDmaU.uint32, rd)
        of 923: discard builder.storectx(storeSpr, irSprNumDmaL.uint32, rd)
        of 952: discard builder.storectx(storeSpr, irSprNumMmcr0.uint32, rd)
        of 956: discard builder.storectx(storeSpr, irSprNumMmcr1.uint32, rd)
        of 953..954: discard builder.storectx(storeSpr, irSprNumPmc0.succ(int n - 953).uint32, rd)
        of 957..958: discard builder.storectx(storeSpr, irSprNumPmc2.succ(int n - 957).uint32, rd)
        of 1008: discard builder.storectx(storeSpr, irSprNumHid0.uint32, rd)
        of 1009: discard builder.storectx(storeSpr, irSprNumHid1.uint32, rd)
        of 1017: discard builder.storectx(storeSpr, irSprNumL2cr.uint32, rd)
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
