import
    options,
    ../../util/[jit/ir, aluhelper], ../dspstate,
    dspfrontendcommon,
    dspjit_secondary,
    fallbacks

using builder: var IrBlockBuilder[DspIrState]

const interpretAlu = false

proc addAccumOp(builder; accumN: uint32, addend: IrInstrRef, secondary: Option[uint16]) =
    let accum = builder.readAccum(accumN)

    if secondary.isSome:
        builder.dispatchSecondary(secondary.get)

    let
        sum = builder.biop(irInstrIAddX, accum, addend)
        ov = builder.biop(irInstrOverflowAddX, accum, addend)
        ca = builder.biop(irInstrCarryAddX, accum, addend)
        zero = builder.biop(irInstrCmpEqualIX, sum, builder.imm(0))
        mi = builder.biop(irInstrCmpLessSIX, sum, builder.imm(0))
        ext = builder.biop(irInstrCmpEqualIX, sum, builder.unop(irInstrExtsw, sum))
        unnorm = builder.biop(irInstrShrLogic,
            builder.unop(irInstrBitNot,
                builder.biop(irInstrBitXor, sum, builder.biop(irInstrShl, sum, builder.imm(1)))),
            builder.imm(31))

    builder.writeStatus(dspStatusBitOv, ov)
    builder.writeStatus(dspStatusBitCa, ca)
    builder.writeStatus(dspStatusBitZr, zero)
    builder.writeStatus(dspStatusBitMi, mi)
    builder.writeStatus(dspStatusBitExt, ext)
    builder.writeStatus(dspStatusBitUnnorm, unnorm)

    builder.writeAccumSignExtend(accumN, sum)

proc logicOp(builder; accumN: uint32, op: IrInstrKind, operand: IrInstrRef, secondary: Option[uint16]) =
    let accum = builder.readAccum(accumN)

    if secondary.isSome:
        builder.dispatchSecondary(secondary.get)

    let
        res = builder.biop(op, accum, operand)
        zero = builder.biop(irInstrCmpEqualIX, res, builder.imm(0))
        mi = builder.biop(irInstrCmpLessSIX, res, builder.imm(0))
        ext = builder.biop(irInstrCmpEqualIX, res, builder.unop(irInstrExtsw, res))
        unnorm = builder.biop(irInstrShrLogic,
            builder.unop(irInstrBitNot,
                builder.biop(irInstrBitXor, res, builder.biop(irInstrShl, res, builder.imm(1)))),
            builder.imm(31))

    builder.writeStatus(dspStatusBitOv, builder.imm(false))
    builder.writeStatus(dspStatusBitCa, builder.imm(false))
    builder.writeStatus(dspStatusBitZr, zero)
    builder.writeStatus(dspStatusBitMi, mi)
    builder.writeStatus(dspStatusBitExt, ext)
    builder.writeStatus(dspStatusBitUnnorm, unnorm)

    builder.writeAccum(accumN, res)

proc mr*(builder; m, r: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mr)
    else:
        discard

proc adsi*(builder; d, i: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.adsi)
    else:
        builder.addAccumOp(d, builder.imm(signExtend[uint64](i, 8) shl 16), none(uint16))

proc adli*(builder; d: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.adli)
        discard builder.fetchFollowingImm
    else:
        builder.addAccumOp(d, builder.imm(signExtend[uint64](builder.fetchFollowingImm(), 16) shl 16), none(uint16))

proc cmpsi*(builder; s, i: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.cmpsi)

proc cmpli*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.cmpli)
    discard builder.fetchFollowingImm

proc lsfi*(builder; d, i: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsfi)

proc asfi*(builder; d, i: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asfi)

proc xorli*(builder; d: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.xorli)
        discard builder.fetchFollowingImm
    else:
        builder.logicOp(d, irInstrBitXorX, builder.imm(uint64(builder.fetchFollowingImm()) shl 16), none(uint16))

proc anli*(builder; d: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.anli)
        discard builder.fetchFollowingImm
    else:
        builder.logicOp(d, irInstrBitAndX, builder.imm((uint64(builder.fetchFollowingImm()) shl 16) or 0xFFFF_FFFF_0000_FFFF'u64), none(uint16))

proc orli*(builder; d: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.orli)
        discard builder.fetchFollowingImm
    else:
        builder.logicOp(d, irInstrBitOrX, builder.imm(uint64(builder.fetchFollowingImm()) shl 16), none(uint16))

proc norm*(builder; d, r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.norm)

proc ddiv*(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ddiv)

proc addc*(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.addc)

proc subc*(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.subc)

proc negc*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.negc)

proc max*(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.max)

proc lsfn*(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsfn)

proc lsfn2*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsfn2)

proc asfn*(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asfn)

proc asfn2*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asfn2)

proc mv*(builder; d, s: uint16) =
    if interpretAlu or DspReg(d) in dspRegCallStack..dspRegLoopCountStack or
        DspReg(s) in dspRegCallStack..dspRegLoopCountStack:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mv)
    else:
        builder.writeReg(DspReg d, builder.readReg(DspReg s))

proc mvsi*(builder; d, i: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mvsi)
    else:
        if d <= 5:
            builder.writeReg(dspRegX0.succ(int d), builder.imm(signExtend(i, 8)))
        else:
            builder.loadAccum(d - 6, builder.imm(signExtend(i, 8)))

proc mvli*(builder; d: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mvli)
        discard builder.fetchFollowingImm()
    else:
        builder.writeReg(DspReg(d), builder.imm(builder.fetchFollowingImm()))

proc clrpsr*(builder; b: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clrpsr)
    else:
        builder.writeStatus(dspStatusBitTb.succ(int b), builder.imm(false))

proc setpsr*(builder; b: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.setpsr)
    else:
        builder.writeStatus(dspStatusBitTb.succ(int b), builder.imm(true))

proc btstl*(builder; b: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.btstl)
        discard builder.fetchFollowingImm()
    else:
        let mask = builder.imm(uint32(builder.fetchFollowingImm()) shl 16)
        builder.writeStatus(dspStatusBitTb,
            builder.biop(irInstrCmpEqualI,
                builder.biop(irInstrBitAnd, builder.readAccum(b), mask),
                builder.imm(0)))

proc btsth*(builder; b: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.btsth)
        discard builder.fetchFollowingImm()
    else:
        let mask = builder.imm(uint32(builder.fetchFollowingImm()) shl 16)
        builder.writeStatus(dspStatusBitTb,
            builder.biop(irInstrCmpEqualI,
                builder.biop(irInstrBitAnd, builder.readAccum(b), mask),
                mask))

proc add*(builder; s, d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.add)
    else:
        builder.addAccumOp(d, case range[0..7](s)
            of 0..3: builder.biop(irInstrShl, builder.unop(irInstrExtsh, builder.readReg(dspRegX0.succ(int s))), builder.imm(16))
            of 4..5: builder.readAuxAccum(s - 4)
            of 6: builder.readAccum(1 - d)
            of 7: builder.readProd(),

            some(x))

proc addl*(builder; s, d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.addl)
    else:
        builder.addAccumOp(d, builder.readReg(dspRegX0.succ(int s)), some(x))

proc sub*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.sub)

proc amv*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.amv)

proc cmp*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.cmp)

proc cmpa*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.cmpa)

proc inc*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.inc)

proc dec*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.dec)

proc abs*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.abs)

proc neg*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.neg)

proc negp*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.negp)

proc clra*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clra)

proc clrp*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clrp)

proc rnd*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.rnd)

proc rndp*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.rndp)

proc tst*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.tst)

proc tst2*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.tst2)

proc tstp*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.tstp)

proc lsl16*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsl16)

proc lsr16*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsr16)

proc asr16*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asr16)

proc addp*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.addp)

proc pnop*(builder; x: uint16) =
    builder.dispatchSecondary(x)

proc clrim*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clrim)

proc clrdp*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clrdp)

proc clrxl*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clrxl)

proc setim*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.setim)

proc setdp*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.setdp)

proc setxl*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.setxl)

proc mpy*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mpy)

proc mpy2*(builder; x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mpy2)

proc mac*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mac)

proc mac2*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mac2)

proc mac3*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mac3)

proc macn*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.macn)

proc macn2*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.macn2)

proc macn3*(builder; s, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.macn3)

proc mvmpy*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mvmpy)

proc rnmpy*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.rnmpy)

proc admpy*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.admpy)

proc nnot*(builder; d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.nnot)
    else:
        builder.logicOp(d, irInstrBitXorX, builder.imm(0xFFFF_0000'u64), some(x))

proc xxor*(builder; s, d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.xxor)
    else:
        builder.logicOp(d, irInstrBitXorX, builder.biop(irInstrBitAndX, builder.readAuxAccum(s), builder.imm(0xFFFF_0000'u64)), some(x))

proc xxor2*(builder; d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.xxor2)
    else:
        builder.logicOp(d, irInstrBitXorX, builder.biop(irInstrBitAndX, builder.readAccum(1-d), builder.imm(0xFFFF_0000'u64)), some(x))

proc aand*(builder; s, d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.aand)
    else:
        builder.logicOp(d, irInstrBitAndX, builder.biop(irInstrBitOrX, builder.readAuxAccum(s), builder.imm(0xFFFF_FFFF_0000_FFFF'u64)), some(x))

proc aand2*(builder; d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.aand2)
    else:
        builder.logicOp(d, irInstrBitAndX, builder.biop(irInstrBitOrX, builder.readAccum(1-d), builder.imm(0xFFFF_FFFF_0000_FFFF'u64)), some(x))

proc oor*(builder; s, d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.oor)
    else:
        builder.logicOp(d, irInstrBitOrX, builder.biop(irInstrBitAndX, builder.readAuxAccum(s), builder.imm(0xFFFF_0000'u64)), some(x))

proc oor2*(builder; d, x: uint16) =
    when interpretAlu:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.oor2)
    else:
        builder.logicOp(d, irInstrBitXorX, builder.biop(irInstrBitAndX, builder.readAccum(1-d), builder.imm(0xFFFF_0000'u64)), some(x))

proc lsf*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsf)

proc lsf2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsf2)

proc asf*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asf)

proc asf2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asf2)

