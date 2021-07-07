import
    ../../util/jit/ir,
    dspfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[DspIrState]

proc mr*(builder; m, r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mr)

proc adsi*(builder; d, i: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.adsi)

proc adli*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.adli)
    discard builder.fetchFollowingImm

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
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.xorli)
    discard builder.fetchFollowingImm

proc anli*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.anli)
    discard builder.fetchFollowingImm

proc orli*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.orli)
    discard builder.fetchFollowingImm

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
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mv)

proc mvsi*(builder; d, i: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mvsi)

proc mvli*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.mvli)
    discard builder.fetchFollowingImm()

proc clrpsr*(builder; b: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.clrpsr)

proc setpsr*(builder; b: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.setpsr)

proc btstl*(builder; b: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.btstl)
    discard builder.fetchFollowingImm()

proc btsth*(builder; b: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.btsth)
    discard builder.fetchFollowingImm()

proc add*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.add)

proc addl*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.addl)

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
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.pnop)

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
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.nnot)

proc xxor*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.xxor)

proc xxor2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.xxor2)

proc aand*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.aand)

proc aand2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.aand2)

proc oor*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.oor)

proc oor2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.oor2)

proc lsf*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsf)

proc lsf2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.lsf2)

proc asf*(builder; s, d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asf)

proc asf2*(builder; d, x: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.asf2)

