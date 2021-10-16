import
    ../dspstate, ../dspdef, dspfrontendcommon,
    fallbacks_secondary,
    ../../util/jit/ir,
    strformat

using builder: var IrBlockBuilder[DspIrState]

proc undefinedSecondary(builder; instr: uint16) =
    raiseAssert(&"undefined secondary dsp instr {builder.regs.pc:02X} {instr:02X}")

proc mr(builder; m, r: uint16) =
    if m != 0:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.mr)

proc mv(builder; d, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.mv)

proc st(builder; s, m, r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.st)

proc ld(builder; d, m, r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.ld)

proc ls(builder; d, m, n, k, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.ls)

proc ldd(builder; d, m, n, r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.ldd)

proc dispatchSecondary*(builder; x: uint16) =
    dspSecondaryDispatch(x, builder, undefinedSecondary)
