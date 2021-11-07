import
    ../dspstate, ../dspdef, dspfrontendcommon,
    fallbacks_secondary,
    ../../util/jit/ir,
    strformat

using builder: var IrBlockBuilder[DspIrState]

const interpretSecondary = false

proc undefinedSecondary(builder; instr: uint16) =
    raiseAssert(&"undefined secondary dsp instr {builder.regs.pc:02X} {instr:02X}")

proc mr(builder; m, r: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.mr)
    else:
        builder.loadStoreAdrInc(builder.readAdr(r), m, r)

proc mv(builder; d, s: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.mv)
    else:
        builder.writeReg x0.succ(int d),
            case range[0..3](s)
            of 0..1: builder.readReg(a0.succ(int s))
            of 2..3: builder.storeAccum(s - 2)

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
