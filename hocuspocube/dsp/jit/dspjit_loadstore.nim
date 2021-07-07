import
    ../../util/jit/ir,
    dspfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[DspIrState]

proc pld*(builder; d, m, r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.pld)

proc ld*(builder; m, r, d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ld)

proc st*(builder; m, r, s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.st)

proc ldsa*(builder; d, a: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ldsa)

proc stsa*(builder; s, a: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.stsa)

proc ldla*(builder; d: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ldla)
    discard builder.fetchFollowingImm()

proc stla*(builder; s: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.stla)
    discard builder.fetchFollowingImm()

proc stli*(builder; a: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.stli)
    discard builder.fetchFollowingImm()
