import
    sets,
    ../../util/jit/ir,
    dspfrontendcommon,
    fallbacks,
    dspblockcache

using builder: var IrBlockBuilder[DspIrState]

proc jmp*(builder; cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.jmp)
    discard builder.fetchFollowingImm
    builder.regs.branch = true

proc jmpr*(builder; r, cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.jmpr)
    builder.regs.branch = true

proc call*(builder; cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.call)
    discard builder.fetchFollowingImm
    builder.regs.branch = true

proc callr*(builder; r, cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.callr)
    builder.regs.branch = true

proc rets*(builder; cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.rets)
    builder.regs.branch = true

proc reti*(builder; cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.reti)
    builder.regs.branch = true

proc exec*(builder; cc: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.exec)
    builder.regs.branch = true

proc loopi*(builder; c: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.loopi)
    loopEnds.incl builder.fetchFollowingImm
    builder.regs.branch = true

proc loop*(builder; r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.loop)
    loopEnds.incl builder.fetchFollowingImm
    builder.regs.branch = true

proc repi*(builder; c: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.repi)
    loopEnds.incl builder.regs.pc + 1
    builder.regs.branch = true

proc rep*(builder; r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.rep)
    loopEnds.incl builder.regs.pc + 1
    builder.regs.branch = true

proc trap*(builder) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.trap)

proc wait*(builder) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.wait)
    builder.regs.branch = true
