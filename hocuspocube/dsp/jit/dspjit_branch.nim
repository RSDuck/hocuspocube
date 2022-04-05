import
    ../../util/jit/ir, ../dspstate,
    dspfrontendcommon,
    fallbacks,
    dspblockcache

using builder: var IrBlockBuilder[DspIrState]

const interpretBranch = false

proc evalCond(builder; cond: uint32): IrInstrRef =
    template isLess: IrInstrRef = builder.biop(condXor, builder.readStatus(dspStatusBitOv), builder.readStatus(dspStatusBitMi))
    template isEqual: IrInstrRef = builder.readStatus(dspStatusBitZr)
    template isLequal: IrInstrRef = builder.biop(condOr, isLess(), isEqual())
    case range[0..15](cond)
    of 0: builder.unop(condNot, isLess)  # greater or equal
    of 1: isLess # less
    of 2: builder.unop(condNot, isLequal) # greater
    of 3: isLequal # less or equal
    of 4: builder.unop(condNot, isEqual) # not equal
    of 5: isEqual # equal
    of 6: builder.unop(condNot, builder.readStatus(dspStatusBitCa))
    of 7: builder.readStatus(dspStatusBitCa)
    of 8: builder.unop(condNot, builder.readStatus(dspStatusBitExt))
    of 9: builder.readStatus(dspStatusBitExt)
    of 10: builder.biop(condAnd, builder.unop(condNot, isEqual), builder.biop(bitOr, builder.readStatus(dspStatusBitExt), builder.readStatus(dspStatusBitUnnorm)))
    of 11: builder.biop(condAnd, isEqual, builder.unop(condNot, builder.biop(bitOr, builder.readStatus(dspStatusBitExt), builder.readStatus(dspStatusBitUnnorm))))
    of 12: builder.unop(condNot, builder.readStatus(dspStatusBitTb))
    of 13: builder.readStatus(dspStatusBitTb)
    of 14: builder.readStatus(dspStatusBitOv)
    of 15: builder.imm(true)

proc jmp*(builder; cc: uint16) =
    when interpretBranch:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.jmp)
        discard builder.fetchFollowingImm
    else:
        let target = builder.fetchFollowingImm

        discard builder.triop(dspBranch, evalCond(builder, cc), builder.imm(target), builder.imm(builder.regs.pc + 1))
    builder.regs.branch = true

proc jmpr*(builder; r, cc: uint16) =
    when interpretBranch:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.jmpr)
    else:
        discard builder.triop(dspBranch, evalCond(builder, cc), builder.readReg(DspReg(r)), builder.imm(builder.regs.pc + 1))
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
    if c > 0:
        markLoopEnd builder.fetchFollowingImm
    else:
        builder.regs.branch = true

proc loop*(builder; r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.loop)
    markLoopEnd builder.fetchFollowingImm
    builder.regs.branch = true

proc repi*(builder; c: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.repi)
    if c > 0:
        markLoopEnd builder.regs.pc + 1
    else:
        builder.regs.branch = true

proc rep*(builder; r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.rep)
    markLoopEnd builder.regs.pc + 1
    builder.regs.branch = true

proc trap*(builder) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.trap)

proc wait*(builder) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.wait)
    builder.regs.branch = true
