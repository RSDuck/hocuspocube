import
    sets,
    ../../util/jit/ir, ../dspstate,
    dspfrontendcommon,
    fallbacks,
    dspblockcache

using builder: var IrBlockBuilder[DspIrState]

const interpretBranch = false

proc evalCond(builder; cond: uint32): IrInstrRef =
    template isLess: IrInstrRef = builder.biop(irInstrCondXor, builder.readStatus(dspStatusBitOv), builder.readStatus(dspStatusBitMi))
    template isEqual: IrInstrRef = builder.readStatus(dspStatusBitZr)
    template isLequal: IrInstrRef = builder.biop(irInstrCondOr, isLess(), isEqual())
    case range[0..15](cond)
    of 0: builder.unop(irInstrCondNot, isLess)  # greater or equal
    of 1: isLess # less
    of 2: builder.unop(irInstrCondNot, isLequal) # greater
    of 3: isLequal # less or equal
    of 4: builder.unop(irInstrCondNot, isEqual) # not equal
    of 5: isEqual # equal
    of 6: builder.unop(irInstrCondNot, builder.readStatus(dspStatusBitCa))
    of 7: builder.readStatus(dspStatusBitCa)
    of 8: builder.unop(irInstrCondNot, builder.readStatus(dspStatusBitExt))
    of 9: builder.readStatus(dspStatusBitExt)
    of 10: builder.biop(irInstrCondAnd, builder.unop(irInstrCondNot, isEqual), builder.biop(irInstrBitOr, builder.readStatus(dspStatusBitExt), builder.readStatus(dspStatusBitUnnorm)))
    of 11: builder.biop(irInstrCondAnd, isEqual, builder.unop(irInstrCondNot, builder.biop(irInstrBitOr, builder.readStatus(dspStatusBitExt), builder.readStatus(dspStatusBitUnnorm))))
    of 12: builder.unop(irInstrCondNot, builder.readStatus(dspStatusBitTb))
    of 13: builder.readStatus(dspStatusBitTb)
    of 14: builder.readStatus(dspStatusBitOv)
    of 15: builder.imm(true)

proc jmp*(builder; cc: uint16) =
    when interpretBranch:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.jmp)
        discard builder.fetchFollowingImm
    else:
        let target = builder.fetchFollowingImm

        discard builder.triop(irInstrBranchDsp, evalCond(builder, cc), builder.imm(target), builder.imm(builder.regs.pc + 1))
    builder.regs.branch = true

proc jmpr*(builder; r, cc: uint16) =
    when interpretBranch:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.jmpr)
    else:
        discard builder.triop(irInstrBranchDsp, evalCond(builder, cc), builder.readReg(DspReg(r)), builder.imm(builder.regs.pc + 1))
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
        loopEnds.incl builder.fetchFollowingImm
    else:
        builder.regs.branch = true

proc loop*(builder; r: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.loop)
    loopEnds.incl builder.fetchFollowingImm
    builder.regs.branch = true

proc repi*(builder; c: uint16) =
    builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.repi)
    if c > 0:
        loopEnds.incl builder.regs.pc + 1
    else:
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
