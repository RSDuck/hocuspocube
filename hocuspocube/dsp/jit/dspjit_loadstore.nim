import
    ../../util/jit/ir,
    ../dspstate,
    dspfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[DspIrState]

const interpretLoadStore = false

proc pld*(builder; d, m, r: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.pld)
    else:
        let adr = builder.readAdr(r)
        builder.loadAccum(d, builder.unop(dspLoadIMem, adr))
        builder.loadStoreAdrInc(adr, m, r)

proc ld*(builder; m, r, d: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ld)
    else:
        let adr = builder.readAdr(r)
        builder.writeReg(DspReg d, builder.unop(dspLoadDMem, adr))
        builder.loadStoreAdrInc(adr, m, r)

proc st*(builder; m, r, s: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.st)
    else:
        let adr = builder.readAdr(r)
        discard builder.biop(dspStoreDMem, adr, builder.readReg(DspReg s))
        builder.loadStoreAdrInc(adr, m, r)

proc ldsa*(builder; d, a: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ldsa)
    else:
        let
            adr = builder.dppAdr(a)
            val = builder.unop(dspLoadDMem, adr)
        if d < 6:
            builder.writeReg(x0.succ(int d), val)
        else:
            builder.loadAccum(d - 6, val)

proc stsa*(builder; s, a: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.stsa)
    else:
        let val = case range[0..7](s)
            of 0..1: builder.readReg(a2.succ(int s))
            of 2..3: builder.imm(0)
            of 4..5: builder.readReg(a0.succ(int s - 4))
            of 6..7: builder.storeAccum(s - 6)
        discard builder.biop(dspStoreDMem, builder.dppAdr(a), val)

proc ldla*(builder; d: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.ldla)
        discard builder.fetchFollowingImm()
    else:
        builder.writeReg(DspReg d, builder.unop(dspLoadDMem, builder.imm(builder.fetchFollowingImm())))

proc stla*(builder; s: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.stla)
        discard builder.fetchFollowingImm()
    else:
        discard builder.biop(dspStoreDMem, builder.imm(builder.fetchFollowingImm()), builder.readReg(DspReg s))

proc stli*(builder; a: uint16) =
    when interpretLoadStore:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks.stli)
        discard builder.fetchFollowingImm()
    else:
        discard builder.biop(dspStoreDMem, builder.imm(a or 0xFF00'u32), builder.imm(builder.fetchFollowingImm()))
