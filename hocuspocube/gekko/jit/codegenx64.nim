import
    options, streams, std/setutils, strformat,
    catnip/[x64assembler, reprotect],
    ../../util/setminmax,
    ../ppcstate, ../memory, ../ppccommon,
    ppcfrontendcommon,
    ir

const
    registersToUse = [regEdi, regEsi, regEbx, regR12d, regR13d, regR14d, regR15d]

    maxSpill = 32

    stackFrameSize = stackShadow + maxSpill*8

    rcpu = regRbp

type
    HostRegSet = set[0..registersToUse.len-1]
    HostRegRange = range[0..registersToUse.len-1]

    RegLoc = enum
        regLocHostGprReg
        regLocHostGprRegImm
        regLocHostSpill

    ActiveReg = object
        val: IrInstrRef
        location: RegLoc
        idx: int32

    RegAlloc = object
        activeRegs: seq[ActiveReg]
        freeRegs: HostRegSet
        freeSpillLocs: set[0..maxSpill-1]

proc toReg(num: int32): Register32 =
    registersToUse[num]

proc getSpillLoc(num: int32): Rm32 =
    mem32(regRsp, stackShadow + num*4)

proc allocHostReg(regalloc: var RegAlloc, s: var AssemblerX64, lockedRegs: HostRegSet): int32 =
    if regalloc.freeRegs == {}:
        # spill a register
        assert regalloc.freeSpillLocs != {}, "no spill locations left"

        # first try to spill an immediate as those are faster to restore
        for i in 0..<regalloc.activeRegs.len:
            if regalloc.activeRegs[i].location == regLocHostGprRegImm and
                regalloc.activeRegs[i].idx notin lockedRegs:

                let reg = regalloc.activeRegs[i].idx
                regalloc.activeRegs.del i
                return reg
        for i in 0..<regalloc.activeRegs.len:
            if regalloc.activeRegs[i].location == regLocHostGprReg and
                regalloc.activeRegs[i].idx notin lockedRegs:

                let reg = regalloc.activeRegs[i].idx
                regalloc.activeRegs[i].location = regLocHostSpill
                regalloc.activeRegs[i].idx = regalloc.freeSpillLocs.popMin()
                s.mov(getSpillLoc(regalloc.activeRegs[i].idx), reg.toReg())
                return reg

        raiseAssert(&"too many register for one operation at once! {regalloc} {lockedRegs}")

    regalloc.freeRegs.popMin()

proc prepareHostRead(regalloc: var RegAlloc, s: var AssemblerX64,
    iref: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: HostRegSet = {}): int32 =

    for i, reg in mpairs regalloc.activeRegs:
        if reg.val == iref:
            if reg.location in {regLocHostGprReg, regLocHostGprRegImm}:
                return int32(i)
            else:
                let targetReg = allocHostReg(regalloc, s, lockedRegs)
                s.mov(targetReg.toReg, getSpillLoc(reg.idx))
                reg.idx = targetReg
                return int32(i)

    if (let instr = blk.getInstr(iref); instr.kind in LoadImmInstrs):
        let targetReg = allocHostReg(regalloc, s, lockedRegs)

        if instr.kind == irInstrLoadImmB:
            s.mov(reg(targetReg.toReg()), int32(instr.immValB))
        else:
            s.mov(reg(targetReg.toReg()), cast[int32](instr.immValI))

        regalloc.activeRegs.add(ActiveReg(location: regLocHostGprRegImm, val: iref, idx: targetReg))
        return int32(regalloc.activeRegs.len-1)

    raiseAssert(&"register was never instantiated {regalloc.activeRegs}")

proc allocOpW0R1(regalloc: var RegAlloc, s: var AssemblerX64,
    src: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: HostRegSet = {}): int32 =
    regalloc.activeRegs[regalloc.prepareHostRead(s, src, blk, lockedRegs)].idx

proc allocOpW0R2(regalloc: var RegAlloc, s: var AssemblerX64,
    src0, src1: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: HostRegSet = {}): (int32, int32) =

    result[0] = regalloc.activeRegs[regalloc.prepareHostRead(s, src0, blk, lockedRegs)].idx
    result[1] = regalloc.activeRegs[regalloc.prepareHostRead(s, src1, blk, lockedRegs + {HostRegRange(result[0])})].idx

proc allocOpW1R0(regalloc: var RegAlloc, s: var AssemblerX64,
    dst: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: HostRegSet = {}): int32 =
    let dstReg = allocHostReg(regalloc, s, lockedRegs)
    regalloc.activeRegs.add(ActiveReg(location: regLocHostGprReg, val: dst, idx: dstReg))
    dstReg

proc allocOpW1R1(regalloc: var RegAlloc, s: var AssemblerX64,
    dst, src: IrInstrRef,
    instrIdx: int32,
    blk: IrBasicBlock): (int32, int32) =

    let srcLoc = prepareHostRead(regalloc, s, src, blk, {})
    if blk.getInstr(src).lastRead == instrIdx:
        # recycle register
        regalloc.activeRegs[srcLoc].val = dst
        (regalloc.activeRegs[srcLoc].idx, regalloc.activeRegs[srcLoc].idx)
    else:
        (allocOpW1R0(regalloc, s, dst, blk, {HostRegRange(regalloc.activeRegs[srcLoc].idx)}), regalloc.activeRegs[srcLoc].idx)

proc allocOpW1R2(regalloc: var RegAlloc, s: var AssemblerX64,
    dst, src0, src1: IrInstrRef,
    instrIdx: int32,
    blk: IrBasicBlock,
    commutative = false): (int32, int32, int32) =

    let
        src0Loc = prepareHostRead(regalloc, s, src0, blk, {})
        src1Loc = prepareHostRead(regalloc, s, src1, blk, {HostRegRange(regalloc.activeRegs[src0Loc].idx)})
    if blk.getInstr(src0).lastRead == instrIdx:
        # recycle register
        regalloc.activeRegs[src0Loc].val = dst
        (regalloc.activeRegs[src0Loc].idx, regalloc.activeRegs[src0Loc].idx, regalloc.activeRegs[src1Loc].idx)
    elif commutative and blk.getInstr(src1).lastRead == instrIdx:
        regalloc.activeRegs[src1Loc].val = dst
        (regalloc.activeRegs[src1Loc].idx, regalloc.activeRegs[src1Loc].idx, regalloc.activeRegs[src0Loc].idx)
    else:
        (allocOpW1R0(regalloc, s, dst, blk, {HostRegRange(regalloc.activeRegs[src0Loc].idx), HostRegRange(regalloc.activeRegs[src0Loc].idx)}),
            regalloc.activeRegs[src0Loc].idx, regalloc.activeRegs[src1Loc].idx)

proc allocOpW1R3(regalloc: var RegAlloc, s: var AssemblerX64,
    dst, src0, src1, src2: IrInstrRef,
    instrIdx: int32,
    blk: IrBasicBlock,
    commutative = false): (int32, int32, int32, int32) =
    (result[0], result[1], result[2]) = regalloc.allocOpW1R2(s, dst, src0, src1, instrIdx, blk, commutative)
    result[3] = prepareHostRead(regalloc, s, src2, blk, {HostRegRange(result[0]), HostRegRange(result[1]), HostRegRange(result[2])})


proc freeExpiredRegs(regalloc: var RegAlloc, blk: IrBasicBlock, pos: int) =
    var i = 0
    while i < regalloc.activeRegs.len:
        if blk.getInstr(regalloc.activeRegs[i].val).lastRead == pos:
            regalloc.freeRegs.incl regalloc.activeRegs[i].idx
            regalloc.activeRegs.del i
        else:
            i += 1

var
    codeMemory {.align(0x1000).}: array[32*1024*1024, byte]
    assembler = initAssemblerX64(cast[ptr UncheckedArray[byte]](addr codeMemory[0]))

doAssert reprotectMemory(addr codeMemory[0], sizeof(codeMemory), {memperm_R, memperm_W, memperm_X})

proc getSprOffset(num: uint32): int32 =
    int32(case IrSprNum(num)
    of irSprNumCr: offsetof(PpcState, cr)
    of irSprNumXer: offsetof(PpcState, xer)
    of irSprNumLr: offsetof(PpcState, lr)
    of irSprNumCtr: offsetof(PpcState, ctr)
    of irSprNumMsr: offsetof(PpcState, msr)
    of irSprNumDsisr: offsetof(PpcState, dsisr)
    of irSprNumDar: offsetof(PpcState, dar)
    of irSprNumSrr0: offsetof(PpcState, srr0)
    of irSprNumSrr1: offsetof(PpcState, srr1)
    of irSprNumHid0: offsetof(PpcState, hid0)
    of irSprNumHid1: offsetof(PpcState, hid1)
    of irSprNumHid2: offsetof(PpcState, hid2)
    of irSprNumL2cr: offsetof(PpcState, l2cr)
    of irSprNumMmcr0: offsetof(PpcState, mmcr0)
    of irSprNumMmcr1: offsetof(PpcState, mmcr1)
    of irSprNumSprg0..irSprNumSprg3: offsetof(PpcState, sprg)+(int(num)-irSprNumSprg0.ord)*4
    of irSprNumPmc0..irSprNumPmc3: offsetof(PpcState, pmc)+(int(num)-irSprNumPmc0.ord)*4
    of irSprNumIBatL0..irSprNumIBatL3: offsetof(PpcState, ibatLo)+(int(num)-irSprNumIBatL0.ord)*4
    of irSprNumIBatU0..irSprNumIBatU3: offsetof(PpcState, ibatHi)+(int(num)-irSprNumIBatU0.ord)*4
    of irSprNumDBatL0..irSprNumDBatL3: offsetof(PpcState, dbatLo)+(int(num)-irSprNumDBatL0.ord)*4
    of irSprNumDBatU0..irSprNumDBatU3: offsetof(PpcState, dbatHi)+(int(num)-irSprNumDBatU0.ord)*4
    of irSprNumGqr0..irSprNumGqr7: offsetof(PpcState, gqr)+(int(num)-irSprNumGqr0.ord)*4
    else: raiseAssert("shouldn't be handled here"))

proc getRegOffset(num: uint32): int32 =
    int32(offsetof(PpcState, r)) + 4'i32*int32(num)

proc initRegAlloc(): RegAlloc =
    RegAlloc(freeRegs: fullSet(range[0..registersToUse.len-1]), freeSpillLocs: fullSet(range[0..maxSpill-1]))

proc printVal(val: uint32) {.cdecl.} =
    echo &"val: {val:08X}"

proc systemCall(state: var PpcState) =
    state.pendingExceptions.incl exceptionSystemCall

var printcmp* = false

proc genCode*(blk: IrBasicBlock, cycles: int32): BlockEntryFunc =
    template s: untyped = assembler

    var regalloc = initRegAlloc()

    result = s.getFuncStart[:BlockEntryFunc]()

    s.push(reg(regRdi))
    s.push(reg(regRsi))
    s.push(reg(regRbx))
    s.push(reg(regR12))
    s.push(reg(regR13))
    s.push(reg(regR14))
    s.push(reg(regR15))
    s.push(reg(regRbp))
    s.sub(reg(regRsp), 8 + stackFrameSize)

    s.mov(reg(regRbp), param1)

    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)

        case instr.kind
        of irInstrLoadImmI:
            discard
        of irInstrLoadImmB:
            discard
        of irInstrLoadReg:
            let
                offset = getRegOffset(instr.ctxLoadIdx)
                dst = regalloc.allocOpW1R0(s, iref, blk)
            s.mov(dst.toReg, mem32(rcpu, int32(offset)))
        of irInstrStoreReg:
            let offset = getRegOffset(instr.ctxStoreIdx)
            if (let imm = blk.isImmValI(instr.source(0)); imm.isSome()):
                s.mov(mem32(rcpu, int32(offset)), cast[int32](imm.get))
            else:
                let src = regalloc.allocOpW0R1(s, instr.ctxStoreSrc, blk)
                s.mov(mem32(rcpu, int32(offset)), src.toReg)
        of irInstrLoadCrBit:
            let dst = regalloc.allocOpW1R0(s, iref, blk)
            s.test(mem32(rcpu, int32 offsetof(PpcState, cr)), cast[int32](1'u32 shl (31'u32-instr.ctxLoadIdx)))
            s.setcc(reg(Register8(dst.toReg.ord)), condNotZero)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrStoreCrBit:
            let src = regalloc.allocOpW0R1(s, instr.ctxStoreSrc, blk)
            s.mov(regEax, mem32(rcpu, int32 offsetof(PpcState, cr)))
            s.aand(reg(regEax), cast[int32](not(1'u32 shl (31'u32-instr.ctxStoreIdx))))
            s.mov(reg(regEcx), src.toReg)
            s.sshl(reg(regEcx), int8(31'u32-instr.ctxStoreIdx))
            s.oor(reg(regEax), regEcx)
            s.mov(mem32(rcpu, int32 offsetof(PpcState, cr)), regEax)
        of irInstrLoadXer:
            let
                dst = regalloc.allocOpW1R0(s, iref, blk)
                bitidx = case IrXerNum(instr.ctxLoadIdx)
                    of irXerNumCa: 29
                    of irXerNumOv: 30
                    of irXerNumSo: 31
            s.test(mem32(rcpu, int32 offsetof(PpcState, xer)), cast[int32](1'u32 shl bitidx))
            s.setcc(reg(Register8(dst.toReg.ord)), condNotZero)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrStoreXer:
            let
                src = regalloc.allocOpW0R1(s, instr.ctxStoreSrc, blk)
                bitidx = case IrXerNum(instr.ctxStoreIdx)
                    of irXerNumCa: 29
                    of irXerNumOv: 30
                    of irXerNumSo: 31
            s.mov(regEax, mem32(rcpu, int32 offsetof(PpcState, xer)))
            s.aand(reg(regEax), cast[int32](not(1'u32 shl bitidx)))
            s.mov(reg(regEcx), src.toReg)
            s.sshl(reg(regEcx), int8 bitidx)
            s.oor(reg(regEax), regEcx)
            s.mov(mem32(rcpu, int32 offsetof(PpcState, xer)), regEax)
        of irInstrLoadSpr:
            let dst = regalloc.allocOpW1R0(s, iref, blk)
            if IrSprNum(instr.ctxLoadIdx) in {irSprNumTbL, irSprNumTbU}:
                s.mov(reg(param1), rcpu)
                s.call(currentTb)
                if IrSprNum(instr.ctxLoadIdx) == irSprNumTbU:
                    s.sshr(reg(regRax), 32)
                s.mov(reg(dst.toReg), regEax)
            elif IrSprNum(instr.ctxLoadIdx) == irSprNumDec:
                s.mov(reg(param1), rcpu)
                s.call(getDecrementer)
                s.mov(reg(dst.toReg), regEax)
            else:
                let offset = getSprOffset(instr.ctxLoadIdx)
                s.mov(dst.toReg, mem32(rcpu, int32(offset)))
        of irInstrStoreSpr:
            if IrSprNum(instr.ctxStoreIdx) in {irSprNumTbL, irSprNumTbU}:
                raiseAssert("to be implemented")
            elif IrSprNum(instr.ctxStoreIdx) == irSprNumDec:
                s.mov(reg(param1), rcpu)
                s.mov(reg(Register32(param2.ord)), regalloc.allocOpW0R1(s, instr.ctxStoreSrc, blk).toReg)
                s.call(setupDecrementer)
            else:
                let offset = getSprOffset(instr.ctxStoreIdx)
                if (let imm = blk.isImmValI(instr.source(0)); imm.isSome()):
                    s.mov(mem32(rcpu, int32(offset)), cast[int32](imm.get))
                else:
                    let src = regalloc.allocOpW0R1(s, instr.ctxStoreSrc, blk)
                    s.mov(mem32(rcpu, int32(offset)), src.toReg)
        of irInstrCsel:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), int32(i), blk)
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.test(reg(src2.toReg), src2.toReg)
            s.cmov(dst.toReg, reg(src1.toReg), condZero)
        of irInstrIAdd, irInstrBitAnd, irInstrBitOr, irInstrBitXor, irInstrMul:
            if (let imm = blk.isEitherImmI(instr.source(0), instr.source(1)); imm.isSome):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, imm.get[0], int32(i), blk)
                if dst != src and instr.kind != irInstrMul: s.mov(reg(dst.toReg), src.toReg)
                case instr.kind
                of irInstrIAdd: s.add(reg(dst.toReg), cast[int32](imm.get[1]))
                of irInstrBitAnd: s.aand(reg(dst.toReg), cast[int32](imm.get[1]))
                of irInstrBitOr: s.oor(reg(dst.toReg), cast[int32](imm.get[1]))
                of irInstrBitXor: s.xxor(reg(dst.toReg), cast[int32](imm.get[1]))
                of irInstrMul: s.imul(dst.toReg, reg(src.toReg), cast[int32](imm.get[1]))
                else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk, true)
                if dst == src1:
                    case instr.kind
                    of irInstrIAdd: s.add(reg(dst.toReg), src0.toReg)
                    of irInstrBitAnd: s.aand(reg(dst.toReg), src0.toReg)
                    of irInstrBitOr: s.oor(reg(dst.toReg), src0.toReg)
                    of irInstrBitXor: s.xxor(reg(dst.toReg), src0.toReg)
                    of irInstrMul: s.imul(dst.toReg, reg(src0.toReg))
                    else: raiseAssert("shouldn't happen")
                else:
                    if dst != src0: s.mov(reg(dst.toReg), src0.toReg)

                    case instr.kind
                    of irInstrIAdd: s.add(reg(dst.toReg), src1.toReg)
                    of irInstrBitAnd: s.aand(reg(dst.toReg), src1.toReg)
                    of irInstrBitOr: s.oor(reg(dst.toReg), src1.toReg)
                    of irInstrBitXor: s.xxor(reg(dst.toReg), src1.toReg)
                    of irInstrMul: s.imul(dst.toReg, reg(src1.toReg))
                    else: raiseAssert("shouldn't happen")
        of irInstrISub:
            if (let imm = blk.isImmValI(instr.source(1)); imm.isSome):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                s.sub(reg(dst.toReg), cast[int32](imm.get))
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk)
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.sub(reg(dst.toReg), src1.toReg)
        of irInstrIAddExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), int32(i), blk)
            s.bt(reg(src2.toReg), 0)
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.adc(reg(dst.toReg), src1.toReg)
        of irInstrISubExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), int32(i), blk)
            s.bt(reg(src2.toReg), 0)
            s.cmc()
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.sbb(reg(dst.toReg), src1.toReg)
        of irInstrMulhS, irInstrMulhU, irInstrDivS, irInstrDivU:
            let
                isDivide = instr.kind in {irInstrDivS, irInstrDivU}
                (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk)
            if isDivide: s.xxor(reg(regEdx), regEdx)
            s.mov(reg(regEax), src0.toReg)
            case instr.kind
            of irInstrMulhS: s.imul(reg(src1.toReg))
            of irInstrMulhU: s.mul(reg(src1.toReg))
            of irInstrDivS: s.idiv(reg(src1.toReg))
            of irInstrDivU: s.ddiv(reg(src1.toReg))
            else: raiseAssert("shouldn't happen")
            s.mov(reg(dst.toReg), if isDivide: regEax else: regEdx)
        of irInstrBitNot:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.nnot(reg(dst.toReg))
        of irInstrRol, irInstrShl, irInstrShrLogic, irInstrShrArith:
            if (let immShift = blk.isImmValI(instr.source(1)); immShift.isSome() and immShift.get <= 31):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                case instr.kind
                of irInstrShl: s.sshl(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of irInstrShrLogic: s.sshr(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of irInstrShrArith: s.sar(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of irInstrRol: s.rol(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk)
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.mov(reg(regEcx), src1.toReg)
                case instr.kind
                of irInstrShl: s.sshl(reg(Register64(dst.toReg.ord)))
                of irInstrShrLogic: s.sshr(reg(Register64(dst.toReg.ord)))
                of irInstrShrArith:
                    s.movsxd(Register64(dst.toReg.ord), reg(dst.toReg))
                    s.sar(reg(Register64(dst.toReg.ord)))
                of irInstrRol: s.rol(reg(dst.toReg))
                else: raiseAssert("shouldn't happen")
        of irInstrClz:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
            s.bsr(regEax, reg(src.toReg))
            s.mov(reg(dst.toReg), 32 xor 0x1F)
            s.cmov(dst.toReg, reg(regEax), condNotZero)
            s.xxor(reg(dst.toReg), 0x1F)
        of irInstrExtsb, irInstrExtsh:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
            case instr.kind
            of irInstrExtsb: s.movsx(dst.toReg, reg(Register8(src.toReg.ord)))
            of irInstrExtsh: s.movsx(dst.toReg, reg(Register16(src.toReg.ord)))
            else: raiseAssert("shouldn't happen")
        of irInstrCondAnd, irInstrCondOr, irInstrCondXor:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk, true)
            if dst == src1:
                case instr.kind
                of irInstrCondAnd: s.aand(reg(dst.toReg), src0.toReg)
                of irInstrCondOr: s.oor(reg(dst.toReg), src0.toReg)
                of irInstrCondXor: s.xxor(reg(dst.toReg), src0.toReg)
                else: raiseAssert("welp")
            else:
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                case instr.kind
                of irInstrCondAnd: s.aand(reg(dst.toReg), src1.toReg)
                of irInstrCondOr: s.oor(reg(dst.toReg), src1.toReg)
                of irInstrCondXor: s.xxor(reg(dst.toReg), src1.toReg)
                else: raiseAssert("welp")
        of irInstrCondNot:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.xxor(reg(dst.toReg), 1)
        of irInstrOverflowAdd:
            raiseAssert("unimplemented code gen")
        of irInstrOverflowSub:
            raiseAssert("unimplemented code gen")
        of irInstrCarryAdd:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk)
            s.mov(reg(regEax), src0.toReg)
            s.add(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrCarrySub:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), int32(i), blk)
            s.cmp(reg(src0.toReg), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrOverflowAddExtended:
            raiseAssert("unimplemented code gen")
        of irInstrOverflowSubExtended:
            raiseAssert("unimplemented code gen")
        of irInstrCarryAddExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), int32(i), blk)
            s.bt(reg(src2.toReg), 0)
            s.mov(reg(regEax), src0.toReg)
            s.adc(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrCarrySubExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), int32(i), blk)
            s.bt(reg(src2.toReg), 0)
            s.cmc()
            s.mov(reg(regEax), src0.toReg)
            s.sbb(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrCmpEqualI, irInstrCmpGreaterUI, irInstrCmpLessUI, irInstrCmpGreaterSI, irInstrCmpLessSI:
            let dst = (if (let imm = blk.isImmValI(instr.source(1)); imm.isSome):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
                    s.cmp(reg(src.toReg), cast[int32](imm.get))
                    dst
                else:
                    let (dst, src0, src1) =
                        regalloc.allocOpW1R2(assembler, iref, instr.source(0), instr.source(1), int32(i), blk)
                    s.cmp(reg(src0.toReg), src1.toReg)
                    dst)
            s.setcc(reg(Register8(dst.toReg.ord)),
                case instr.kind
                of irInstrCmpEqualI: condZero
                of irInstrCmpGreaterUI: condNbequal
                of irInstrCmpLessUI: condBelow
                of irInstrCmpGreaterSI: condNotLequal
                of irInstrCmpLessSI: condLess
                else: raiseAssert("welp"))
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrLoadU8, irInstrLoadU16, irInstrLoadS16, irInstrLoad32:
            let (dst, adr) = regalloc.allocOpW1R1(s, iref, instr.source(0), int32(i), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of irInstrLoad32:
                s.call(jitReadMemory[uint32])
                s.mov(reg(dst.toReg), regEax)
            of irInstrLoadU16:
                s.call(jitReadMemory[uint16])
                s.movzx(dst.toReg, reg(regAx))
            of irInstrLoadS16:
                s.call(jitReadMemory[uint16])
                s.movsx(dst.toReg, reg(regAx))
            of irInstrLoadU8:
                s.call(jitReadMemory[uint8])
                s.movzx(dst.toReg, reg(regAl))
            else:
                raiseAssert("welp")
        of irInstrStore8, irInstrStore16, irInstrStore32:
            let (adr, val) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of irInstrStore32:
                s.mov(reg(Register32(param3.ord)), val.toReg)
                s.call(jitWriteMemory[uint32])
            of irInstrStore16:
                s.movzx(Register32(param3.ord), reg(Register16(val.toReg.ord)))
                s.call(jitWriteMemory[uint16])
            of irInstrStore8:
                s.movzx(Register32(param3.ord), reg(Register8(val.toReg.ord)))
                s.call(jitWriteMemory[uint8])
            else: raiseAssert("welp")
        of irInstrBranch:
            #assert blk.isImmVal(instr.source(0), false), "should have been lowered before"
            if blk.isImmVal(instr.source(0), true):
                if (let immTarget = blk.isImmValI(instr.source(1)); immTarget.isSome()):
                    s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), cast[int32](immTarget.get()))
                else:
                    let target = regalloc.allocOpW0R1(s, instr.source(1), blk)
                    s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), target.toReg)
            else:
                let (cond, target) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
                s.mov(reg(regEax), cast[int32](blk.isImmValI(instr.source(2)).get))
                s.mov(reg(regEcx), target.toReg)
                s.test(reg(cond.toReg), cond.toReg)
                s.cmov(regEax, reg(regEcx), condNotZero)

                s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), regEax)
        of irInstrSyscall:
            s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), cast[int32](blk.isImmValI(instr.source(0)).get))
            s.mov(reg(param1), rcpu)
            s.call(systemCall)

        regalloc.freeExpiredRegs(blk, i)

        assert regalloc.freeRegs.len == registersToUse.len-regalloc.activeRegs.len, &"{regalloc}"

    s.mov(reg(regEax), cycles)

    s.add(reg(regRsp), 8 + stackFrameSize)
    s.pop(reg(regRbp))
    s.pop(reg(regR15))
    s.pop(reg(regR14))
    s.pop(reg(regR13))
    s.pop(reg(regR12))
    s.pop(reg(regRbx))
    s.pop(reg(regRsi))
    s.pop(reg(regRdi))
    s.ret()

    assert s.offset < sizeof(codeMemory)

    let resultFile = newFileStream("block.bin", fmWrite)
    resultFile.writeData(result, cast[ByteAddress](s.curAdr) - cast[ByteAddress](result))
    resultFile.close()