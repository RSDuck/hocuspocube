import
    options, streams, std/setutils, strformat,
    catnip/[x64assembler, reprotect],
    ../../util/setminmax,
    ../../gekko/[ppcstate, ppccommon, memory, jit/gekkoblockcache],
    ../../dsp/[dspstate, jit/dspblockcache],
    ir

when defined(windows):
    const
        registersToUse = [regEdi, regEsi, regEbx, regR12d, regR13d, regR14d, regR15d]
        xmmsToUse = [regXmm6, regXmm7, regXmm8, regXmm9, regXmm10, regXmm11, regXmm12, regXmm13, regXmm14, regXmm15]
else:
    const
        registersToUse = [regEbx, regR12d, regR13d, regR14d, regR15d]

const
    maxSpill = 32

    stackFrameSize = stackShadow + maxSpill*16

    rcpu = regRbp

#[
    stack layout:
        >------------------- rsp
        | shadow (0x20)
        | spill area (16*maxSpill)
        | callee saved xmms (optional)
        | dummy for alignment
        | pushed integer registers
        | return address
]#

type
    HostIRegRange = range[0..registersToUse.len-1]
    HostFRegRange = range[0..xmmsToUse.len-1]

    RegLoc = enum
        regLocHostGprReg
        regLocHostGprRegImm
        regLocHostSpill

    ActiveReg = object
        val: IrInstrRef
        location: RegLoc
        idx: int32

    RegAlloc[T] = object
        activeRegs: seq[ActiveReg]
        freeRegs: set[T]
        freeSpillLocs: ptr set[0..maxSpill-1]

    FlagState = enum
        flagStateUnknown
        flagStateCmpZero
        flagStateCmpVal

proc toReg(num: int32): Register32 =
    registersToUse[num]

proc toXmm(num: int32): RegisterXmm =
    xmmsToUse[num]

proc getSpillLoc(num: int32): Rm32 =
    mem32(regRsp, stackShadow + num*16)
proc getSpillLocXmm(num: int32): RmXmm =
    memXmm(regRsp, stackShadow + num*16)

proc allocHostReg[T](regalloc: var RegAlloc[T], s: var AssemblerX64, lockedRegs: set[T]): int32 =
    if regalloc.freeRegs == {}:
        # spill a register
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

                assert regalloc.freeSpillLocs[] != {}, &"no spill locations left {regalloc}"

                let reg = regalloc.activeRegs[i].idx
                regalloc.activeRegs[i].location = regLocHostSpill
                regalloc.activeRegs[i].idx = regalloc.freeSpillLocs[].popMin()
                when T is HostIRegRange:
                    s.mov(getSpillLoc(regalloc.activeRegs[i].idx), reg.toReg())
                else:
                    s.movapd(getSpillLocXmm(regalloc.activeRegs[i].idx), reg.toXmm())
                return reg

        raiseAssert(&"too many register for one operation at once! {regalloc} {lockedRegs}")

    regalloc.freeRegs.popMin()

proc prepareHostRead[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    iref: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: set[T] = {}): int =

    for i, reg in mpairs regalloc.activeRegs:
        if reg.val == iref:
            if reg.location in {regLocHostGprReg, regLocHostGprRegImm}:
                return i
            else:
                let targetReg = allocHostReg(regalloc, s, lockedRegs)
                when T is HostIRegRange:
                    s.mov(targetReg.toReg, getSpillLoc(reg.idx))
                else:
                    s.movapd(targetReg.toXmm, getSpillLocXmm(reg.idx))
                regalloc.freeSpillLocs[].incl reg.idx
                reg.location = regLocHostGprReg
                reg.idx = targetReg
                return i

    if (let instr = blk.getInstr(iref); instr.kind in LoadImmInstrs):
        assert T isnot HostFRegRange

        let targetReg = allocHostReg(regalloc, s, lockedRegs)

        if instr.kind == irInstrLoadImmB:
            s.mov(reg(targetReg.toReg()), int32(instr.immValB))
        else:
            s.mov(reg(targetReg.toReg()), cast[int32](instr.immValI))

        regalloc.activeRegs.add(ActiveReg(location: regLocHostGprRegImm, val: iref, idx: targetReg))
        return regalloc.activeRegs.len-1

    raiseAssert(&"register was never instantiated {iref} {regalloc.activeRegs}")

proc allocOpW0R1[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    src: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: set[T] = {}): int32 =
    regalloc.activeRegs[regalloc.prepareHostRead(s, src, blk, lockedRegs)].idx

proc allocOpW0R2[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    src0, src1: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: set[T] = {}): (T, T) =

    result[0] = allocOpW0R1(regalloc, s, src0, blk, lockedRegs)
    result[1] = allocOpW0R1(regalloc, s, src1, blk, lockedRegs + {result[0]})

proc allocOpW1R0[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst: IrInstrRef,
    blk: IrBasicBlock,
    lockedRegs: set[T] = {}): T =
    let dstReg = allocHostReg(regalloc, s, lockedRegs)
    regalloc.activeRegs.add(ActiveReg(location: regLocHostGprReg, val: dst, idx: dstReg))
    dstReg

proc allocOpW1R1[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst, src: IrInstrRef,
    instrIdx: int,
    blk: IrBasicBlock): (T, T) =

    if blk.getInstr(src).lastRead == instrIdx:
        let srcLoc = prepareHostRead(regalloc, s, src, blk, {})
        # recycle register
        regalloc.activeRegs[srcLoc].location = regLocHostGprReg
        regalloc.activeRegs[srcLoc].val = dst
        result[0] = T(regalloc.activeRegs[srcLoc].idx)
        result[1] = result[0]
    else:
        result[0] = allocOpW1R0(regalloc, s, dst, blk)
        result[1] = allocOpW0R1(regalloc, s, src, blk, {result[0]})

proc allocOpW1R2[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst, src0, src1: IrInstrRef,
    instrIdx: int,
    blk: IrBasicBlock,
    commutative = false,
    lockedRegs: set[T] = {}): (T, T, T) =

    if blk.getInstr(src0).lastRead == instrIdx:
        # recycle register
        let src0Loc = prepareHostRead(regalloc, s, src0, blk, lockedRegs)
        regalloc.activeRegs[src0Loc].location = regLocHostGprReg
        regalloc.activeRegs[src0Loc].val = dst
        result[0] = T(regalloc.activeRegs[src0Loc].idx)
        result[1] = result[0]
        if src0 != src1:
            result[2] = regalloc.allocOpW0R1(s, src1, blk, lockedRegs + {result[0]})
        else:
            result[2] = result[1]
    else:
        result[0] = allocOpW1R0(regalloc, s, dst, blk, lockedRegs)
        result[1] = allocOpW0R1(regalloc, s, src0, blk, lockedRegs + {result[0]})
        result[2] = allocOpW0R1(regalloc, s, src1, blk, lockedRegs + {result[0], result[1]})

proc allocOpW1R3[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst, src0, src1, src2: IrInstrRef,
    instrIdx: int,
    blk: IrBasicBlock,
    commutative = false): (T, T, T, T) =
    result[3] = allocOpW0R1(regalloc, s, src2, blk)
    (result[0], result[1], result[2]) = regalloc.allocOpW1R2(s, dst, src0, src1, instrIdx, blk, commutative, {result[3]})

proc freeExpiredRegs[T](regalloc: var RegAlloc[T], blk: IrBasicBlock, pos: int) =
    var i = 0
    while i < regalloc.activeRegs.len:
        if blk.getInstr(regalloc.activeRegs[i].val).lastRead == pos:
            if regalloc.activeRegs[i].location in {regLocHostGprReg, regLocHostGprRegImm}:
                regalloc.freeRegs.incl regalloc.activeRegs[i].idx
            else:
                regalloc.freeSpillLocs[].incl regalloc.activeRegs[i].idx
            regalloc.activeRegs.del i
        else:
            i += 1

var
    codeMemory* {.align(0x1000).}: array[32*1024*1024, byte]
    assembler = initAssemblerX64(cast[ptr UncheckedArray[byte]](addr codeMemory[0]))

doAssert reprotectMemory(addr codeMemory[0], sizeof(codeMemory), {memperm_R, memperm_W, memperm_X})

proc getSprOffset(num: IrSprNum): int32 =
    int32(case num
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
    of irSprNumWpar: offsetof(PpcState, wpar)
    of irSprNumDmaU: offsetof(PpcState, dmaU)
    of irSprNumDmaL: offsetof(PpcState, dmaL)
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

proc initRegAlloc[T](spillLocs: ptr set[0..maxSpill-1]): RegAlloc[T] =
    RegAlloc[T](freeRegs: fullSet(T), freeSpillLocs: spillLocs)

proc printVal(val: uint32) {.cdecl.} =
    echo &"val: {val:08X}"

let
    singlesOne {.align(16).} = [1f, 1f, 1f, 1f]
    doublesOne {.align(16).} = [1.0, 1.0]
    doubleSignMask {.align(16).} = [0x8000000000000000'u64, 0]
    doubleSignMaskInv {.align(16).} = [not 0x8000000000000000'u64, not 0'u64]
    doubleSignMaskPair {.align(16).} = [0x8000000000000000'u64, 0x8000000000000000'u64]
    doubleSignMaskPairInv {.align(16).} = [not 0x8000000000000000'u64, not 0x8000000000000000'u64]
    singleSignMask {.align(16).} = [0x80000000'u32, 0, 0, 0]
    singleSignMaskInv {.align(16).} = [not 0x80000000'u32, not 0'u32, not 0'u32, not 0'u32]
    singleSignMaskPair {.align(16).} = [0x80000000'u32, 0x80000000'u32, 0, 0]
    singleSignMaskPairInv {.align(16).} = [not 0x80000000'u32, not 0x80000000'u32, not 0'u32, not 0'u32]

proc dumpLastFunc*(start: pointer) =
    let file = newFileStream("block.bin", fmWrite)
    file.writeData(start, assembler.getFuncStart[:ByteAddress]() - cast[ByteAddress](start))
    file.close()

proc genCode*(blk: IrBasicBlock, cycles: int32, fexception, idleLoop: bool): pointer =
    template s: untyped = assembler

    if sizeof(codeMemory) - s.offset < 64*1024:
        clearBlockCache()
        s.offset = 0

    var
        freeSpillLocs = fullSet(range[0..maxSpill-1])
        regalloc = initRegAlloc[HostIRegRange](addr freeSpillLocs)
        xmmRegalloc = initRegAlloc[HostFRegRange](addr freeSpillLocs)
        flagstate = flagStateUnknown
        flagstateL, flagstateR: IrInstrRef

        floatExceptionBranch, idleLoopBranch: ForwardsLabel

    template setFlagUnk(): untyped =
        flagstate = flagStateUnknown
    template setFlagCmpZero(val: IrInstrRef): untyped =
        flagstate = flagStateCmpZero
        flagstateL = val
    template setFlagCmp(a, b: IrInstrRef): untyped =
        flagstate = flagStateCmpVal
        flagstateL = a
        flagstateR = b

    result = s.getFuncStart[:pointer]()

    s.push(reg(regRdi))
    s.push(reg(regRsi))
    s.push(reg(regRbx))
    s.push(reg(regR12))
    s.push(reg(regR13))
    s.push(reg(regR14))
    s.push(reg(regR15))
    s.push(reg(regRbp))
    let stackOffset = int32(8 + (if fexception: xmmsToUse.len*16 else: 0) + stackFrameSize)
    s.sub(reg(regRsp), stackOffset)
    if fexception:
        for i in 0..<xmmsToUse.len:
            s.movaps(memXmm(regRsp, int32(stackFrameSize + i*16)), xmmsToUse[i])

    s.mov(reg(regRbp), param1)

    if fexception:
        s.mov(reg(param1), rcpu)
        s.call(handleFException)
        s.test(reg(regEax), regEax)
        floatExceptionBranch = s.jcc(condNotZero, true)

    for i in 0..<blk.instrs.len:
        let
            iref = blk.instrs[i]
            instr = blk.getInstr(iref)

        #echo &"processing instr {i} {regalloc}"

        case instr.kind
        of irInstrIdentity:
            raiseAssert("should have been lowered")
        of irInstrLoadImmI:
            discard
        of irInstrLoadImmB:
            discard
        of irInstrCallInterpreterPpc:
            s.mov(mem32(rcpu, int32(offsetof(PpcState, pc))), cast[int32](instr.pc))
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), cast[int32](instr.instr))
            s.call(instr.target)
            setFlagUnk()
        of irInstrCallInterpreterDsp:
            s.mov(mem16(rcpu, int32(offsetof(DspState, pc))), cast[int16](instr.pc))
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), cast[int32](instr.instr))
            s.call(instr.target)
            setFlagUnk()
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
                let src = regalloc.allocOpW0R1(s, instr.source(0), blk)
                s.mov(mem32(rcpu, int32(offset)), src.toReg)
        of irInstrLoadCrBit:
            let dst = regalloc.allocOpW1R0(s, iref, blk)
            s.test(mem32(rcpu, int32 offsetof(PpcState, cr)), cast[int32](1'u32 shl (31'u32-instr.ctxLoadIdx)))
            s.setcc(reg(Register8(dst.toReg.ord)), condNotZero)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of irInstrStoreCrBit:
            let src = regalloc.allocOpW0R1(s, instr.source(0), blk)
            s.mov(regEax, mem32(rcpu, int32 offsetof(PpcState, cr)))
            s.aand(reg(regEax), cast[int32](not(1'u32 shl (31'u32-instr.ctxStoreIdx))))
            s.mov(reg(regEcx), src.toReg)
            s.sshl(reg(regEcx), int8(31'u32-instr.ctxStoreIdx))
            s.oor(reg(regEax), regEcx)
            s.mov(mem32(rcpu, int32 offsetof(PpcState, cr)), regEax)
            setFlagUnk()
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
            setFlagUnk()
        of irInstrStoreXer:
            let
                src = regalloc.allocOpW0R1(s, instr.source(0), blk)
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
            setFlagUnk()
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
                let offset = getSprOffset(IrSprNum instr.ctxLoadIdx)
                s.mov(dst.toReg, mem32(rcpu, int32(offset)))
            setFlagUnk()
        of irInstrStoreSpr:
            let num = IrSprNum instr.ctxStoreIdx
            if num in {irSprNumTbL, irSprNumTbU, irSprNumDec, irSprNumWpar, irSprNumHid0, irSprNumDmaL}:
                s.mov(reg(param1), rcpu)
                s.mov(reg(Register32(param2.ord)), regalloc.allocOpW0R1(s, instr.source(0), blk).toReg)
                case num
                of irSprNumDec: s.call(setupDecrementer)
                of irSprNumTbL: s.call(setTbl)
                of irSprNumTbU: s.call(setTbu)
                of irSprNumHid0: s.call(setHid0)
                of irSprNumWpar: s.call(setWpar)
                of irSprNumDmaL: s.call(setDmaL)
                else: raiseAssert("welp!")
            else:
                let offset = getSprOffset(num)
                if (let imm = blk.isImmValI(instr.source(0)); imm.isSome()):
                    s.mov(mem32(rcpu, int32(offset)), cast[int32](imm.get))
                else:
                    let src = regalloc.allocOpW0R1(s, instr.source(0), blk)
                    s.mov(mem32(rcpu, int32(offset)), src.toReg)
            setFlagUnk()
        of irInstrCsel:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.test(reg(src2.toReg), src2.toReg)
            s.cmov(dst.toReg, reg(src1.toReg), condZero)
            setFlagUnk()
        of irInstrIAdd, irInstrBitAnd, irInstrBitOr, irInstrBitXor, irInstrMul:
            var
                comparesToZero = instr.kind in {irInstrBitAnd, irInstrBitOr, irInstrBitXor}
                destroysFlags = not comparesToZero
            if (let imm = blk.isEitherImmI(instr.source(0), instr.source(1)); imm.isSome):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, imm.get[0], i, blk)

                if instr.kind == irInstrBitAnd and imm.get[1] == 0xFF'u32:
                    s.movzx(dst.toReg, reg(Register8(src.toReg.ord)))
                    comparesToZero = false
                    destroysFlags = false
                elif instr.kind == irInstrBitAnd and imm.get[1] == 0xFFFF'u32:
                    s.movzx(dst.toReg, reg(Register16(src.toReg.ord)))
                    comparesToZero = false
                    destroysFlags = false
                else:
                    if dst != src and instr.kind != irInstrMul: s.mov(reg(dst.toReg), src.toReg)
                    case instr.kind
                    of irInstrIAdd: s.add(reg(dst.toReg), cast[int32](imm.get[1]))
                    of irInstrBitAnd: s.aand(reg(dst.toReg), cast[int32](imm.get[1]))
                    of irInstrBitOr: s.oor(reg(dst.toReg), cast[int32](imm.get[1]))
                    of irInstrBitXor: s.xxor(reg(dst.toReg), cast[int32](imm.get[1]))
                    of irInstrMul: s.imul(dst.toReg, reg(src.toReg), cast[int32](imm.get[1]))
                    else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, true)
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

                if comparesToZero:
                    setFlagCmpZero(iref)
                elif destroysFlags:
                    setFlagUnk()
        of irInstrISub:
            if (let imm = blk.isImmValI(instr.source(1)); imm.isSome):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                s.sub(reg(dst.toReg), cast[int32](imm.get))
            elif blk.isImmVal(instr.source(0), 0):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(1), i, blk)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                s.neg(reg(dst.toReg))
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.sub(reg(dst.toReg), src1.toReg)
            setFlagCmp(instr.source(0), instr.source(1))
        of irInstrIAddExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.adc(reg(dst.toReg), src1.toReg)
            setFlagUnk()
        of irInstrISubExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            s.cmc()
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.sbb(reg(dst.toReg), src1.toReg)
            setFlagUnk()
        of irInstrMulhS, irInstrMulhU, irInstrDivS, irInstrDivU:
            let
                isDivide = instr.kind in {irInstrDivS, irInstrDivU}
                (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, not isDivide)
            s.mov(reg(regEax), src0.toReg)
            if instr.kind == irInstrDivU:
                s.xxor(reg(regEdx), regEdx)
            elif instr.kind == irInstrDivS:
                s.cdq()
            case instr.kind
            of irInstrMulhS:
                s.imul(reg(src1.toReg))
                s.mov(reg(dst.toReg), regEdx)
            of irInstrMulhU:
                s.mul(reg(src1.toReg))
                s.mov(reg(dst.toReg), regEdx)
            of irInstrDivS:
                s.test(reg(src1.toReg), src1.toReg)
                let skipDiv0 = s.jcc(condZero, false)

                s.cmp(reg(src1.toReg), -1)
                let divisorNotMinusOne = s.jcc(condNotZero, false)
                s.cmp(reg(regEax), low(int32))
                let skipDivLowest = s.jcc(condZero, false)

                s.label(divisorNotMinusOne)
                s.idiv(reg(src1.toReg))
                s.mov(reg(dst.toReg), if isDivide: regEax else: regEdx)

                let skipProperResult = s.jmp(false)
                s.label skipDiv0
                s.xxor(reg(dst.toReg), dst.toReg)

                s.label skipDivLowest
                s.sub(reg(dst.toReg), 1)

                s.label skipProperResult
            of irInstrDivU:
                s.test(reg(src1.toReg), src1.toReg)
                let skipDiv0 = s.jcc(condZero, false)

                s.ddiv(reg(src1.toReg))
                s.mov(reg(dst.toReg), if isDivide: regEax else: regEdx)
                let skipProperResult = s.jmp(false)

                s.label skipDiv0
                s.xxor(reg(dst.toReg), dst.toReg)

                s.label skipProperResult
            else: raiseAssert("shouldn't happen")
            setFlagUnk()
        of irInstrBitNot:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.nnot(reg(dst.toReg))
            setFlagCmpZero(iref)
        of irInstrRol, irInstrShl, irInstrShrLogic, irInstrShrArith:
            if (let immShift = blk.isImmValI(instr.source(1)); immShift.isSome() and immShift.get <= 31):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                case instr.kind
                of irInstrShl: s.sshl(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of irInstrShrLogic: s.sshr(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of irInstrShrArith: s.sar(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of irInstrRol: s.rol(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
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
                s.mov(dst.toReg, reg(dst.toReg))
            setFlagUnk()
        of irInstrClz:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.bsr(regEax, reg(src.toReg))
            s.mov(reg(dst.toReg), 32 xor 0x1F)
            s.cmov(dst.toReg, reg(regEax), condNotZero)
            s.xxor(reg(dst.toReg), 0x1F)
            setFlagUnk()
        of irInstrExtsb, irInstrExtsh:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            case instr.kind
            of irInstrExtsb: s.movsx(dst.toReg, reg(Register8(src.toReg.ord)))
            of irInstrExtsh: s.movsx(dst.toReg, reg(Register16(src.toReg.ord)))
            else: raiseAssert("shouldn't happen")
        of irInstrCondAnd, irInstrCondOr, irInstrCondXor:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, true)
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
            setFlagUnk()
        of irInstrCondNot:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.xxor(reg(dst.toReg), 1)
            setFlagUnk()
        of irInstrOverflowAdd:
            raiseAssert("unimplemented code gen")
        of irInstrOverflowSub:
            raiseAssert("unimplemented code gen")
        of irInstrCarryAdd:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.mov(reg(regEax), src0.toReg)
            s.add(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of irInstrCarrySub:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.cmp(reg(src0.toReg), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of irInstrOverflowAddExtended:
            raiseAssert("unimplemented code gen")
        of irInstrOverflowSubExtended:
            raiseAssert("unimplemented code gen")
        of irInstrCarryAddExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            s.mov(reg(regEax), src0.toReg)
            s.adc(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrCarrySubExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            s.cmc()
            s.mov(reg(regEax), src0.toReg)
            s.sbb(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of irInstrCmpEqualI, irInstrCmpGreaterUI, irInstrCmpLessUI, irInstrCmpGreaterSI, irInstrCmpLessSI:
            let dst = 
                (if flagstate == flagStateCmpVal and
                        flagstateL == instr.source(0) and flagstateR == instr.source(1):
                    regalloc.allocOpW1R0(s, iref, blk)
                elif (let imm = blk.isImmValI(instr.source(1)); imm.isSome):
                    if imm.get == 0 and flagstate == flagStateCmpZero and flagstateL == instr.source(0):
                        regalloc.allocOpW1R0(s, iref, blk)
                    else:
                        let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                        s.cmp(reg(src.toReg), cast[int32](imm.get))
                        if imm.get == 0:
                            setFlagCmpZero(instr.source(0))
                        else:
                            setFlagCmp(instr.source(0), instr.source(1))
                        dst
                else:
                    let (dst, src0, src1) =
                        regalloc.allocOpW1R2(assembler, iref, instr.source(0), instr.source(1), i, blk)
                    s.cmp(reg(src0.toReg), src1.toReg)
                    setFlagCmp(instr.source(0), instr.source(1))
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
            let (dst, adr) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
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
            setFlagUnk()
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
            setFlagUnk()
        of irInstrLoadFss, irInstrLoadFsd:
            let
                dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
                adr = regalloc.allocOpW0R1(s, instr.source(0), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of irInstrLoadFss:
                s.call(jitReadMemory[uint32])
                s.movd(dst.toXmm, reg(regEax))
            of irInstrLoadFsd:
                s.call(jitReadMemory[uint64])
                s.movq(dst.toXmm, reg(regRax))
            else: raiseAssert("welp")
            setFlagUnk()
        of irInstrStoreFss, irInstrStoreFsd:
            let
                adr = regalloc.allocOpW0R1(s, instr.source(0), blk)
                val = xmmRegalloc.allocOpW0R1(s, instr.source(1), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of irInstrStoreFss:
                s.movd(reg(Register32(param3.ord)), val.toXmm)
                s.call(jitWriteMemory[uint32])
            of irInstrStoreFsd:
                s.movq(reg(param3), val.toXmm)
                s.call(jitWriteMemory[uint64])
            else: raiseAssert("welp")
            setFlagUnk()
        of irInstrLoadFsq, irInstrLoadFpq:
            let
                dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
                # the gqr register is currently wasted
                (adr, _) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of irInstrLoadFsq:
                s.call(jitReadMemory[uint32])
                s.movd(dst.toXmm, reg(regEax))
                s.unpcklps(dst.toXmm, memXmm(unsafeAddr singlesOne[0]))
            of irInstrLoadFpq:
                s.call(jitReadMemory[uint64])
                # necessary because when saved with big endian the elements are swapped
                s.ror(reg(regRax), 32)
                s.movq(dst.toXmm, reg(regRax))
            else: raiseAssert("welp")
            setFlagUnk()
        of irInstrStoreFsq, irInstrStoreFpq:
            let
                src = xmmRegalloc.allocOpW0R1(s, instr.source(1), blk)
                (adr, _) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(2), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of irInstrStoreFsq:
                s.movd(reg(Register32(param3.ord)), src.toXmm)
                s.call(jitWriteMemory[uint32])
            of irInstrStoreFpq:
                s.movq(reg(param3), src.toXmm)
                s.ror(reg(param3), 32)
                s.call(jitWriteMemory[uint64])
            else: raiseAssert("welp")
            setFlagUnk()
        of irInstrBranchPpc:
            #assert blk.isImmVal(instr.source(0), false), "should have been lowered before"
            if blk.isImmVal(instr.source(0), true):
                if (let immTarget = blk.isImmValI(instr.source(1)); immTarget.isSome()):
                    s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), cast[int32](immTarget.get()))
                else:
                    let target = regalloc.allocOpW0R1(s, instr.source(1), blk)
                    s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), target.toReg)

                if idleLoop:
                    s.mov(reg(regEax), -1)
            else:
                let (cond, target) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
                s.mov(reg(regEax), cast[int32](blk.isImmValI(instr.source(2)).get))
                s.mov(reg(regEcx), target.toReg)
                s.test(reg(cond.toReg), cond.toReg)
                s.cmov(regEax, reg(regEcx), condNotZero)
                s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), regEax)

                if idleLoop:
                    s.mov(reg(regEax), -1)
                    idleLoopBranch = s.jcc(condNotZero, false)
            setFlagUnk()
        of irInstrSyscallPpc:
            s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), cast[int32](blk.isImmValI(instr.source(0)).get))
            s.mov(reg(param1), rcpu)
            s.call(systemCall)
            setFlagUnk()
        of irInstrLoadFpr:
            let dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
            s.movsd(dst.toXmm, memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxLoadIdx)))
        of irInstrStoreFpr:
            let src = xmmRegalloc.allocOpW0R1(s, instr.source(0), blk)
            s.movsd(memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxStoreIdx)), src.toXmm())
        of irInstrLoadFprPair:
            let dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
            s.movapd(dst.toXmm, memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxLoadIdx)))
        of irInstrStoreFprPair:
            let src = xmmRegalloc.allocOpW0R1(s, instr.source(0), blk)
            s.movapd(memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxStoreIdx)), src.toXmm())
        of irInstrFSwizzleD00:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.movddup(dst.toXmm, reg(src.toXmm))
        of irInstrFSwizzleD11:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.unpckhpd(dst.toXmm, reg(dst.toXmm))
        of irInstrFSwizzleS00:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.movsldup(dst.toXmm, reg(src.toXmm))
        of irInstrFSwizzleS11:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.movshdup(dst.toXmm, reg(src.toXmm))
        of irInstrFMergeS00, irInstrFMergeS11:
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            if instr.kind == irInstrFMergeS00:
                s.unpcklps(dst.toXmm, reg(src1.toXmm))
            elif instr.kind == irInstrFMergeS11:
                s.unpckhps(dst.toXmm, reg(src1.toXmm))
        of irInstrFMergeS01:
            # the operands are flipped intentionally
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(1), instr.source(0), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            s.movss(dst.toXmm, reg(src1.toXmm))
        of irInstrFMergeS10:
            # unfortunately there is single instruction to do this on x64 :(
            # so what we do instead is we merge the lower half of the second operand into the first operand
            # and then we only need to swap the lower two singles
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            s.movss(dst.toXmm, reg(src1.toXmm))
            s.shufps(dst.toXmm, reg(dst.toXmm), 1) # swap the lower to floats
        of irInstrFMergeD00, irInstrFMergeD01, irInstrFMergeD10, irInstrFMergeD11:
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            if instr.kind == irInstrFMergeD00:
                s.unpcklpd(dst.toXmm, reg(src1.toXmm))
            elif instr.kind == irInstrFMergeD11:
                s.unpckhpd(dst.toXmm, reg(src1.toXmm))
            else:
                s.shufpd(dst.toXmm, reg(src1.toXmm),
                    if instr.kind == irInstrFMergeD01:
                        0x2'i8
                    else:
                        0x1'i8)
        of irInstrCvtsd2ss, irInstrCvtss2sd, irInstrCvtpd2ps, irInstrCvtps2pd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            case instr.kind
            of irInstrCvtsd2ss: s.cvtsd2ss(dst.toXmm, reg(src.toXmm))
            of irInstrCvtss2sd: s.cvtss2sd(dst.toXmm, reg(src.toXmm))
            of irInstrCvtpd2ps: s.cvtpd2ps(dst.toXmm, reg(src.toXmm))
            of irInstrCvtps2pd: s.cvtps2pd(dst.toXmm, reg(src.toXmm))
            else: raiseAssert("shouldn't happen")
        # todo do something faster for those:
        of irInstrFRessd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivsd(dst.toXmm, reg(src.toXmm))
            else:
                s.movsd(regXmm0, reg(src.toXmm))
                s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivsd(dst.toXmm, reg(regXmm0))
        of irInstrFResss:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movss(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivss(dst.toXmm, reg(src.toXmm))
            else:
                s.movss(regXmm0, reg(src.toXmm))
                s.movss(dst.toXmm, memXmm(unsafeAddr singlesOne[0]))
                s.ddivss(dst.toXmm, reg(regXmm0))
        of irInstrFRsqrtsd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtsd(regXmm0, reg(src.toXmm))
            s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivsd(dst.toXmm, reg(regXmm0))
        of irInstrFRsqrtss:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtss(regXmm0, reg(src.toXmm))
            s.movss(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivss(dst.toXmm, reg(regXmm0))
        of irInstrFRespd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivpd(dst.toXmm, reg(src.toXmm))
            else:
                s.movapd(regXmm0, reg(src.toXmm))
                s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivpd(dst.toXmm, reg(regXmm0))
        of irInstrFResps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivps(dst.toXmm, reg(src.toXmm))
            else:
                s.movaps(regXmm0, reg(src.toXmm))
                s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivps(dst.toXmm, reg(regXmm0))
        of irInstrFRsqrtpd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtpd(regXmm0, reg(src.toXmm))
            s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivpd(dst.toXmm, reg(regXmm0))
        of irInstrFRsqrtps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtps(regXmm0, reg(src.toXmm))
            s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivps(dst.toXmm, reg(regXmm0))
        of irInstrFNegsd, irInstrFNegpd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.xxorpd(dst.toXmm, memXmm(
                if instr.kind == irInstrFNegsd:
                    unsafeAddr doubleSignMask[0]
                else:
                    unsafeAddr doubleSignMaskPair[0]))
        of irInstrFNegss, irInstrFNegps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.xxorps(dst.toXmm, memXmm(
                if instr.kind == irInstrFNegss:
                    unsafeAddr singleSignMask[0]
                else:
                    unsafeAddr singleSignMaskPair[0]))
        of irInstrFAbssd, irInstrFAbspd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.aandpd(dst.toXmm, memXmm(
                if instr.kind == irInstrFAbssd:
                    unsafeAddr doubleSignMaskInv[0]
                else:
                    unsafeAddr doubleSignMaskPairInv[0]))
        of irInstrFAbsss, irInstrFAbsps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.aandpd(dst.toXmm, memXmm(
                if instr.kind == irInstrFAbssd:
                    unsafeAddr singleSignMaskInv[0]
                else:
                    unsafeAddr singleSignMaskPairInv[0]))
        of irInstrFAddsd, irInstrFSubsd, irInstrFMulsd, irInstrFDivsd,
            irInstrFAddpd, irInstrFSubpd, irInstrFMulpd, irInstrFDivpd,
            irInstrFAddss, irInstrFSubss, irInstrFMulss, irInstrFDivss,
            irInstrFAddps, irInstrFSubps, irInstrFMulps, irInstrFDivps:
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            case instr.kind
            of irInstrFAddsd: s.addsd(dst.toXmm, reg(src1.toXmm))
            of irInstrFSubsd: s.subsd(dst.toXmm, reg(src1.toXmm))
            of irInstrFMulsd: s.mulsd(dst.toXmm, reg(src1.toXmm))
            of irInstrFDivsd: s.ddivsd(dst.toXmm, reg(src1.toXmm))
            of irInstrFAddpd: s.addpd(dst.toXmm, reg(src1.toXmm))
            of irInstrFSubpd: s.subpd(dst.toXmm, reg(src1.toXmm))
            of irInstrFMulpd: s.mulpd(dst.toXmm, reg(src1.toXmm))
            of irInstrFDivpd: s.ddivpd(dst.toXmm, reg(src1.toXmm))
            of irInstrFAddss: s.addss(dst.toXmm, reg(src1.toXmm))
            of irInstrFSubss: s.subss(dst.toXmm, reg(src1.toXmm))
            of irInstrFMulss: s.mulss(dst.toXmm, reg(src1.toXmm))
            of irInstrFDivss: s.ddivss(dst.toXmm, reg(src1.toXmm))
            of irInstrFAddps: s.addps(dst.toXmm, reg(src1.toXmm))
            of irInstrFSubps: s.subps(dst.toXmm, reg(src1.toXmm))
            of irInstrFMulps: s.mulps(dst.toXmm, reg(src1.toXmm))
            of irInstrFDivps: s.ddivps(dst.toXmm, reg(src1.toXmm))
            else: raiseAssert("shouldn't happen")
        of irInstrFMaddsd, irInstrFMsubsd, irInstrFNmaddsd, irInstrFNmsubsd,
            irInstrFMaddpd, irInstrFMsubpd, irInstrFNmaddpd, irInstrFNmsubpd,
            irInstrFMaddss, irInstrFMsubss, irInstrFNmaddss, irInstrFNmsubss,
            irInstrFMaddps, irInstrFMsubps, irInstrFNmaddps, irInstrFNmsubps:
            let (dst, src0, src1, src2) = xmmRegalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            case instr.kind:
            of irInstrFMaddsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.addsd(dst.toXmm, reg(src1.toXmm))
            of irInstrFMsubsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.subsd(dst.toXmm, reg(src1.toXmm))
            of irInstrFNmaddsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.addsd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMask[0]))
            of irInstrFNmsubsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.subsd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMask[0]))
            of irInstrFMaddpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.addpd(dst.toXmm, reg(src1.toXmm))
            of irInstrFMsubpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.subpd(dst.toXmm, reg(src1.toXmm))
            of irInstrFNmaddpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.addpd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMaskPair[0]))
            of irInstrFNmsubpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.subpd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMaskPair[0]))
            of irInstrFMaddss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.addss(dst.toXmm, reg(src1.toXmm))
            of irInstrFMsubss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.subss(dst.toXmm, reg(src1.toXmm))
            of irInstrFNmaddss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.addss(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMask[0]))
            of irInstrFNmsubss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.subss(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMask[0]))
            of irInstrFMaddps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.addps(dst.toXmm, reg(src1.toXmm))
            of irInstrFMsubps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.subps(dst.toXmm, reg(src1.toXmm))
            of irInstrFNmaddps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.addps(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMaskPair[0]))
            of irInstrFNmsubps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.subps(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMaskPair[0]))
            else: raiseAssert("shouldn't happen")
        of irInstrCmpEqualFsd, irInstrCmpGreaterFsd, irInstrCmpLessFsd, irInstrCmpUnorderedsd:
            let
                dst = regalloc.allocOpW1R0(s, iref, blk)
                (src0, src1) = xmmRegalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
            if not(flagstate == flagStateCmpVal and
                flagstateL == instr.source(0) and flagstateR == instr.source(1)):
                s.ucomisd(src0.toXmm, reg(src1.toXmm))
                setFlagCmp(instr.source(0), instr.source(1))
            s.setcc(reg(Register8(dst.toReg.ord)),
                case instr.kind
                of irInstrCmpEqualFsd: condZero
                of irInstrCmpGreaterFsd: condNbequal
                of irInstrCmpLessFsd: condBelow
                of irInstrCmpUnorderedsd: condParityEven
                else: raiseAssert("welp"))
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of irInstrCvtsd2intTrunc, irInstrCvtss2intTrunc:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if instr.kind == irInstrCvtsd2intTrunc:
                s.cvttpd2dq(dst.toXmm, reg(src.toXmm))
            else:
                s.cvttps2dq(dst.toXmm, reg(src.toXmm))

        regalloc.freeExpiredRegs(blk, i)
        xmmRegalloc.freeExpiredRegs(blk, i)

    if not idleLoop or idleLoopBranch != ForwardsLabel():
        s.mov(reg(regEax), cycles)

    if idleLoop and idleLoopBranch != ForwardsLabel(): s.label(idleLoopBranch)
    if fexception: s.label(floatExceptionBranch)

    if fexception:
        for i in 0..<xmmsToUse.len:
            s.movaps(xmmsToUse[i], memXmm(regRsp, int32(stackFrameSize + i*16)))
    s.add(reg(regRsp), stackOffset)
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
