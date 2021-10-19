import
    options, streams, std/setutils, strformat,
    catnip/[x64assembler, reprotect],
    ../../util/setminmax,
    ../../gekko/[ppcstate, ppccommon, memory, jit/gekkoblockcache],
    ../../dsp/[dspstate, jit/dspblockcache],
    ir

when defined(windows):
    const
        registersToUse = [regEdi, regEsi, regEbx, regR12d, regR13d, regR14d, regR15d,
            regR10d, regR11d]
        xmmsToUse = [regXmm6, regXmm7, regXmm8, regXmm9, regXmm10, regXmm11, regXmm12, regXmm13, regXmm14, regXmm15,
            regXmm4, regXmm5]
        calleeSavedRegsNum = 7
        calleeSavedXmmsNum = 10
else:
    const
        registersToUse = [regEbx, regR12d, regR13d, regR14d, regR15d,
            regR8d, regR9d, regR10d, regR11d]
        xmmsToUse = [regXmm4, regXmm5,
            regXmm6, regXmm7, regXmm8, regXmm9, regXmm10, regXmm11, regXmm12, regXmm13, regXmm14, regXmm15]
        calleeSavedRegsNum = 5
        calleeSavedXmmsNum = 0

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
        flagStateCmpValX
        flagStateCmpZeroX

proc toReg(num: int32): Register32 =
    registersToUse[num]

proc toReg64(num: int32): Register64 =
    Register64(registersToUse[num].ord)

proc toXmm(num: int32): RegisterXmm =
    xmmsToUse[num]

proc getSpillOffset(num: int32): int32 =
    stackShadow + num*16

proc spillRegister[T](regalloc: var RegAlloc[T], s: var AssemblerX64, blk: IrBasicBlock, regIdx: int) =
    assert regalloc.activeRegs[regIdx].location == regLocHostGprReg
    assert regalloc.freeSpillLocs[] != {}, &"no spill locations left {regalloc}"

    let reg = regalloc.activeRegs[regIdx].idx
    regalloc.activeRegs[regIdx].location = regLocHostSpill
    regalloc.activeRegs[regIdx].idx = regalloc.freeSpillLocs[].popMin()

    let offset = getSpillOffset(regalloc.activeRegs[regIdx].idx)
    when T is HostIRegRange:
        if blk.getInstr(regalloc.activeRegs[regIdx].val).kind in HasWideResult:
            s.mov(mem64(regRsp, offset), reg.toReg64)
        else:
            s.mov(mem32(regRsp, offset), reg.toReg)
    else:
        s.movapd(memXmm(regRsp, offset), reg.toXmm())

proc prepareCall[T](regalloc: var RegAlloc[T], s: var AssemblerX64, blk: IrBasicBlock, writeVal: IrInstrRef) =
    var i = 0
    while i < regalloc.activeRegs.len:
        block deleted:
            if regalloc.activeRegs[i].location in {regLocHostGprReg, regLocHostGprRegImm} and
                regalloc.activeRegs[i].val != writeVal:
                if regalloc.activeRegs[i].idx >= (when T is HostIRegRange: calleeSavedRegsNum else: calleeSavedXmmsNum):
                    regalloc.freeRegs.incl regalloc.activeRegs[i].idx
                    if regalloc.activeRegs[i].location == regLocHostGprReg:
                        regalloc.spillRegister(s, blk, i)
                    elif regalloc.activeRegs[i].location == regLocHostGprRegImm:
                        regalloc.activeRegs.del i
                        break deleted
            i += 1


proc allocHostReg[T](regalloc: var RegAlloc[T], s: var AssemblerX64, blk: IrBasicBlock, lockedRegs: set[T]): int32 =
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

                let reg = regalloc.activeRegs[i].idx
                regalloc.spillRegister(s, blk, i)

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
                let
                    targetReg = allocHostReg(regalloc, s, blk, lockedRegs)
                    offset = getSpillOffset(reg.idx)
                when T is HostIRegRange:
                    if blk.getInstr(iref).kind in HasWideResult:
                        s.mov(targetReg.toReg64, mem64(regRsp, offset))
                    else:
                        s.mov(targetReg.toReg, mem32(regRsp, offset))
                else:
                    s.movapd(targetReg.toXmm, memXmm(regRsp, offset))
                regalloc.freeSpillLocs[].incl reg.idx
                reg.location = regLocHostGprReg
                reg.idx = targetReg
                return i

    if (let instr = blk.getInstr(iref); instr.kind == loadImmI):
        assert T isnot HostFRegRange

        let targetReg = allocHostReg(regalloc, s, blk, lockedRegs)

        if (instr.immValI and 0xFFFF_FFFF_0000_0000'u64) == 0:
            s.mov(reg(targetReg.toReg()), cast[int32](instr.immValI))
        elif (instr.immValI and 0xFFFF_FFFF_0000_0000'u64) == 0xFFFF_FFFF_0000_0000'u64:
            s.mov(reg(targetReg.toReg64()), cast[int32](instr.immValI))
        else:
            s.mov(targetReg.toReg64(), cast[int64](instr.immValI))

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
    let dstReg = allocHostReg(regalloc, s, blk, lockedRegs)
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
    elif commutative and blk.getInstr(src1).lastRead == instrIdx:
        # recycle register
        let src1Loc = prepareHostRead(regalloc, s, src1, blk, lockedRegs)
        regalloc.activeRegs[src1Loc].location = regLocHostGprReg
        regalloc.activeRegs[src1Loc].val = dst
        assert src0 != src1
        result[0] = T(regalloc.activeRegs[src1Loc].idx)
        result[1] = regalloc.allocOpW0R1(s, src0, blk, lockedRegs + {result[0]})
        result[2] = result[0]
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

proc getDspRegOffset(num: DspReg): int32 =
    case num
    of r0..r3: int32(offsetof(DspState, adrReg) + (ord(num) - ord(r0)) * 2)
    of m0..m3: int32(offsetof(DspState, incReg) + (ord(num) - ord(m0)) * 2)
    of l0..l3: int32(offsetof(DspState, incReg) + (ord(num) - ord(l0)) * 2)
    of x0..y0: int32(offsetof(DspState, auxAccum) + (ord(num) - ord(x0)) * 4)
    of x1..y1: int32(offsetof(DspState, auxAccum) + (ord(num) - ord(x0)) * 4 + 2)
    of a0..b0: int32(offsetof(DspState, mainAccum) + (ord(num) - ord(a0)) * 4)
    of a1..b1: int32(offsetof(DspState, mainAccum) + (ord(num) - ord(a1)) * 4 + 2)
    of a2..b2: int32(offsetof(DspState, mainAccum) + (ord(num) - ord(a2)) * 4 + 4)
    of dpp: int32(offsetof(DspState, dpp))
    of DspReg.ps0: int32(offsetof(DspState, prod))
    of DspReg.ps1: int32(offsetof(DspState, prod) + 2)
    of ps2: int32(offsetof(DspState, prod) + 4)
    of DspReg.pc1: int32(offsetof(DspState, prodcarry))
    of psr: int32(offsetof(DspState, status))
    else: raiseAssert(&"blah {num}")

proc getPpcRegOffset(num: uint32): int32 =
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
    template setFlagCmpZeroX(val: IrInstrRef): untyped =
        flagstate = flagStateCmpZeroX
        flagstateL = val
    template setFlagCmp(a, b: IrInstrRef): untyped =
        flagstate = flagStateCmpVal
        flagstateL = a
        flagstateR = b
    template setFlagCmpX(a, b: IrInstrRef): untyped =
        flagstate = flagStateCmpValX
        flagstateL = a
        flagstateR = b

    result = s.getFuncStart[:pointer]()

    for i in 0..<calleeSavedRegsNum:
        s.push(reg(Register64(registersToUse[i].ord)))
    s.push(reg(regRbp))
    let
        stackAlignAdjustment = if ((calleeSavedRegsNum + 1) mod 2) == 0: 8 else: 0
        stackOffset = int32(stackAlignAdjustment + (if fexception: calleeSavedXmmsNum*16 else: 0) + stackFrameSize)
    s.sub(reg(regRsp), stackOffset)
    if fexception:
        for i in 0..<calleeSavedXmmsNum:
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

        template beforeCall =
            regalloc.prepareCall(s(), blk, iref)
            xmmRegAlloc.prepareCall(s(), blk, iref)

        #echo &"processing instr {i} {regalloc}"

        case instr.kind
        of identity, loadCrBit, storeCrBit, extractLo..mergeHi:
            raiseAssert(&"should have been lowered {instr.kind}")
        of loadImmI:
            discard
        of ppcCallInterpreter:
            s.mov(mem32(rcpu, int32(offsetof(PpcState, pc))), cast[int32](instr.pc))
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), cast[int32](instr.instr))
            beforeCall()
            s.call(instr.target)
            setFlagUnk()
        of dspCallInterpreter:
            s.mov(mem16(rcpu, int32(offsetof(DspState, pc))), cast[int16](instr.pc))
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), cast[int32](instr.instr))
            beforeCall()
            s.call(instr.target)
            setFlagUnk()
        of loadPpcReg:
            let
                offset = getPpcRegOffset(instr.ctxLoadIdx)
                dst = regalloc.allocOpW1R0(s, iref, blk)
            s.mov(dst.toReg, mem32(rcpu, int32(offset)))
        of storePpcReg:
            let offset = getPpcRegOffset(instr.ctxStoreIdx)
            if (let imm = blk.isImmValI(instr.source(0)); imm.isSome()):
                s.mov(mem32(rcpu, int32(offset)), cast[int32](imm.get))
            else:
                let src = regalloc.allocOpW0R1(s, instr.source(0), blk)
                s.mov(mem32(rcpu, int32(offset)), src.toReg)
        of loadXer:
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
        of storeXer:
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
        of loadSpr:
            let dst = regalloc.allocOpW1R0(s, iref, blk)
            if IrSprNum(instr.ctxLoadIdx) in {irSprNumTbL, irSprNumTbU}:
                s.mov(reg(param1), rcpu)
                beforeCall()
                s.call(currentTb)
                if IrSprNum(instr.ctxLoadIdx) == irSprNumTbU:
                    s.sshr(reg(regRax), 32)
                s.mov(reg(dst.toReg), regEax)
                setFlagUnk()
            elif IrSprNum(instr.ctxLoadIdx) == irSprNumDec:
                s.mov(reg(param1), rcpu)
                beforeCall()
                s.call(getDecrementer)
                s.mov(reg(dst.toReg), regEax)
                setFlagUnk()
            else:
                let offset = getSprOffset(IrSprNum instr.ctxLoadIdx)
                s.mov(dst.toReg, mem32(rcpu, int32(offset)))
        of storeSpr:
            let num = IrSprNum instr.ctxStoreIdx
            if num in {irSprNumTbL, irSprNumTbU, irSprNumDec, irSprNumWpar, irSprNumHid0, irSprNumDmaL}:
                s.mov(reg(param1), rcpu)
                s.mov(reg(Register32(param2.ord)), regalloc.allocOpW0R1(s, instr.source(0), blk).toReg)
                beforeCall()
                case num
                of irSprNumDec: s.call(setupDecrementer)
                of irSprNumTbL: s.call(setTbl)
                of irSprNumTbU: s.call(setTbu)
                of irSprNumHid0: s.call(setHid0)
                of irSprNumWpar: s.call(setWpar)
                of irSprNumDmaL: s.call(setDmaL)
                else: raiseAssert("welp!")

                setFlagUnk()
            else:
                let offset = getSprOffset(num)
                if (let imm = blk.isImmValI(instr.source(0)); imm.isSome()):
                    s.mov(mem32(rcpu, int32(offset)), cast[int32](imm.get))
                else:
                    let src = regalloc.allocOpW0R1(s, instr.source(0), blk)
                    s.mov(mem32(rcpu, int32(offset)), src.toReg)
        of loadAccum:
            let dst = regalloc.allocOpW1R0(s, iref, blk)
            case DspAccum(instr.ctxLoadIdx)
            of dspAccumA: s.mov(dst.toReg64, mem64(rcpu, int32(offsetof(DspState, mainAccum))))
            of dspAccumB: s.mov(dst.toReg64, mem64(rcpu, int32(offsetof(DspState, mainAccum) + 8)))
            of dspAccumX: s.movsxd(dst.toReg64, mem32(rcpu, int32(offsetof(DspState, auxAccum))))
            of dspAccumY: s.movsxd(dst.toReg64, mem32(rcpu, int32(offsetof(DspState, auxAccum) + 4)))
            of dspAccumProd: s.mov(dst.toReg64, mem64(rcpu, int32(offsetof(DspState, prod))))
        of storeAccum:
            let src = regalloc.allocOpW0R1(s, instr.source(0), blk)
            case DspAccum(instr.ctxStoreIdx)
            of dspAccumA: s.mov(mem64(rcpu, int32(offsetof(DspState, mainAccum))), src.toReg64)
            of dspAccumB: s.mov(mem64(rcpu, int32(offsetof(DspState, mainAccum) + 8)), src.toReg64)
            of dspAccumX: s.mov(mem32(rcpu, int32(offsetof(DspState, auxAccum))), src.toReg)
            of dspAccumY: s.mov(mem32(rcpu, int32(offsetof(DspState, auxAccum) + 4)), src.toReg)
            of dspAccumProd: s.mov(mem64(rcpu, int32(offsetof(DspState, prod))), src.toReg64)
        of loadStatusBit:
            let dst = regalloc.allocOpW1R0(s, iref, blk)
            s.movzx(dst.toReg, mem16(rcpu, int32(offsetof(DspState, status))))
            s.sshr(reg(dst.toReg), int8(instr.ctxLoadIdx))
            s.aand(reg(dst.toReg), 1)
        of storeStatusBit:
            let
                src = regalloc.allocOpW0R1(s, instr.source(0), blk)
            s.movzx(regEax, mem16(rcpu, int32 offsetof(DspState, status)))
            s.aand(reg(regEax), cast[int32](not(1'u32 shl instr.ctxStoreIdx)))
            s.mov(reg(regEcx), src.toReg)
            s.sshl(reg(regEcx), int8 instr.ctxStoreIdx)
            s.oor(reg(regEax), regEcx)
            s.mov(mem16(rcpu, int32 offsetof(DspState, status)), regAx)
            setFlagUnk()
        of loadDspReg:
            let
                dst = regalloc.allocOpW1R0(s, iref, blk)
                offset = getDspRegOffset(DspReg instr.ctxLoadIdx)
            s.movzx(dst.toReg, mem16(rcpu, int32(offset)))
        of storeDspReg:
            let
                dst = regalloc.allocOpW0R1(s, instr.source(0), blk)
                offset = getDspRegOffset(DspReg instr.ctxStoreIdx)
            if DspReg(instr.ctxStoreIdx) in {a2, b2, ps2}:
                s.mov(mem32(rcpu, int32(offset)), dst.toReg)
            else:
                s.mov(mem16(rcpu, int32(offset)), Register16(ord(dst.toReg)))
        of csel:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.test(reg(src2.toReg), src2.toReg)
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.cmov(dst.toReg, reg(src1.toReg), condZero)
            setFlagCmpZero(instr.source(2))
        of cselX:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.test(reg(src2.toReg64), src2.toReg64)
            if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)
            s.cmov(dst.toReg64, reg(src1.toReg64), condZero)
            setFlagUnk()
        of iAdd, bitAnd, bitOr, bitXor, InstrKind.iMul:
            var
                comparesToZero = instr.kind in {bitAnd, bitOr, bitXor}
                destroysFlags = not comparesToZero
            if (let imm = blk.isEitherImmI(instr.source(0), instr.source(1)); imm.isSome):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, imm.get[0], i, blk)

                if instr.kind == bitAnd and imm.get[1] == 0xFF'u32:
                    s.movzx(dst.toReg, reg(Register8(src.toReg.ord)))
                    comparesToZero = false
                    destroysFlags = false
                elif instr.kind == bitAnd and imm.get[1] == 0xFFFF'u32:
                    s.movzx(dst.toReg, reg(Register16(src.toReg.ord)))
                    comparesToZero = false
                    destroysFlags = false
                else:
                    if dst != src and instr.kind != iMul: s.mov(reg(dst.toReg), src.toReg)
                    case instr.kind
                    of iAdd: s.add(reg(dst.toReg), cast[int32](imm.get[1]))
                    of bitAnd: s.aand(reg(dst.toReg), cast[int32](imm.get[1]))
                    of bitOr: s.oor(reg(dst.toReg), cast[int32](imm.get[1]))
                    of bitXor: s.xxor(reg(dst.toReg), cast[int32](imm.get[1]))
                    of InstrKind.iMul: s.iMul(dst.toReg, reg(src.toReg), cast[int32](imm.get[1]))
                    else: raiseAssert("shouldn't happen")
            else:
                var
                    comparesToZero = instr.kind in {bitAnd, bitOr, bitXor}
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, true)
                if dst == src1:
                    case instr.kind
                    of iAdd: s.add(reg(dst.toReg), src0.toReg)
                    of bitAnd: s.aand(reg(dst.toReg), src0.toReg)
                    of bitOr: s.oor(reg(dst.toReg), src0.toReg)
                    of bitXor: s.xxor(reg(dst.toReg), src0.toReg)
                    of InstrKind.iMul: s.iMul(dst.toReg, reg(src0.toReg))
                    else: raiseAssert("shouldn't happen")
                else:
                    if dst != src0: s.mov(reg(dst.toReg), src0.toReg)

                    case instr.kind
                    of iAdd: s.add(reg(dst.toReg), src1.toReg)
                    of bitAnd: s.aand(reg(dst.toReg), src1.toReg)
                    of bitOr: s.oor(reg(dst.toReg), src1.toReg)
                    of bitXor: s.xxor(reg(dst.toReg), src1.toReg)
                    of InstrKind.iMul: s.iMul(dst.toReg, reg(src1.toReg))
                    else: raiseAssert("shouldn't happen")

                if comparesToZero:
                    setFlagCmpZero(iref)
                elif destroysFlags:
                    setFlagUnk()
        of iAddX, bitAndX, bitOrX, bitXorX:
            if (let imm = blk.isEitherImmIX(instr.source(0), instr.source(1)); imm.isSome and cast[int32](imm.get[1]) == cast[int64](imm.get[1])):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, imm.get[0], i, blk)

                if instr.kind == bitAnd and imm.get[1] == 0xFF'u32:
                    s.movzx(dst.toReg, reg(Register8(src.toReg.ord)))
                elif instr.kind == bitAnd and imm.get[1] == 0xFFFF'u32:
                    s.movzx(dst.toReg, reg(Register16(src.toReg.ord)))
                else:
                    if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                    case instr.kind
                    of iAddX: s.add(reg(dst.toReg64), cast[int32](imm.get[1]))
                    of bitAndX: s.aand(reg(dst.toReg64), cast[int32](imm.get[1]))
                    of bitOrX: s.oor(reg(dst.toReg64), cast[int32](imm.get[1]))
                    of bitXorX: s.xxor(reg(dst.toReg64), cast[int32](imm.get[1]))
                    else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, true)
                if dst == src1:
                    case instr.kind
                    of iAddX: s.add(reg(dst.toReg64), src0.toReg64)
                    of bitAndX: s.aand(reg(dst.toReg64), src0.toReg64)
                    of bitOrX: s.oor(reg(dst.toReg64), src0.toReg64)
                    of bitXorX: s.xxor(reg(dst.toReg64), src0.toReg64)
                    else: raiseAssert("shouldn't happen")
                else:
                    if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)

                    case instr.kind
                    of iAddX: s.add(reg(dst.toReg64), src1.toReg64)
                    of bitAndX: s.aand(reg(dst.toReg64), src1.toReg64)
                    of bitOrX: s.oor(reg(dst.toReg64), src1.toReg64)
                    of bitXorX: s.xxor(reg(dst.toReg64), src1.toReg64)
                    else: raiseAssert("shouldn't happen")

            setFlagUnk()
        of iSub:
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
        of iSubX:
            if (let imm = blk.isImmValIX(instr.source(1)); imm.isSome and cast[int32](imm.get) == cast[int64](imm.get)):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                s.sub(reg(dst.toReg64), cast[int32](imm.get))
            elif blk.isImmValX(instr.source(0), 0):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(1), i, blk)
                if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                s.neg(reg(dst.toReg64))
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
                if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)
                s.sub(reg(dst.toReg64), src1.toReg64)
            setFlagUnk()
        of iAddExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.adc(reg(dst.toReg), src1.toReg)
            setFlagUnk()
        of iSubExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            s.cmc()
            if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
            s.sbb(reg(dst.toReg), src1.toReg)
            setFlagUnk()
        of iMulhS, iMulhU, iDivS, iDivU:
            let
                isDivide = instr.kind in {iDivS, iDivU}
                (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, not isDivide)
            s.mov(reg(regEax), src0.toReg)
            if instr.kind == iDivU:
                s.xxor(reg(regEdx), regEdx)
            elif instr.kind == iDivS:
                s.cdq()
            case instr.kind
            of iMulhS:
                s.iMul(reg(src1.toReg))
                s.mov(reg(dst.toReg), regEdx)
            of iMulhU:
                s.mul(reg(src1.toReg))
                s.mov(reg(dst.toReg), regEdx)
            of iDivS:
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
            of iDivU:
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
        of bitNot:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.nnot(reg(dst.toReg))
            setFlagCmpZero(iref)
        of bitNotX:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.nnot(reg(dst.toReg64))
            setFlagUnk()
        of InstrKind.rol, lsl, lsr, asr:
            if (let immShift = blk.isImmValI(instr.source(1)); immShift.isSome()):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                case instr.kind
                of lsl: s.sshl(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of lsr: s.sshr(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of asr: s.sar(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                of InstrKind.rol: s.rol(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.mov(reg(regEcx), src1.toReg)
                case instr.kind
                of lsl: s.sshl(reg(dst.toReg))
                of lsr: s.sshr(reg(dst.toReg))
                of asr: s.sar(reg(dst.toReg))
                of InstrKind.rol: s.rol(reg(dst.toReg))
                else: raiseAssert("shouldn't happen")
            setFlagUnk()
        of lslX, lsrX, asrX:
            if (let immShift = blk.isImmValI(instr.source(1)); immShift.isSome()):
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                case instr.kind
                of lslX: s.sshl(reg(dst.toReg64), int8(immShift.get and 0x3F'u32))
                of lsrX: s.sshr(reg(dst.toReg64), int8(immShift.get and 0x3F'u32))
                of asrX: s.sar(reg(dst.toReg64), int8(immShift.get and 0x3F'u32))
                else: raiseAssert("shouldn't happen")
            else:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
                if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)
                s.mov(reg(regEcx), src1.toReg)
                case instr.kind
                of lslX: s.sshl(reg(dst.toReg64))
                of lsrX: s.sshr(reg(dst.toReg64))
                of asrX: s.sar(reg(dst.toReg64))
                else: raiseAssert("shouldn't happen")
            setFlagUnk()
        of clz:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.bsr(regEax, reg(src.toReg))
            s.mov(reg(dst.toReg), 32 xor 0x1F)
            s.cmov(dst.toReg, reg(regEax), condNotZero)
            s.xxor(reg(dst.toReg), 0x1F)
            setFlagUnk()
        of extsb, extsh, extsw, extzw:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            case instr.kind
            of extsb: s.movsx(dst.toReg, reg(Register8(src.toReg.ord)))
            of extsh: s.movsx(dst.toReg, reg(Register16(src.toReg.ord)))
            of extsw: s.movsxd(dst.toReg64, reg(src.toReg))
            of extzw: s.mov(dst.toReg, reg(src.toReg))
            else: raiseAssert("shouldn't happen")
        of condAnd, condOr, condXor:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk, true)
            if dst == src1:
                case instr.kind
                of condAnd: s.aand(reg(dst.toReg), src0.toReg)
                of condOr: s.oor(reg(dst.toReg), src0.toReg)
                of condXor: s.xxor(reg(dst.toReg), src0.toReg)
                else: raiseAssert("welp")
            else:
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                case instr.kind
                of condAnd: s.aand(reg(dst.toReg), src1.toReg)
                of condOr: s.oor(reg(dst.toReg), src1.toReg)
                of condXor: s.xxor(reg(dst.toReg), src1.toReg)
                else: raiseAssert("welp")
            setFlagUnk()
        of condNot:
            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.mov(reg(dst.toReg), src.toReg)
            s.xxor(reg(dst.toReg), 1)
            setFlagUnk()
        of overflowAdd:
            raiseAssert("unimplemented code gen")
        of overflowSub:
            raiseAssert("unimplemented code gen")
        of overflowAddX:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.mov(reg(regRax), src0.toReg64)
            s.add(reg(regRax), src1.toReg64)
            s.setcc(reg(Register8(dst.toReg.ord)), condOverflow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of overflowSubX:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.mov(reg(regRax), src0.toReg64)
            s.sub(reg(regRax), src1.toReg64)
            s.setcc(reg(Register8(dst.toReg.ord)), condOverflow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of carryAdd:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.mov(reg(regEax), src0.toReg)
            s.add(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of carryAddX:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.mov(reg(regRax), src0.toReg64)
            s.add(reg(regRax), src1.toReg64)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of carrySub:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.cmp(reg(src0.toReg), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of carrySubX:
            let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            s.cmp(reg(src0.toReg64), src1.toReg64)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of overflowAddExtended:
            raiseAssert("unimplemented code gen")
        of overflowSubExtended:
            raiseAssert("unimplemented code gen")
        of carryAddExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            s.mov(reg(regEax), src0.toReg)
            s.adc(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of carrySubExtended:
            let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            s.bt(reg(src2.toReg), 0)
            s.cmc()
            s.mov(reg(regEax), src0.toReg)
            s.sbb(reg(regEax), src1.toReg)
            s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            setFlagUnk()
        of iCmpEqual, iCmpGreaterU, iCmpLessU, iCmpGreaterS, iCmpLessS:
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
                of iCmpEqual: condZero
                of iCmpGreaterU: condNbequal
                of iCmpLessU: condBelow
                of iCmpGreaterS: condNotLequal
                of iCmpLessS: condLess
                else: raiseAssert("welp"))
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of iCmpEqualX, iCmpGreaterUX, iCmpLessUX, iCmpGreaterSX, iCmpLessSX:
            let dst =
                (if flagstate == flagStateCmpValX and
                        flagstateL == instr.source(0) and flagstateR == instr.source(1):
                    regalloc.allocOpW1R0(s, iref, blk)
                elif (let imm = blk.isImmValIX(instr.source(1)); imm.isSome and cast[int32](imm.get) == cast[int64](imm.get)):
                    if imm.get == 0 and flagstate == flagStateCmpZeroX and flagstateL == instr.source(0):
                        regalloc.allocOpW1R0(s, iref, blk)
                    else:
                        let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
                        s.cmp(reg(src.toReg64), cast[int32](imm.get))
                        if imm.get == 0:
                            setFlagCmpZeroX(instr.source(0))
                        else:
                            setFlagCmpX(instr.source(0), instr.source(1))
                        dst
                else:
                    let (dst, src0, src1) =
                        regalloc.allocOpW1R2(assembler, iref, instr.source(0), instr.source(1), i, blk)
                    s.cmp(reg(src0.toReg64), src1.toReg64)
                    setFlagCmpX(instr.source(0), instr.source(1))
                    dst)
            s.setcc(reg(Register8(dst.toReg.ord)),
                case instr.kind
                of iCmpEqualX: condZero
                of iCmpGreaterUX: condNbequal
                of iCmpLessUX: condBelow
                of iCmpGreaterSX: condNotLequal
                of iCmpLessSX: condLess
                else: raiseAssert("welp"))
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of ppcLoadU8, ppcLoadU16, ppcLoadS16, ppcLoad32:
            let (dst, adr) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            beforeCall()
            case instr.kind
            of ppcLoad32:
                s.call(jitReadMemory[uint32])
                s.mov(reg(dst.toReg), regEax)
            of ppcLoadU16:
                s.call(jitReadMemory[uint16])
                s.movzx(dst.toReg, reg(regAx))
            of ppcLoadS16:
                s.call(jitReadMemory[uint16])
                s.movsx(dst.toReg, reg(regAx))
            of ppcLoadU8:
                s.call(jitReadMemory[uint8])
                s.movzx(dst.toReg, reg(regAl))
            else:
                raiseAssert("welp")
            setFlagUnk()
        of ppcStore8, ppcStore16, ppcStore32:
            let (adr, val) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            case instr.kind
            of ppcStore32:
                s.mov(reg(Register32(param3.ord)), val.toReg)
                beforeCall()
                s.call(jitWriteMemory[uint32])
            of ppcStore16:
                s.movzx(Register32(param3.ord), reg(Register16(val.toReg.ord)))
                beforeCall()
                s.call(jitWriteMemory[uint16])
            of ppcStore8:
                s.movzx(Register32(param3.ord), reg(Register8(val.toReg.ord)))
                beforeCall()
                s.call(jitWriteMemory[uint8])
            else: raiseAssert("welp")
            setFlagUnk()
        of ppcLoadFss, ppcLoadFsd:
            let
                dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
                adr = regalloc.allocOpW0R1(s, instr.source(0), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            beforeCall()
            case instr.kind
            of ppcLoadFss:
                s.call(jitReadMemory[uint32])
                s.movd(dst.toXmm, reg(regEax))
            of ppcLoadFsd:
                s.call(jitReadMemory[uint64])
                s.movq(dst.toXmm, reg(regRax))
            else: raiseAssert("welp")
            setFlagUnk()
        of ppcStoreFss, ppcStoreFsd:
            let
                adr = regalloc.allocOpW0R1(s, instr.source(0), blk)
                val = xmmRegalloc.allocOpW0R1(s, instr.source(1), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            beforeCall()
            case instr.kind
            of ppcStoreFss:
                s.movd(reg(Register32(param3.ord)), val.toXmm)
                s.call(jitWriteMemory[uint32])
            of ppcStoreFsd:
                s.movq(reg(param3), val.toXmm)
                s.call(jitWriteMemory[uint64])
            else: raiseAssert("welp")
            setFlagUnk()
        of ppcLoadFsq, ppcLoadFpq:
            let
                dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
                # the gqr register is currently wasted
                (adr, _) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            beforeCall()
            case instr.kind
            of ppcLoadFsq:
                s.call(jitReadMemory[uint32])
                s.movd(dst.toXmm, reg(regEax))
                s.unpcklps(dst.toXmm, memXmm(unsafeAddr singlesOne[0]))
            of ppcLoadFpq:
                s.call(jitReadMemory[uint64])
                # necessary because when saved with big endian the elements are swapped
                s.ror(reg(regRax), 32)
                s.movq(dst.toXmm, reg(regRax))
            else: raiseAssert("welp")
            setFlagUnk()
        of ppcStoreFsq, ppcStoreFpq:
            let
                src = xmmRegalloc.allocOpW0R1(s, instr.source(1), blk)
                (adr, _) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(2), blk)
            s.mov(reg(param1), rcpu)
            s.mov(reg(Register32(param2.ord)), adr.toReg)
            beforeCall()
            case instr.kind
            of ppcStoreFsq:
                s.movd(reg(Register32(param3.ord)), src.toXmm)
                s.call(jitWriteMemory[uint32])
            of ppcStoreFpq:
                s.movq(reg(param3), src.toXmm)
                s.ror(reg(param3), 32)
                s.call(jitWriteMemory[uint64])
            else: raiseAssert("welp")
            setFlagUnk()
        of ppcBranch, dspBranch:
            #assert blk.isImmVal(instr.source(0), false), "should have been lowered before"
            if blk.isImmVal(instr.source(0), true):
                if (let immTarget = blk.isImmValI(instr.source(1)); immTarget.isSome()):
                    if instr.kind == ppcBranch:
                        s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), cast[int32](immTarget.get()))
                    else:
                        s.mov(mem16(rcpu, int32 offsetof(DspState, pc)), cast[int16](immTarget.get()))
                else:
                    let target = regalloc.allocOpW0R1(s, instr.source(1), blk)
                    if instr.kind == ppcBranch:
                        s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), target.toReg)
                    else:
                        s.mov(mem16(rcpu, int32 offsetof(DspState, pc)), Register16(target.toReg.ord))

                if idleLoop:
                    s.mov(reg(regEax), -1)
            else:
                let (cond, target) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
                s.mov(reg(regEax), cast[int32](blk.isImmValI(instr.source(2)).get))
                s.mov(reg(regEcx), target.toReg)
                s.test(reg(cond.toReg), cond.toReg)
                s.cmov(regEax, reg(regEcx), condNotZero)
                if instr.kind == ppcBranch:
                    s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), regEax)
                else:
                    s.mov(mem16(rcpu, int32 offsetof(DspState, pc)), regAx)

                if idleLoop:
                    s.mov(reg(regEax), -1)
                    idleLoopBranch = s.jcc(condNotZero, false)
            setFlagUnk()
        of ppcSyscall:
            s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), cast[int32](blk.isImmValI(instr.source(0)).get))
            s.mov(reg(param1), rcpu)
            beforeCall()
            s.call(systemCall)
            setFlagUnk()
        of loadFpr:
            let dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
            s.movsd(dst.toXmm, memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxLoadIdx)))
        of storeFpr:
            let src = xmmRegalloc.allocOpW0R1(s, instr.source(0), blk)
            s.movsd(memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxStoreIdx)), src.toXmm())
        of loadFprPair:
            let dst = xmmRegalloc.allocOpW1R0(s, iref, blk)
            s.movapd(dst.toXmm, memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxLoadIdx)))
        of storeFprPair:
            let src = xmmRegalloc.allocOpW0R1(s, instr.source(0), blk)
            s.movapd(memXmm(rcpu, int32 offsetof(PpcState, fr) + 8*2*int32(instr.ctxStoreIdx)), src.toXmm())
        of fSwizzleD00:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.movddup(dst.toXmm, reg(src.toXmm))
        of fSwizzleD11:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.unpckhpd(dst.toXmm, reg(dst.toXmm))
        of fSwizzleS00:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.movsldup(dst.toXmm, reg(src.toXmm))
        of fSwizzleS11:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.movshdup(dst.toXmm, reg(src.toXmm))
        of fMergeS00, fMergeS11:
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            if instr.kind == fMergeS00:
                s.unpcklps(dst.toXmm, reg(src1.toXmm))
            elif instr.kind == fMergeS11:
                s.unpckhps(dst.toXmm, reg(src1.toXmm))
        of fMergeS01:
            # the operands are flipped intentionally
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(1), instr.source(0), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            s.movss(dst.toXmm, reg(src1.toXmm))
        of fMergeS10:
            # unfortunately there is single instruction to do this on x64 :(
            # so what we do instead is we merge the lower half of the second operand into the first operand
            # and then we only need to swap the lower two singles
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            s.movss(dst.toXmm, reg(src1.toXmm))
            s.shufps(dst.toXmm, reg(dst.toXmm), 1) # swap the lower two floats
        of fMergeD00, fMergeD01, fMergeD10, fMergeD11:
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            if instr.kind == fMergeD00:
                s.unpcklpd(dst.toXmm, reg(src1.toXmm))
            elif instr.kind == fMergeD11:
                s.unpckhpd(dst.toXmm, reg(src1.toXmm))
            else:
                s.shufpd(dst.toXmm, reg(src1.toXmm),
                    if instr.kind == fMergeD01:
                        0x2'i8
                    else:
                        0x1'i8)
        of InstrKind.cvtsd2ss, InstrKind.cvtss2sd, InstrKind.cvtpd2ps, InstrKind.cvtps2pd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            case instr.kind
            of InstrKind.cvtsd2ss: s.cvtsd2ss(dst.toXmm, reg(src.toXmm))
            of InstrKind.cvtss2sd: s.cvtss2sd(dst.toXmm, reg(src.toXmm))
            of InstrKind.cvtpd2ps: s.cvtpd2ps(dst.toXmm, reg(src.toXmm))
            of InstrKind.cvtps2pd: s.cvtps2pd(dst.toXmm, reg(src.toXmm))
            else: raiseAssert("shouldn't happen")
        # todo do something faster for those:
        of fRessd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivsd(dst.toXmm, reg(src.toXmm))
            else:
                s.movsd(regXmm0, reg(src.toXmm))
                s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivsd(dst.toXmm, reg(regXmm0))
        of fResss:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movss(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivss(dst.toXmm, reg(src.toXmm))
            else:
                s.movss(regXmm0, reg(src.toXmm))
                s.movss(dst.toXmm, memXmm(unsafeAddr singlesOne[0]))
                s.ddivss(dst.toXmm, reg(regXmm0))
        of fRsqrtsd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtsd(regXmm0, reg(src.toXmm))
            s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivsd(dst.toXmm, reg(regXmm0))
        of fRsqrtss:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtss(regXmm0, reg(src.toXmm))
            s.movss(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivss(dst.toXmm, reg(regXmm0))
        of fRespd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivpd(dst.toXmm, reg(src.toXmm))
            else:
                s.movapd(regXmm0, reg(src.toXmm))
                s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivpd(dst.toXmm, reg(regXmm0))
        of fResps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src:
                s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivps(dst.toXmm, reg(src.toXmm))
            else:
                s.movaps(regXmm0, reg(src.toXmm))
                s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivps(dst.toXmm, reg(regXmm0))
        of fRsqrtpd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtpd(regXmm0, reg(src.toXmm))
            s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivpd(dst.toXmm, reg(regXmm0))
        of fRsqrtps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            s.sqrtps(regXmm0, reg(src.toXmm))
            s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
            s.ddivps(dst.toXmm, reg(regXmm0))
        of fNegsd, fNegpd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.xxorpd(dst.toXmm, memXmm(
                if instr.kind == fNegsd:
                    unsafeAddr doubleSignMask[0]
                else:
                    unsafeAddr doubleSignMaskPair[0]))
        of fNegss, fNegps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.xxorps(dst.toXmm, memXmm(
                if instr.kind == fNegss:
                    unsafeAddr singleSignMask[0]
                else:
                    unsafeAddr singleSignMaskPair[0]))
        of fAbssd, fAbspd:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.aandpd(dst.toXmm, memXmm(
                if instr.kind == fAbssd:
                    unsafeAddr doubleSignMaskInv[0]
                else:
                    unsafeAddr doubleSignMaskPairInv[0]))
        of fAbsss, fAbsps:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
            s.aandpd(dst.toXmm, memXmm(
                if instr.kind == fAbssd:
                    unsafeAddr singleSignMaskInv[0]
                else:
                    unsafeAddr singleSignMaskPairInv[0]))
        of fAddsd, fSubsd, fMulsd, fDivsd,
            fAddpd, fSubpd, fMulpd, fDivpd,
            fAddss, fSubss, fMulss, fDivss,
            fAddps, fSubps, fMulps, fDivps:
            let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            case instr.kind
            of fAddsd: s.addsd(dst.toXmm, reg(src1.toXmm))
            of fSubsd: s.subsd(dst.toXmm, reg(src1.toXmm))
            of fMulsd: s.mulsd(dst.toXmm, reg(src1.toXmm))
            of fDivsd: s.ddivsd(dst.toXmm, reg(src1.toXmm))
            of fAddpd: s.addpd(dst.toXmm, reg(src1.toXmm))
            of fSubpd: s.subpd(dst.toXmm, reg(src1.toXmm))
            of fMulpd: s.mulpd(dst.toXmm, reg(src1.toXmm))
            of fDivpd: s.ddivpd(dst.toXmm, reg(src1.toXmm))
            of fAddss: s.addss(dst.toXmm, reg(src1.toXmm))
            of fSubss: s.subss(dst.toXmm, reg(src1.toXmm))
            of fMulss: s.mulss(dst.toXmm, reg(src1.toXmm))
            of fDivss: s.ddivss(dst.toXmm, reg(src1.toXmm))
            of fAddps: s.addps(dst.toXmm, reg(src1.toXmm))
            of fSubps: s.subps(dst.toXmm, reg(src1.toXmm))
            of fMulps: s.mulps(dst.toXmm, reg(src1.toXmm))
            of fDivps: s.ddivps(dst.toXmm, reg(src1.toXmm))
            else: raiseAssert("shouldn't happen")
        of fMaddsd, fMsubsd, fNmaddsd, fNmsubsd,
            fMaddpd, fMsubpd, fNmaddpd, fNmsubpd,
            fMaddss, fMsubss, fNmaddss, fNmsubss,
            fMaddps, fMsubps, fNmaddps, fNmsubps:
            let (dst, src0, src1, src2) = xmmRegalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, blk)
            if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
            case instr.kind:
            of fMaddsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.addsd(dst.toXmm, reg(src1.toXmm))
            of fMsubsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.subsd(dst.toXmm, reg(src1.toXmm))
            of fNmaddsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.addsd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMask[0]))
            of fNmsubsd:
                s.mulsd(dst.toXmm, reg(src2.toXmm))
                s.subsd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMask[0]))
            of fMaddpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.addpd(dst.toXmm, reg(src1.toXmm))
            of fMsubpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.subpd(dst.toXmm, reg(src1.toXmm))
            of fNmaddpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.addpd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMaskPair[0]))
            of fNmsubpd:
                s.mulpd(dst.toXmm, reg(src2.toXmm))
                s.subpd(dst.toXmm, reg(src1.toXmm))
                s.xxorpd(dst.toXmm, memXmm(unsafeAddr doubleSignMaskPair[0]))
            of fMaddss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.addss(dst.toXmm, reg(src1.toXmm))
            of fMsubss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.subss(dst.toXmm, reg(src1.toXmm))
            of fNmaddss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.addss(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMask[0]))
            of fNmsubss:
                s.mulss(dst.toXmm, reg(src2.toXmm))
                s.subss(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMask[0]))
            of fMaddps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.addps(dst.toXmm, reg(src1.toXmm))
            of fMsubps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.subps(dst.toXmm, reg(src1.toXmm))
            of fNmaddps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.addps(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMaskPair[0]))
            of fNmsubps:
                s.mulps(dst.toXmm, reg(src2.toXmm))
                s.subps(dst.toXmm, reg(src1.toXmm))
                s.xxorps(dst.toXmm, memXmm(unsafeAddr singleSignMaskPair[0]))
            else: raiseAssert("shouldn't happen")
        of fCmpEqualsd, fCmpGreatersd, fCmpLesssd, fUnorderedsd:
            let
                dst = regalloc.allocOpW1R0(s, iref, blk)
                (src0, src1) = xmmRegalloc.allocOpW0R2(s, instr.source(0), instr.source(1), blk)
            if not(flagstate == flagStateCmpVal and
                flagstateL == instr.source(0) and flagstateR == instr.source(1)):
                s.ucomisd(src0.toXmm, reg(src1.toXmm))
                setFlagCmp(instr.source(0), instr.source(1))
            s.setcc(reg(Register8(dst.toReg.ord)),
                case instr.kind
                of fCmpEqualsd: condZero
                of fCmpGreatersd: condNbequal
                of fCmpLesssd: condBelow
                of fUnorderedsd: condParityEven
                else: raiseAssert("welp"))
            s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
        of cvtsd2intTrunc, cvtss2intTrunc:
            let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, blk)
            if instr.kind == cvtsd2intTrunc:
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
        for i in 0..<calleeSavedXmmsNum:
            s.movaps(xmmsToUse[i], memXmm(regRsp, int32(stackFrameSize + i*16)))
    s.add(reg(regRsp), stackOffset)
    s.pop(reg(regRbp))
    for i in countdown(calleeSavedRegsNum-1, 0):
        s.pop(reg(Register64(registersToUse[i].ord)))
    s.ret()

    assert s.offset < sizeof(codeMemory)
