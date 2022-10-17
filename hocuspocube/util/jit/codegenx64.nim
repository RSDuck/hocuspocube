import
    options, streams, std/setutils, strformat, tables,
    catnip/[x64assembler, reprotect],
    ../../util/setminmax,
    ../../gekko/[ppcstate, ppccommon, memory, jit/gekkoblockcache],
    ../../dsp/[dspstate, dsp, dspcommon, jit/dspblockcache],
    ir

proc systemStep(): bool {.importc.}
proc compileBlockPpc(): gekkoblockcache.BlockEntryFunc {.importc: "compileBlockPpc".}
proc compileBlockDsp(): dspblockcache.BlockEntryFunc {.importc: "compileBlockDsp".}

when defined(windows):
    const
        registersToUse = [regEdi, regEsi, regEbx, regR12d, regR13d, regR14d,
            regR10d, regR11d]
        xmmsToUse = [regXmm6, regXmm7, regXmm8, regXmm9, regXmm10, regXmm11, regXmm12, regXmm13, regXmm14, regXmm15,
            regXmm4, regXmm5]
        calleeSavedRegsNum = 6
        calleeSavedXmmsNum = 10
else:
    const
        registersToUse = [regEbx, regR12d, regR13d, regR14d,
            regR8d, regR9d, regR10d, regR11d]
        xmmsToUse = [regXmm4, regXmm5,
            regXmm6, regXmm7, regXmm8, regXmm9, regXmm10, regXmm11, regXmm12, regXmm13, regXmm14, regXmm15]
        calleeSavedRegsNum = 4
        calleeSavedXmmsNum = 0

const
    maxSpill = 32

    stackFrameSize = stackShadow + maxSpill*16

    rcpu = regRbp
    rmemStart = regR15

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

proc spillRegister[T](regalloc: var RegAlloc[T], s: var AssemblerX64, fn: IrFunc, regIdx: int) =
    assert regalloc.activeRegs[regIdx].location == regLocHostGprReg
    assert regalloc.freeSpillLocs[] != {}, &"no spill locations left {regalloc}"

    let reg = regalloc.activeRegs[regIdx].idx
    regalloc.activeRegs[regIdx].location = regLocHostSpill
    regalloc.activeRegs[regIdx].idx = regalloc.freeSpillLocs[].popMin()

    let offset = getSpillOffset(regalloc.activeRegs[regIdx].idx)
    when T is HostIRegRange:
        if fn.getInstr(regalloc.activeRegs[regIdx].val).kind in HasWideResult:
            s.mov(mem64(regRsp, offset), reg.toReg64)
        else:
            s.mov(mem32(regRsp, offset), reg.toReg)
    else:
        s.movapd(memXmm(regRsp, offset), reg.toXmm())

proc prepareCall[T](regalloc: var RegAlloc[T], s: var AssemblerX64, fn: IrFunc, writeVal: IrInstrRef) =
    var i = 0
    while i < regalloc.activeRegs.len:
        block deleted:
            if regalloc.activeRegs[i].location in {regLocHostGprReg, regLocHostGprRegImm} and
                    regalloc.activeRegs[i].val != writeVal:
                if regalloc.activeRegs[i].idx >= (when T is HostIRegRange: calleeSavedRegsNum else: calleeSavedXmmsNum):
                    regalloc.freeRegs.incl regalloc.activeRegs[i].idx
                    if regalloc.activeRegs[i].location == regLocHostGprReg:
                        regalloc.spillRegister(s, fn, i)
                    elif regalloc.activeRegs[i].location == regLocHostGprRegImm:
                        regalloc.activeRegs.del i
                        break deleted
            i += 1


template doCall(s: var AssemblerX64, regs: set[Register64], xmms: set[RegisterXmm], body: untyped): untyped =
    s.pushfq()
    for reg in regs:
        s.push(x64assembler.reg(reg))
    let
        xmmSpace = 16*card(xmms)
        callAlign = stackShadow + (if ((card(regs)+1) mod 2) == 0: 0 else: 8)
    if xmmSpace+callAlign > 0:
        s.sub(reg(regRsp), int32 xmmSpace+callAlign)

    var i = 0
    for xmm in xmms:
        s.movapd(memXmm(regRsp, int32(i+stackShadow)), xmm)
        i += 16

    body

    i = 0
    for xmm in xmms:
        s.movapd(xmm, memXmm(regRsp, int32(i+stackShadow)))
        i += 16

    if xmmSpace+callAlign > 0:
        s.add(reg(regRsp), int32 xmmSpace+callAlign)
    for i in countdown(high(Register64).ord, low(Register64).ord):
        if Register64(i) in regs:
            s.pop(x64assembler.reg(Register64(i)))
    s.popfq()

proc getCallerSavedRegs[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
        writeVal: IrInstrRef): auto =
    when T is HostIRegRange:
        type RegSetType = Register64
    else:
        type RegSetType = RegisterXmm
    var regs: set[RegSetType]
    for i in 0..<regalloc.activeRegs.len:
        let reg = regalloc.activeRegs[i]
        if reg.location != regLocHostSpill and reg.val != writeVal and
                reg.idx >= (when T is HostIRegRange: calleeSavedRegsNum else: calleeSavedXmmsNum):
            when T is HostIRegRange:
                regs.incl reg.idx.toReg64
            else:
                regs.incl reg.idx.toXmm
    regs

proc allocHostReg[T](regalloc: var RegAlloc[T], s: var AssemblerX64, fn: IrFunc, lockedRegs: set[T]): int32 =
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
                regalloc.spillRegister(s, fn, i)

                return reg

        raiseAssert(&"too many register for one operation at once! {regalloc} {lockedRegs}")

    regalloc.freeRegs.popMin()

proc prepareHostRead[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    iref: IrInstrRef,
    fn: IrFunc,
    lockedRegs: set[T] = {}): int =

    for i, reg in mpairs regalloc.activeRegs:
        if reg.val == iref:
            case reg.location
            of regLocHostGprReg, regLocHostGprRegImm:
                return i
            of regLocHostSpill:
                let
                    targetReg = allocHostReg(regalloc, s, fn, lockedRegs)
                    offset = getSpillOffset(reg.idx)
                when T is HostIRegRange:
                    if fn.getInstr(iref).kind in HasWideResult:
                        s.mov(targetReg.toReg64, mem64(regRsp, offset))
                    else:
                        s.mov(targetReg.toReg, mem32(regRsp, offset))
                else:
                    s.movapd(targetReg.toXmm, memXmm(regRsp, offset))
                regalloc.freeSpillLocs[].incl reg.idx
                reg.location = regLocHostGprReg
                reg.idx = targetReg
                return i

    if (let instr = fn.getInstr(iref); instr.kind == loadImmI):
        assert T isnot HostFRegRange

        let targetReg = allocHostReg(regalloc, s, fn, lockedRegs)

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
    fn: IrFunc,
    lockedRegs: set[T] = {}): int32 =
    regalloc.activeRegs[regalloc.prepareHostRead(s, src, fn, lockedRegs)].idx

proc allocOpW0R2[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    src0, src1: IrInstrRef,
    fn: IrFunc,
    lockedRegs: set[T] = {}): (T, T) =

    result[0] = allocOpW0R1(regalloc, s, src0, fn, lockedRegs)
    result[1] = allocOpW0R1(regalloc, s, src1, fn, lockedRegs + {result[0]})

proc allocOpW1R0[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst: IrInstrRef,
    fn: IrFunc,
    lockedRegs: set[T] = {}): T =
    let dstReg = allocHostReg(regalloc, s, fn, lockedRegs)
    regalloc.activeRegs.add(ActiveReg(location: regLocHostGprReg, val: dst, idx: dstReg))
    dstReg

proc allocOpW1R1[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst, src: IrInstrRef,
    instrIdx: int,
    fn: IrFunc): (T, T) =

    if fn.getInstr(src).lastRead == instrIdx:
        let srcLoc = prepareHostRead(regalloc, s, src, fn, {})
        # recycle register
        regalloc.activeRegs[srcLoc].location = regLocHostGprReg
        regalloc.activeRegs[srcLoc].val = dst
        result[0] = T(regalloc.activeRegs[srcLoc].idx)
        result[1] = result[0]
    else:
        result[0] = allocOpW1R0(regalloc, s, dst, fn)
        result[1] = allocOpW0R1(regalloc, s, src, fn, {result[0]})

proc allocOpW1R2[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst, src0, src1: IrInstrRef,
    instrIdx: int,
    fn: IrFunc,
    commutative = false,
    lockedRegs: set[T] = {}): (T, T, T) =

    if fn.getInstr(src0).lastRead == instrIdx:
        # recycle register
        let src0Loc = prepareHostRead(regalloc, s, src0, fn, lockedRegs)
        regalloc.activeRegs[src0Loc].location = regLocHostGprReg
        regalloc.activeRegs[src0Loc].val = dst
        result[0] = T(regalloc.activeRegs[src0Loc].idx)
        result[1] = result[0]
        if src0 != src1:
            result[2] = regalloc.allocOpW0R1(s, src1, fn, lockedRegs + {result[0]})
        else:
            result[2] = result[1]
    elif commutative and fn.getInstr(src1).lastRead == instrIdx:
        # recycle register
        let src1Loc = prepareHostRead(regalloc, s, src1, fn, lockedRegs)
        regalloc.activeRegs[src1Loc].location = regLocHostGprReg
        regalloc.activeRegs[src1Loc].val = dst
        assert src0 != src1
        result[0] = T(regalloc.activeRegs[src1Loc].idx)
        result[1] = regalloc.allocOpW0R1(s, src0, fn, lockedRegs + {result[0]})
        result[2] = result[0]
    else:
        result[0] = allocOpW1R0(regalloc, s, dst, fn, lockedRegs)
        result[1] = allocOpW0R1(regalloc, s, src0, fn, lockedRegs + {result[0]})
        result[2] = allocOpW0R1(regalloc, s, src1, fn, lockedRegs + {result[0], result[1]})

proc allocOpW1R3[T](regalloc: var RegAlloc[T], s: var AssemblerX64,
    dst, src0, src1, src2: IrInstrRef,
    instrIdx: int,
    fn: IrFunc,
    commutative = false): (T, T, T, T) =
    result[3] = allocOpW0R1(regalloc, s, src2, fn)
    (result[0], result[1], result[2]) = regalloc.allocOpW1R2(s, dst, src0, src1, instrIdx, fn, commutative, {result[3]})

proc freeExpiredRegs[T](regalloc: var RegAlloc[T], fn: IrFunc, pos: int) =
    var i = 0
    while i < regalloc.activeRegs.len:
        if fn.getInstr(regalloc.activeRegs[i].val).lastRead == pos:
            if regalloc.activeRegs[i].location in {regLocHostGprReg, regLocHostGprRegImm}:
                regalloc.freeRegs.incl regalloc.activeRegs[i].idx
            else:
                regalloc.freeSpillLocs[].incl regalloc.activeRegs[i].idx
            regalloc.activeRegs.del i
        else:
            i += 1

type PatchLoc = object
    replacement: int32
    startOffset, len: int16

var
    codeMemory {.align(0x1000).}: array[32*1024*1024, byte]
    assembler = initAssemblerX64(cast[ptr UncheckedArray[byte]](addr codeMemory[0]))
    assemblerFar = initAssemblerX64(cast[ptr UncheckedArray[byte]](addr codeMemory[len(codeMemory) div 2]))

    patches: Table[int, PatchLoc]

    ppcRun*: proc(state: var PpcState) {.cdecl.}
    ppcDone, ppcSliceDone: pointer

    dspRun*: proc(state: var DspState) {.cdecl.}
    dspDone: pointer

doAssert reprotectMemory(addr codeMemory[0], sizeof(codeMemory), {memperm_R, memperm_W, memperm_X})

proc jumpToNextBlockPpc(s: var AssemblerX64) =
    #s.int3()
    s.mov(reg(param1), rcpu)
    s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), Register32 ord(param2))
    s.call(gekkoblockcache.lookupBlockTranslateAddr)
    s.test(reg(regRax), regRax)
    let skipCompileBlock = s.jcc(condNotZero, false)
    s.call(compileBlockPpc)
    s.label(skipCompileBlock)
    s.jmp(reg(regRax))

proc jumpToNextBlockDsp(s: var AssemblerX64) =
    s.mov(mem16(rcpu, int32 offsetof(DspState, pc)), Register16 ord(param1))
    # giant hack
    s.mov(reg(Register32 ord(param1)), -1)
    s.call(handleLoopStack)
    s.mov(Register16 ord(param1), mem16(rcpu, int32 offsetof(DspState, pc)))
    s.call(dspblockcache.lookupBlock)
    s.test(reg(regRax), regRax)
    let skipCompileBlock = s.jcc(condNotZero, false)
    s.call(compileBlockDsp)
    s.label(skipCompileBlock)
    s.jmp(reg(regRax))


proc stackSetupEnter(s: var AssemblerX64, needFp: bool): int32 =
    for i in 0..<calleeSavedRegsNum:
        s.push(reg(Register64(registersToUse[i].ord)))
    s.push(reg(regRbp))
    s.push(reg(regR15))
    const stackAlignAdjustment = if ((calleeSavedRegsNum + 2) mod 2) == 0: 8 else: 0
    let stackOffset = int32(stackAlignAdjustment + (if needFp: calleeSavedXmmsNum*16 else: 0) + stackFrameSize)
    s.sub(reg(regRsp), stackOffset)
    if needFp:
        for i in 0..<calleeSavedXmmsNum:
            s.movaps(memXmm(regRsp, int32(stackFrameSize + i*16)), xmmsToUse[i])

    stackOffset

proc stackSetupLeave(s: var AssemblerX64, needFp: bool, stackOffset: int32) =
    if needFp:
        for i in 0..<calleeSavedXmmsNum:
            s.movaps(xmmsToUse[i], memXmm(regRsp, int32(stackFrameSize + i*16)))
    s.add(reg(regRsp), stackOffset)
    s.pop(reg(regR15))
    s.pop(reg(regRbp))
    for i in countdown(calleeSavedRegsNum-1, 0):
        s.pop(reg(Register64(registersToUse[i].ord)))
    s.ret()


proc genPpcStartup() =
    template s: untyped = assembler

    block:
        dspRun = s.getFuncStart[:typeof(dspRun)]()

        let stackOffset = stackSetupEnter(s, false)
        s.mov(reg(rcpu), param1)

        s.movzx(Register32 ord(param1), mem16(rcpu, int32 offsetof(DspState, pc)))
        s.jumpToNextBlockDsp()

        dspDone = s.getFuncStart[:pointer]()
        s.stackSetupLeave(false, stackOffset)

    block:
        ppcRun = s.getFuncStart[:typeof(ppcRun)]()

        let stackOffset = s.stackSetupEnter(true)
        s.mov(reg(rcpu), param1)
        s.mov(Register32 ord(param2), mem32(rcpu, int32 offsetof(PpcState, pc)))
        s.jumpToNextBlockPpc()

        ppcDone = s.getFuncStart[:pointer]()
        s.stackSetupLeave(true, stackOffset)

    block:
        ppcSliceDone = s.getFuncStart[:pointer]()
        s.call(systemStep)
        s.test(reg(regEax), regEax)
        let doneForever = s.jcc(condZero, false)
        s.mov(reg(param1), rcpu)
        s.call(ppccommon.handleExceptions)
        s.mov(Register32 ord(param2), mem32(rcpu, int32 offsetof(PpcState, pc)))
        s.jumpToNextBlockPpc()
        s.label(doneForever)
        s.jmp(ppcDone)

genPpcStartup()

proc handleSegfault*(faultAdr: var pointer): bool =
    #echo "trying to patch segfault..."
    patches.withValue(cast[int](faultAdr), patch):
        #echo "patching after segfault ", patch[]
        var s = initAssemblerX64(cast[ptr UncheckedArray[byte]](cast[int](faultAdr)+patch.startOffset))
        s.jmp(cast[pointer](addr codeMemory[patch.replacement]))
        s.nop(patch.len-5)
        assert patch.len >= 5
        faultAdr = cast[pointer](cast[int](faultAdr) + patch.startOffset)
        return true
    false

template patchableLoadStore(genInline, genPatch: untyped): untyped =
    let start = s.getFuncStart[:int]()
    var memAccess = start
    template memAccessPos: untyped {.inject, used.} =
        memAccess = s.getFuncStart[:int]()
    block:
        template smem: untyped {.inject.} = s
        genInline
    let
        endLoc = s.getFuncStart[:int]()
        len = endLoc-start
    assert len >= 5
    patches[memAccess] = PatchLoc(
        replacement: int32(sfar.getFuncStart[:int]()-cast[int](addr codeMemory)),
        startOffset: int16(start-memAccess),
        len: int16(len))

    let
        callerSavedRegs = regalloc.getCallerSavedRegs(sfar, iref)
        callerSavedXmms = xmmRegalloc.getCallerSavedRegs(sfar, iref)

    sfar.mov(reg(param1), rcpu)
    doCall(sfar, callerSavedRegs, callerSavedXmms):
        template smem: untyped {.inject.} = sfar
        genPatch

    sfar.jmp(cast[pointer](endLoc))

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

proc genCode*(fn: IrFunc, dataAdrSpace = pointer(nil), entryPoints: var seq[pointer]) =
    template s: untyped = assembler
    template sfar: untyped = assemblerFar

    if (sizeof(codeMemory) div 2) - s.offset < 64*1024:
        clearBlockCache()
        s.offset = 0

    var
        forwardLabels: seq[(ForwardsLabel, IrBasicBlock)]
        backwardsLabels: seq[(BackwardsLabel, IrBasicBlock)]

    for blkIdx in 0..<fn.blocks.len:
        let blk = fn.blocks[blkIdx]

        entryPoints.add s.getFuncStart[:pointer]()

        backwardsLabels.add (s.label(), blk)
        block:
            var i = 0
            while i < forwardLabels.len:
                let label = forwardLabels[i]
                if label[1] == blk:
                    s.label label[0]
                    forwardLabels.del i
                else:
                    i += 1

        var
            freeSpillLocs = fullSet(range[0..maxSpill-1])
            regalloc = initRegAlloc[HostIRegRange](addr freeSpillLocs)
            xmmRegalloc = initRegAlloc[HostFRegRange](addr freeSpillLocs)
            flagstate = flagStateUnknown
            flagstateL, flagstateR: IrInstrRef

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

        if dataAdrSpace != nil:
            s.mov(rmemStart, cast[int64](dataAdrSpace))

        for i in 0..<blk.instrs.len:
            let
                iref = blk.instrs[i]
                instr = fn.getInstr(iref)

            template beforeCall =
                regalloc.prepareCall(s(), fn, iref)
                xmmRegAlloc.prepareCall(s(), fn, iref)

            #echo &"processing instr {i} {regalloc}"

            case instr.kind
            of identity, extractBit, mergeBit, extractLo..mergeMidHi:
                raiseAssert(&"should have been lowered {instr.kind}")
            of loadImmI:
                discard
            of ctxLoad8, ctxLoad16, ctxLoadU32, ctxLoadS32, ctxLoad64:
                let dst = regalloc.allocOpW1R0(s, iref, fn)
                case instr.kind
                of ctxLoad8: s.movzx(dst.toReg, mem8(rcpu, int32 instr.ctxOffset))
                of ctxLoad16: s.movzx(dst.toReg, mem16(rcpu, int32 instr.ctxOffset))
                of ctxLoadU32: s.mov(dst.toReg, mem32(rcpu, int32 instr.ctxOffset))
                of ctxLoadS32: s.movsxd(dst.toReg64, mem32(rcpu, int32 instr.ctxOffset))
                of ctxLoad64: s.mov(dst.toReg64, mem64(rcpu, int32 instr.ctxOffset))
                else: raiseAssert("should not happen")
            of ctxLoadFpr, ctxLoadFprPair:
                let dst = xmmRegalloc.allocOpW1R0(s, iref, fn)
                case instr.kind
                of ctxLoadFpr: s.movsd(dst.toXmm, memXmm(rcpu, int32 instr.ctxOffset))
                of ctxLoadFprPair: s.movapd(dst.toXmm, memXmm(rcpu, int32 instr.ctxOffset))
                else: raiseAssert("should not happen")
            of ctxStore8, ctxStore16, ctxStore32, ctxStore64:
                let src = regalloc.allocOpW0R1(s, instr.source(0), fn)
                case instr.kind
                of ctxStore8: s.mov(mem8(rcpu, int32 instr.ctxOffset), Register8 src.toReg.ord)
                of ctxStore16: s.mov(mem16(rcpu, int32 instr.ctxOffset), Register16 src.toReg.ord)
                of ctxStore32: s.mov(mem32(rcpu, int32 instr.ctxOffset), src.toReg)
                of ctxStore64: s.mov(mem64(rcpu, int32 instr.ctxOffset), src.toReg64)
                else: raiseAssert("should not happen")
            of ctxStoreFpr, ctxStoreFprPair:
                let src = xmmRegalloc.allocOpW0R1(s, instr.source(0), fn)
                case instr.kind
                of ctxStoreFpr: s.movsd(memXmm(rcpu, int32 instr.ctxOffset), src.toXmm)
                of ctxStoreFprPair: s.movapd(memXmm(rcpu, int32 instr.ctxOffset), src.toXmm)
                else: raiseAssert("should not happen")
            of sprLoad32:
                let dst = regalloc.allocOpW1R0(s, iref, fn)
                s.mov(reg(param1), rcpu)
                beforeCall()
                case instr.spr
                of tbL, tbU:
                    s.call(currentTb)
                    if instr.spr == tbU:
                        s.sshr(reg(regRax), 32)
                of decrementer:
                    s.call(getDecrementer)
                else:
                    raiseAssert("shouldn't need this code path")
                s.mov(dst.toReg, reg(regEax))
                setFlagUnk()
            of sprStore32:
                let src = regalloc.allocOpW0R1(s, instr.source(0), fn)
                s.mov(reg(param1), rcpu)
                s.mov(reg(Register32 param2.ord), src.toReg)
                beforeCall()
                case instr.spr
                of tbL: s.call(setTbl)
                of tbU: s.call(setTbu)
                of decrementer: s.call(setupDecrementer)
                of wpar: s.call(setWpar)
                of dmaL: s.call(setDmaL)
                of hid0: s.call(setHid0)
                of iBatLo0..iBatLo3:
                    s.mov(reg(param3), int32 instr.spr.ord-iBatLo0.ord)
                    s.call(setIBatLo)
                of iBatHi0..iBatHi3:
                    s.mov(reg(param3), int32 instr.spr.ord-iBatHi0.ord)
                    s.call(setIBatHi)
                of dBatLo0..dBatLo3:
                    s.mov(reg(param3), int32 instr.spr.ord-dBatLo0.ord)
                    s.call(setDBatLo)
                of dBatHi0..dBatHi3:
                    s.mov(reg(param3), int32 instr.spr.ord-dBatHi0.ord)
                    s.call(setDBatHi)
                else: raiseAssert(&"cannot generate spr store for {instr.spr}")
                setFlagUnk()
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
            of csel:
                let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
                s.test(reg(src2.toReg), src2.toReg)
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.cmov(dst.toReg, reg(src1.toReg), condZero)
                setFlagCmpZero(instr.source(2))
            of cselX:
                let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
                s.test(reg(src2.toReg64), src2.toReg64)
                if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)
                s.cmov(dst.toReg64, reg(src1.toReg64), condZero)
                setFlagUnk()
            of iAdd, bitAnd, bitOr, bitXor, InstrKind.iMul:
                var
                    comparesToZero = instr.kind in {bitAnd, bitOr, bitXor}
                    destroysFlags = not comparesToZero
                if (let imm = fn.isEitherImmI(instr.source(0), instr.source(1)); imm.isSome):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, imm.get[0], i, fn)

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
                        of InstrKind.iMul: s.imul(dst.toReg, reg(src.toReg), cast[int32](imm.get[1]))
                        else: raiseAssert("shouldn't happen")
                else:
                    var
                        comparesToZero = instr.kind in {bitAnd, bitOr, bitXor}
                    let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn, true)
                    if dst == src1:
                        case instr.kind
                        of iAdd: s.add(reg(dst.toReg), src0.toReg)
                        of bitAnd: s.aand(reg(dst.toReg), src0.toReg)
                        of bitOr: s.oor(reg(dst.toReg), src0.toReg)
                        of bitXor: s.xxor(reg(dst.toReg), src0.toReg)
                        of InstrKind.iMul: s.imul(dst.toReg, reg(src0.toReg))
                        else: raiseAssert("shouldn't happen")
                    else:
                        if dst != src0: s.mov(reg(dst.toReg), src0.toReg)

                        case instr.kind
                        of iAdd: s.add(reg(dst.toReg), src1.toReg)
                        of bitAnd: s.aand(reg(dst.toReg), src1.toReg)
                        of bitOr: s.oor(reg(dst.toReg), src1.toReg)
                        of bitXor: s.xxor(reg(dst.toReg), src1.toReg)
                        of InstrKind.iMul: s.imul(dst.toReg, reg(src1.toReg))
                        else: raiseAssert("shouldn't happen")

                    if comparesToZero:
                        setFlagCmpZero(iref)
                    elif destroysFlags:
                        setFlagUnk()
            of iAddX, bitAndX, bitOrX, bitXorX, iMulX:
                if (let imm = fn.isEitherImmIX(instr.source(0), instr.source(1)); imm.isSome and cast[int32](imm.get[1]) == cast[int64](imm.get[1])):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, imm.get[0], i, fn)

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
                        of iMulX: s.imul(dst.toReg64, reg(src.toReg64), cast[int32](imm.get[1]))
                        else: raiseAssert("shouldn't happen")
                else:
                    let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn, true)
                    if dst == src1:
                        case instr.kind
                        of iAddX: s.add(reg(dst.toReg64), src0.toReg64)
                        of bitAndX: s.aand(reg(dst.toReg64), src0.toReg64)
                        of bitOrX: s.oor(reg(dst.toReg64), src0.toReg64)
                        of bitXorX: s.xxor(reg(dst.toReg64), src0.toReg64)
                        of iMulX: s.imul(dst.toReg64, reg(src0.toReg64))
                        else: raiseAssert("shouldn't happen")
                    else:
                        if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)

                        case instr.kind
                        of iAddX: s.add(reg(dst.toReg64), src1.toReg64)
                        of bitAndX: s.aand(reg(dst.toReg64), src1.toReg64)
                        of bitOrX: s.oor(reg(dst.toReg64), src1.toReg64)
                        of bitXorX: s.xxor(reg(dst.toReg64), src1.toReg64)
                        of iMulX: s.imul(dst.toReg64, reg(src1.toReg64))
                        else: raiseAssert("shouldn't happen")

                setFlagUnk()
            of iSub:
                if (let imm = fn.isImmValI(instr.source(1)); imm.isSome):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                    if dst != src: s.mov(reg(dst.toReg), src.toReg)
                    s.sub(reg(dst.toReg), cast[int32](imm.get))
                elif fn.isImmVal(instr.source(0), 0):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(1), i, fn)
                    if dst != src: s.mov(reg(dst.toReg), src.toReg)
                    s.neg(reg(dst.toReg))
                else:
                    let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                    if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                    s.sub(reg(dst.toReg), src1.toReg)
                setFlagCmp(instr.source(0), instr.source(1))
            of iSubX:
                if (let imm = fn.isImmValIX(instr.source(1)); imm.isSome and cast[int32](imm.get) == cast[int64](imm.get)):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                    if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                    s.sub(reg(dst.toReg64), cast[int32](imm.get))
                elif fn.isImmValX(instr.source(0), 0):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(1), i, fn)
                    if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                    s.neg(reg(dst.toReg64))
                else:
                    let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                    if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)
                    s.sub(reg(dst.toReg64), src1.toReg64)
                setFlagUnk()
            of iAddExtended:
                let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
                s.bt(reg(src2.toReg), 0)
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.adc(reg(dst.toReg), src1.toReg)
                setFlagUnk()
            of iSubExtended:
                let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
                s.bt(reg(src2.toReg), 0)
                s.cmc()
                if dst != src0: s.mov(reg(dst.toReg), src0.toReg)
                s.sbb(reg(dst.toReg), src1.toReg)
                setFlagUnk()
            of iMulhS, iMulhU, iDivS, iDivU:
                let
                    isDivide = instr.kind in {iDivS, iDivU}
                    (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn, not isDivide)
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
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                s.nnot(reg(dst.toReg))
                setFlagCmpZero(iref)
            of bitNotX:
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                s.nnot(reg(dst.toReg64))
                setFlagUnk()
            of InstrKind.rol, lsl, lsr, asr:
                if (let immShift = fn.isImmValI(instr.source(1)); immShift.isSome()):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                    if dst != src: s.mov(reg(dst.toReg), src.toReg)
                    case instr.kind
                    of lsl: s.sshl(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                    of lsr: s.sshr(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                    of asr: s.sar(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                    of InstrKind.rol: s.rol(reg(dst.toReg), int8(immShift.get and 0x1F'u32))
                    else: raiseAssert("shouldn't happen")
                else:
                    let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
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
                if (let immShift = fn.isImmValI(instr.source(1)); immShift.isSome()):
                    let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                    if dst != src: s.mov(reg(dst.toReg64), src.toReg64)
                    case instr.kind
                    of lslX: s.sshl(reg(dst.toReg64), int8(immShift.get and 0x3F'u32))
                    of lsrX: s.sshr(reg(dst.toReg64), int8(immShift.get and 0x3F'u32))
                    of asrX: s.sar(reg(dst.toReg64), int8(immShift.get and 0x3F'u32))
                    else: raiseAssert("shouldn't happen")
                else:
                    let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                    if dst != src0: s.mov(reg(dst.toReg64), src0.toReg64)
                    s.mov(reg(regEcx), src1.toReg)
                    case instr.kind
                    of lslX: s.sshl(reg(dst.toReg64))
                    of lsrX: s.sshr(reg(dst.toReg64))
                    of asrX: s.sar(reg(dst.toReg64))
                    else: raiseAssert("shouldn't happen")
                setFlagUnk()
            of clz:
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.bsr(regEax, reg(src.toReg))
                s.mov(reg(dst.toReg), 32 xor 0x1F)
                s.cmov(dst.toReg, reg(regEax), condNotZero)
                s.xxor(reg(dst.toReg), 0x1F)
                setFlagUnk()
            of extsb, extsh, extsbX, extshX, extswX, extzwX:
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                case instr.kind
                of extsb: s.movsx(dst.toReg, reg(Register8(src.toReg.ord)))
                of extsh: s.movsx(dst.toReg, reg(Register16(src.toReg.ord)))
                of extsbX: s.movsx(dst.toReg64, reg(Register8(src.toReg.ord)))
                of extshX: s.movsx(dst.toReg64, reg(Register16(src.toReg.ord)))
                of extswX: s.movsxd(dst.toReg64, reg(src.toReg))
                of extzwX: s.mov(dst.toReg, reg(src.toReg))
                else: raiseAssert("shouldn't happen")
            of condAnd, condOr, condXor:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn, true)
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
                let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.mov(reg(dst.toReg), src.toReg)
                s.xxor(reg(dst.toReg), 1)
                setFlagUnk()
            of overflowAdd:
                raiseAssert("unimplemented code gen")
            of overflowSub:
                raiseAssert("unimplemented code gen")
            of overflowAddX:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                s.mov(reg(regRax), src0.toReg64)
                s.add(reg(regRax), src1.toReg64)
                s.setcc(reg(Register8(dst.toReg.ord)), condOverflow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
                setFlagUnk()
            of overflowSubX:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                s.mov(reg(regRax), src0.toReg64)
                s.sub(reg(regRax), src1.toReg64)
                s.setcc(reg(Register8(dst.toReg.ord)), condOverflow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
                setFlagUnk()
            of carryAdd:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                s.mov(reg(regEax), src0.toReg)
                s.add(reg(regEax), src1.toReg)
                s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
                setFlagUnk()
            of carryAddX:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                s.mov(reg(regRax), src0.toReg64)
                s.add(reg(regRax), src1.toReg64)
                s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
                setFlagUnk()
            of carrySub:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                s.cmp(reg(src0.toReg), src1.toReg)
                s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
                setFlagUnk()
            of carrySubX:
                let (dst, src0, src1) = regalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                s.cmp(reg(src0.toReg64), src1.toReg64)
                s.setcc(reg(Register8(dst.toReg.ord)), condNotBelow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
                setFlagUnk()
            of overflowAddExtended:
                raiseAssert("unimplemented code gen")
            of overflowSubExtended:
                raiseAssert("unimplemented code gen")
            of carryAddExtended:
                let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
                s.bt(reg(src2.toReg), 0)
                s.mov(reg(regEax), src0.toReg)
                s.adc(reg(regEax), src1.toReg)
                s.setcc(reg(Register8(dst.toReg.ord)), condBelow)
                s.movzx(dst.toReg, reg(Register8(dst.toReg.ord)))
            of carrySubExtended:
                let (dst, src0, src1, src2) = regalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
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
                        regalloc.allocOpW1R0(s, iref, fn)
                    elif (let imm = fn.isImmValI(instr.source(1)); imm.isSome):
                        if imm.get == 0 and flagstate == flagStateCmpZero and flagstateL == instr.source(0):
                            regalloc.allocOpW1R0(s, iref, fn)
                        else:
                            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                            s.cmp(reg(src.toReg), cast[int32](imm.get))
                            if imm.get == 0:
                                setFlagCmpZero(instr.source(0))
                            else:
                                setFlagCmp(instr.source(0), instr.source(1))
                            dst
                    else:
                        let (dst, src0, src1) =
                            regalloc.allocOpW1R2(assembler, iref, instr.source(0), instr.source(1), i, fn)
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
                        regalloc.allocOpW1R0(s, iref, fn)
                    elif (let imm = fn.isImmValIX(instr.source(1)); imm.isSome and cast[int32](imm.get) == cast[int64](imm.get)):
                        if imm.get == 0 and flagstate == flagStateCmpZeroX and flagstateL == instr.source(0):
                            regalloc.allocOpW1R0(s, iref, fn)
                        else:
                            let (dst, src) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                            s.cmp(reg(src.toReg64), cast[int32](imm.get))
                            if imm.get == 0:
                                setFlagCmpZeroX(instr.source(0))
                            else:
                                setFlagCmpX(instr.source(0), instr.source(1))
                            dst
                    else:
                        let (dst, src0, src1) =
                            regalloc.allocOpW1R2(assembler, iref, instr.source(0), instr.source(1), i, fn)
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
            of ppcLoadU8, ppcLoadU16, ppcLoad32:
                let (dst, adr) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)

                patchableLoadStore:
                    case instr.kind
                    of ppcLoad32:
                        smem.movbe(dst.toReg, memMemOnly(rmemStart, adr.toReg64))
                    of ppcLoadU16:
                        smem.movbe(Register16 dst.toReg.ord, memMemOnly(rmemStart, adr.toReg64))
                        smem.movzx(dst.toReg, reg(Register16 dst.toReg.ord))
                    of ppcLoadU8:
                        smem.movzx(dst.toReg, mem8(rmemStart, adr.toReg64))
                    else: raiseAssert("welp")
                do:
                    smem.mov(reg(Register32(param2.ord)), adr.toReg)
                    case instr.kind
                    of ppcLoad32:
                        smem.call(jitReadMemory[uint32])
                        smem.mov(reg(dst.toReg), regEax)
                    of ppcLoadU16:
                        smem.call(jitReadMemory[uint16])
                        smem.movzx(dst.toReg, reg(regAx))
                    of ppcLoadU8:
                        smem.call(jitReadMemory[uint8])
                        smem.movzx(dst.toReg, reg(regAl))
                    else: raiseAssert("welp")
            of ppcStore8, ppcStore16, ppcStore32:
                let (adr, val) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), fn)

                patchableLoadStore:
                    case instr.kind
                    of ppcStore32:
                        smem.movbe(memMemOnly(rmemStart, adr.toReg64), val.toReg)
                    of ppcStore16:
                        smem.movbe(memMemOnly(rmemStart, adr.toReg64), Register16(val.toReg.ord))
                    of ppcStore8:
                        smem.mov(mem8(rmemStart, adr.toReg64), Register8(val.toReg.ord))
                        smem.nop(1) # super stupid, but we need atleast 5 bytes to patch in a call instruction here
                    else: raiseAssert("welp")
                do:
                    smem.mov(reg(Register32(param2.ord)), adr.toReg)
                    case instr.kind
                    of ppcStore32:
                        smem.mov(reg(Register32(param3.ord)), val.toReg)
                        smem.call(jitWriteMemory[uint32])
                    of ppcStore16:
                        smem.movzx(Register32(param3.ord), reg(Register16(val.toReg.ord)))
                        smem.call(jitWriteMemory[uint16])
                    of ppcStore8:
                        smem.movzx(Register32(param3.ord), reg(Register8(val.toReg.ord)))
                        smem.call(jitWriteMemory[uint8])
                    else: raiseAssert("welp")
            of ppcLoadFss, ppcLoadFsd:
                let
                    dst = xmmRegalloc.allocOpW1R0(s, iref, fn)
                    adr = regalloc.allocOpW0R1(s, instr.source(0), fn)

                patchableLoadStore:
                    case instr.kind
                    of ppcLoadFss:
                        smem.movbe(regEax, memMemOnly(rmemStart, adr.toReg64))
                        smem.movd(dst.toXmm, reg(regEax))
                    of ppcLoadFsd:
                        smem.movbe(regRax, memMemOnly(rmemStart, adr.toReg64))
                        smem.movq(dst.toXmm, reg(regRax))
                    else: raiseAssert("welp")
                do:
                    smem.mov(reg(Register32(param2.ord)), adr.toReg)
                    case instr.kind
                    of ppcLoadFss:
                        smem.call(jitReadMemory[uint32])
                        smem.movd(dst.toXmm, reg(regEax))
                    of ppcLoadFsd:
                        smem.call(jitReadMemory[uint64])
                        smem.movq(dst.toXmm, reg(regRax))
                    else: raiseAssert("welp")
            of ppcStoreFss, ppcStoreFsd:
                let
                    adr = regalloc.allocOpW0R1(s, instr.source(0), fn)
                    val = xmmRegalloc.allocOpW0R1(s, instr.source(1), fn)

                patchableLoadStore:
                    case instr.kind
                    of ppcStoreFss:
                        smem.movd(reg(regEax), val.toXmm)
                        memAccessPos
                        smem.movbe(memMemOnly(rmemStart, adr.toReg64), regEax)
                    of ppcStoreFsd:
                        smem.movq(reg(regRax), val.toXmm)
                        memAccessPos
                        smem.movbe(memMemOnly(rmemStart, adr.toReg64), regRax)
                    else: raiseAssert("welp")
                do:
                    smem.mov(reg(Register32(param2.ord)), adr.toReg)
                    case instr.kind
                    of ppcStoreFss:
                        smem.movd(reg(Register32(param3.ord)), val.toXmm)
                        smem.call(jitWriteMemory[uint32])
                    of ppcStoreFsd:
                        smem.movq(reg(param3), val.toXmm)
                        smem.call(jitWriteMemory[uint64])
                    else: raiseAssert("welp")
            of ppcLoadFsq, ppcLoadFpq:
                let
                    dst = xmmRegalloc.allocOpW1R0(s, iref, fn)
                    # the gqr register is currently wasted
                    (adr, _) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), fn)
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
                    src = xmmRegalloc.allocOpW0R1(s, instr.source(1), fn)
                    (adr, _) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(2), fn)
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
            of dspLoadIMem, dspLoadDMem:
                let (dst, adr) = regalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.movzx(Register32(param1.ord), reg(Register16(adr.toReg.ord)))
                beforeCall()
                case instr.kind
                of dspLoadIMem:
                    s.call(dsp.instrRead)
                of dspLoadDMem:
                    s.call(dsp.dataRead)
                else: raiseAssert("welp")
                s.movzx(dst.toReg, reg(regAx))
                setFlagUnk()
            of dspStoreDMem:
                let (adr, val) = regalloc.allocOpW0R2(s, instr.source(0), instr.source(1), fn)
                beforeCall()
                s.movzx(Register32(param1.ord), reg(Register16(adr.toReg.ord)))
                s.movzx(Register32(param2.ord), reg(Register16(val.toReg.ord)))
                s.call(dsp.dataWrite)
                setFlagUnk()
            of fSwizzleD00:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.movddup(dst.toXmm, reg(src.toXmm))
            of fSwizzleD11:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
                s.unpckhpd(dst.toXmm, reg(dst.toXmm))
            of fSwizzleS00:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.movsldup(dst.toXmm, reg(src.toXmm))
            of fSwizzleS11:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.movshdup(dst.toXmm, reg(src.toXmm))
            of fMergeS00, fMergeS11:
                let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
                if instr.kind == fMergeS00:
                    s.unpcklps(dst.toXmm, reg(src1.toXmm))
                elif instr.kind == fMergeS11:
                    s.unpckhps(dst.toXmm, reg(src1.toXmm))
            of fMergeS01:
                # the operands are flipped intentionally
                let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(1), instr.source(0), i, fn)
                if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
                s.movss(dst.toXmm, reg(src1.toXmm))
            of fMergeS10:
                # unfortunately there is single instruction to do this on x64 :(
                # so what we do instead is we merge the lower half of the second operand into the first operand
                # and then we only need to swap the lower two singles
                let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
                if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
                s.movss(dst.toXmm, reg(src1.toXmm))
                s.shufps(dst.toXmm, reg(dst.toXmm), 1) # swap the lower two floats
            of fMergeD00, fMergeD01, fMergeD10, fMergeD11:
                let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
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
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                case instr.kind
                of InstrKind.cvtsd2ss: s.cvtsd2ss(dst.toXmm, reg(src.toXmm))
                of InstrKind.cvtss2sd: s.cvtss2sd(dst.toXmm, reg(src.toXmm))
                of InstrKind.cvtpd2ps: s.cvtpd2ps(dst.toXmm, reg(src.toXmm))
                of InstrKind.cvtps2pd: s.cvtps2pd(dst.toXmm, reg(src.toXmm))
                else: raiseAssert("shouldn't happen")
            # todo do something faster for those:
            of fRessd:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src:
                    s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivsd(dst.toXmm, reg(src.toXmm))
                else:
                    s.movsd(regXmm0, reg(src.toXmm))
                    s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivsd(dst.toXmm, reg(regXmm0))
            of fResss:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src:
                    s.movss(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivss(dst.toXmm, reg(src.toXmm))
                else:
                    s.movss(regXmm0, reg(src.toXmm))
                    s.movss(dst.toXmm, memXmm(unsafeAddr singlesOne[0]))
                    s.ddivss(dst.toXmm, reg(regXmm0))
            of fRsqrtsd:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.sqrtsd(regXmm0, reg(src.toXmm))
                s.movsd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivsd(dst.toXmm, reg(regXmm0))
            of fRsqrtss:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.sqrtss(regXmm0, reg(src.toXmm))
                s.movss(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivss(dst.toXmm, reg(regXmm0))
            of fRespd:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src:
                    s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivpd(dst.toXmm, reg(src.toXmm))
                else:
                    s.movapd(regXmm0, reg(src.toXmm))
                    s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivpd(dst.toXmm, reg(regXmm0))
            of fResps:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src:
                    s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivps(dst.toXmm, reg(src.toXmm))
                else:
                    s.movaps(regXmm0, reg(src.toXmm))
                    s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                    s.ddivps(dst.toXmm, reg(regXmm0))
            of fRsqrtpd:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.sqrtpd(regXmm0, reg(src.toXmm))
                s.movapd(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivpd(dst.toXmm, reg(regXmm0))
            of fRsqrtps:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                s.sqrtps(regXmm0, reg(src.toXmm))
                s.movaps(dst.toXmm, memXmm(unsafeAddr doublesOne[0]))
                s.ddivps(dst.toXmm, reg(regXmm0))
            of fNegsd, fNegpd:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
                s.xxorpd(dst.toXmm, memXmm(
                    if instr.kind == fNegsd:
                        unsafeAddr doubleSignMask[0]
                    else:
                        unsafeAddr doubleSignMaskPair[0]))
            of fNegss, fNegps:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
                s.xxorps(dst.toXmm, memXmm(
                    if instr.kind == fNegss:
                        unsafeAddr singleSignMask[0]
                    else:
                        unsafeAddr singleSignMaskPair[0]))
            of fAbssd, fAbspd:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if dst != src: s.movapd(dst.toXmm, reg(src.toXmm))
                s.aandpd(dst.toXmm, memXmm(
                    if instr.kind == fAbssd:
                        unsafeAddr doubleSignMaskInv[0]
                    else:
                        unsafeAddr doubleSignMaskPairInv[0]))
            of fAbsss, fAbsps:
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
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
                let (dst, src0, src1) = xmmRegalloc.allocOpW1R2(s, iref, instr.source(0), instr.source(1), i, fn)
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
                let (dst, src0, src1, src2) = xmmRegalloc.allocOpW1R3(s, iref, instr.source(0), instr.source(1), instr.source(2), i, fn)
                if dst != src0: s.movapd(dst.toXmm, reg(src0.toXmm))
                case instr.kind:
                of fMaddsd:
                    s.vfmadd132sd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMsubsd:
                    s.vfmsub132sd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                #[
                    Annoyingly with x64 negated multiply-add is defined as
                        -(a * c) + b
                    while PowerPC defines it as:
                        -((a * c) + b)

                    and negated multiply-sub as:
                        -(a * c) - b
                    while PowerPC defines it as:
                        -((a * c) - b)

                    By the laws of algebra we can deduce that
                        x64 nmadd <=> PowerPC nmsub and
                        x64 nmsub <=> PowerPC

                    but this is floating point math, so algebra doesn't apply here!

                    For once, both negated madd and msub should result -0 in the PowerPC variant.
                    For x64 it is only nmsub which has this.

                    We could implement nmadd by using madd and then negating the result.
                    Unfortunately this would also flip the sign bit of NaN values, which should not occur!
                    Also the NaN propagation is wrong anyway (a, b, c in PowerPC vs. a, c, b everywhere else).

                    So in the end it's all just a mess however we flip it.
                ]#
                of fNmaddsd:
                    s.vfnmsub132sd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmsubsd:
                    s.vfnmadd132sd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMaddpd:
                    s.vfmadd132pd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMsubpd:
                    s.vfmsub132pd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmaddpd:
                    s.vfnmsub132pd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmsubpd:
                    s.vfnmadd132pd(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMaddss:
                    s.vfmadd132ss(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMsubss:
                    s.vfmsub132ss(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmaddss:
                    s.vfnmsub132ss(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmsubss:
                    s.vfnmadd132ss(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMaddps:
                    s.vfmadd132ps(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fMsubps:
                    s.vfmsub132ps(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmaddps:
                    s.vfnmsub132ps(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                of fNmsubps:
                    s.vfnmadd132ps(dst.toXmm, src1.toXmm, reg(src2.toXmm))
                else: raiseAssert("shouldn't happen")
            of fCmpEqualsd, fCmpGreatersd, fCmpLesssd, fUnorderedsd:
                let
                    dst = regalloc.allocOpW1R0(s, iref, fn)
                    (src0, src1) = xmmRegalloc.allocOpW0R2(s, instr.source(0), instr.source(1), fn)
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
                let (dst, src) = xmmRegalloc.allocOpW1R1(s, iref, instr.source(0), i, fn)
                if instr.kind == cvtsd2intTrunc:
                    s.cvttpd2dq(dst.toXmm, reg(src.toXmm))
                else:
                    s.cvttps2dq(dst.toXmm, reg(src.toXmm))
            of funcInternBranchUncond:
                if blkIdx+1 == fn.blocks.len or fn.blocks[blkIdx+1] != instr.blockUncond:
                    block isBackwards:
                        for label in backwardsLabels:
                            if label[1] == instr.blockUncond:
                                s.jmp(label[0])
                                break isBackwards

                        forwardLabels.add((s.jmp(true), instr.blockUncond))
            of funcInternBranch:
                let cond = regalloc.allocOpW0R1(s, instr.source(0), fn)
                s.test(reg(cond.toReg), cond.toReg)

                if blkIdx+1 == fn.blocks.len or fn.blocks[blkIdx+1] != instr.blockTaken:
                    # see whether it is a backwards branch
                    block isBackwards:
                        for label in backwardsLabels:
                            if label[1] == instr.blockTaken:
                                s.jcc(condNotZero, label[0])
                                break isBackwards

                        forwardLabels.add((s.jcc(condNotZero, true), instr.blockTaken))
                if blkIdx+1 == fn.blocks.len or fn.blocks[blkIdx+1] != instr.blockNotTaken:
                    # see whether it is a backwards branch
                    block isBackwards:
                        for label in backwardsLabels:
                            if label[1] == instr.blockNotTaken:
                                s.jcc(condZero, label[0])
                                break isBackwards

                        forwardLabels.add((s.jcc(condZero, true), instr.blockNotTaken))
            of dispatchExternalPpc:
                let target = regalloc.allocOpW0R1(s, instr.source(0), fn)
                s.mov(reg(Register32 ord(param2)), target.toReg)
                s.jumpToNextBlockPpc()
            of dispatchExternalDsp:
                let target = regalloc.allocOpW0R1(s, instr.source(0), fn)
                s.mov(reg(Register32 ord(param1)), target.toReg)
                s.jumpToNextBlockDsp()
            of leaveJitPpc:
                s.jmp(ppcSliceDone)
            of leaveJitDsp:
                s.jmp(dspDone)
            of syscallPpc:
                let target = regalloc.allocOpW0R1(s, instr.source(0), fn)
                s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), target.toReg)
                s.mov(reg(param1), rcpu)
                s.call(systemCall)
                s.mov(reg(Register32 ord(param2)), regEax)
                s.jumpToNextBlockPpc()
            of fpExceptionPpc:
                let target = regalloc.allocOpW0R1(s, instr.source(0), fn)
                s.mov(mem32(rcpu, int32 offsetof(PpcState, pc)), target.toReg)
                s.mov(reg(param1), rcpu)
                s.call(handleFException)
                s.mov(reg(Register32 ord(param2)), regEax)
                s.jumpToNextBlockPpc()

            regalloc.freeExpiredRegs(fn, i)
            xmmRegalloc.freeExpiredRegs(fn, i)

    assert s.offset < sizeof(codeMemory)
