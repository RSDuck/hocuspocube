import
    stew/bitops2, strformat,

    ../../util/aluhelper,
    ppcinterpreter_aux,
    ../ppcstate,

    ../../cycletiming, ../memory, ../ppccommon,
    ../gekko # kind of stupid breaks loose coupling a bit

using state: var PpcState

template stubbedMemLog(msg: string): untyped =
    discard

template r(num: uint32): uint32 {.dirty.} = state.r[num]
#template fr(num: uint32): PairedSingle {.dirty.} = state.fr[num]

proc eieio*(state) =
    raiseAssert "instr not implemented eieio"

proc isync*(state) =
    # do something?
    discard

proc lwarx*(state; d, a, b: uint32) =
    raiseAssert "instr not implemented lwarx"

proc stwcxdot*(state; s, a, b: uint32) =
    raiseAssert "instr not implemented stwcxdot"

proc sync*(state) =
    # do something?
    discard

proc rfi*(state) =
    state.msr.exceptionSaved = state.srr1.exceptionSaved
    state.msr.pow = false
    state.pc = state.srr0 - 4

proc sc*(state) =
    #echo "system call"
    state.pendingExceptions.incl exceptionSystemCall

proc tw*(state; to, a, b: uint32) =
    raiseAssert "instr not implemented tw"

proc twi*(state; to, a, imm: uint32) =
    let simm = signExtend(imm, 16)
    # this is heavily todo:
    assert not(to.getBit(4) and cast[int32](r(a)) < cast[int32](simm))
    assert not(to.getBit(3) and cast[int32](r(a)) > cast[int32](simm))
    assert not(to.getBit(2) and r(a) == simm)
    assert not(to.getBit(1) and r(a) < simm), &"{r(a)} < {simm} {to:X}"
    assert not(to.getBit(0) and r(a) > simm)

proc mcrxr*(state; crfD: uint32) =
    raiseAssert "instr not implemented mcrxr"

proc mfcr*(state; d: uint32) =
    r(d) = uint32 state.cr

proc mfmsr*(state; d: uint32) =
    r(d) = uint32(state.msr)

proc mfspr*(state; d, spr: uint32) =
    let n = decodeSplitSpr spr

    r(d) = case n
        of 1: uint32(state.xer)
        of 8: uint32(state.lr)
        of 9: uint32(state.ctr)
        of 937..938: uint32(state.pmc[n - 937])
        of 941..942: uint32(state.pmc[n - 941 + 2])
        of 936: uint32(state.mmcr0) # UMMCR0
        of 940: uint32(state.mmcr1) # UMMCR1
        else:
            doAssert not state.msr.pr, "unprivileged spr access"
            case n
            of 18: state.dsisr
            of 19: state.dar
            of 22:
                state.getDecrementer()
            of 26: state.srr0
            of 27: uint32(state.srr1)
            of 272..275: state.sprg[n - 272]
            of 528..535:
                let n = n - 528
                if (n and 1) == 0:
                    uint32 state.ibatHi[n shr 1]
                else:
                    uint32 state.ibatLo[n shr 1]
            of 536..543:
                let n = n - 536
                if (n and 1) == 0:
                    uint32 state.dbatHi[n shr 1]
                else:
                    uint32 state.dbatLo[n shr 1]
            of 1008: uint32(state.hid0)
            of 1009: uint32(state.hid1)
            of 912..919: uint32(state.gqr[n - 912])
            of 920: uint32(state.hid2)
            of 921: uint32(state.wpar)
            of 922: uint32(state.dmaU)
            of 923: uint32(state.dmaL)
            of 1017: uint32(state.l2cr)
            of 952: uint32(state.mmcr0)
            of 956: uint32(state.mmcr1)
            of 953..954: uint32(state.pmc[n - 953])
            of 957..958: uint32(state.pmc[n - 957 + 2])
            else:
                raiseAssert &"unknown spr register {n}"

proc mftb*(state; d, tpr: uint32) =
    let n = decodeSplitSpr(tpr)

    r(d) = case n
        of 268: uint32(state.currentTb())
        of 269: uint32(state.currentTb() shr 32)
        else:
            raiseAssert &"unknown mftb register {n}"

proc mtcrf*(state; s, crm: uint32) =
    let mask = makeFieldMask(crm)
    state.cr = Cr(uint32(state.cr) and not(mask) or (r(s) and mask))

proc mtmsr*(state; s: uint32) =
    # TODO: mask this properly
    state.msr = Msr(r(s))

proc mcrfs*(state; crfD, crfS: uint32) =
    handleFloatException:
        state.cr.crf(int crfD, state.fpscr.crf(int crfS))
        state.fpscr.exceptionBit = state.fpscr.exceptionBit and not(0xF'u32 shl (7-crfS)*4)

proc mtspr*(state; d, spr: uint32) =
    # TODO: a ton of validation/masking misses here!
    let n = decodeSplitSpr spr

    case n
    of 1: state.xer.mutable = r(d)
    of 8: state.lr = r(d) and not(0x3'u32)
    of 9: state.ctr = r(d)
    else:
        # TODO: processor exception
        doAssert not state.msr.pr, "unprivileged spr access"
        case n
        of 18: state.dsisr = r(d)
        of 19: state.dar = r(d)
        of 22: state.setupDecrementer(r(d))
        of 26: state.srr0 = r(d) and not(0x3'u32)
        of 27: state.srr1 = Srr1 r(d)
        of 272..275: state.sprg[n - 272] = r(d)
        of 284:
            state.tbInit = (state.currentTb() and not(0xFFFFFFFF'u64)) or r(d)
            state.tbInitTimestamp = gekkoTimestamp
        of 285:
            state.tbInit = (state.currentTb() and 0xFFFFFFFF'u64) or (uint64(r(d)) shl 32)
            state.tbInitTimestamp = gekkoTimestamp
        of 528..535:
            let n = n - 528
            # TODO: validate ibats
            if (n and 1) == 0:
                state.ibatHi[n shr 1] = BatHi r(d)
            else:
                state.ibatLo[n shr 1] = BatLo r(d)
        of 536..543:
            let n = n - 536
            # TODO: validate dbats
            if (n and 1) == 0:
                state.dbatHi[n shr 1] = BatHi r(d)
            else:
                state.dbatLo[n shr 1] = BatLo r(d)
        of 1008:
            state.hid0 = Hid0 r(d)
            if state.hid0.icfi:
                state.hid0.icfi = false
                echo "flash icache"
                flashInvalidateICache()
            if state.hid0.dcfi:
                state.hid0.dcfi = false

        of 1009: state.hid1 = Hid1 r(d)
        of 912..919: state.gqr[n - 912] = Gqr r(d)
        of 920: state.hid2.mutable = r(d)
        of 921:
            state.gatherpipeOffset = 0
            state.wpar.gbAddr = r(d)
        of 922:
            state.dmaU = DmaU r(d)
        of 923:
            state.dmaL = DmaL r(d)

            if state.hid2.lce:
                if state.dmaL.flush:
                    # nothing too flush, because we're soo fast
                    state.dmaL.flush = false
                if state.dmaL.trigger:
                    # do DMA immediately!
                    state.dmaL.trigger = false

                    let cacheLines =
                        if state.dmaL.lenLo == 0 and state.dmaU.lenHi == 0:
                            128'u32
                        else:
                            state.dmaL.lenLo or (state.dmaU.lenHi shl 2)
                    echo &"dma {state.dmaL.load} lc: {state.dmaL.lcAdr:08X} mem: {state.dmaU.memAdr:08X} {cacheLines} lines {gekkoState.pc:08X} {gekkoState.lr:08X}"

                    # we currently don't check if lcAdr is really in locked cache
                    # bad!
                    if state.dmaL.load:
                        for i in 0..<cacheLines*4:
                            state.writeMemory[:uint64](state.dmaL.lcAdr + i * 8, state.readMemory[:uint64](state.dmaU.memAdr + i * 8))
                    else:
                        for i in 0..<cacheLines*4:
                            state.writeMemory[:uint64](state.dmaU.memAdr + i * 8, state.readMemory[:uint64](state.dmaL.lcAdr + i * 8))
        of 1017: state.l2cr = L2Cr r(d)
        of 952: state.mmcr0 = Mmcr0 r(d)
        of 956: state.mmcr1 = Mmcr1 r(d)
        of 953..954: state.pmc[n - 953] = Pmc r(d)
        of 957..958: state.pmc[n - 957 + 2] = Pmc r(d)
        else:
            raiseAssert &"unknown spr register {n}"

proc dcbf*(state; a, b: uint32) =
    stubbedMemLog "dcbf stubbed"

proc dcbi*(state; a, b: uint32) =
    stubbedMemLog "dcbi stubbed"

proc dcbst*(state; a, b: uint32) =
    stubbedMemLog "dcbst stubbed"

proc dcbt*(state; a, b: uint32) =
    stubbedMemLog "dcbt stubbed"

proc dcbtst*(state; a, b: uint32) =
    stubbedMemLog "dcbst stubbed"

proc mfsr*(state; d, sr: uint32) =
    raiseAssert "instr not implemented mfsr"

proc mfsrin*(state; d, b: uint32) =
    raiseAssert "instr not implemented mfsrin"

proc mtsr*(state; s, sr: uint32) =
    echo "mtsr instruction stubbed"

proc mtsrin*(state; s, b: uint32) =
    raiseAssert "instr not implemented mtsrin"

proc tlbie*(state; b: uint32) =
    raiseAssert "instr not implemented tlbie"

proc tlbsync*(state) =
    raiseAssert "instr not implemented tlbsync"

proc eciwx*(state; d, a, b: uint32) =
    raiseAssert "instr not implemented eciwx"

proc ecowx*(state; s, a, b: uint32) =
    raiseAssert "instr not implemented ecowx"
