import
    stew/bitops2,
    strformat,

    ppcinterpreter_aux,
    ../ppcstate

using state: var PpcState

template r(num: uint32): uint32 {.dirty.} = state.r[num]
#template fr(num: uint32): PairedSingle {.dirty.} = state.fr[num]

proc eieio*(state) =
    doAssert false, "instr not implemented"

proc isync*(state) =
    # do something?
    discard

proc lwarx*(state; d, a, b: uint32) =
    doAssert false, "instr not implemented"

proc stwcxdot*(state; s, a, b: uint32) =
    doAssert false, "instr not implemented"

proc sync*(state) =
    # do something?
    discard

proc rfi*(state) =
    state.msr.exceptionSaved = state.srr1.exceptionSaved
    state.msr.pow = false
    state.pc = state.srr0 - 4    

proc sc*(state) =
    echo "system call"
    state.pendingExceptions.incl exceptionSystemCall

proc tw*(state; to, a, b: uint32) =
    doAssert false, "instr not implemented"

proc twi*(state; to, a, imm: uint32) =
    doAssert false, "instr not implemented"

proc mcrxr*(state; crfS: uint32) =
    doAssert false, "instr not implemented"

proc mfcr*(state; d: uint32) =
    r(d) = uint32 state.cr

proc mfmsr*(state; d: uint32) =
    r(d) = uint32(state.msr)

proc decodeSplitSpr(spr: uint32): uint32 =
    ((spr and 0x1F) shl 5) or (spr shr 5)

proc mfspr*(state; d, spr: uint32) =
    let n = decodeSplitSpr spr

    r(d) = case n
        of 1: uint32(state.xer)
        of 8: uint32(state.lr)
        of 9: uint32(state.ctr)
        else:
            doAssert not state.msr.pr, "unprivileged spr access"
            case n
            of 18: state.dsisr
            of 19: state.dar
            of 22: state.dec
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
                    uint32 state.ibatHi[n shr 1]
                else:
                    uint32 state.ibatLo[n shr 1]
            of 1008: uint32(state.hid0)
            of 1009: uint32(state.hid1)
            of 912..919: uint32(state.gqr[n - 912])
            of 920: uint32(state.hid2)
            of 921: uint32(state.wpar)
            of 1017: uint32(state.l2cr)
            of 952: uint32(state.mmcr0)
            of 936: uint32(state.mmcr0) # UMMCR0
            of 956: uint32(state.mmcr1)
            of 940: uint32(state.mmcr1) # MMCR1
            of 953..954: uint32(state.pmc[n - 953])
            of 937..938: uint32(state.pmc[n - 937])
            of 957..958: uint32(state.pmc[n - 957])
            of 941..942: uint32(state.pmc[n - 941])
            else:
                doAssert false, &"unknown spr register {n}"
                0'u32

proc mftb*(state; d, tpr: uint32) =
    let n = decodeSplitSpr(tpr)

    r(d) = case n
        of 268: uint32(state.tb)
        of 269: uint32(state.tb shr 32)
        else:
            doAssert false, &"unknown mftb register {n}"
            0'u32

proc mtcrf*(state; s, crm: uint32) =
    let mask = makeFieldMask(crm)
    state.cr = Cr(uint32(state.cr) and not(mask) or (r(s) and mask))

proc mtmsr*(state; s: uint32) =
    # TODO: mask this properly
    state.msr = Msr(r(s))

proc mcrfs*(state; crfD, crfS: uint32) =
    state.cr.crf(int crfD, state.fpscr.crf(int crfS))
    state.fpscr.exceptionBit = state.fpscr.exceptionBit and not(0xF'u32 shl crfS*4)

proc mtspr*(state; d, spr: uint32) =
    # TODO: a ton of validation/masking misses here!
    let n = decodeSplitSpr spr

    case n
    of 1: state.xer = Xer(r(d))
    of 8: state.lr = r(d) and not(0x3'u32)
    of 9: state.ctr = r(d)
    else:
        # TODO: processor exception
        doAssert not state.msr.pr, "unprivileged spr access"
        case n
        of 18: state.dsisr = r(d)
        of 19: state.dar = r(d)
        of 22: state.dec = r(d)
        of 26: state.srr0 = r(d) and not(0x3'u32)
        of 27: state.srr1 = Srr1 r(d)
        of 272..275: state.sprg[n - 272] = r(d)
        of 284: state.tb = (state.tb and not(0xFFFFFFFF'u64)) or r(d)
        of 285: state.tb = (state.tb and 0xFFFFFFFF'u64) or (uint64(r(d)) shl 32)
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
        of 1008: state.hid0 = Hid0 r(d)
        of 1009: state.hid1 = Hid1 r(d)
        of 912..919: state.gqr[n - 912] = Gqr r(d)
        of 920: state.hid2 = Hid2 r(d)
        of 921:
            state.gatherpipeOffset = 0
            state.wpar.gbAddr = r(d)
        of 1017: state.l2cr = L2Cr r(d)
        of 952: state.mmcr0 = Mmcr0 r(d)
        of 956: state.mmcr1 = Mmcr1 r(d)
        of 953..954: state.pmc[n - 953] = Pmc r(d)
        of 957..958: state.pmc[n - 957] = Pmc r(d)
        else:
            doAssert false, &"unknown spr register {n}"

proc dcbf*(state; a, b: uint32) =
    echo "dcbf stubbed"

proc dcbi*(state; a, b: uint32) =
    echo "dcbi stubbed"

proc dcbst*(state; a, b: uint32) =
    doAssert false, "instr not implemented"

proc dcbt*(state; a, b: uint32) =
    doAssert false, "instr not implemented"

proc dcbtst*(state; a, b: uint32) =
    doAssert false, "instr not implemented"

proc dcbz*(state; a, b: uint32) =
    doAssert false, "instr not implemented"

proc icbi*(state; a, b: uint32) =
    echo "icbi stubbed"

proc mfsr*(state; d, sr: uint32) =
    doAssert false, "instr not implemented"

proc mfsrin*(state; d, b: uint32) =
    doAssert false, "instr not implemented"

proc mtsr*(state; s, sr: uint32) =
    echo "mtsr instruction stubbed"

proc mtsrin*(state; s, b: uint32) =
    doAssert false, "instr not implemented"

proc tlbie*(state; b: uint32) =
    doAssert false, "instr not implemented"

proc tlbsync*(state) =
    doAssert false, "instr not implemented"

proc eciwx*(state; d, a, b: uint32) =
    doAssert false, "instr not implemented"

proc ecowx*(state; s, a, b: uint32) =
    doAssert false, "instr not implemented"

proc dcbz_l*(state; a, b: uint32) =
    doAssert false, "instr not implemented"