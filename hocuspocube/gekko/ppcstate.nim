import
    bitops, ../util/bitstruct,
    ../cycletiming

# it's so exhausting to translate these bit structures
# from the manual, because for some reason they think the first bit is the most significant?
# That's just stupid! 

makeBitStruct uint32, *Xer:
    so[31] {.mutable.}: bool # summary overflow
    ov[30] {.mutable.}: bool # overflow
    ca[29] {.mutable.}: bool # carry
    byteCount[0..7] {.mutable.}: uint32 # byte count for string instructions

makeBitStruct uint32, *Cr:
    crf[n, (7 - n)*4..((7 - n)*4)+3]: uint32
    bit[n, 31 - n]: bool
    so[n, (7 - n)*4+0]: bool # summary overflow
    eq[n, (7 - n)*4+1]: bool # equal
    gt[n, (7 - n)*4+2]: bool # greater
    lt[n, (7 - n)*4+3]: bool # less

    ox[24]: bool # float overflow
    vx[25]: bool # float invalid operation
    fex[26]: bool # float enabled exception summary
    fx[27]: bool # float exception summary

makeBitStruct uint32, *Fpscr:
    rn[0..1]: uint32 # rounding mode
    ni[2]: bool # enable none IEEE mode
    xe[3]: bool # inexact exception enable
    ze[4]: bool # IEEE divide by 0 exception enable
    ue[5]: bool # IEEE underflow exception enable
    oe[6]: bool # IEEE overflow exception enable
    ve[7]: bool # invald operation exception enable
    vxcvi[8] {.exceptionBit.}: bool # invalid integer convert exception
    vxsqrt[9] {.exceptionBit.}: bool # invalid sqrt exception
    vxsoft[10] {.exceptionBit.}: bool # invalid operation for sw request exception
    fprf[12..16]: uint32 # result flags
    fi[17]: bool # inexact
    fr[18]: bool # rounded
    vxvc[19] {.exceptionBit.}: bool # invalid compare exception
    vximz[20] {.exceptionBit.}: bool # invalid operation infinity * 0 exception
    vxzdz[21] {.exceptionBit.}: bool # invalid operation 0 / 0 exception
    vxidi[22] {.exceptionBit.}: bool # invalid operation infinity / infinity exception
    vxisi[23] {.exceptionBit.}: bool # invalid operation infinity - inifinity exception
    vxsnan[24] {.exceptionBit.}: bool # snan exception
    xx[25] {.exceptionBit.}: bool # inexact exception
    zx[26] {.exceptionBit.}: bool # zero divide exception
    ux[27] {.exceptionBit.}: bool # underflow exception
    ox[28] {.exceptionBit.}: bool # overflow exception
    vx[29] {.exceptionBit.}: bool # invalid operation summary
    fex[30]: bool # enabled exception summary
    fx[31] {.exceptionBit.}: bool # exception summary

    bit[n, 31 - n]: bool
    crf[n, (7 - n)*4..((7 - n)*4)+3]: uint32

makeBitStruct uint32, *Msr:
    le[0] {.exceptionSaved, zeroOnException.}: bool # little endian mode
    ri[1] {.exceptionSaved, zeroOnException.}: bool # exception recoverable
    dr[4] {.exceptionSaved, zeroOnException.}: bool # data address translation
    ir[5] {.exceptionSaved, zeroOnException.}: bool # instruction address translation
    ip[6] {.exceptionSaved.}: bool # exception prefix (base address)
    fe1[8] {.exceptionSaved, zeroOnException.}: bool # float point exception mode 1
    be[9] {.exceptionSaved, zeroOnException.}: bool # branch trace enable
    se[10] {.exceptionSaved, zeroOnException.}: bool # single step trace enable
    fe0[11] {.exceptionSaved, zeroOnException.}: bool # float point exception mode 0
    me[12] {.exceptionSaved.}: bool # machine check exception enable
    fp[13] {.exceptionSaved, zeroOnException.}: bool # enable float point
    pr[14] {.exceptionSaved, zeroOnException.}: bool # privilege level
    ee[15] {.exceptionSaved, zeroOnException.}: bool # external interrupt
    ile[16]: bool # exception little endian mode
    pow[18] {.zeroOnException.}: bool # power management enable

makeBitStruct uint32, *Srr1:
    info0[27..30]: uint32
    info1[16..21]: uint32

    le[0] {.exceptionSaved.}: bool
    ri[1] {.exceptionSaved.}: bool
    dr[4] {.exceptionSaved.}: bool
    ir[5] {.exceptionSaved.}: bool
    ip[6] {.exceptionSaved.}: bool
    fe1[8] {.exceptionSaved.}: bool
    be[9] {.exceptionSaved.}: bool
    se[10] {.exceptionSaved.}: bool
    fe0[11] {.exceptionSaved.}: bool
    me[12] {.exceptionSaved.}: bool
    fp[13] {.exceptionSaved.}: bool
    pr[14] {.exceptionSaved.}: bool
    ee[15] {.exceptionSaved.}: bool

makeBitStruct uint32, *BatHi:
    vp[0]: bool
    vs[1]: bool
    bl[2..12]: uint32
    _[17..31] {.bepi.}: uint32
makeBitStruct uint32, *BatLo:
    pp[0..1]: uint32
    wimg[3..6]: uint32
    _[17..31] {.brpn.}: uint32

proc isValid*(lo: BatLo, hi: BatHi): bool =
    (hi.vp or hi.vs) and
        (((hi.bl + 1) and hi.bl) == 0) and
        ((hi.bepi and (not(hi.bl) shl 17)) == hi.bepi)

makeBitStruct uint32, *Hid0:
    noopti[0]: bool # disable dcbt and dcbtst
    bht[2]: bool # enable branch history table
    abe[3]: bool # address broadcast enable
    fbiob[4]: bool # not used anymore
    btic[5]: bool # branch target cache enable
    dcfa[6]: bool # data cache flush assist
    sge[7]: bool # store gathering enabled
    ifem[8]: bool # enable m bit
    spd[9]: bool # disable speculative cache access
    dcfi[10]: bool # throw away entire data cache
    icfi[11]: bool # throw away entire instruction cache
    dlock[12]: bool # data cache lock
    ilock[13]: bool # instruction cache lock
    dce[14]: bool # data cache enable
    ice[15]: bool # instruction cache enable
    nhr[16]: bool # not hard reset (what a great bit)
    dpm[20]: bool # dynamic power management
    sleep[21]: bool # sleep mode enable
    nap[22]: bool # nap mode enable
    doze[23]: bool # doze mode enable
    par[24]: bool # the
    eclk[25]: bool # remaining bits
    eice[26]: bool # are all
    bclk[27]: bool # hw bus stuff
    ebd[28]: bool # I have
    eba[29]: bool # no idea of

makeBitStruct uint32, *Hid1:
    pc4[27]: bool
    pc3[28]: bool
    pc2[29]: bool
    pc1[30]: bool
    pc0[31]: bool

makeBitStruct uint32, *Hid2:
    dqoee[16] {.mutable.}: bool # DMA queue overflow error enable
    dqmee[17] {.mutable.}: bool # DMA cache miss error enable
    dncee[18] {.mutable.}: bool # DMA access to normal cache error enable
    dchee[19] {.mutable.}: bool # dcbz_l cache hit error enable
    dqoerr[20] {.mutable.}: bool # DMA queue overflow error
    dcmerr[21] {.mutable.}: bool # DMA cache miss error
    dncerr[22] {.mutable.}: bool # DMA access to normal cache error
    dcherr[23] {.mutable.}: bool # dcbz_l cache hit error
    dmaql[24..27]: uint32 # DMA queue length
    lce[28] {.mutable.}: bool # locked cache enable
    pse[29] {.mutable.}: bool # paired singles enabled
    wpe[30] {.mutable.}: bool # write gather pipe enabled
    lsqe[31] {.mutable.}: bool # load store quantisied instructions enabled

type
    GqrType* = enum
        gqrFloat
        gqrReserved1
        gqrReserved2
        gqrReserved3
        gqrU8
        gqrU16
        gqrS8
        gqrS16

makeBitStruct uint32, *Gqr:
    stType[0..2]: GqrType # type for store
    stScale[8..13]: uint32 # scale for store
    ldType[16..18]: GqrType # type for load
    ldScale[24..29]: uint32 # scale for load

makeBitStruct uint32, *L2cr:
    l2ip[0]: bool
    l2ts[18]: bool
    l2wt[19]: bool
    l2i[21]: bool
    l2do[22]: bool
    l2ce[30]: bool
    l2e[31]: bool

makeBitStruct uint32, *Mmcr0:
    pmc2select[0..5]: uint32
    pmc1select[6..12]: uint32
    pmctrigger[13]: bool
    pmc2intcontrol[14]: bool
    pmc1intcontrol[15]: bool
    threshold[16..21]: uint32
    intonbittrans[22]: bool
    rtcselect[23..24]: uint32
    discount[25]: bool
    enint[26]: bool
    dmr[27]: bool
    dms[28]: bool
    du[29]: bool
    dp[30]: bool
    dis[31]: bool

makeBitStruct uint32, *Mmcr1:
    pmc3select[0..4]: uint32
    pmc4select[5..9]: uint32

makeBitStruct uint32, *Pmc:
    counter[0..30]: uint32
    ov[31]: bool

makeBitStruct uint32, *Wpar:
    bne[0]: bool
    _[5..31] {.gbAddr.}: uint32

makeBitStruct uint32, *DmaU:
    lenHi[0..4]: uint32
    _[5..31] {.memAdr.}: uint32

makeBitStruct uint32, *DmaL:
    flush[0]: bool
    trigger[1]: bool
    lenLo[2..3]: uint32
    load[4]: bool
    _[5..31] {.lcAdr.}: uint32

type
    PpcException* = enum
        # ordered by priority
        # not entirely accurate (some things need to be further split up to be handled correctly)

        # priority 0
        exceptionSystemReset
        exceptionInstrAddrBreak
        exceptionIsi

        # priority 1
        exceptionMachineCheck
        exceptionProgram

        # priority 2
        exceptionSystemCall

        # priority 3
        exceptionNoFloatPoint
        exceptionExternal

        # priority 4
        exceptionPerformance

        # priority 5
        exceptionDecrementer
        
        # priority 6
        exceptionAlignment

        # priority 7-10
        exceptionDsi

        # priority 11
        exceptionTrace

    PairedSingle* = array[2, float64]

    PpcState* = object
        r*: array[32, uint32]
        fr* {.align(16).}: array[32, PairedSingle]

        cr*: Cr
        xer*: Xer
        lr*, ctr*, pc*: uint32

        msr*: Msr

        fpscr*: Fpscr

        ibatHi*: array[4, BatHi]
        ibatLo*: array[4, BatLo]
        dbatHi*: array[4, BatHi]
        dbatLo*: array[4, BatLo]

        wpar*: Wpar
        gatherPipeOffset*: uint32
        gatherpipe*: array[128, byte]

        gqr*: array[8, Gqr]

        pendingExceptions*: set[PpcException]
        negativeCycles*: int32

        hid0*: Hid0
        hid1*: Hid1
        hid2*: Hid2

        srr0*: uint32
        srr1*: Srr1

        sprg*: array[4, uint32]

        dar*: uint32

        tbInit*: uint64
        tbInitTimestamp*: int64

        decInit*: uint32
        decInitTimestamp*: int64
        decDoneEvent*: EventToken

        l2cr*: L2cr

        mmcr0*: Mmcr0
        mmcr1*: Mmcr1

        pmc*: array[4, Pmc]

        dsisr*: uint32

        dmaU*: DmaU
        dmaL*: DmaL

# ppc is really weird when it comes to floats…
func ps0*(ps: PairedSingle): float64 {.inline.} = ps[0]
func `ps0=`*(ps: var PairedSingle, val: float64) {.inline.} =
    ps[0] = float32(val)

func ps1*(ps: PairedSingle): float64 {.inline.} = ps[1]
func `ps1=`*(ps: var PairedSingle, val: float64) =
    ps[1] = float32(val)

func double*(ps: PairedSingle): float64 {.inline.} = ps[0]
func `double=`*(ps: var PairedSingle, val: float64) {.inline.} =
    ps[0] = val
