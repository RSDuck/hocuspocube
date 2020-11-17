import
    ../util/bitstruct, ../util/ioregs,
    dspstate,
    strformat

#[
    DSP Init Sequence:
        Switching bit11 of DspCsr (here labeled as bootRom) from one to zero
        initialises an init sequence (it's status is indicated by bit 10 "busyCopying").
        It copies a payload of 1024kb from the start of ARAM to the start of IRAM.
        This is as far as I know only used for the init ucode (as Dolphin calls it).
        It fulfills three tasks:
            - Reading out both the entire IROM and DROM. The read value is always
                discarded. Each load is proceeded by a nop. I assume this to be
                some kind of init sequence for the ROM, but couldn't test it.
                (Maybe this is what duddie is talking about? "This usually happens
                during boottime because DSP ROM is not enabled at cold reset and needs
                to be reenabled by small stub executed in IRAM. ")
            - Zero data RAM
            - Wait for any value to be received in it's mailbox and send back
                the value 0x00544348. The DSP is halted afterwards.

        Afterwards the program stored in the IROM is used to transfer programs to the DSP.
]#

makeBitStruct uint16, *DspCsr:
    reset[0] {.mutable.}: bool
    piint[1]: bool
    halt[2] {.mutable.}: bool
    aidint[3]: bool
    aidintmask[4] {.mutable.}: bool
    arint[5]: bool
    arintmsk[6] {.mutable.}: bool
    dspint[7]: bool
    dspintmsk[8] {.mutable.}: bool
    dspdma[9]: bool
    busyCopying[10]: bool # ist doch was anderes?
    bootRom[11] {.mutable.}: bool

makeBitStruct uint32, DspMailbox:
    data[0..30]: uint32
    status[31]: bool

    lo[0..15]: uint16
    hi[16..31]: uint16
    hiWrite[16..30]: uint16

makeBitStruct uint32, *DspDmaCnt:
    direction[31]: bool
    length[0..30]: uint32

    hi[16..31]: uint16
    lo[0..15]: uint16

var
    theDspState*: DspState

    dspCsr*: DspCsr

    cmb: DspMailbox
    dmb: DspMailbox

    arDmaStatus* = -1
    arDmaCnt*: DspDmaCnt
    arDmaMmAddr*: uint32
    arDmaArAddr*: uint32

    iram*: array[0x1000, uint16]
    irom*: array[0x1000, uint16]

    dram*: array[0x1000, uint16]
    drom*: array[0x800, uint16]

    aram*: array[16*1024, uint8]

    IRamStartAddr = 0'u16
    IRomStartAddr = 0x1000'u16

    DRomStartAddr = 0x1000'u16

dspCsr.halt = true

# DSP side memory
proc instrRead*(`addr`: uint16): uint16 =
    if `addr` < IRomStartAddr:
        iram[`addr` and 0xFFF]
    else:
        irom[`addr` and 0xFFF]

proc instrWrite*(`addr`, val: uint16) =
    if `addr` < IRomStartAddr:
        iram[`addr` and 0xFFF] = val
    else:
        echo &"unknown dsp instr write {`addr`:04X} {`val`:X}"

proc dataRead*(`addr`: uint16): uint16 =
    if `addr` < DRomStartAddr:
        dram[`addr` and 0xFFF]
    elif `addr` < DRomStartAddr + uint16(dram.len):
        drom[`addr` and uint16(drom.len - 1)]
    else:
        case `addr`
        of 0xFFFC: dmb.hi
        of 0xFFFD: dmb.lo
        of 0xFFFE: cmb.hi
        of 0xFFFF: dmb.status = false; cmb.lo
        else: echo &"unknown dsp data read {`addr`:X}"; 0'u16

proc dataWrite*(`addr`, val: uint16) =
    if `addr` < DromStartAddr:
        dram[`addr`] = val
    else:
        case `addr`
        of 0xFFFC: dmb.hiWrite = val
        of 0xFFFD: dmb.status = true; dmb.lo = val
        else: echo &"unknown dsp data write {`addr`:04X} {`val`:X}"

ioBlock dsp, 0x200:
of cmbh, 0x00, 2:
    read: cmb.hi
    write: cmb.hiWrite = val
of cmbl, 0x02, 2:
    read: cmb.lo
    write: cmb.lo = val; cmb.status = true
of dmbh, 0x04, 2:
    read: dmb.hi
of dmbl, 0x06, 2:
    read: dmb.status = false; dmb.lo
of dspcr, 0x0A, 2:
    read: uint16(dspCsr)
    write:
        let val = DspCsr(val)

        if val.bootRom != dspCsr.bootRom:
            if val.bootRom:
                theDspState.pc = IRomStartAddr
            else:
                theDspState.pc = IRamStartAddr
                dspCsr.busyCopying = true

        dspCsr.mutable = val.mutable

        if val.aidint: dspCsr.aidint = false
        if val.arint: dspCsr.arint = false
        if val.dspint: dspCsr.dspint = false
of arDmaMmAddr, 0x20, 4:
    read: arDmaMmAddr
    write: arDmaMmAddr = val
of arDmaArAddr, 0x24, 4:
    read: arDmaArAddr
    write: arDmaArAddr = val
of arDmaCntHi, 0x28, 2:
    read: arDmaCnt.hi
    write: arDmaCnt.hi = val
of arDmaCntLo, 0x2A, 2:
    read: arDmaCnt.lo
    write:
        # TODO: find out:
        # * when exactly dsp dma bit is set
        # * what happens if the transfer is aborted
        # * is dmacnt decremented?
        arDmaCnt.lo = val
        arDmaStatus = int arDmaCnt.length