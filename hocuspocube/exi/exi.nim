import
    bitops, strformat,
    ../util/[ioregs, bitstruct],
    ../gekko/[gekko, memory]

template exiLog(message: string) =
    discard

type
    ExiDevice* = ref object of RootObj
        transactionPos*: uint32

    ExiClkRate = enum
        exiClk1Mhz
        exiClk2Mhz
        exiClk4Mhz
        exiClk8Mhz
        exiClk16Mhz
        exiClk32Mhz
        exiClkReserved0
        exiClkReserved1
    
    ExiTransferKind = enum
        exiTransferRead
        exiTransferWrite
        exiTransferReadWrite
        exiTransferReserved

makeBitStruct uint32, ExiCsr:
    exiintmsk[0] {.mutable.}: bool
    exiint[1]: bool
    tcintmsk[2] {.mutable.}: bool
    tcint[3]: bool
    clk[4..6] {.mutable.}: ExiClkRate
    cs[7..9]: uint32
    extintmsk[10] {.mutable.}: bool
    extint[11]: bool
    ext[12]: bool # interrupt when something was removed from the memcard slots
    romdis[13]: bool

makeBitStruct uint32, ExiCr:
    tstart[0]: bool
    dma[1] {.mutable.}: bool
    transferKind[2..3] {.mutable.}: ExiTransferKind
    tlen[4..5] {.mutable.}: uint32

method select*(dev: ExiDevice, status: bool) {.base.} =
    if status:
        dev.transactionPos = 0

method exchange*(dev: ExiDevice, response: var openArray[byte], input: openArray[byte]) {.base.} =
    raiseAssert("unimplemented method exchange")

template inbytes*(): untyped =
    max(response.len, input.len)

func readByte*(input: openArray[byte], idx: int): byte =
    if input.len > 0: input[idx] else: 0xFF
func writeByte*(response: var openArray[byte], idx: int, data: byte) =
    if response.len > 0: response[idx] = data

type
    ExiChannel = object
        csr: ExiCsr
        cr: ExiCr
        dmaStart, dmaLen: HwPtr
        data: uint32
        device: ExiDevice

var
    channels: array[3, ExiChannel]
    devMemcardSlots: array[2, ExiDevice]

proc updateInt() =
    var intSet = false
    for chan in 0..<3:
        intSet = intSet or
            (channels[chan].csr.tcint and channels[chan].csr.tcintmsk) or
            (channels[chan].csr.exiint and channels[chan].csr.exiintmsk) or
            (channels[chan].csr.extint and channels[chan].csr.extintmsk)
    setExtInt extintExi, intSet

proc setMemcardSlot*(slot: int, dev: ExiDevice) =
    if devMemcardSlots[slot] != nil:
        channels[slot].csr.extint = true

    devMemcardSlots[slot] = dev
    channels[slot].csr.ext = dev != nil
    updateInt()

proc setExiPeripheralInt*(dev: ExiDevice) =
    for i in 0'u32..<3:
        if channels[i].device == dev:
            echo &"peripheral int {i} set"
            channels[i].csr.exiint = true
            updateInt()
            return
    
    raiseAssert("trying to set exi int for unknown or not selected device")

proc descrambleBios*(): bool = not channels[0].csr.romdis

import
    ad16, rtcsramrom

proc getDevice(channel, device: range[0..2]): ExiDevice =
    case channel
    of 0:
        case device
        of 0: devMemcardSlots[0]
        of 1: devRtcsramrom
        of 2: nil
    of 1:
        case device
        of 0: devMemcardSlots[1]
        else: nil
    of 2:
        case device
        of 0: devAd16
        else: nil

ioBlock exi, 0x40:
of exiCsr, 0x00, 4, 3, 20:
    read: uint32 channels[idx].csr
    write:
        channels[idx].csr.mutable = val
        let
            val = ExiCsr val

            # if more than one device is flagged
            # it will be interpreted as 0
            cs = if (val.cs and (val.cs - 1)) != 0: 0'u32 else: val.cs

        if idx == 0 and val.romdis:
            channels[idx].csr.romdis = false

        if val.tcint:
            channels[idx].csr.tcint = false
        if val.exiint:
            channels[idx].csr.exiint = false
        if val.extint:
            channels[idx].csr.extint = false
        updateInt()

        if cs != channels[idx].csr.cs:
            channels[idx].csr.cs = cs
            if channels[idx].device != nil:
                exiLog &"exi chan {idx} delesect"
                channels[idx].device.select(false)
                channels[idx].device = nil

            if cs != 0:
                let devnum = countTrailingZeroBits(cs)
                exiLog &"exi chan {idx} select {devnum}"
                channels[idx].device = getDevice(idx, devnum)

                if channels[idx].device != nil:
                    channels[idx].device.select(true)
of exiDmaMar, 0x04, 4, 3, 20:
    read: uint32 channels[idx].dmaStart
    write: channels[idx].dmaStart.adr = val
of exiDmaLen, 0x08, 4, 3, 20:
    read: uint32 channels[idx].dmaLen
    write: channels[idx].dmaLen.adr = val
of exiCr, 0x0C, 4, 3, 20:
    read: uint32 channels[idx].cr
    write:
        channels[idx].cr.mutable = val
        let val = ExiCr val

        if val.tstart:
            if channels[idx].device == nil:
                exiLog &"exi transfer {idx} with no device selected?"
                return

            if val.dma:
                assert channels[idx].cr.transferKind in {exiTransferRead, exiTransferWrite}

                let
                    startAdr = channels[idx].dmaStart.adr
                    len = channels[idx].dmaLen.adr

                exiLog &"exi {idx} dma {channels[idx].cr.transferKind} from {startAdr:08X} {len:08X} {gekkoState.pc:08X}"

                if channels[idx].cr.transferKind == exiTransferRead:
                    withMainRamOpenArrayWrite(startAdr, len, byte):
                        let empty: array[0, byte] = []
                        channels[idx].device.exchange(ramArr, empty)
                else:
                    withMainRamOpenArray(startAdr, len, byte):
                        var dummyOut: array[0, byte] = []
                        channels[idx].device.exchange(dummyOut, ramArr)

                channels[idx].csr.tcint = true
                updateInt()
            else:
                assert channels[idx].cr.transferKind != exiTransferReserved

                var
                    input = [0xFF'u8, 0xFF'u8, 0xFF'u8, 0xFF'u8]
                    response = [0xFF'u8, 0xFF'u8, 0xFF'u8, 0xFF'u8]

                exiLog &"exi {idx} imm {channels[idx].cr.transferKind} len {channels[idx].cr.tlen+1} {channels[idx].data:08X} {gekkoState.pc:08X}"

                if channels[idx].cr.transferKind in {exiTransferWrite, exiTransferReadWrite}:
                    var imm = channels[idx].data
                    for i in 0..channels[idx].cr.tlen:
                        input[i] = uint8(imm shr ((3-i)*8))

                channels[idx].device.exchange(toOpenArray(response, 0, channels[idx].cr.tlen),
                    toOpenArray(input, 0, channels[idx].cr.tlen))

                if channels[idx].cr.transferKind in {exiTransferRead, exiTransferReadWrite}:
                    for i in 0..channels[idx].cr.tlen:
                        let shift = (3-i)*8
                        channels[idx].data = (channels[idx].data and not(0xFF'u32 shl shift)) or (uint32(response[i]) shl shift)
                    exiLog &"read result {channels[idx].data:08X}"
of exiData, 0x10, 4, 3, 20:
    read: channels[idx].data
    write: channels[idx].data = val