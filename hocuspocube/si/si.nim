import
    stew/endians2, stew/bitops2,
    strformat,
    ../cycletiming,
    ../util/ioregs, ../util/bitstruct,
    ../gecko/gecko

# data received from polling 
makeBitStruct uint32, SicInBufH:
    inputData[0..29]: uint32
    errlatch[30]: bool # whether an error is flagged for this channel (doesn't need to be stem from a poll)
    errstat[31]: bool # whether any error occured during the last poll

makeBitStruct uint32, SiPoll:
    vbcpy[0..3] {.mutable.}: uint32 # if enabled delay flush to backbuffer until vblank
    en[4..7] {.mutable.}: uint32 # enable polling for port/channel
    y[8..15] {.mutable.}: uint32 # amount of polls per frame
    x[16..25] {.mutable.}: uint32 # the amount of scanlines to wait between polls

makeBitStruct uint32, SiComCsr:
    # yagd defines a meaning to a few more bits, but without really showing what they're useful for
    # libogc and dolphin ignore them, so we'll join them for now
    tstart[0]: bool # kickoff com transfer/whether it's going
    channel[1..2] {.mutable.}: uint32 # channel for com transfer
    inLen[8..14] {.mutable.}: uint32 # com transfer receive length in bytes
    outLen[16..22] {.mutable.}: uint32 # com transfer send length in bytes
    rdstintmask[27] {.mutable.}: bool # mask assertion of PI interrupt for poll
    rdstint[28]: bool # whether poll was performed (and an interrupt asserted) interrupt was asserted
    comerr[29]: bool # whether an error during comtransfer occured (I guess it's similar to errstat)
    tcintmsk[30] {.mutable.}: bool # mask assertion of PI interrupt for com transfer
    tcint[31]: bool # whether com transfer finished interrupt was asserted

makeBitStruct uint32, SiSr:
    unrun[n, 8 * (3 - n)]: bool # received less than expcted
    ovrun[n, 8 * (3 - n) + 1]: bool # received more than expected
    coll[n, 8 * (3 - n) + 2]: bool # collision while serial transfer
    norep[n, 8 * (3 - n) + 3]: bool # no response on this channel
    wrst[n, 8 * (3 - n) + 4]: bool # copy status of this buffer
    rdst[n, 8 * (3 - n) + 5]: bool # new data through poll available

    wr[31]: bool # copy all poll outbuffers into back buffer/whether copying is done

type
    SiTransferState* = enum
        siErrNone
        siErrCollision # started receiving data before finished writing => out length wrong
        siErrNoResponse

    SiDeviceProc* = proc(device: SiDevice, command: openArray[byte], recvData: var seq[byte]): SiTransferState

    SiDevice* = ref object of RootObj
        transact*: SiDeviceProc

var
    outBuffers: array[4, uint32]
    outBuffersBack: array[4, uint32] # those are the ones to be transmitted via serial

    inBuffersH: array[4, SicInBufH]
    inBuffersL: array[4, uint32]

    siPoll: SiPoll
    siComCsr: SiComCsr
    siSr: SiSr

    siBuffer: array[128, byte]

    nextPoll = InvalidEventToken

    siDevices: array[4, SiDevice]

proc configureSiDevice*(channel: int, transact: SiDevice) =
    siDevices[channel] = transact

proc performSiTransfer(channel: int, command: openArray[byte], recvData: var seq[byte]): SiTransferState =
    if siDevices[channel] != nil:
        siDevices[channel].transact(siDevices[channel], command, recvData)
    else:
        siErrNoResponse

proc updateErrorFlags(channel, inOutCmp: int, reply: SiTransferState): bool =
    case reply
    of siErrNone: discard
    of siErrNoResponse: siSr.norep(channel, true)
    of siErrCollision: siSr.coll(channel, true)

    if inOutCmp > 0:
        # received more than expected
        siSr.ovrun(channel, true)
    elif inOutCmp < 0:
        # received less than expected
        siSr.unrun(channel, true)

    inBuffersH[channel].errstat = siSr.norep(channel) or
        siSr.unrun(channel) or siSr.ovrun(channel) or siSr.coll(channel)

    reply != siErrNone or inOutCmp != 0

func pack4BytesBE(data: openArray[byte]): uint32 =
    (uint32(data[0]) shl 24) or
        (uint32(data[1]) shl 16) or
        (uint32(data[2]) shl 8) or
        uint32(data[3])

proc updateInt() =
    setExtInt extintSi, (siComCsr.rdstint and siComCsr.rdstintmask) or
        (siComCsr.tcint and siComCsr.tcintmsk)

proc poll(pollIdx: int, pollMask: uint32, timestamp: int64, scanlineLength: int64) =
    for i in 0..<4:
        if pollMask.getBit(3 - i):
            let outData = [byte(outBuffersBack[i] shr 16), byte(outBuffersBack[i] shr 8), byte(outBuffersBack[i])]
            var recvData: seq[byte]

            let resp = performSiTransfer(i, outData, recvData)
            inBuffersH[i].errstat = updateErrorFlags(i, recvData.len - 8, resp)

            if not inBuffersH[i].errstat:
                inBuffersH[i].inputData = pack4BytesBE(recvData)
                inBuffersL[i] = pack4BytesBE(toOpenArray(recvData, 4, 7))

            echo &"polled {inBuffersH[i].errstat} {recvData[0]:02X} {uint32(inBuffersH[i]):08X} {inBuffersL[i]:08X} {uint32(siSr):08X}"

            siSr.rdst(i, true)

            siComCsr.rdstint = true
            updateInt()

    nextPoll = scheduleEvent(timestamp + scanlineLength * int64(siPoll.x), 0,
        proc(timestamp: int64) = poll(pollIdx + 1, pollMask, timestamp, scanlineLength))

proc startSiPoll*(timestamp: int64, scanlineLength: int64) =
    if nextPoll != InvalidEventToken:
        cancelEvent nextPoll

    if siPoll.y > 0:
        poll(0, siPoll.en, timestamp, scanlineLength)


ioBlock si, 0x100:
# since these registers are interlaced we unfortunately need to register them all by hand
of sicoutbuf, 0x0, 4, 4, 12:
    read: outBuffers[idx]
    write: outBuffers[idx] = val
of sicinbufh, 0x4, 4, 4, 12:
    read:
        siSr.rdst(int idx, false)
        uint32 inBuffersH[idx]
of sicinbufl, 0x8, 4, 4, 12:
    read: inBuffersL[idx]

of siPoll, 0x30, 4:
    read: uint32 siPoll
    write: siPoll.mutable = val
of siComCsr, 0x34, 4:
    write:
        siComCsr.mutable = val

        let val = SiComCsr val
        if val.tcint:
            siComCsr.tcint = false
        if val.rdstint:
            siComCsr.rdstint = false
        updateInt()

        if val.tstart:
            let
                outLen = if siComCsr.outLen == 0: 128 else: int(siComCsr.outLen)
                inLen = if siComCsr.inLen == 0: 128 else: int(siComCsr.inLen)

            var recvData: seq[byte]
            let resp = performSiTransfer(int val.channel,
                toOpenArray(siBuffer, 0, outLen - 1),
                recvData)

            siComCsr.comerr = updateErrorFlags(int val.channel, recvData.len - inLen, resp)

            if not siComCsr.comerr:
                for i in 0..<recvData.len:
                    siBuffer[i] = recvData[i]

            siComCsr.tcint = true
            updateInt()

            echo &"tcom {siComCsr.comerr} {siComCsr.outLen} {siComCsr.inLen} {recvData.len} {resp} {uint32(siSr):08X}"
    read: uint32 siComCsr
of siSr, 0x38, 4:
    write:
        let val = SiSr val
        for i in 0..<4:
            if val.unrun(i):
                siSr.unrun(i, false)
            if val.ovrun(i):
                siSr.unrun(i, false)
            if val.coll(i):
                siSr.unrun(i, false)
            if val.norep(i):
                siSr.unrun(i, false)
        if val.wr:
            for i in 0..<4:
                outBuffersBack[i] = outBuffers[i]
    read: uint32 siSr

of sibuf, 0x80, 1, 32:
    # a bit stupid to convert the values which were converted to little endian for us
    # back to big endian, though atleast gcc seems to be eable to figure out that bswap(bswap(val)) == val
    read: siBuffer[idx]
    write:
        siBuffer[idx] = val