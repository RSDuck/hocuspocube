import
    util/ioregs, util/bitstruct,
    cycletiming,
    gekko/gekko, si/si,
    gekko/ppcstate,

    strformat

when defined(nintendoswitch):
    import frontend/switch
else:
    import frontend/sdl

template viLog(msg: string): untyped =
    discard

# cycles here generally refer to VI cycles

makeBitStruct uint16, Vtr: # vertical timing
    equ[0..3] {.mutable.}: uint32 # equalisation pulse, apparently in one and a half lines
    acv[4..13] {.mutable.}: uint32 # active video in full lines

makeBitStruct uint16, Dcr: # display config
    # TODO: find out how these two behave
    enb[0]: bool # enable
    rst[1]: bool # reset
    prog[2] {.mutable.}: bool # progressive
    d[3] {.mutable.}: bool # stereo 3D mode
    le0[4..5] {.mutable.}: uint32 # display latch 0
    le1[6..7] {.mutable.}: uint32 # display latch 1
    fmt[8..9] {.mutable.}: uint32 # (color) format (NTSC, PAL, MPAL, Debug)

makeBitStruct uint32, Htr0: # horizontal timing 0
    hlw[0..8] {.mutable.}: uint32 # cycles per halfline
    hce[16..22] {.mutable.}: uint32 # cycles from hsync start to color burst start
    hcs[24..30] {.mutable.}: uint32 # cycles from hsync start to color burst end

makeBitStruct uint32, Htr1: # horizontal timing 1
    hsy[0..6] {.mutable.}: uint32 # hsync width width
    hbe[7..16] {.mutable.}: uint32 # cycles from hsync start (or the start of the line) to hblank end
    hbs[17..26] {.mutable.}: uint32 # cycles from half of a scanline to hblank start

makeBitStruct uint32, Vt: # vertical blanking
    prb[0..9] {.mutable.}: uint32 # pre blanking in half scanlines
    psb[16..25] {.mutable.}: uint32 # post blanking in half scanlines

makeBitStruct uint32, FieldBase: # controls the address of a field
    fbb[0..23] {.mutableTL, mutableBL, mutableR.}: uint32 # base address
    xof[24..27] {.mutableTL.}: uint32 # xof, offset onto of the base address in 2 byte (1 pixel) steps
    pageOffset[28] {.mutableTL, mutableBL.}: bool # if set fbb is left shifted by 5 
    clear[31]: bool # writing this bit clears the whole register for top left? (TODO: investigate this further)

makeBitStruct uint32, Di: # display interrupt
    hct[0..9] {.mutable.}: uint32 # horizontal position
    vct[16..25] {.mutable.}: uint32 # vertical position
    enb[28] {.mutable.}: bool # enable interrupt
    sts[31]: bool # interrupt status

makeBitStruct uint16, Hsw:
    std[0..7] {.mutable.}: uint32 # framebuffer stride in 16 pixels/32 bytes units
    wpl[8..14] {.mutable.}: uint32 # framebuffer width threshold in 16 pixels/32 bytes
    # units (active video is glitched beyond it or stops if the hstepper is enabled)

makeBitStruct uint16, Viclk:
    s[0] {.mutable.}: uint32 # whether to use the 27 Mhz or the 54 Mhz clock

makeBitStruct uint16, Visel: # VI DTV status register
    digital[0]: bool # whether the digital video output is used
    ntscj[1]: bool # Dolphin sets this if the console region is NTSC-J, but is it constant?

type
    FramePhase = enum
        framePhaseEquOdd
        framePhasePrbOdd
        framePhaseAcvOdd
        framePhasePsbOdd
        framePhaseEquEven
        framePhasePrbEven
        framePhaseAcvEven
        framePhasePsbEven

var
    curFramePhase: FramePhase
    curFramePhaseStartTimestamp: int64
    curFramePhaseStartX, curFramePhaseStartY: int64
    nextFramePhaseEvent: EventToken
    nextFramePhaseEventTimestmap: int64

    vtr: Vtr
    htr0: Htr0
    htr1: Htr1
    vto, vte: Vt
    viclk: Viclk

    dcr: Dcr

    tfbl, bfbl: FieldBase
    tfbr, bfbr: FieldBase

    di: array[4, Di]
    diEvents = [InvalidEventToken, InvalidEventToken, InvalidEventToken, InvalidEventToken]

    hsw: Hsw

    visel: Visel

proc cyclesPerSample(): int64 =
    gekkoCyclesPerViCycle[viclk.s] * 2

proc rasterYPos(timestamp: int64): int64 =
    curFramePhaseStartY +
        ((((timestamp - curFramePhaseStartTimestamp) div cyclesPerSample()) + curFramePhaseStartX) div (int64(htr0.hlw) * 2))

proc rasterXPos(timestamp: int64): int64 =
    (((timestamp - curFramePhaseStartTimestamp) div cyclesPerSample()) + curFramePhaseStartX) mod (int64(htr0.hlw) * 2)

proc updateInt() =
    setExtInt extintVi, di[0].sts or di[1].sts or di[2].sts or di[3].sts

proc rescheduleInt(timestamp: int64, num: int) =
    if diEvents[num] != InvalidEventToken:
        diEvents[num].cancelEvent()

    let
        curHPosition = rasterXPos(timestamp)
        curLine = rasterYPos(timestamp)

    viLog &"rescheduling vi {num} {di[num].enb} {di[num].hct} {di[num].vct}"
    if dcr.enb and
        di[num].enb and
        not di[num].sts and
        di[num].vct > 0 and
        di[num].hct > 0 and
        curLine <= int64(di[num].vct) - 1 and
        curHPosition <= int64(di[num].hct) - 1:

        let
            intTimestamp = timestamp +
                (if curLine == int64(di[num].vct) - 1:
                    int64(di[num].hct - 1) - curHPosition
                else:
                    int64(htr0.hlw) * 2 - curHPosition +
                    int64(di[num].hct - 1) +
                    (int64(di[num].vct) - 1 - curLine - 1) * int64(htr0.hlw) * 2) * cyclesPerSample()
        viLog &"sucessfully {intTimestamp} {timestamp} {curHPosition} {curLine}"

        assert rasterXPos(intTimestamp) == int64(di[num].hct) - 1
        assert rasterYPos(intTimestamp) == int64(di[num].vct) - 1

        diEvents[num] = scheduleEvent(intTimestamp,
            1, proc(timestamp: int64) =
                viLog &"di event {di[num].hct} {di[num].vct} {rasterXPos(timestamp)} {rasterYPos(timestamp)} {timestamp}"
                assert rasterXPos(timestamp) == int64(di[num].hct) - 1
                assert rasterYPos(timestamp) == int64(di[num].vct) - 1
                diEvents[num] = InvalidEventToken
                di[num].sts = true
                updateInt())


func toFix8(x: float): int32 =
    int32(x * float(0x1000))

func convertYuvToRgb(y, cb, cr: uint8): (uint8, uint8, uint8) =
    # this is for you Hydr8gon
    let yFix = int32(y) shl 12

    let
        resFixR = (yFix + toFix8(1.371) * (int32(cr) - 128)) shr 12
        resFixG = (yFix - toFix8(0.698) * (int32(cr) - 128) - toFix8(0.336) * (int32(cb) - 128)) shr 12
        resFixB = (yFix + toFix8(1.732) * (int32(cb) - 128)) shr 12

    result[0] = uint8(clamp(resFixR, 0, 255))
    result[1] = uint8(clamp(resFixG, 0, 255))
    result[2] = uint8(clamp(resFixB, 0, 255))

proc packRgb(r, g, b: uint8): uint32 =
    uint32(r) or (uint32(g) shl 8) or (uint32(b) shl 16) or (0xFF'u32 shl 24)

proc convertLineYuvToRgb(dst: var openArray[uint32], src: openArray[uint32], width: int) =
    for x in 0..<width div 2:
        let
            srcSample = src[x]
            y1 = uint8 srcSample
            y2 = uint8(srcSample shr 16)
            cb = uint8(srcSample shr 8)
            cr = uint8(srcSample shr 24)

            (r1, g1, b1) = convertYuvToRgb(y1, cb, cr)
            (r2, g2, b2) = convertYuvToRgb(y2, cb, cr)

        dst[x * 2] = packRgb(r1, g1, b1)
        dst[x * 2 + 1] = packRgb(r2, g2, b2)

proc calcFramebufferAddr(odd: bool): uint32 =
    let fieldBase = if odd: tfbl else: bfbl

    result = fieldBase.fbb
    # this is not a mistake
    # xof and pageOffset exist only once
    if tfbl.pageOffset:
        result = result shl 5
    else:
        result = result and not(0x1F'u32)

    result += tfbl.xof * 2

proc readOutField(odd: bool) =
    let frameWidth = hsw.wpl * 16
    var
        frameHeight = vtr.acv
        frameStride = hsw.std * 32
        frameAdr = calcFramebufferAddr(odd)

    #assert abs(int(calcFramebufferAddr(true)) - int(calcFramebufferAddr(false))) == int(frameStride div 2),
    #    &"proper interlacing isn't supported {frameStride} {frameWidth} {calcFramebufferAddr(true):08X} {calcFramebufferAddr(false):08X}"
    if not dcr.prog and abs(int(calcFramebufferAddr(true)) - int(calcFramebufferAddr(false))) == int(frameStride div 2):
        frameHeight *= 2
        frameStride = frameStride div 2

        # pretty pointless how everything is called "top" and "bottom" field here
        # when in the end the field order is determined by the timing registers anyway
        if odd and vto.prb == vte.prb + 1:
            frameAdr -= frameStride
        if not odd and vto.prb == vte.prb - 1:
            frameAdr -= frameStride

    echo &"field read out {frameAdr:08X} {frameWidth}*{frameHeight} {uint32(tfbl):08X} {uint32(bfbl):08X} {calcFramebufferAddr(false):08X} {calcFramebufferAddr(true):08X} stride {frameStride}"

    var
        frameDataRgba = newSeq[uint32](frameWidth * frameHeight)
    for i in 0..<frameHeight:
        convertLineYuvToRgb(toOpenArray(frameDataRgba, int(i*frameWidth), int((i+1)*frameWidth-1)),
            toOpenArray(cast[ptr UncheckedArray[uint32]](addr mainRAM[frameAdr]), 0, int(frameWidth) div 2 - 1),
            int frameWidth)
        frameAdr += frameStride

    presentFrame int frameWidth, int frameHeight, frameDataRgba

proc currentPhaseHalfLines(): int64 =
    int64(case curFramePhase
        of framePhaseEquOdd, framePhaseEquEven: vtr.equ * 3
        of framePhaseAcvOdd, framePhaseAcvEven: vtr.acv * 2
        of framePhasePrbOdd: vto.prb
        of framePhasePsbOdd: vto.psb
        of framePhasePrbEven: vte.prb
        of framePhasePsbEven: vte.psb)

proc currentTb(state: PpcState): uint64 =
    uint64((gekkoTimestamp - state.tbInitTimestamp) div gekkoCyclesPerTbCycle) + state.tbInit

proc startTimingPhase(timestamp: int64) =
    if curFramePhase != high(FramePhase):
        let
            curX = rasterXPos(timestamp)
            curY = rasterYPos(timestamp)

        curFramePhaseStartX = curX
        curFramePhaseStartY = curY

        curFramePhase.inc()
    else:
        curFramePhaseStartX = 0
        curFramePhaseStartY = 0

        curFramePhase = low(FramePhase)

    curFramePhaseStartTimestamp = timestamp

    if curFramePhase == framePhaseEquOdd:
        for i in 0..<4:
            if diEvents[i] != InvalidEventToken:
                cancelEvent diEvents[i]
            rescheduleInt(timestamp, i)

    if curFramePhase in {framePhasePsbOdd, framePhasePsbEven}:
        startSiPoll timestamp, int64(htr0.hlw) * 2 * cyclesPerSample()

        if vtr.acv == 0:
            presentBlankFrame()
        else:
            readOutField(curFramePhase == framePhasePsbOdd)
    let
        halfLinesUntilNextPhase = currentPhaseHalfLines()
        nextTimestamp = timestamp + int64(halfLinesUntilNextPhase) * int64(htr0.hlw) * cyclesPerSample()

    echo &"transition frame phase {gekkoState.pc:08X} {curFramePhase} {halfLinesUntilNextPhase} {nextTimestamp} {htr0.hlw} {curFramePhaseStartX} {curFramePhaseStartY} {curFramePhaseStartTimestamp} {gekkoState.currentTb()}"

    nextFramePhaseEvent = scheduleEvent(nextTimestamp, 0, startTimingPhase)

template rescheduleFramePhase(doChange): untyped =
    #[if dcr.enb:
        let src = instantiationInfo()
        viLog "resched frame phase " & $src

        let
            prevHalfLines = currentPhaseHalfLines()
            advancement = gekkoTimestamp - curFramePhaseStartTimestamp
        curFramePhaseStartX = rasterXPos(gekkoTimestamp)
        curFramePhaseStartY = rasterYPos(gekkoTimestamp)
        curFramePhaseStartTimestamp = gekkoTimestamp

        doChange

        for i in 0..<4:
            if diEvents[i] != InvalidEventToken:
                cancelEvent diEvents[i]
            rescheduleInt(gekkoTimestamp, i)

        let
            halfLines = currentPhaseHalfLines()
            cycles = halfLines * int64(htr0.hlw) * cyclesPerSample()
        if prevHalfLines != halfLines:
            viLog "changed relevant register"

            cancelEvent nextFramePhaseEvent
            if advancement >= cycles:
                startTimingPhase(gekkoTimestamp)
            else:
                nextFramePhaseEvent = scheduleEvent(curFramePhaseStartTimestamp + (cycles - advancement), 0, startTimingPhase)
    else:]#
    doChange

visel.digital = true

ioBlock vi, 0x100:
of vtr, 0x00, 2:
    read: uint16 vtr
    write:
        viLog &"write vtr {val:02X}"
        rescheduleFramePhase:
            vtr.mutable = val
of dcr, 0x02, 2:
    read: uint16 dcr
    write:
        dcr.mutable = val
        viLog &"write dcr {val:02X} {gekkoState.pc:08X}"
        let val = Dcr val
        if not(dcr.enb) and val.enb:
            dcr.enb = true
            viLog "enable video"
            curFramePhaseStartTimestamp = gekkoTimestamp
            curFramePhase = framePhasePsbEven
            nextFramePhaseEventTimestmap = gekkoTimestamp
            if nextFramePhaseEvent != InvalidEventToken:
                cancelEvent nextFramePhaseEvent
            startTimingPhase(gekkoTimestamp)
of htr0, 0x04, 4:
    read: uint32 htr0
    write:
        rescheduleFramePhase:
            htr0.mutable = val
of htr1, 0x08, 4:
    read: uint32 htr1
    write:
        rescheduleFramePhase:
            htr1.mutable = val
of vto, 0x0C, 4:
    read: uint32 vto
    write:
        viLog &"write vto {val:02X}"
        vto.mutable = val
of vte, 0x10, 4:
    read: uint32 vte
    write:
        viLog &"write vte {val:02X}"
        rescheduleFramePhase:
            vte.mutable = val
of tfbl, 0x1C, 4:
    read: uint32 tfbl
    write:
        if FieldBase(val).clear:
            tfbl.mutableBL = 0
        else:
            tfbl.mutableBL = val
of tfbr, 0x20, 4:
    read: uint32 tfbr
    write:
        tfbr.mutableR = val
of bfbl, 0x24, 4:
    read: uint32 bfbl
    write:
        bfbl.mutableBL = val
of bfbr, 0x28, 4:
    read: uint32 bfbr
    write: bfbr.mutableR = val
of dpv, 0x2C, 2:
    read: result = uint16(rasterYPos(gekkoTimestamp) + 1); echo &"read dpv position {result} {gekkoTimestamp} {gekkoState.pc:08X}"
    write: echo "raster beam position moved vertically (is that even possible welp!)"
of dph, 0x2E, 2:
    read: result = uint16(rasterXPos(gekkoTimestamp) + 1)
    write: echo "raster beam position horizontally welp (is that even possible welp!)"
of di, 0x30, 4, 4:
    read: uint32 di[idx]
    write:
        di[idx].mutable = val
        if not Di(val).sts:
            di[idx].sts = false
            updateInt()
        rescheduleInt(gekkoTimestamp, int idx)
of hsw, 0x48, 2:
    read: uint16 hsw
    write: hsw = Hsw val
of viclk, 0x6C, 2:
    read: uint16 viclk
    write:
        rescheduleFramePhase:
            viclk = Viclk val
of visel, 0x6E, 2:
    read: uint16 visel
