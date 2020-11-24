import
    util/ioregs, util/bitstruct,
    cycletiming,
    gecko/gecko, si/si,
    frontend/sdl,

    strformat

# cycles here generally refer to VI cycles

makeBitStruct uint16, Vtr: # vertical timing
    equ[0..3] {.mutable.}: uint32 # equalisation pulse, apparently in one and a half lines
    acv[4..13] {.mutable.}: uint32 # active video in full lines

makeBitStruct uint16, Dcr: # display config
    # TODO: find out how these two behave
    enb[0] {.mutable.}: bool # enable
    rst[1] {.mutable.}: bool # reset
    i[2] {.mutable.}: bool # interlacing
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

var
    vtr: Vtr
    htr0: Htr0
    htr1: Htr1

    vto, vte: Vt

    dcr: Dcr

    tfbl, bfbl: FieldBase
    tfbr, bfbr: FieldBase

    di: array[4, Di]
    diEvents = [InvalidEventToken, InvalidEventToken, InvalidEventToken, InvalidEventToken]

    hsw: Hsw

    viclk: Viclk
    visel: Visel

    framestartTimestamp: int64

    # this is where we read out the current framebuffer
    frameReadOut = InvalidEventToken
    nextFieldEvent = InvalidEventToken

proc rasterYPos(timestamp: int64): int64 =
    (timestamp - framestartTimestamp) div (int64(htr0.hlw) * 2 * geckoCyclesPerViCycle[viclk.s])

proc rasterXPos(timestamp: int64): int64 =
    (timestamp - framestartTimestamp) div geckoCyclesPerViCycle[viclk.s] mod (int64(htr0.hlw) * 2)

proc triggerInt(num: int) =
    #echo "vi interrupt ", num, " ", di[num].hct, " ", di[num].vct
    di[num].sts = true
    triggerInt extintVi

proc rescheduleInt(timestamp: int64, num: int) =
    if diEvents[num] != InvalidEventToken:
        diEvents[num].cancelEvent()

    #echo &"rescheduling vi {num} {di[num].enb} {di[num].vct} {di[num].hct}"

    if dcr.enb and
        di[num].enb and
        not di[num].sts and
        di[num].vct > 0 and
        di[num].hct > 0 and
        rasterYPos(timestamp) <= int64(di[num].vct - 1) and
        rasterXPos(timestamp) <= int64(di[num].hct - 1):

        let
            cycle = geckoCyclesPerViCycle[viclk.s]
            intTimestamp = framestartTimestamp + int64(di[num].hct - 1) * cycle + int64(di[num].vct - 1) * int64(htr0.hlw) * 2 * cycle
        #echo &"sucessfully {intTimestamp} {timestamp}"
        diEvents[num] = scheduleEvent(intTimestamp,
            1, proc(timestamp: int64) =
                diEvents[num] = InvalidEventToken
                triggerInt(num))


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

proc convertYuvToRgb(dst: var openArray[uint32], src: openArray[uint32], width, height: int) =
    for y in 0..<height:
        for x in 0..<width div 2:
            let
                srcSample = src[y * (width div 2) + x]
                y1 = uint8 srcSample
                y2 = uint8(srcSample shr 16)
                cb = uint8(srcSample shr 8)
                cr = uint8(srcSample shr 24)

                (r1, g1, b1) = convertYuvToRgb(y1, cb, cr)
                (r2, g2, b2) = convertYuvToRgb(y2, cb, cr)

            dst[y * width + x * 2] = packRgb(r1, g1, b1)
            dst[y * width + x * 2 + 1] = packRgb(r2, g2, b2)

proc calcFramebufferAddr(odd: bool): uint32 =
    let fieldBase = if odd: tfbl else: bfbl

    result = fieldBase.fbb
    if fieldBase.pageOffset:
        result = result shl 5
    else:
        result = result and not(0x1F'u32)

    result += tfbl.xof * 2

proc onVblank(odd: bool, timestamp: int64) =
    startSiPoll timestamp, int64(htr0.hlw) * 2 * geckoCyclesPerViCycle[viclk.s]

    let
        frameWidth = hsw.wpl * 16
        frameHeight = vtr.acv
    
    assert dcr.i, "interlacing is currently not supported!"

    echo &"field out read {frameWidth}*{frameHeight}"

    var
        frameDataYuv = newSeq[byte](frameWidth * frameHeight * 2)
        frameDataRgba = newSeq[uint32](frameWidth * frameHeight)
    copyMem(addr frameDataYuv[0], addr MainRAM[calcFramebufferAddr odd], frameDataYuv.len)

    convertYuvToRgb frameDataRgba,
        toOpenArray(cast[ptr UncheckedArray[uint32]](addr frameDataYUV[0]), 0, int(frameWidth * frameHeight div 2)),
        int(frameWidth), int(frameHeight)

    presentFrame int frameWidth, int frameHeight, frameDataRgba

proc startField(timestamp: int64, odd: bool) =
    if not dcr.enb:
        return

    if nextFieldEvent != InvalidEventToken:
        cancelEvent nextFieldEvent
    if frameReadOut != InvalidEventToken:
        cancelEvent frameReadOut

    if odd:
        framestartTimestamp = timestamp
        for i in 0..<4:
            rescheduleInt(timestamp, i)

    let
        vblankingReg = if odd: vto else: vte
        fieldHalfLines = vtr.acv * 2 + vtr.equ * 3 + vblankingReg.prb + vblankingReg.psb
        fieldActiveEndHalfLines = vblankingReg.psb + vtr.acv * 2

        nextFieldTimestamp = timestamp + int64(fieldHalfLines) * int64(htr0.hlw) * geckoCyclesPerViCycle[viclk.s]
        nextReadOut = timestamp + ((int64(fieldActiveEndHalfLines) - 1) * int64(htr0.hlw) + int64(htr1.hbs)) * geckoCyclesPerViCycle[viclk.s]

    echo &"starting field {fieldHalfLines}, {odd} {vtr.acv}"

    nextFieldEvent = scheduleEvent(nextFieldTimestamp,
        0, proc(timestamp: int64) =
            nextFieldEvent = InvalidEventToken
            startField(timestamp, odd xor true))
    if vtr.acv > 0:
        frameReadOut = scheduleEvent(nextReadOut,
            0, proc(timestamp: int64) =
                frameReadOut = InvalidEventToken
                onVblank(odd, timestamp))
    else:
        presentBlankFrame()

proc rescheduleVi =
    startField(geckoTimestamp, true)

visel.digital = true

ioBlock vi, 0x100:
of vtr, 0x00, 2:
    read: uint16 vtr
    write:
        vtr.mutable = val
        rescheduleVi()
of dcr, 0x02, 2:
    read: uint16 dcr
    write:
        dcr.mutable = val
        rescheduleVi()
of htr0, 0x04, 4:
    read: uint32 htr0
    write:
        htr0.mutable = val
        rescheduleVi()
of htr1, 0x08, 4:
    read: uint32 htr1
    write:
        htr1.mutable = val
        rescheduleVi()
of vto, 0x0C, 4:
    read: uint32 vto
    write:
        vto.mutable = val
        rescheduleVi()
of vte, 0x10, 4:
    read: uint32 vte
    write:
        vte.mutable = val
        rescheduleVi()
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
    read: result = uint16(rasterYPos(geckoTimestamp) + 1); echo &"read current position {result} {geckoState.pc:08X}"
    write: echo "raster beam position moved vertically (is that even possible welp!)"
of dph, 0x2E, 2:
    read: result = uint16(rasterXPos(geckoTimestamp) + 1); echo "read current hposition ", result
    write: echo "raster beam position horizontally welp (is that even possible welp!)"
of di, 0x30, 4, 4:
    read: uint32 di[idx]
    write:
        di[idx].mutable = val
        if not Di(val).sts:
            di[idx].sts = false
        rescheduleInt(geckoTimestamp, int idx)
of hsw, 0x48, 2:
    read: uint16 hsw
    write: hsw = Hsw val
of viclk, 0x6C, 2:
    read: uint16 viclk
    write:
        viclk = Viclk val
        rescheduleVi()
of visel, 0x6E, 2:
    read: uint16 visel
