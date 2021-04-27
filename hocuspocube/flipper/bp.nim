import
    strformat,

    ../util/bitstruct,

    ../gekko/gekko,

    rasterinterfacecommon,

    pe

type
    CopyMode = enum
        copyTexture
        copyXfb

    TxTextureFmt* = enum
        txTexfmtI4
        txTexfmtI8
        txTexfmtIA4
        txTexfmtIA8
        txTexfmtRGB565
        txTexfmtRGB5A3
        txTexfmtRGBA8
        txTexfmtReserved1
        txTexfmtC4
        txTexfmtC8
        txTexfmtC14X2
        txTexfmtReserved2
        txTexfmtReserved3
        txTexfmtReserved4
        txTexfmtCmp
        txTexfmtReserved5

    TxLutFmt* = enum
        txLutfmtIA8
        txLutfmtRGB565
        txLutfmtRGB5A3
        txLutfmtReserved

    Ras1TrefColor* = enum
        ras1trefColorColor0
        ras1trefColorColor1
        ras1trefColorReserved1
        ras1trefColorReserved2
        ras1trefColorReserved3
        ras1trefColorBump
        ras1trefColorBumpN
        ras1trefColorZero

    TevColorEnvSel* = enum
        ccCPrev
        ccAPrev
        ccC0
        ccA0
        ccC1
        ccA1
        ccC2
        ccA2
        ccTexC
        ccTexA
        ccRasC
        ccRasA
        ccOne
        ccHalf
        ccKonst
        ccZero

    TevBias* = enum
        tevBiasZero
        tevBiasHalf
        tevBiasMinusHalf
        # not actually a bias. It's just the way they squeezed the other operations in here
        tevBiasCompareOp

    TevScale* = enum
        tevScale1
        tevScale2
        tevScale4
        tevScaleHalf

    TevAlphaEnvSel* = enum
        caAPrev
        caA0
        caA1
        caA2
        caTexA
        caRasA
        caKonst
        caZero

    TevKColorSel* = enum
        tevKColorSel1
        tevKColorSel7of8
        tevKColorSel3of4
        tevKColorSel5of8
        tevKColorSel1of2
        tevKColorSel3of8
        tevKColorSel1of4
        tevKColorSel1of8
        tevKColorSelReserved1
        tevKColorSelReserved2
        tevKColorSelReserved3
        tevKColorSelReserved4
        tevKColorSelK0
        tevKColorSelK1
        tevKColorSelK2
        tevKColorSelK3
        tevKColorSelK0R
        tevKColorSelK1R
        tevKColorSelK2R
        tevKColorSelK3R
        tevKColorSelK0G
        tevKColorSelK1G
        tevKColorSelK2G
        tevKColorSelK3G
        tevKColorSelK0B
        tevKColorSelK1B
        tevKColorSelK2B
        tevKColorSelK3B
        tevKColorSelK0A
        tevKColorSelK1A
        tevKColorSelK2A
        tevKColorSelK3A

    TevKAlphaSel* = enum
        tevkAlphaSel1
        tevkAlphaSel7of8
        tevkAlphaSel3of4
        tevkAlphaSel5of8
        tevkAlphaSel1of2
        tevkAlphaSel3of8
        tevkAlphaSel1of4
        tevkAlphaSel1of8
        tevkAlphaSelReserved1
        tevkAlphaSelReserved2
        tevkAlphaSelReserved3
        tevkAlphaSelReserved4
        tevkAlphaSelReserved5
        tevkAlphaSelReserved6
        tevkAlphaSelReserved7
        tevkAlphaSelReserved8
        tevkAlphaSelK0R
        tevkAlphaSelK1R
        tevkAlphaSelK2R
        tevkAlphaSelK3R
        tevkAlphaSelK0G
        tevkAlphaSelK1G
        tevkAlphaSelK2G
        tevkAlphaSelK3G
        tevkAlphaSelK0B
        tevkAlphaSelK1B
        tevkAlphaSelK2B
        tevkAlphaSelK3B
        tevkAlphaSelK0A
        tevkAlphaSelK1A
        tevkAlphaSelK2A
        tevkAlphaSelK3A

    AlphaCompLogic* = enum
        alphaCompLogicAnd
        alphaCompLogicOr
        alphaCompLogicXor
        alphaCompLogicXnor

    ZEnvOp* = enum
        zenvOpDisable
        zenvOpAdd
        zenvOpReplace
        zenvOpReserved

    ZEnvOpType* = enum
        zenvOpTypeU8
        zenvOpTypeU16
        zenvOpTypeU24
        zenvOpTypeReserved

makeBitStruct uint32, *GenMode:
    ntex[0..3]: uint32
    ncol[4..8]: uint32
    msen[9]: bool
    ntev[10..13]: uint32
    cullmode[14..15]: CullFace
    nbmp[16..18]: uint32
    zfreeze[19]: bool

makeBitStruct uint32, CopyExecute:
    mode[14]: CopyMode
    clear[11]: bool

makeBitStruct uint32, EfbCoordPair:
    x[0..9]: uint32
    y[10..24]: uint32

makeBitStruct uint32, EfbCopyStride:
    stride[0..9]: uint32

makeBitStruct uint32, ScissorCoords:
    y[0..11]: uint32
    x[12..23]: uint32

makeBitStruct uint32, ScissorOffset:
    x[0..9]: uint32
    y[10..23]: uint32

makeBitStruct uint32, *ZMode:
    enable[0]: bool
    fun[1..3]: CompareFunction
    update[4]: bool

makeBitStruct uint32, *PeCMode0:
    # still misses all the logicop stuff
    blendEnable[0]: bool
    colorUpdate[3]: bool
    alphaUpdate[4]: bool
    dstFactor[5..7]: BlendFactor
    srcFactor[8..10]: BlendFactor
    blendOp[11]: BlendOp

makeBitStruct uint32, *Ras1Tref:
    texmap0[0..2]: uint32
    texcoord0[3..5]: uint32
    texmapEnable0[6]: bool
    color0[7..9]: Ras1TrefColor

    texmap1[12..14]: uint32
    texcoord1[15..17]: uint32
    texmapEnable1[18]: bool
    color1[19..21]: Ras1TrefColor

makeBitStruct uint32, *TevColorEnv:
    seld[0..3]: TevColorEnvSel
    selc[4..7]: TevColorEnvSel
    selb[8..11]: TevColorEnvSel
    sela[12..15]: TevColorEnvSel
    bias[16..17]: TevBias
    sub[18]: bool
    clamp[19]: bool
    scale[20..21]: TevScale
    dst[22..23]: uint32

makeBitStruct uint32, *TevAlphaEnv:
    rswap[0..1]: uint32
    tswap[2..3]: uint32
    seld[4..6]: TevAlphaEnvSel
    selc[7..9]: TevAlphaEnvSel
    selb[10..12]: TevAlphaEnvSel
    sela[13..15]: TevAlphaEnvSel
    bias[16..17]: TevBias
    sub[18]: bool
    clamp[19]: bool
    scale[20..21]: TevScale
    dst[22..23]: uint32

makeBitStruct uint32, *TevRegister:
    r[0..10]: uint32
    b[0..10]: uint32
    a[12..22]: uint32
    g[12..22]: uint32
    setKonst[23]: bool

makeBitStruct uint32, *TevKSel:
    swaprb[0..1]: uint32
    swapga[2..3]: uint32
    kcsel0[4..8]: TevKColorSel
    kasel0[9..13]: TevKAlphaSel
    kcsel1[14..18]: TevKColorSel
    kasel1[19..23]: TevKAlphaSel

makeBitStruct uint32, *SuSize:
    size[0..15]: uint32
    rangeBias[16]: bool
    cylindricalWrapping[17]: bool
    texcoordLinesOffset[18]: bool
    texcoordPointOffset[19]: bool

makeBitStruct uint32, *TxSetMode0:
    wrapS[0..1]: TextureWrapMode
    wrapT[2..3]: TextureWrapMode
    magFilter[4]: TextureMagFilter
    minFilter[5..7]: TextureMinFilter
    diaglod[8]: bool
    loadbias[9..18]: uint32
    maxani[19..20]: uint32
    lodclamp[21]: bool

makeBitStruct uint32, *TxSetMode1:
    minlod[0..7]: uint32
    maxlod[8..15]: uint32

makeBitStruct uint32, *TxSetImage0:
    width[0..9]: uint32
    height[10..19]: uint32
    fmt[20..23]: TxTextureFmt

makeBitStruct uint32, *TxSetImage12:
    tmemOffset[0..14]: uint32
    cacheWidth[15..17]: uint32
    cacheHeight[18..20]: uint32
    preloaded[21]: bool

makeBitStruct uint32, *TxSetLut:
    tmemOffset[0..9]: uint32
    fmt[10..11]: TxLutFmt

makeBitStruct uint32, *AlphaCompare:
    ref0[0..7]: uint32
    ref1[8..15]: uint32
    comp0[16..18]: CompareFunction
    comp1[19..21]: CompareFunction
    logic[22..23]: AlphaCompLogic

makeBitStruct uint32, *TevZEnv1:
    typ[0..1]: ZEnvOpType
    op[2..3]: ZEnvOp

proc convertRgbToYuv(r0, g0, b0, r1, g1, b1: uint8): (uint8, uint8, uint8, uint8) =
    result[0] = uint8 clamp(((int32(r0) * 77) div 256) + ((int32(g0) * 150) div 256) + ((int32(b0) * 29) div 256), 0, 255)
    result[1] = uint8 clamp(((int32(r1) * 77) div 256) + ((int32(g1) * 150) div 256) + ((int32(b1) * 29) div 256), 0, 255)

    let
        r = (int32(r0) + int32(r1)) div 2
        g = (int32(g0) + int32(g1)) div 2
        b = (int32(b0) + int32(b1)) div 2
    result[2] = uint8 clamp(((-44 * r) div 256) - ((87 * g) div 256) + ((131 * b) div 256) + 128, 0, 255)
    result[3] = uint8 clamp(((131 * r) div 256) - ((110 * g) div 256) - ((21 * b) div 256) + 128, 0, 255)

func unpackRgb(val: uint32): (uint8, uint8, uint8) =
    result[0] = uint8(val)
    result[1] = uint8(val shr 8)
    result[2] = uint8(val shr 16)

proc convertLineRgbToYuv(dst: ptr UncheckedArray[uint32], src: openArray[uint32], width: int) =
    for x in 0..<width div 2:
        let
            (r0, g0, b0) = unpackRgb(src[x * 2])
            (r1, g1, b1) = unpackRgb(src[x * 2 + 1])

            (y0, y1, u, v) = convertRgbToYuv(r0, g0, b0, r1, g1, b1)
        dst[x] = y0 or (uint32(y1) shl 16) or (uint32(u) shl 8) or (uint32(v) shl 24)

type TexMap* = object
    setMode0*: TxSetMode0
    setMode1*: TxSetMode1
    setImage0*: TxSetImage0
    setImage1*, setImage2*: TxSetImage12
    setImage3*: uint32
    setLut*: TxSetLut

proc adr*(map: TexMap): uint32 = map.setImage3 shl 5
proc width*(map: TexMap): uint32 = map.setImage0.width + 1
proc height*(map: TexMap): uint32 = map.setImage0.height + 1
proc mipmapsEnabled*(map: TexMap): bool =
    map.setMode0.minFilter in {textureMinFilterLinMipLin, textureMinFilterLinMipNear, textureMinFilterNearMipLin, textureMinFilterNearMipNear}
proc levels*(map: TexMap): uint32 =
    if map.mipmapsEnabled:
        1'u32
    else:
        (map.setMode1.maxlod + 15) shr 4

var
    clearR, clearG, clearB, clearA: uint8
    clearZ: uint32

    efbCopyDst: uint32
    efbCopyDstStride: EfbCopyStride
    efbCopySrcPos, efbCopySize: EfbCoordPair

    scissorTL, scissorBR: ScissorCoords
    scissorOffset: ScissorOffset

    copyExecute: CopyExecute

    zmode*: ZMode

    sSize*: array[8, SuSize]
    tSize*: array[8, SuSize]

    texMaps*: array[8, TexMap]

    peCMode0*: PeCMode0

    colorEnv*: array[16, TevColorEnv]
    alphaEnv*: array[16, TevAlphaEnv]
    ras1Tref*: array[8, Ras1Tref]

    zenv0*: uint32
    zenv1*: TevZEnv1

    tevRegister*: array[8, TevRegister]
    konstants*: array[8, TevRegister]

    tevKSel*: array[8, TevKSel]

    genMode*: GenMode

    alphaCompare*: AlphaCompare

    efbCopyStepY: uint32

    bpMask: uint32

proc getRas1Tref*(regs: array[8, Ras1Tref], i: uint32):
    tuple[texmap: uint32, texcoord: uint32, texmapEnable: bool, color: Ras1TrefColor] =

    let reg = regs[i div 2]
    if (i mod 2) == 0:
        (reg.texmap0, reg.texcoord0, reg.texmapEnable0, reg.color0)
    else:
        (reg.texmap1, reg.texcoord1, reg.texmapEnable1, reg.color1)

proc getTevKSel*(regs: array[8, TevKSel], i: uint32): (TevKColorSel, TevKAlphaSel) =
    let reg = regs[i div 2]
    if (i mod 2) == 0:
        (reg.kcsel0, reg.kasel0)
    else:
        (reg.kcsel1, reg.kasel1)

proc getScissor*(): (int32, int32, int32, int32) =
    result[0] = int32(scissorTL.x) - 342
    result[1] = int32(scissorTL.y) - 342
    result[2] = int32(scissorBR.x) - int32(scissorTL.x) + 1
    result[3] = int32(scissorBR.y) - int32(scissorTL.y) + 1

proc getScissorOffset*(): (int32, int32) =
    result[0] = int32(scissorOffset.x) * 2 - 342
    result[1] = int32(scissorOffset.y) * 2 - 342

import
    rasterinterface

proc maskedWrite[T](register: var T, val: uint32): bool {.discardable.} =
    let prevValue = register
    register = T((uint32(register) and not(bpMask)) or (val and bpMask))
    register != prevValue

proc bpWrite*(adr, val: uint32) =
    case adr
    of 0x00:
        if genMode.maskedWrite val:
            rasterStateDirty = true
    of 0x01..0x04:
        # copy filter stuff
        discard
    of 0x20:
        if scissorTL.maskedWrite val:
            rasterStateDirty = true
    of 0x21:
        if scissorBR.maskedWrite val:
            rasterStateDirty = true
    of 0x28..0x2F:
        ras1Tref[adr - 0x28].maskedWrite val
    of 0x30..0x3F:
        if
            (if (adr mod 2) == 0:
                sSize[(adr - 0x30) div 2].maskedWrite val
            else:
                tSize[(adr - 0x30) div 2].maskedWrite val):
            registerUniformDirty = true
    of 0x40:
        if zmode.maskedWrite val:
            rasterStateDirty = true
    of 0x41:
        if peCMode0.maskedWrite val:
            rasterStateDirty = true
    of 0x49:
        efbCopySrcPos.maskedWrite val
    of 0x4A:
        efbCopySize.maskedWrite val
    of 0x4B:
        efbCopyDst.maskedWrite val
    of 0x4D:
        efbCopyDstStride.maskedWrite val
    of 0x45:
        if ((val and bpMask) and 0x2) != 0:
            finishFrame()

            pe.flagFinish()
            echo "pe finish!"
    of 0x4E:
        efbCopyStepY.maskedWrite val
    of 0x4F:
        clearR = uint8(val)
        clearA = uint8(val shr 8)
    of 0x50:
        clearB = uint8(val)
        clearG = uint8(val shr 8)
    of 0x51:
        clearZ.maskedWrite val
    of 0x52:
        copyExecute.maskedWrite val

        let
            srcX = efbCopySrcPos.x
            srcY = efbCopySrcPos.y
            width = efbCopySize.x
            height = efbCopySize.y
            stride = efbCopyDstStride.stride shl 5

        echo &"copy execute {srcX}, {srcY} {width}x{height} to {efbCopyDst:08X} stride: {stride} {efbCopyStepY}"

        doAssert copyExecute.mode == copyXfb

        var efbContent = newSeq[uint32](width * height)
        retrieveFrame(efbContent, srcX, srcY, width, height)

        var
            adr = efbCopyDst shl 5
            uniqueAdr = efbCopyDst shl 5
            line = efbCopyStepY - 1 # in .8 fix point like efbCopyStepY
            copied = 0
        for i in 0..<height:
            uniqueAdr = adr
            convertLineRgbToYuv(cast[ptr UncheckedArray[uint32]](addr mainRAM[uniqueAdr]),
                toOpenArray(efbContent, int (height-i-1)*width, int (height-i-1+1)*width-1),
                int width)
            adr += stride
            line += efbCopyStepY
            copied += 1

            while (line shr 8) == i:
                copyMem(addr mainRAM[adr], addr mainRAM[uniqueAdr], width*2)
                line += efbCopyStepY
                adr += stride
                copied += 1

        #echo &"actually copied {copied} lines"

        if copyExecute.clear:
            rasterinterface.clear(clearR, clearG, clearB, clearA, clearZ, peCMode0.colorUpdate, peCMode0.alphaUpdate, zmode.update)
    of 0x53, 0x54:
        discard # also copy filter
    of 0x59:
        if scissorOffset.maskedWrite val:
            rasterStateDirty = true
    of 0x80..0x83:
        let idx = adr - 0x80
        if texMaps[idx].setMode0.maskedWrite val:
            samplerStateDirty.incl idx
    of 0x84..0x87:
        let idx = adr - 0x84
        if texMaps[idx].setMode1.maskedWrite val:
            samplerStateDirty.incl idx
    of 0x88..0x8B:
        let idx = adr - 0x88
        if texMaps[idx].setImage0.maskedWrite val:
            imageStateDirty.incl idx
            registerUniformDirty = true
    of 0x8C..0x8F:
        let idx = adr - 0x8C
        if texMaps[idx].setImage1.maskedWrite val:
            imageStateDirty.incl idx
    of 0x90..0x93:
        let idx = adr - 0x90
        if texMaps[idx].setImage2.maskedWrite val:
            imageStateDirty.incl idx
    of 0x94..0x97:
        let idx = adr - 0x94
        if texMaps[idx].setImage3.maskedWrite val:
            imageStateDirty.incl idx
    of 0xA0..0xA3:
        let idx = adr - 0xA0 + 4
        if texMaps[idx].setMode0.maskedWrite val:
            samplerStateDirty.incl idx
    of 0xA4..0xA7:
        let idx = adr - 0xA4 + 4
        if texMaps[idx].setMode1.maskedWrite val:
            samplerStateDirty.incl idx
    of 0xA8..0xAB:
        let idx = adr - 0xA8 + 4
        if texMaps[idx].setImage0.maskedWrite val:
            imageStateDirty.incl idx
            registerUniformDirty = true
    of 0xAC..0xAF:
        let idx = adr - 0xAC + 4
        if texMaps[idx].setImage1.maskedWrite val:
            imageStateDirty.incl idx
    of 0xB0..0xB3:
        let idx = adr - 0xB0 + 4
        if texMaps[idx].setImage2.maskedWrite val:
            imageStateDirty.incl idx
    of 0xB4..0xB7:
        let idx = adr - 0xb4 + 4
        if texMaps[idx].setImage3.maskedWrite val:
            imageStateDirty.incl idx
    of 0xC0..0xDF:
        if (adr mod 2) == 0:
            colorEnv[(adr - 0xC0) div 2].maskedWrite val
        else:
            alphaEnv[(adr - 0xC1) div 2].maskedWrite val
    of 0xE0..0xE7:
        var dirty = false
        # TODO: figure out how exactly this works
        if TevRegister(val).setKonst:
            dirty = konstants[adr - 0xE0].maskedWrite uint32(val)
        else:
            dirty = tevRegister[adr - 0xE0].maskedWrite uint32(val)
        registerUniformDirty = registerUniformDirty or dirty
    of 0xF3:
        let val = AlphaCompare val
        if alphaCompare.ref0 != val.ref0 or alphaCompare.ref1 != val.ref1:
            registerUniformDirty = true
        alphaCompare.maskedWrite uint32(val)
    of 0xF4:
        if zenv0.maskedWrite(val):
            registerUniformDirty = true
    of 0xF5:
        zenv1.maskedWrite val
    of 0xF6..0xFD:
        tevKSel[adr - 0xF6].maskedWrite val
    of 0xFE:
        bpMask = val
        return
    else: echo &"unknown bp write {adr:02X} {val:06X}"

    bpMask = 0xFFFFFF'u32