import
    ../util/bitstruct

type
    CompareFunction* = enum
        compareNever
        compareLess
        compareEqual
        compareLequal
        compareGreater
        compareNequal
        compareGequal
        compareAlways

    TextureWrapMode* = enum
        textureWrapClamp
        textureWrapRepeat
        textureWrapMirror
        textureWrapUnused
    TextureMagFilter* = enum
        textureMagFilterNear
        textureMagFilterLinear
    TextureMinFilter* = enum
        textureMinFilterNear
        textureMinFilterNearMipNear
        textureMinFilterNearMipLin
        textureMinFilterReserved1
        textureMinFilterLin
        textureMinFilterLinMipNear
        textureMinFilterLinMipLin
        textureMinFilterReserved2

    CullFace* = enum
        cullNone
        cullFront
        cullBack
        cullAll

    BlendOp* = enum
        blendAdd
        blendRevSub

    BlendFactor* = enum
        blendFactorZero
        blendFactorOne
        blendFactorSrcColor
        blendFactorInvSrcColor
        blendFactorSrcAlpha
        blendFactorInvSrcAlpha
        blendFactorDstAlpha
        blendFactorInvDstAlpha
        blendFactorDstColor
        blendFactorInvDstColor
        # these aren't actual flipper blend factors, they're
        # just here so that we don't need a second enum
        blendFactorSrc1Color
        blendFactorInvSrc1Color
        blendFactorSrc1Alpha
        blendFactorInvSrc1Alpha

    LogicOp* = enum
        loClear
        loAnd
        loRevAnd
        loCopy
        loInvAnd
        loNop
        loXor
        loOr
        loNor
        loEquiv
        loInv
        loRevOr
        loInvCopy
        loInvOr
        loNand
        loSet

    PeFmt* = enum
        peFmtRGB8Z24
        peFmtRGBA6Z24
        peFmtRGB565Z16
        peFmtZ24
        peFmtI8
        peFmtYUV420

    CmprZFmt* = enum
        cmprZLinear
        cmprZNear
        cmprZMid
        cmprZFar

    CopyMode* = enum
        copyTexture
        copyXfb

    CopyTexFmt* = enum
        copyTexfmtR4
        copyTexfmtReservedC0x1
        copyTexfmtRA4
        copyTexfmtRA8
        copyTexfmtRGB565
        copyTexfmtRGB5A3
        copyTexfmtRGBA8
        copyTexfmtA8
        copyTexfmtR8
        copyTexfmtG8
        copyTexfmtB8
        copyTexfmtRG8
        copyTexfmtGB8
        copyTexfmtReservedC0xD
        copyTexfmtReservedC0xE
        copyTexfmtReservedC0xF

    CopyTexIFmt* = enum
        copyTexfmtI4
        copyTexfmtI8
        copyTexfmtIA4
        copyTexfmtIA8
        copyTexfmtReservedI0x4
        copyTexfmtReservedI0x5
        copyTexfmtYUVA8
        copyTexfmtReservedI0x7
        copyTexfmtReservedI0x8
        copyTexfmtReservedI0x9
        copyTexfmtReservedI0xA
        copyTexfmtReservedI0xB
        copyTexfmtReservedI0xC
        copyTexfmtReservedI0xD
        copyTexfmtReservedI0xE
        copyTexfmtReservedI0xF

    CopyTexZFmt* = enum
        copyTexfmtZ4
        copyTexfmtZ8
        copyTexfmtZReserved0x2
        copyTexfmtZReserved0x3
        copyTexfmtZReserved0x4
        copyTexfmtZReserved0x5
        copyTexfmtZ24X8
        copyTexfmtZReserved0x7
        copyTexfmtZReserved0x8
        copyTexfmtZ8M
        copyTexfmtZ8L
        copyTexfmtZ16
        copyTexfmtZ16L
        copyTexfmtZReserved0xD
        copyTexfmtZReserved0xE
        copyTexfmtZReserved0xF

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

    IndTexFmt* = enum
        itf8
        itf5
        itf4
        itf3

    IndAlphaSel* = enum
        itbaOff
        itbaS
        itbaT
        itbaU

    IndMatId* = enum
        itmOff
        itm0
        itm1
        itm2
        itmS0
        itmS1
        itmS2
        itmT0
        itmT1
        itmT2

    IndWrap* = enum
        itwOff
        itw256
        itw128
        itw64
        itw32
        itw16
        itw0

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

    TevCompOperand* = enum
        tevCompOperandR8
        tevCompOperandGR16
        tevCompOperandBGR24
        tevCompOperandRGB8

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

    TexFmtProperties* = object
        tileW*, tileH*: uint8 # as log2 (to calculate proper size use 1 shl tileW/tileH)
        cacheLines*: uint8

const texFmtProperties*: array[TxTextureFmt, TexFmtProperties] = [
    # I4
    TexFmtProperties(tileW: 3, tileH: 3, cacheLines: 1),
    # I8
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # IA4
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # IA8
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # RGB565
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # RGB5A3
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # RGBA8
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 2),
    TexFmtProperties(),
    # C4
    TexFmtProperties(tileW: 3, tileH: 3, cacheLines: 1),
    # C8
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # C14X2
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    TexFmtProperties(),
    TexFmtProperties(),
    TexFmtProperties(),
    # Cmp
    TexFmtProperties(tileW: 3, tileH: 3, cacheLines: 1),
    TexFmtProperties(),
]

const texCopyFmtProperties*: array[CopyTexFmt, TexFmtProperties] = [
    # R4
    TexFmtProperties(tileW: 3, tileH: 3, cacheLines: 1),
    TexFmtProperties(),
    # RA4
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # RA8
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # RGB565
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # RGB5A3
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # RGBA8
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 2),
    # A8
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # R8
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # G8
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # B8
    TexFmtProperties(tileW: 3, tileH: 2, cacheLines: 1),
    # RG8
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    # GB8
    TexFmtProperties(tileW: 2, tileH: 2, cacheLines: 1),
    TexFmtProperties(),
    TexFmtProperties(),
    TexFmtProperties(),
]

proc numTilesX*(fmt: TexFmtProperties, width: uint32): uint32 =
    (width + (1'u32 shl fmt.tileW) - 1) shr fmt.tileW
proc numTilesY*(fmt: TexFmtProperties, height: uint32): uint32 =
    (height + (1'u32 shl fmt.tileH) - 1) shr fmt.tileH
proc roundedWidth*(fmt: TexFmtProperties, width: uint32): uint32 =
    fmt.numTilesX(width) shl fmt.tileW
proc roundedHeight*(fmt: TexFmtProperties, height: uint32): uint32 =
    fmt.numTilesY(height) shl fmt.tileH

proc textureDataSize*(fmt: TexFmtProperties, width, height: uint32): uint32 =
    let
        numTilesX = fmt.numTilesX(width)
        numTilesY = fmt.numTilesY(height)

    result = numTilesX * numTilesY * 32
    if fmt.cacheLines == 2:
        result *= 2

const PalettedTexFmts* = {txTexfmtC4, txTexfmtC8, txTexfmtC14X2}

makeBitStruct uint32, *GenMode:
    ntex[0..3]: uint32
    ncol[4..8]: uint32
    msen[9]: bool
    ntev[10..13]: uint32
    cullmode[14..15]: CullFace
    nbmp[16..18]: uint32
    zfreeze[19]: bool

makeBitStruct uint32, *CopyExecute:
    fmtHi[3]: uint32
    fmtLo[4..6]: uint32
    mipmap[9]: bool
    yscale[10]: bool
    clear[11]: bool
    mode[14]: CopyMode
    intensity[15]: bool

proc fmt*(copyExecute: CopyExecute): uint32 =
    copyExecute.fmtLo or (copyExecute.fmtHi shl 3)

makeBitStruct uint32, *EfbCoordPair:
    x[0..9]: uint32
    y[10..24]: uint32

makeBitStruct uint32, *EfbCopyStride:
    stride[0..9]: uint32

makeBitStruct uint32, *ScissorCoords:
    y[0..11]: uint32
    x[12..23]: uint32

makeBitStruct uint32, *ScissorOffset:
    x[0..9]: uint32
    y[10..23]: uint32

makeBitStruct uint32, *ZMode:
    enable[0]: bool
    fun[1..3]: CompareFunction
    update[4]: bool

makeBitStruct uint32, *PeCntrl:
    fmt[0..2]: PeFmt
    zfmt[3..4]: CmprZFmt
    zcompLoc[6]: bool

makeBitStruct uint32, *PeCMode0:
    blendEnable[0]: bool
    logicOpEnable[1]: bool
    colorUpdate[3]: bool
    alphaUpdate[4]: bool
    dstFactor[5..7]: BlendFactor
    srcFactor[8..10]: BlendFactor
    blendOp[11]: BlendOp
    logicOp[12..15]: LogicOp

makeBitStruct uint32, *PeCMode1:
    dstAlphaVal[0..7]: uint32
    dstAlphaEnable[8]: bool

makeBitStruct uint32, *IndMatElement:
    element0[0..10]: uint32
    element1[11..21]: uint32
    s[22..23]: uint32
    sTop[22]: uint32

makeBitStruct uint32, *IndCmd:
    stage[0..1]: uint32
    fmt[2..3]: IndTexFmt
    biasS[4]: bool
    biasT[5]: bool
    biasU[6]: bool
    alphaSel[7..8]: IndAlphaSel
    matId[9..12]: IndMatId
    wrapS[13..15]: IndWrap
    wrapT[16..18]: IndWrap
    utclod[19]: bool
    addprev[20]: bool

makeBitStruct uint32, *Ras1SS:
    sshift0[0..3]: uint32
    tshift0[4..7]: uint32
    sshift1[8..11]: uint32
    tshift1[12..15]: uint32

proc getRas1SS*(regs: array[2, Ras1SS], i: uint32): (uint32, uint32) =
    let reg = regs[i div 2]
    if (i mod 2) == 0:
        (reg.sshift0, reg.tshift0)
    else:
        (reg.sshift1, reg.tshift1)

makeBitStruct uint32, *Ras1Iref:
    texmap[n, n*6..n*6+2]: uint32
    coord[n, n*6+3..n*6+5]: uint32
    stage[n, n*6..n*6+5]: uint32

makeBitStruct uint32, *Ras1Tref:
    texmap0[0..2]: uint32
    texcoord0[3..5]: uint32
    texmapEnable0[6]: bool
    color0[7..9]: Ras1TrefColor

    texmap1[12..14]: uint32
    texcoord1[15..17]: uint32
    texmapEnable1[18]: bool
    color1[19..21]: Ras1TrefColor

proc getRas1Tref*(regs: array[8, Ras1Tref], i: uint32):
    tuple[texmap: uint32, texcoord: uint32, texmapEnable: bool, color: Ras1TrefColor] =

    let reg = regs[i div 2]
    if (i mod 2) == 0:
        (reg.texmap0, reg.texcoord0, reg.texmapEnable0, reg.color0)
    else:
        (reg.texmap1, reg.texcoord1, reg.texmapEnable1, reg.color1)

makeBitStruct uint32, *TevColorEnv:
    seld[0..3]: TevColorEnvSel
    selc[4..7]: TevColorEnvSel
    selb[8..11]: TevColorEnvSel
    sela[12..15]: TevColorEnvSel
    bias[16..17]: TevBias

    # for normal ops
    sub[18]: bool
    scale[20..21]: TevScale

    # for comparison ops
    equal[18]: bool
    compOp[20..21]: TevCompOperand

    clamp[19]: bool
    dst[22..23]: uint32

makeBitStruct uint32, *TevAlphaEnv:
    rswap[0..1]: uint32
    tswap[2..3]: uint32
    seld[4..6]: TevAlphaEnvSel
    selc[7..9]: TevAlphaEnvSel
    selb[10..12]: TevAlphaEnvSel
    sela[13..15]: TevAlphaEnvSel
    bias[16..17]: TevBias

    # for normal ops
    sub[18]: bool
    scale[20..21]: TevScale

    # for comparison ops
    equal[18]: bool
    compOp[20..21]: TevCompOperand

    clamp[19]: bool
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

proc getTevKSel*(regs: array[8, TevKSel], i: uint32): (TevKColorSel, TevKAlphaSel) =
    let reg = regs[i div 2]
    if (i mod 2) == 0:
        (reg.kcsel0, reg.kasel0)
    else:
        (reg.kcsel1, reg.kasel1)

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

makeBitStruct uint32, *TxLoadTLut1:
    tmemAdr[0..9]: uint32
    size[10..20]: uint32

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
