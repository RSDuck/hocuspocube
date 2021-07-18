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
        blendSub

    BlendFactor* = enum
        blendFactorZero
        blendFactorOne
        blendFactorSrcColor
        blendFactorInvSrcColor
        blendFactorSrcAlpha
        blendFactorInvSrcAlpha
        blendFactorDstAlpha
        blendFactorInvDstAlpha

    CopyMode* = enum
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
    mode[14]: CopyMode
    clear[11]: bool

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
