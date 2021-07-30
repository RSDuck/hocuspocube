import
    strformat,

    rasterinterfacecommon,

    ../util/bitstruct

template xfLog(msg: string): untyped =
    discard

type
    ProjMatKind = enum
        projPerspective
        projOrthographic

    TexcoordProjKind* = enum
        texcoordProjSt
        texcoordProjStq
    TexcoordInputForm* = enum
        texcoordInputFormAB11
        texcoordInputFormABC1
    TexcoordGenKind* = enum
        texcoordGenKindRegular
        texcoordGenKindEmbossMap
        texcoordGenKindColorStrgbc0
        texcoordGenKindColorStrgbc1
    TexcoordGenSrc* = enum
        texcoordGenSrcGeom 
        texcoordGenSrcNrm 
        texcoordGenSrcColor
        texcoordGenSrcBinrm 
        texcoordGenSrcTngt
        texcoordGenSrcTex0
        texcoordGenSrcTex1
        texcoordGenSrcTex2
        texcoordGenSrcTex3
        texcoordGenSrcTex4
        texcoordGenSrcTex5
        texcoordGenSrcTex6
        texcoordGenSrcTex7

    LightCtrlKind* = enum
        lightCtrlColor0
        lightCtrlColor1
        lightCtrlAlpha0
        lightCtrlAlpha1
    AttenSelect* = enum
        attenSelectSpecular
        attenSelectDiffSpotlight
    DiffuseAtten* = enum
        diffuseAtten1
        diffuseAttenNL
        diffuseAttenNLClamped
        diffuseAttenReserved
    MatColorSrc* = enum
        matColorSrcRegister
        matColorSrcPerVertex

    ViewportItem* = enum
        viewportX0
        viewportY0
        viewportZRange
        viewportX1
        viewportY1
        viewportFar

makeBitStruct uint32, *MatIndexLo:
    geometryIdx[0..5]: uint32
    tex0[6..11]: uint32
    tex1[12..17]: uint32
    tex2[18..23]: uint32
    tex3[24..29]: uint32

makeBitStruct uint32, *MatIndexHi:
    tex4[0..5]: uint32
    tex5[6..11]: uint32
    tex6[12..17]: uint32
    tex7[18..23]: uint32

makeBitStruct uint32, *TexcoordGen:
    proj[1]: TexcoordProjKind
    inputForm[2]: TexcoordInputForm
    kind[4..5]: TexcoordGenKind # allegedly this also includes bit 6 but why?
    src[7..11]: TexcoordGenSrc
    embossSrc[12..14]: uint32
    embossLight[15..17]: uint32

makeBitStruct uint32, *MatColor:
    alpha[0..7]: uint32
    blue[8..15]: uint32
    green[16..23]: uint32
    red[24..31]: uint32

makeBitStruct uint32, *LightCtrl:
    enableLighting[1]: bool
    matSrc[0]: MatColorSrc
    ambSrc[6]: MatColorSrc
    diffAtten[7..8]: DiffuseAtten
    attenEnable[9]: bool
    attenSelect[10]: AttenSelect

    lights[n, if n >= 4: (n-4+11) else: (n+2)]: bool

makeBitStruct uint32, *DualTex:
    normalise[8]: bool
    dualMtx[0..5]: uint32

var
    # it's weird that those registers need to be specified twice
    # this needs some investigation, would be interesting to know what happens if only one of them is set
    matIdxLo*: MatIndexLo
    matIdxHi*: MatIndexHi

    viewport*: array[ViewportItem, float32]
    projMat: array[6, float32]
    projMatKind: ProjMatKind

    numTexcoordGen*: uint32
    texcoordGen*: array[8, TexcoordGen]

    enableDualTex*: bool
    dualTex*: array[8, DualTex]

    numColors*: uint32
    matColorsRegs*: array[2, MatColor]
    ambColorsRegs*: array[2, MatColor]
    lightCtrls*: array[LightCtrlKind, LightCtrl]

proc getViewport*(): (float32, float32, float32, float32, float32, float32) =
    result[2] = viewport[viewportX0] * 2
    result[3] = -viewport[viewportY0] * 2
    result[0] = viewport[viewportX1] - 342f - viewport[viewportX0]
    result[1] = viewport[viewportY1] - 342f + viewport[viewportY0]

    result[5] = viewport[viewportFar] / 16777215f
    result[4] = result[5] - (viewport[viewportZRange] / 16777215f)

proc translateProj*(proj: var array[16, float32]) =
    for i in 0..<16:
        proj[i] = 0f
    proj[0+0*4] = projMat[0]
    proj[1+1*4] = projMat[2]
    proj[2+2*4] = projMat[4]
    proj[2+3*4] = projMat[5]
    case projMatKind
    of projPerspective:
        proj[0+2*4] = projMat[1]
        proj[1+2*4] = projMat[3]
        proj[3+2*4] = -1f
    of projOrthographic:
        proj[0+3*4] = projMat[1]
        proj[1+3*4] = projMat[3]
        proj[3+3*4] = 1f

import
    rasterinterface

proc xfWrite*(adr, val: uint32) =
    case adr
    of 0..0xFF:
        xfLog &"load mat {adr} {cast[float32](val)} {val:08X}"
        xfMemoryUniform.posTexMats[adr] = cast[float32](val)
        xfMemoryDirty = true
    of 0x400..0x45F:
        let offset = adr - 0x400'u32
        xfLog &"load nrm mat {offset} {cast[float32](val)}"
        xfMemoryUniform.nrmMats[(offset div 3) * 4 + (offset mod 3)] = cast[float32](val)
        xfMemoryDirty = true
    of 0x500..0x5FF:
        let offset = adr - 0x500
        xfLog &"load post tex mat {offset} {cast[float32](val)}"
        xfMemoryUniform.postTexMats[offset] = cast[float32](val)
        xfMemoryDirty = true
    of 0x600..0x67F:
        let lightNum = (adr and 0xFF) div 16
        case cast[0x0..0xF](adr and 0xF)
        of 0x0, 0x1, 0x2: #[echo &"writing reserved light value {adr:04X} {val:08X}"]#discard
        of 0x3: xfMemoryUniform.lightColor[lightNum] = val
        of 0x4: xfMemoryUniform.lightDirectionA0[lightNum*4+3] = cast[float32](val)
        of 0x5: xfMemoryUniform.lightPositionA1[lightNum*4+3] = cast[float32](val)
        of 0x6..0x9: xfMemoryUniform.lightA2K0K1K2[lightNum*4+((adr and 0xF) - 0x6)] = cast[float32](val)
        of 0xA..0xC: xfMemoryUniform.lightPositionA1[lightNum*4+((adr and 0xF) - 0xA)] = cast[float32](val)
        of 0xD..0xF: xfMemoryUniform.lightDirectionA0[lightNum*4+((adr and 0xF) - 0xD)] = cast[float32](val)
        xfMemoryDirty = true
    of 0x1008:
        # INVTXSPEC
        # should produce some undefined behaviour if this mismatches with cp
        # currently not handled
        discard
    of 0x1009:
        numColors = val
    of 0x100A..0x100B:
        ambColorsRegs[adr - 0x100A] = MatColor val
        registerUniformDirty = true
    of 0x100C..0x100D:
        matColorsRegs[adr - 0x100C] = MatColor val
        registerUniformDirty = true
    of 0x100E..0x1011:
        lightCtrls[LightCtrlKind(adr - 0x100E)] = LightCtrl val
    of 0x1012:
        enableDualTex = (val and 1) != 0
    of 0x1018:
        matIdxLo = MatIndexLo val
        registerUniformDirty = true
    of 0x1019:
        matIdxHi = MatIndexHi val
        registerUniformDirty = true
    of 0x101A..0x101F:
        viewport[ViewportItem(adr - 0x101A)] = cast[float32](val)
        rasterStateDirty = true
        xfLog &"viewport val {adr - 0x101A} {cast[float32](val)}"
    of 0x1020..0x1025:
        xfLog &"loading proj mat val {adr - 0x1020} {cast[float32](val)}"
        projMat[adr - 0x1020] = cast[float32](val)
        registerUniformDirty = true
    of 0x1026:
        projMatKind = ProjMatKind(val and 1)
        registerUniformDirty = true
    of 0x103F:
        numTexcoordGen = val
    of 0x1040..0x1047:
        texcoordGen[adr - 0x1040] = TexcoordGen val
    of 0x1050..0x1057:
        dualTex[adr - 0x1050] = DualTex val
        xfMemoryDirty = true
        registerUniformDirty = true
    else: echo &"unknown xf write {adr:04X} {val:08X}"
