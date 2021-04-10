import
    strformat

# here goes everything to be used by the backend

type
    VertexAttrKind* = enum
        vtxAttrPosNrmMat
        vtxAttrTexMat0
        vtxAttrTexMat1
        vtxAttrTexMat2
        vtxAttrTexMat3
        vtxAttrTexMat4
        vtxAttrTexMat5
        vtxAttrTexMat6
        vtxAttrTexMat7
        vtxAttrPosition
        vtxAttrNormal
        vtxAttrColor0
        vtxAttrColor1
        vtxAttrTexCoord0
        vtxAttrTexCoord1
        vtxAttrTexCoord2
        vtxAttrTexCoord3
        vtxAttrTexCoord4
        vtxAttrTexCoord5
        vtxAttrTexCoord6
        vtxAttrTexCoord7
    VertexAttrSize* = enum
        vtxAttrPosition3
        vtxAttrNormalNBT
        vtxAttrTexCoord0ST
        vtxAttrTexCoord1ST
        vtxAttrTexCoord2ST
        vtxAttrTexCoord3ST
        vtxAttrTexCoord4ST
        vtxAttrTexCoord5ST
        vtxAttrTexCoord6ST
        vtxAttrTexCoord7ST

    PrimitiveKind* = enum
        primitiveQuads # on Switch we have those natively, otherwise we emulate them with geometry shaders
        primitiveQuads2
        primitiveTriangles
        primitiveTriangleStrips
        primitiveTriangleFan
        primitiveLines
        primitiveLineStrip
        primitivePoints

    DynamicVertexFmt* = object
        enabledAttrs*: set[VertexAttrKind]
        attrSizes*: set[VertexAttrSize]
        vertexSize*: int
        attrOffsets*: array[VertexAttrKind, uint8]

    VertexBuffer* = object
        curOffset: int
        curFmt*: DynamicVertexFmt
        data*: seq[byte]

    NativeShader* = ref object of RootObj

    ShaderStage* = enum
        shaderStageVertex
        shaderStageFragment
        shaderStageGeometry

    CompareFunction* = enum
        compareNever
        compareLess
        compareEqual
        compareLequal
        compareGreater
        compareNequal
        compareGequal
        compareAlways
    
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

    TextureFormat* = enum
        texfmtI8
        texfmtL8
        texfmtIA8
        texfmtRGBA8
        texfmtRGB5A1
        texfmtRGB565
        texfmtDepth24

    NativeTexture* = ref object of RootObj
        width*, height*, miplevels*: int
        fmt*: TextureFormat

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

    NativeSampler* = ref object of RootObj
        wrapS*, wrapT*: TextureWrapMode
        magFilter*: TextureMagFilter
        minFilter*: TextureMinFilter

    XfMemoryUniform* = object
        posTexMats*: array[64*4, float32]
        nrmMats*: array[32*4, float32]
        postTexMats*: array[64*4, float32]
        lightColor*: array[4*2, uint32]
        lightPositionA1*: array[4*8, float32]
        lightDirectionA0*: array[4*8, float32]
        lightA2K0K1K2*: array[4*8, float32]

    RegistersUniform* = object
        projection*: array[16, float32]
        matIndices0*: uint32
        matInidces1*: uint32
        dualTexMatIndices0*, dualTexMatIndices1*: uint32
        texcoordScale*: array[8*2, float32]
        textureSizes*: array[8*2*2, float32]
        regValues*: array[8, uint32]
        konstants*: array[8, uint32]
        matColors*: array[4, uint32]
        alphaRefs*: uint32
        zenvBias*: uint32
        pad0, pad1: uint32

using vtxbuffer: var VertexBuffer

# for faster comparison
proc `==`*(a, b: DynamicVertexFmt): bool =
    a.enabledAttrs == b.enabledAttrs and a.attrSizes == b.attrSizes

proc genDynamicVtxFmt*(attrs: set[VertexAttrKind], attrSizes: set[VertexAttrSize]): DynamicVertexFmt =
    result.enabledAttrs = attrs
    result.attrSizes = attrSizes
    result.attrOffsets[vtxAttrPosition] = 0
    result.vertexSize += (if vtxAttrPosition3 in attrSizes: 3*4 else: 2*4)

    template doAttr(attr, size): untyped =
        if attr in attrs:
            result.attrOffsets[attr] = uint8 result.vertexSize
            result.vertexSize += size
        else:
            result.attrOffsets[attr] = 0xFF'u8
    doAttr(vtxAttrPosNrmMat, 1)
    for i in 0..<8:
        doAttr(vtxAttrTexMat0.succ(i), 1)
    doAttr vtxAttrNormal, if vtxAttrNormalNBT in attrSizes: 9*4 else: 3*4
    doAttr vtxAttrColor0, 4
    doAttr vtxAttrColor1, 4
    template doTexCoord(n): untyped =
        doAttr `vtxAttrTexCoord n`, if `vtxAttrTexCoord n ST` in attrSizes: 2*4 else: 1*4
    doTexCoord 0
    doTexCoord 1
    doTexCoord 2
    doTexCoord 3
    doTexCoord 4
    doTexCoord 5
    doTexCoord 6
    doTexCoord 7

proc clear*(vtxbuffer) =
    vtxbuffer.data.setLen(0)
    vtxbuffer.curOffset = 0

proc startVertex*(vtxbuffer) =
    vtxbuffer.curOffset = vtxbuffer.data.len
    vtxbuffer.data.setLen(vtxbuffer.data.len + vtxbuffer.curFmt.vertexSize)

proc define*[T](vtxbuffer; attr: VertexAttrKind, data: openArray[T], offset = 0) =
    when #[not defined(release)]#true:
        assert attr in vtxbuffer.curFmt.enabledAttrs, &"{attr} not in {vtxbuffer.curFmt.enabledAttrs}"
        let endOffset = data.len + offset
        case attr
        of vtxAttrPosition: assert endOffset <= (if vtxAttrPosition3 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrNormal: assert endOffset <= (if vtxAttrNormalNBT in vtxbuffer.curFmt.attrSizes: 9 else: 3)
        of vtxAttrTexCoord0: assert endOffset <= (if vtxAttrTexCoord0ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord1: assert endOffset <= (if vtxAttrTexCoord1ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord2: assert endOffset <= (if vtxAttrTexCoord2ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord3: assert endOffset <= (if vtxAttrTexCoord3ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord4: assert endOffset <= (if vtxAttrTexCoord4ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord5: assert endOffset <= (if vtxAttrTexCoord5ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord6: assert endOffset <= (if vtxAttrTexCoord6ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        of vtxAttrTexCoord7: assert endOffset <= (if vtxAttrTexCoord7ST in vtxbuffer.curFmt.attrSizes: 2 else: 1)
        else: discard
    let dataOffset =
        vtxbuffer.curOffset + int(vtxbuffer.curFmt.attrOffsets[attr]) + offset * sizeof(T)
    copyMem(addr vtxbuffer.data[dataOffset],
        unsafeAddr data[0],
        sizeof(T)*data.len)
    #echo data, " ", sizeof(T)*data.len
    #echo toOpenArray(vtxbuffer.data, dataOffset, dataOffset+sizeof(T)*data.len-1)

proc generateQuadIndices*(data: ptr UncheckedArray[uint32], offset, count: int) =
    var
        count = count
        i = 0
    while count >= 4:
        data[i*5+0] = uint32(offset + i*4) + 1
        data[i*5+1] = uint32(offset + i*4) + 2
        data[i*5+2] = uint32(offset + i*4) + 0
        data[i*5+3] = uint32(offset + i*4) + 3
        data[i*5+4] = 0xFFFFFFFF'u32
        count -= 4
        i += 1