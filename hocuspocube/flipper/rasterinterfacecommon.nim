import hashes

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
        vtxAttrTexCoord03
        vtxAttrTexCoord13
        vtxAttrTexCoord23
        vtxAttrTexCoord33
        vtxAttrTexCoord43
        vtxAttrTexCoord53
        vtxAttrTexCoord63
        vtxAttrTexCoord73

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

    XfMemoryUniform* = object
        posTexMats*: array[64*4, float32]
        nrmMats*: array[32*4, float32]
        postTexMats*: array[64*4, float32]

    XfRegistersUniform* = object
        projection*: array[16, float32]
        defaultMats*: array[4, uint32]

    NativeShader* = ref object of RootObj

    ShaderStage* = enum
        shaderStageVertex
        shaderStageFragment
        shaderStageGeometry

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
    doAttr vtxAttrNormal, if vtxAttrNormalNBT in attrSizes: 9*4 else: 3*4
    doAttr vtxAttrColor0, 4
    doAttr vtxAttrColor1, 4
    template doTexCoord(n): untyped =
        doAttr `vtxAttrTexCoord n`, if `vtxAttrTexCoord n 3` in attrSizes: 3*4 else: 2*4
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
    when not defined(release):
        assert attr in vtxbuffer.curFmt.enabledAttrs
        let endOffset = data.len + offset
        case attr
        of vtxAttrPosition: assert endOffset <= (if vtxAttrPosition3 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrNormal: assert endOffset <= (if vtxAttrNormalNBT in vtxbuffer.curFmt.attrSizes: 9 else: 3)
        of vtxAttrTexCoord0: assert endOffset <= (if vtxAttrTexCoord03 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord1: assert endOffset <= (if vtxAttrTexCoord13 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord2: assert endOffset <= (if vtxAttrTexCoord23 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord3: assert endOffset <= (if vtxAttrTexCoord33 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord4: assert endOffset <= (if vtxAttrTexCoord43 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord5: assert endOffset <= (if vtxAttrTexCoord53 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord6: assert endOffset <= (if vtxAttrTexCoord63 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        of vtxAttrTexCoord7: assert endOffset <= (if vtxAttrTexCoord73 in vtxbuffer.curFmt.attrSizes: 3 else: 2)
        else: discard
    let dataOffset =
        vtxbuffer.curOffset + int(vtxbuffer.curFmt.attrOffsets[attr]) + offset * sizeof(T)
    copyMem(addr vtxbuffer.data[dataOffset],
        unsafeAddr data[0],
        sizeof(T)*data.len)
    #echo data, " ", sizeof(T)*data.len
    #echo toOpenArray(vtxbuffer.data, dataOffset, dataOffset+sizeof(T)*data.len-1)