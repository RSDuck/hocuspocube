import
    strformat, stew/endians2,
    ../util/bitstruct,

    ../gekko/gekko,

    xf, bp,
    rasterinterface,
    rasterinterfacecommon

template cpLog(msg: string): untyped =
    discard

type
    DrawCallDesc = object
        primitiveKind: PrimitiveKind
        verticesCount, vertexFormat: int

    CmdListParser* = object
        verticesRemaining: int
        drawCallDesc: DrawCallDesc
        pendingData: seq[byte]

    VertexAttrState = enum
        vtxAttrElided
        vtxAttrDirect
        vtxAttrIndexed8
        vtxAttrIndexed16

    VertexCoordFmt = enum
        vtxCoordU8
        vtxCoordS8
        vtxCoordU16
        vtxCoordS16
        vtxCoordF32
        vtxCoordUnused5
        vtxCoordUnused6
        vtxCoordUnused7
    VertexColorFmt = enum
        vtxColRGB565
        vtxColRGB888
        vtxColRGB888X
        vtxColRGBA4444
        vtxColRGBA6666
        vtxColRGBA8888
        vtxColUnused6
        vtxColUnused7

    VtxCoordCnt = enum
        vtxCoord2
        vtxCoord3
    VertexNormalCnt = enum
        vtxNrmSingleNormal
        vtxNrmNormalTripple
    VertexColorCnt = enum
        vtxColRGB
        vtxColRGBA
    VtxTexcoordCnt = enum
        vtxTexcoordS
        vtxTexcoordST

    VertexArrayKind = enum
        vtxArrayGeometry
        vtxArrayNormals
        vtxArrayColors0
        vtxArrayColors1
        vtxArrayTexcoord0
        vtxArrayTexcoord1
        vtxArrayTexcoord2
        vtxArrayTexcoord3
        vtxArrayTexcoord4
        vtxArrayTexcoord5
        vtxArrayTexcoord6
        vtxArrayTexcoord7
        vtxArrayIdxRegA
        vtxArrayIdxRegB
        vtxArrayIdxRegC
        vtxArrayIdxRegD

const
    CmdNop = 0'u8
    CmdLoadCp = 0x08'u8
    CmdLoadXf = 0x10'u8
    CmdLoadIdx = {0x20'u8, 0x28'u8, 0x30'u8, 0x38'u8}
    CmdCallDl = 0x40'u8
    CmdInvVtxCache = 0x48'u8
    CmdLoadBp = 0x61'u8
    CmdDraw = {0x80'u8..(0xB8 or 7)}

# Vcd Lo/hi specify which vertex attributes will be included in a draw call
# and if they're included how
# matrix attributes are always indexed, so they're just a bool
makeBitStruct uint32, VcdLo:
    pnmidx[0]: bool
    t0midx[1]: bool
    t1midx[2]: bool
    t2midx[3]: bool
    t3midx[4]: bool
    t4midx[5]: bool
    t5midx[6]: bool
    t6midx[7]: bool
    t7midx[8]: bool
    pos[9..10]: VertexAttrState
    nrm[11..12]: VertexAttrState
    col0[13..14]: VertexAttrState
    col1[15..16]: VertexAttrState

makeBitStruct uint32, VcdHi:
    tex0[0..1]: VertexAttrState
    tex1[2..3]: VertexAttrState
    tex2[4..5]: VertexAttrState
    tex3[6..7]: VertexAttrState
    tex4[8..9]: VertexAttrState
    tex5[10..11]: VertexAttrState
    tex6[12..13]: VertexAttrState
    tex7[14..15]: VertexAttrState

# VatA/B specify the format of the individual attributes
makeBitStruct uint32, VatA:
    poscnt[0]: VtxCoordCnt
    posfmt[1..3]: VertexCoordFmt
    posshift[4..8]: uint32
    nrmcnt[9]: VertexNormalCnt
    # allegedly unsigned formats aren't allowed
    # while they're not really useful for normals
    # I doubt that they don't work
    nrmfmt[10..12]: VertexCoordFmt
    col0cnt[13]: VertexColorCnt
    col0fmt[14..16]: VertexColorFmt
    col1cnt[17]: VertexColorCnt
    col1fmt[18..20]: VertexColorFmt
    tex0cnt[21]: VtxTexcoordCnt
    tex0fmt[22..24]: VertexCoordFmt
    tex0shift[25..29]: uint32
    byteDequant[30]: bool
    trippleNormalIdx[31]: bool

makeBitStruct uint32, VatB:
    tex1cnt[0]: VtxTexcoordCnt
    tex1fmt[1..3]: VertexCoordFmt
    tex1shift[4..8]: uint32
    tex2cnt[9]: VtxTexcoordCnt
    tex2fmt[10..12]: VertexCoordFmt
    tex2shift[13..17]: uint32
    tex3cnt[18]: VtxTexcoordCnt
    tex3fmt[19..21]: VertexCoordFmt
    tex3shift[22..26]: uint32
    tex4cnt[27]: VtxTexcoordCnt
    tex4fmt[28..30]: VertexCoordFmt
    vcacheEnhance[31]: bool

makeBitStruct uint32, VatC:
    tex4shift[0..4]: uint32
    tex5cnt[5]: VtxTexcoordCnt
    tex5fmt[6..8]: VertexCoordFmt
    tex5shift[9..13]: uint32
    tex6cnt[14]: VtxTexcoordCnt
    tex6fmt[15..17]: VertexCoordFmt
    tex6shift[18..22]: uint32
    tex7cnt[23]: VtxTexcoordCnt
    tex7fmt[24..26]: VertexCoordFmt
    tex7shift[27..31]: uint32

makeBitStruct uint32, ArrayStride:
    stride[0..7]: uint32

type
    VertexFmt = object
        vcdLo: VcdLo
        vcdHi: VcdHi

        vatA: VatA
        vatB: VatB
        vatC: VatC
    VertexArray = object
        base: HwPtr
        stride: ArrayStride

proc read[T](arr: VertexArray, idx: uint32, offset = 0'u32): T =
    fromBE cast[ptr T](addr mainRAM[arr.base.adr + arr.stride.stride * idx + offset * uint32(sizeof(T))])[]

proc calcVertexSize(fmt: VertexFmt): uint32 =
    result += uint32(fmt.vcdLo.pnmidx) +
        uint32(fmt.vcdLo.t0midx) + uint32(fmt.vcdLo.t1midx) +
        uint32(fmt.vcdLo.t2midx) + uint32(fmt.vcdLo.t3midx) +
        uint32(fmt.vcdLo.t4midx) + uint32(fmt.vcdLo.t5midx) +
        uint32(fmt.vcdLo.t6midx) + uint32(fmt.vcdLo.t7midx)

    const
        coordSizes: array[VertexCoordFmt, int] = [1, 1, 2, 2, 4, 0, 0, 0]
        colSizes: array[VertexColorFmt, int] = [2, 3, 4, 2, 3, 4, 0, 0]
    result += (case fmt.vcdLo.pos
            of vtxAttrElided: 0
            of vtxAttrIndexed8: 1
            of vtxAttrIndexed16: 2
            of vtxAttrDirect: coordsizes[fmt.vatA.posfmt] * (if fmt.vatA.poscnt == vtxCoord2: 2 else: 3))
    result += (case fmt.vcdLo.nrm
        of vtxAttrElided: 0
        of vtxAttrIndexed8:
            (if fmt.vatA.nrmcnt == vtxNrmNormalTripple and fmt.vatA.trippleNormalIdx: 3 else: 1)
        of vtxAttrIndexed16:
            (if fmt.vatA.nrmcnt == vtxNrmNormalTripple and fmt.vatA.trippleNormalIdx: 2*3 else: 2)
        of vtxAttrDirect:
            (if fmt.vatA.nrmcnt == vtxNrmNormalTripple: 9 else: 3) * coordSizes[fmt.vatA.nrmfmt])
    template addColSize(prop, prop2): untyped =
        result += (case prop
            of vtxAttrElided: 0
            of vtxAttrIndexed8: 1
            of vtxAttrIndexed16: 2
            of vtxAttrDirect: colSizes[prop2])
    addColSize fmt.vcdLo.col0, fmt.vatA.col0fmt
    addColSize fmt.vcdLo.col1, fmt.vatA.col1fmt
    template addTexCoord(prop, prop2, cnt): untyped =
        result += (case prop
            of vtxAttrElided: 0
            of vtxAttrIndexed8: 1
            of vtxAttrIndexed16: 2
            of vtxAttrDirect: coordsizes[prop2] * (if cnt == vtxTexcoordS: 1 else: 2))
    addTexCoord fmt.vcdHi.tex0, fmt.vatA.tex0fmt, fmt.vatA.tex0cnt
    addTexCoord fmt.vcdHi.tex1, fmt.vatB.tex1fmt, fmt.vatB.tex1cnt
    addTexCoord fmt.vcdHi.tex2, fmt.vatB.tex2fmt, fmt.vatB.tex2cnt
    addTexCoord fmt.vcdHi.tex3, fmt.vatB.tex3fmt, fmt.vatB.tex3cnt
    addTexCoord fmt.vcdHi.tex4, fmt.vatB.tex4fmt, fmt.vatB.tex4cnt
    addTexCoord fmt.vcdHi.tex5, fmt.vatC.tex5fmt, fmt.vatC.tex5cnt
    addTexCoord fmt.vcdHi.tex6, fmt.vatC.tex6fmt, fmt.vatC.tex6cnt
    addTexCoord fmt.vcdHi.tex7, fmt.vatC.tex7fmt, fmt.vatC.tex7cnt

var
    vertexFormats: array[8, VertexFmt]
    dynamicVertexFmts: array[8, DynamicVertexFmt]
    vertexFormatSizes: array[8, uint32]
    vertexFormatDirty: set[0..7]

    vcdLo: VcdLo
    vcdHi: VcdHi

    vertexArrays*: array[VertexArrayKind, VertexArray]

proc cpWrite*(adr, val: uint32) =
    template modifyFmt(lowerbound, typ, store): untyped =
        let fmtIdx {.inject.} = adr - lowerbound
        if store != typ(val):
            vertexFormatDirty.incl int(fmtIdx)
            store = typ(val)

    case adr
    # those are the default matrices
    # which exist for both cp and xf
    # we only use the xf ones for now
    of 0x30, 0x40: discard
    of 0x50:
        vcdLo = VcdLo val
    of 0x60:
        vcdHi = VcdHi val
    of 0x70..0x77:
        modifyFmt 0x70, VatA, vertexFormats[fmtIdx].vatA
    of 0x80..0x87:
        modifyFmt 0x80, VatB, vertexFormats[fmtIdx].vatB
    of 0x90..0x97:
        modifyFmt 0x90, VatC, vertexFormats[fmtIdx].vatC
    of 0xA0..0xAF: vertexArrays[VertexArrayKind(adr - 0xA0)].base = HwPtr val
    of 0xB0..0xBF: vertexArrays[VertexArrayKind(adr - 0xB0)].stride = ArrayStride val
    else: echo &"unknown cp write {adr:04X} {val:08X}"

makeBitStruct uint32, CmdLoadXfParam:
    adr[0..15]: uint32
    len[16..31]: uint32

makeBitStruct uint32, CmdLoadIdxParam:
    len[0..11]: uint32
    adr[12..15]: uint32
    idx[16..31]: uint32

makeBitStruct uint32, CmdLoadBpParam:
    adr[24..31]: uint32
    val[0..23]: uint32

proc queueData*(parser: var CmdListParser, data: openArray[byte]) =
    parser.pendingData.add(data)

proc hasData*(parser: CmdListParser): bool =
    parser.pendingData.len > 0

template read8: uint8 =
    let val = data[result]
    result += 1
    val
template read16: uint16 =
    let val = fromBE cast[ptr uint16](unsafeAddr data[result])[]
    result += 2
    val
template read32: uint32 =
    let val = fromBE cast[ptr uint32](unsafeAddr data[result])[]
    result += 4
    val
template readFloat: float32 =
    cast[float32](read32())

proc processVertices(data: openArray[byte], offset: int, draw: DrawCallDesc, verticesRemaining: var int): int =
    result = offset

    let
        fmt = vertexFormats[draw.vertexFormat]
        vertexSize = int vertexFormatSizes[draw.vertexFormat]
        verticesToProcess = min((data.len - result) div vertexSize, verticesRemaining)

    for i in 0..<verticesToProcess:
        curVertexBuffer.startVertex()

        const rcpTable = (proc(): array[16, float32] =
            for i in 0..<16:
                result[i] = 1f / float32(1 shl i))()

        template readIdx(typ): uint32 =
            if typ == vtxAttrIndexed8:
                uint32(read8()) else: uint32(read16())

        template numElementsPosition(cnt): int =
            case cnt
            of vtxCoord2: 2
            of vtxCoord3: 3
        template numElementsTexCoord(cnt): int =
            case cnt
            of vtxTexcoordS: 1
            of vtxTexcoordST: 2

        template loadFixedPoint(status, finalCoords, numElements, arr, typ, typSigned, directRead, shift) =
            let factor = if fmt.vatA.byteDequant or typ is uint16: rcpTable[shift] else: 1f
            if status == vtxAttrDirect:
                for i in 0..<numElements:
                    finalCoords[i] = factor * float32(cast[typSigned](directRead))
            else:
                let idx = readIdx(status)
                for i in 0..<numElements:
                    finalCoords[i] = factor *
                        float32(cast[typSigned](vertexArrays[arr].read[:typ](idx, uint32(i))))

        template processCoord(attrKind, status, fmt, cnt, shift, arr, maxElements; finalOffset = 0; isNormal = false): untyped =
            if status != vtxAttrElided:
                let numElements = cnt
                var finalCoords: array[maxElements, float32]
                case fmt
                of vtxCoordU8:
                    let finalShift = when isNormal: 7 else: shift
                    loadFixedPoint(status, finalCoords, numElements, arr, uint8, uint8, read8(), finalShift)
                of vtxCoordS8:
                    let finalShift = when isNormal: 6 else: shift
                    loadFixedPoint(status, finalCoords, numElements, arr, uint8, int8, read8(), finalShift)
                of vtxCoordU16:
                    let finalShift = when isNormal: 15 else: shift
                    loadFixedPoint(status, finalCoords, numElements, arr, uint16, uint16, read16(), finalShift)
                of vtxCoordS16:
                    let finalShift = when isNormal: 14 else: shift
                    loadFixedPoint(status, finalCoords, numElements, arr, uint16, int16, read16(), finalShift)
                of vtxCoordF32:
                    if status == vtxAttrDirect:
                        for i in 0..<numElements:
                            finalCoords[i] = readFloat()
                    else:
                        let idx = readIdx(status)
                        for i in 0..<numElements:
                            finalCoords[i] = cast[float32](vertexArrays[arr].read[:uint32](idx, uint32(i)))
                else: echo "invalid fmt ", status
                curVertexBuffer.define[:float32](attrKind, toOpenArray(finalCoords, 0, numElements - 1), finalOffset)
                #echo "decoded coord ", finalCoords

        template colRead(status, typ, directRead, arr): untyped =
            if status == vtxAttrDirect:
                directRead
            else:
                vertexArrays[arr].read[:typ](readIdx(status))

        template processColor(attrKind, status, fmt, cnt, arr): untyped =
            if status != vtxAttrElided:
                var color: uint32
                case fmt
                of vtxColRGB565:
                    let col = uint32 colRead(status, uint16, read16(), arr)
                    color = ((col shl 3) and 0xF8) or
                        ((col shl 6) and 0xF800) or
                        ((col shl 8) and 0xF80000) or
                        0xFF000000'u32
                of vtxColRGB888:
                    if status == vtxAttrDirect:
                        color = read32()
                        result -= 1
                    else:
                        let idx = readIdx(status)
                        color = vertexArrays[arr].read[:uint32](idx)
                    color = toBE(color) or 0xFF000000'u32
                of vtxColRGBA4444:
                    let col = uint32 colRead(status, uint16, read16(), arr)
                    color = ((col shl 4) and 0xF0) or
                        ((col shl 8) and 0xF000) or
                        ((col shl 12) and 0xF00000) or
                        0xFF000000'u32
                of vtxColRGB888X:
                    color = toBE colRead(status, uint32, read32(), arr) or 0xFF000000'u32
                of vtxColRGBA6666:
                    #[var
                        col0: uint16
                        col1: uint8
                    if status == vtxAttrDirect:
                        col0 = read16()
                        col1 = read8()
                    else:
                        let idx = readIdx(status)
                        col0 = vertexArrays[arr].read[:uint8](idx, 2)
                        col1 = vertexArrays[arr].read[:uint16](idx)
                    color[0] = byte((col0 shl 2) and 0xFC)
                    color[1] = byte((col0 shr 4) and 0xFC)
                    color[2] = byte(((col0 shr 10) and 0x3C) or ((col1 shl 6) and 0xC0))
                    color[3] = byte(col1 and 0xFC)]#
                    echo "stupid rgba6666 format!"
                of vtxColRGBA8888:
                    color = toBE colRead(status, uint32, read32(), arr)
                else: echo "invalid color fmt ", fmt
                #echo "decoded color: ", color
                curVertexBuffer.define[:uint32](attrKind, [color])

        template processIdx(attrKind, status) =
            if status:
                curVertexBuffer.define[:uint8](attrKind, [read8()])
        processIdx(vtxAttrPosNrmMat, fmt.vcdLo.pnmidx)
        processIdx(vtxAttrTexMat0, fmt.vcdLo.t0midx)
        processIdx(vtxAttrTexMat1, fmt.vcdLo.t1midx)
        processIdx(vtxAttrTexMat2, fmt.vcdLo.t2midx)
        processIdx(vtxAttrTexMat3, fmt.vcdLo.t3midx)
        processIdx(vtxAttrTexMat4, fmt.vcdLo.t4midx)
        processIdx(vtxAttrTexMat5, fmt.vcdLo.t5midx)
        processIdx(vtxAttrTexMat6, fmt.vcdLo.t6midx)
        processIdx(vtxAttrTexMat7, fmt.vcdLo.t7midx)

        processCoord(vtxAttrPosition, fmt.vcdLo.pos, fmt.vatA.posfmt, numElementsPosition(fmt.vatA.poscnt), fmt.vatA.posshift, vtxArrayGeometry, 3)

        if fmt.vcdLo.nrm != vtxAttrElided:
            case fmt.vatA.nrmcnt
            of vtxNrmSingleNormal:
                processCoord(vtxAttrNormal, fmt.vcdLo.nrm, fmt.vatA.nrmfmt, 3, 0, vtxArrayNormals, 3, isNormal = true)
            of vtxNrmNormalTripple:
                if fmt.vatA.trippleNormalIdx:
                    processCoord(vtxAttrNormal, fmt.vcdLo.nrm, fmt.vatA.nrmfmt, 3, 0, vtxArrayNormals, 3, 0, isNormal = true)
                    processCoord(vtxAttrNormal, fmt.vcdLo.nrm, fmt.vatA.nrmfmt, 3, 0, vtxArrayNormals, 3, 3, isNormal = true)
                    processCoord(vtxAttrNormal, fmt.vcdLo.nrm, fmt.vatA.nrmfmt, 3, 0, vtxArrayNormals, 3, 6, isNormal = true)
                else:
                    processCoord(vtxAttrNormal, fmt.vcdLo.nrm, fmt.vatA.nrmfmt, 9, 6, vtxArrayNormals, 9, isNormal = true)

        processColor(vtxAttrColor0, fmt.vcdLo.col0, fmt.vatA.col0fmt, fmt.vatA.col0cnt, vtxArrayColors0)
        processColor(vtxAttrColor1, fmt.vcdLo.col1, fmt.vatA.col1fmt, fmt.vatA.col1cnt, vtxArrayColors1)

        processCoord(vtxAttrTexcoord0, fmt.vcdHi.tex0, fmt.vatA.tex0fmt, numElementsTexCoord(fmt.vatA.tex0cnt), fmt.vatA.tex0shift, vtxArrayTexcoord0, 3)
        processCoord(vtxAttrTexcoord1, fmt.vcdHi.tex1, fmt.vatB.tex1fmt, numElementsTexCoord(fmt.vatB.tex1cnt), fmt.vatB.tex1shift, vtxArrayTexcoord1, 3)
        processCoord(vtxAttrTexcoord2, fmt.vcdHi.tex2, fmt.vatB.tex2fmt, numElementsTexCoord(fmt.vatB.tex2cnt), fmt.vatB.tex2shift, vtxArrayTexcoord2, 3)
        processCoord(vtxAttrTexcoord3, fmt.vcdHi.tex3, fmt.vatB.tex3fmt, numElementsTexCoord(fmt.vatB.tex3cnt), fmt.vatB.tex3shift, vtxArrayTexcoord3, 3)
        processCoord(vtxAttrTexcoord4, fmt.vcdHi.tex4, fmt.vatB.tex4fmt, numElementsTexCoord(fmt.vatB.tex4cnt), fmt.vatC.tex4shift, vtxArrayTexcoord4, 3)
        processCoord(vtxAttrTexcoord5, fmt.vcdHi.tex5, fmt.vatC.tex5fmt, numElementsTexCoord(fmt.vatC.tex5cnt), fmt.vatC.tex5shift, vtxArrayTexcoord5, 3)
        processCoord(vtxAttrTexcoord6, fmt.vcdHi.tex6, fmt.vatC.tex6fmt, numElementsTexCoord(fmt.vatC.tex6cnt), fmt.vatC.tex6shift, vtxArrayTexcoord6, 3)
        processCoord(vtxAttrTexcoord7, fmt.vcdHi.tex7, fmt.vatC.tex7fmt, numElementsTexCoord(fmt.vatC.tex7cnt), fmt.vatC.tex7shift, vtxArrayTexcoord7, 3)

    cpLog &"ate {verticesToProcess} vertices (size: {vertexSize} end offset {result} total: {result-offset} bytes | bytes remaining: {data.len-result})"
    verticesRemaining -= verticesToProcess

    if verticesRemaining == 0:
        draw(draw.primitiveKind, draw.verticesCount, dynamicVertexFmts[draw.vertexFormat])

proc run(data: openArray[byte],
    draw: var DrawCallDesc, verticesRemaining: var int,
    inCommandList: bool): int =
    #var dataStr = ""
    #for i in 0..<data.len:
    #    dataStr &= &"{data[i]:02X} "
    #echo "data: ", dataStr
    template ensureLength(n: int): untyped =
        if data.len - result < n:
            result = startOffset
            break mainLoop

    block mainLoop:
        while result < data.len:
            if verticesRemaining == 0:
                while result < data.len:
                    let
                        startOffset = result
                        commandTyp = read8()

                    case commandTyp
                    of CmdNop:
                        cpLog &"command nop"
                    of CmdLoadCp:
                        ensureLength 5
                        let
                            adr = read8()
                            val = read32()
                        cpLog &"load cp {adr:02X} {val:08X}"
                        cpWrite(adr, val)
                    of CmdLoadXf:
                        ensureLength 4
                        let params = CmdLoadXfParam read32()
                        cpLog &"load xf {params.adr:04X} {params.len:04X}"
                        ensureLength int(params.len + 1) * 4
                        for i in 0..params.len:
                            xfWrite(params.adr + i, read32())
                    of CmdLoadIdx:
                        ensureLength 6
                        let
                            idx = read16()
                            rest = read32()
                        cpLog &"load indexed {idx} {rest:08X}"
                    of CmdCallDl:
                        doAssert not inCommandList, "recursive command list"
                        ensureLength 8
                        let
                            adr = HwPtr(read32()).adr
                            size = read32()
                        cpLog &"call disp list {adr:08X} {size:08X}"

                        var
                            verticesRemaining = 0
                            drawCallDesc: DrawCallDesc
                        let left = run(toOpenArray(mainRAM, adr, adr + size - 1), drawCallDesc, verticesRemaining, true)
                        assert left == int(size), "invalid display list"
                        assert verticesRemaining == 0, "display list ended with invalid drawcall"
                    of CmdInvVtxCache:
                        cpLog "invalidate vertex cache"
                    of CmdLoadBp:
                        ensureLength 4
                        let params = read32()
                        cpLog &"load bp {params:04X} {result} {data.len}"
                        bpWrite(params shr 24, params and 0xFFFFFF)
                    of CmdDraw:
                        ensureLength 2
                        let num = read16()
                        draw.verticesCount = int num
                        draw.primitiveKind = PrimitiveKind((commandTyp - 0x80'u8) shr 3)
                        draw.vertexFormat = int(commandTyp and 0x7'u8)

                        if draw.vertexFormat in vertexFormatDirty or
                            vcdLo != vertexFormats[draw.vertexFormat].vcdLo or
                            vcdHi != vertexFormats[draw.vertexFormat].vcdHi:
                            vertexFormatDirty.excl draw.vertexFormat

                            vertexFormats[draw.vertexFormat].vcdLo = vcdLo
                            vertexFormats[draw.vertexFormat].vcdHi = vcdHi

                            let fmt = vertexFormats[draw.vertexFormat]

                            vertexFormatSizes[draw.vertexFormat] = uint32 calcVertexSize(fmt)

                            var
                                enabledSet: set[VertexAttrKind]
                                sizesSet: set[VertexAttrSize]
                            template doMat(vcd, attr): untyped =
                                if vcd: enabledSet.incl attr
                            doMat(fmt.vcdLo.pnmidx, vtxAttrPosNrmMat)
                            doMat(fmt.vcdLo.t0midx, vtxAttrTexMat0)
                            doMat(fmt.vcdLo.t1midx, vtxAttrTexMat1)
                            doMat(fmt.vcdLo.t2midx, vtxAttrTexMat2)
                            doMat(fmt.vcdLo.t3midx, vtxAttrTexMat3)
                            doMat(fmt.vcdLo.t4midx, vtxAttrTexMat4)
                            doMat(fmt.vcdLo.t5midx, vtxAttrTexMat5)
                            doMat(fmt.vcdLo.t6midx, vtxAttrTexMat6)
                            doMat(fmt.vcdLo.t7midx, vtxAttrTexMat7)
                            template doCoord(vcd, cnt, attr, hasAdditional, additionalAttr): untyped =
                                if vcd != vtxAttrElided:
                                    enabledSet.incl attr
                                    if cnt == hasAdditional: sizesSet.incl additionalAttr
                            doCoord(fmt.vcdLo.pos, fmt.vatA.poscnt, vtxAttrPosition, vtxCoord3, vtxAttrPosition3)
                            doCoord(fmt.vcdLo.nrm, fmt.vatA.nrmcnt, vtxAttrNormal, vtxNrmNormalTripple, vtxAttrNormalNBT)
                            if fmt.vcdLo.col0 != vtxAttrElided: enabledSet.incl vtxAttrColor0
                            if fmt.vcdLo.col1 != vtxAttrElided: enabledSet.incl vtxAttrColor1
                            doCoord(fmt.vcdHi.tex0, fmt.vatA.tex0cnt, vtxAttrTexCoord0, vtxTexcoordST, vtxAttrTexCoord0ST)
                            doCoord(fmt.vcdHi.tex1, fmt.vatB.tex1cnt, vtxAttrTexCoord1, vtxTexcoordST, vtxAttrTexCoord1ST)
                            doCoord(fmt.vcdHi.tex2, fmt.vatB.tex2cnt, vtxAttrTexCoord2, vtxTexcoordST, vtxAttrTexCoord2ST)
                            doCoord(fmt.vcdHi.tex3, fmt.vatB.tex3cnt, vtxAttrTexCoord3, vtxTexcoordST, vtxAttrTexCoord3ST)
                            doCoord(fmt.vcdHi.tex4, fmt.vatB.tex4cnt, vtxAttrTexCoord4, vtxTexcoordST, vtxAttrTexCoord4ST)
                            doCoord(fmt.vcdHi.tex5, fmt.vatC.tex5cnt, vtxAttrTexCoord5, vtxTexcoordST, vtxAttrTexCoord5ST)
                            doCoord(fmt.vcdHi.tex6, fmt.vatC.tex6cnt, vtxAttrTexCoord6, vtxTexcoordST, vtxAttrTexCoord6ST)
                            doCoord(fmt.vcdHi.tex7, fmt.vatC.tex7cnt, vtxAttrTexCoord7, vtxTexcoordST, vtxAttrTexCoord6ST)

                            dynamicVertexFmts[draw.vertexFormat] = genDynamicVtxFmt(enabledSet, sizesSet)
                        cpLog &"draw {draw.primitiveKind} {result} ({draw.vertexFormat}) {draw.verticesCount} vertices vtx size: {vertexFormatSizes[draw.vertexFormat]}"
                        curVertexBuffer.curFmt = dynamicVertexFmts[draw.vertexFormat]
                        verticesRemaining = draw.verticesCount
                        break
                    else:
                        echo &"unknown command type {commandTyp:02X}"
            else:
                result = processVertices(data, result, draw, verticesRemaining)
                if verticesRemaining > 0:
                    break

proc run*(parser: var CmdListParser) =
    let offset = run(parser.pendingData, parser.drawCallDesc, parser.verticesRemaining, false)

    if offset < parser.pendingData.len:
        let leftover = parser.pendingData.len - offset
        moveMem(addr parser.pendingData[0], addr parser.pendingData[offset], leftover)
        parser.pendingData.setLen(leftover)
    else:
        parser.pendingData.setLen(0)