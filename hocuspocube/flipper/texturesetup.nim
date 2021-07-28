import
    hashes, tables, strformat,
    bitops,
    stew/[endians2, bitops2],
    xxhash,

    ../util/bitstruct,

    rasterinterfacecommon,
    opengl/rasterogl,
    bpcommon,
    ../gekko/gekko

var palHashCache: Table[(uint32, uint32), uint64]

proc clearPalHashCache*() =
    palHashCache.clear()

iterator doTileLoop*(width, height, tileW, tileH: int, stepW = 1, stepH = 1, secondLine = 0, flipY = false, tileStride = 0): (int, int) =
    var tileSrc = 0
    let
        stride = if flipY: -width else: width
        yadd = if flipY: (height - 1) * width else: 0
        tileStride = if tileStride == 0: (secondLine+(tileW div stepW)*(tileH div stepH))*(width div tileW) else: tileStride
    for y in countup(0, height - 1, tileH):
        var tileLine = tileSrc
        tileSrc += tileStride
        for x in countup(0, width - 1, tileW):
            let tileOffset = x + yadd + y * stride
            for j in countup(0, tileH - 1, stepH):
                let lineOffset = tileOffset + j * stride
                for i in countup(0, tileW - 1, stepW):
                    yield (lineOffset + i, tileLine)
                    tileLine += 1
            tileLine += secondLine

type
    TexFmtProperties* = object
        tileW*, tileH*: uint8 # as log2 (to calculate proper size use 1 shl tileW/tileH)
        cacheLines*: uint8

const texFmtProperties: array[TxTextureFmt, TexFmtProperties] = [
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

type
    TextureKey* = object
        fmt: TxTextureFmt
        width, height: uint32
        adr: uint32

        palHash: uint64
        palFmt: TxLutFmt

    TextureCacheEntry = object
        native: NativeTexture
        dataSize: uint32
        invalid: bool
var
    textures: Table[TextureKey, TextureCacheEntry]

proc hash(key: TextureKey): Hash =
    result = result !& hash(key.fmt)
    result = result !& hash(key.width)
    result = result !& hash(key.height)
    result = result !& hash(key.adr)
    if key.fmt in PalettedTexFmts:
        result = result !& hash(key.palFmt)
        result = result !& hash(key.palHash)
    result = !$result

proc `==`(a, b: TextureKey): bool =
    result = a.fmt == b.fmt and
        a.width == b.width and a.height == b.height and
        a.adr == b.adr
    if result:
        if a.palFmt != b.palFmt:
            return false
        if a.palHash != b.palHash:
            return false

proc invalidateTexture*(adr: uint32) =
    # pretty inefficient
    for key, entry in mpairs textures:
        if adr >= key.adr and adr < key.adr + entry.dataSize:
            if not entry.invalid:
                echo &"invalidating texture at {key.adr:08X} (write to {adr:08X})"
            entry.invalid = true

import bp

proc calcTexPalHash(adr, size: uint32): uint64 =
    assert adr + size <= uint32(sizeof(tmem))
    if size <= 32:
        # for small sizes the overhead of always just rehashing is probably similar
        # compared to the table operations and the associated clears
        result = XXH3_64bits(cast[ptr UncheckedArray[byte]](addr tmem[adr]), csize_t(size))
    else:
        palHashCache.withValue((adr, size), value) do:
            result = value[]
        do:
            result = XXH3_64bits(cast[ptr UncheckedArray[byte]](addr tmem[adr]), csize_t(size))
            palHashCache[(adr, size)] = result

proc decodeTextureI4(dst, src: ptr UncheckedArray[byte], width, height: int) =
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 8, 2):
        let srcPixels = src[srcIdx]
        dst[dstIdx] = srcPixels and 0xF0
        if dst[dstIdx].getBit(4):
            dst[dstIdx].setMask 0x0F
        dst[dstIdx + 1] = srcPixels shl 4
        if dst[dstIdx + 1].getBit(4):
            dst[dstIdx + 1].setMask 0x0F

proc decodeTextureI8(dst, src: ptr UncheckedArray[byte], width, height: int) =
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 4):
        dst[dstIdx] = src[srcIdx]

proc decodeTextureIA4(dst, src: ptr UncheckedArray[byte], width, height: int) =
    let dst = cast[ptr UncheckedArray[uint16]](dst)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 4):
        var
            intensity = uint16((src[srcIdx] and 0xF) shl 4)
            alpha = uint16(src[srcIdx] and 0xF0)
        if intensity.getBit(4): intensity = intensity or 0x0F
        if alpha.getBit(4): alpha = alpha or 0x0F
        dst[dstIdx] = (alpha shl 8) or intensity

proc decodeTextureIA8RGB565(dst, src: ptr UncheckedArray[byte], width, height: int) =
    let
        dst = cast[ptr UncheckedArray[uint16]](dst)
        src = cast[ptr UncheckedArray[uint16]](src)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 4, 4):
        dst[dstIdx] = fromBE src[srcIdx]

proc RGB5A3ToRGBA8*(color: uint32): uint32 =
    if (color and 0x8000) != 0:
        # color is RGB5
        result = ((color and 0x1F) shl 19) or
            ((color and 0x3E0) shl 6) or
            ((color and 0x7C00) shr 7) or
            0xFF000000'u32
        if result.getBit(3+0):
            result.setMask 0x7
        if result.getBit(8+3):
            result.setMask 0x700
        if result.getBit(16+3):
            result.setMask 0x70000
    else:
        # color is RGB4A3
        result = ((color and 0xF) shl 20) or
            ((color and 0xF0) shl 8) or
            ((color and 0xF00) shr 4) or
            ((color and 0x7000) shl 17)
        if result.getBit(0+4):
            result.setMask 0xF
        if result.getBit(8+4):
            result.setMask 0xF00
        if result.getBit(16+4):
            result.setMask 0xF0000
        if result.getBit(24+5):
            result.setMask 0x1F000000

proc decodeTextureRGB5A3(dst, src: ptr UncheckedArray[byte], width, height: int) =
    let
        dst = cast[ptr UncheckedArray[uint32]](dst)
        src = cast[ptr UncheckedArray[uint16]](src)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 4, 4):
        dst[dstIdx] = RGB5A3ToRGBA8(fromBE src[srcIdx])

proc decodeTextureRGBA8(dst, src: ptr UncheckedArray[byte], width, height: int) =
    let
        dst = cast[ptr UncheckedArray[uint32]](dst)
        src = cast[ptr UncheckedArray[uint16]](src)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 4, 4, 1, 1, 16):
        var
            ra = uint32(src[srcIdx])
            bg = uint32(src[srcIdx+16])

        dst[dstIdx] = (ra shl 24) or (bg shl 8) or ((ra and 0xFF00'u32) shr 8)

proc decodeTextureC4IA8RGB565(dst, src: ptr UncheckedArray[byte], width, height: int, palette: ptr UncheckedArray[uint16]) =
    let dst = cast[ptr UncheckedArray[uint16]](dst)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 8, 2):
        let srcPixels = src[srcIdx]
        dst[dstIdx] = fromBE palette[srcPixels shr 4]
        dst[dstIdx + 1] = fromBE palette[srcPixels and 0xF]

proc decodeTextureC4RGB5A3(dst, src: ptr UncheckedArray[byte], width, height: int, palette: ptr UncheckedArray[uint16]) =
    let dst = cast[ptr UncheckedArray[uint32]](dst)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 8, 2):
        let srcPixels = src[srcIdx]
        dst[dstIdx] = RGB5A3ToRGBA8(fromBE palette[srcPixels shr 4])
        dst[dstIdx + 1] = RGB5A3ToRGBA8(fromBE palette[srcPixels and 0xF])

proc decodeTextureC8IA8RGB565(dst, src: ptr UncheckedArray[byte], width, height: int, palette: ptr UncheckedArray[uint16]) =
    let dst = cast[ptr UncheckedArray[uint16]](dst)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 4):
        dst[dstIdx] = fromBE palette[src[srcIdx]]

proc decodeTextureC8RGB5A3(dst, src: ptr UncheckedArray[byte], width, height: int, palette: ptr UncheckedArray[uint16]) =
    let dst = cast[ptr UncheckedArray[uint32]](dst)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 4):
        dst[dstIdx] = RGB5A3ToRGBA8(fromBE palette[src[srcIdx]])

makeBitStruct uint64, CmprBlock:
    indices[n, (n*2)..(n*2)+1]: uint32
    color0[48..63]: uint32
    color1[32..47]: uint32
    b0[48..52]: uint32
    g0[53..58]: uint32
    r0[59..63]: uint32
    b1[32..36]: uint32
    g1[37..42]: uint32
    r1[43..47]: uint32

proc blend3of8(first, second: uint32): uint32 =
    (first * 3 + second * 5) div 8

proc blendAvg(first, second: uint32): uint32 =
    (first + second) div 2

proc packRGB565A1ToRGBA8(r, g, b, a: uint32): uint32 =
    result = (r shl (0+3)) or (g shl (8+2)) or (b shl (16+3)) or ((if a == 0: 0'u32 else: 255'u32) shl 24)
    if result.getBit(0+3):
        result = result or 0x7
    if result.getBit(8+2):
        result = result or 0x300
    if result.getBit(16+3):
        result = result or 0x70000

proc decodeTextureCmpr(dst, src: ptr UncheckedArray[byte], width, height: int) =
    let
        dst = cast[ptr UncheckedArray[uint32]](dst)
        src = cast[ptr UncheckedArray[uint64]](src)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 8, 4, 4):
        let
            cmprBlock = CmprBlock fromBE(src[srcIdx])

            (r2, g2, b2, a2, r3, g3, b3, a3) =
                if cmprBlock.color0 > cmprBlock.color1:
                    (blend3of8(cmprBlock.r0, cmprBlock.r1), blend3of8(cmprBlock.g0, cmprBlock.g1), blend3of8(cmprBlock.b0, cmprBlock.b1), 1'u32,
                        blend3of8(cmprBlock.r1, cmprBlock.r0), blend3of8(cmprBlock.g1, cmprBlock.g0), blend3of8(cmprBlock.b1, cmprBlock.b0), 1'u32)
                else:
                    let
                        r = blendAvg(cmprBlock.r0, cmprBlock.r1)
                        g = blendAvg(cmprBlock.g0, cmprBlock.g1)
                        b = blendAvg(cmprBlock.b0, cmprBlock.b1)
                    (r, g, b, 1'u32, r, g, b, 0'u32)

            colors = [
                packRGB565A1ToRGBA8(cmprBlock.r0, cmprBlock.g0, cmprBlock.b0, 1),
                packRGB565A1ToRGBA8(cmprBlock.r1, cmprBlock.g1, cmprBlock.b1, 1),
                packRGB565A1ToRGBA8(r2, g2, b2, a2),
                packRGB565A1ToRGBA8(r3, g3, b3, a3)]

        for j in 0..<4:
            for i in 0..<4:
                let idx = cmprBlock.indices((3 - i) + (3 - j) * 4)
                dst[dstIdx + j * width + i] = colors[idx]


proc getTargetFmt(fmt: TxTextureFmt, palfmt: TxLutFmt): TextureFormat =
    case fmt
    of txTexfmtRGB5A3, txTexfmtRGBA8, txTexfmtCmp: texfmtRGBA8
    of txTexfmtRGB565: texfmtRGB565
    of txTexfmtI4, txTexfmtI8: texfmtI8
    of txTexfmtIA4, txTexfmtIA8: texfmtIA8
    of txTexfmtC4, txTexfmtC8:
        case palfmt
        of txLutfmtIA8: texfmtIA8
        of txLutfmtRGB565: texfmtRGB565
        of txLutfmtRGB5A3: texfmtRGBA8
        else: raiseAssert("reserved value")
    else: raiseAssert(&"texfmt {fmt} not supported yet!")

proc setupSampler*(n: int) =
    discard

proc setupTexture*(n: int) =
    let
        texmap = texMaps[n]
        texpalAdr = (texmap.setLut.tmemOffset shl 9) + 0x80000
    var key = TextureKey(
            fmt: texmap.setImage0.fmt,
            width: texmap.width,
            height: texmap.height,
            adr: texmap.adr)

    if key.fmt in PalettedTexFmts:
        key.palFmt = texmap.setLut.fmt
        key.palHash = calcTexPalHash(texpalAdr,
            (case key.fmt
            of txTexfmtC4: 16'u32
            of txTexfmtC8: 256'u32
            of txTexfmtC14X2: (1'u32 shl 14)
            else: raiseAssert("reserved value")) * 2)

    #echo &"setup texture {n} {key.width}x{key.height} {key.adr:08X} {key.fmt} {texpalAdr:X} {key.palHash:016X} {key.palFmt}"

    assert(not texmap.setImage1.preloaded)

    var texture: TextureCacheEntry
    if (texture = textures.getOrDefault(key); texture.native == nil):
        # load in texture
        let
            fmtInfo = texFmtProperties[key.fmt]
            targetFmt = getTargetFmt(key.fmt, texmap.setLut.fmt)
            dataSize = fmtInfo.textureDataSize(key.width, key.height)

        for i in 0..<dataSize div 32:
            mainRAMTagging[(key.adr shr 5) + i] = memoryTagTexture

        texture.dataSize = dataSize
        texture.native = rasterogl.createTexture(int key.width, int key.height, 1, targetFmt)
        textures[key] = texture
        texture.invalid = true

        echo &"creating texture {n} {key.width}x{key.height} {key.adr:08X} {key.fmt} {texpalAdr:X} {key.palHash:016X} {key.palFmt}"

    if texture.invalid:
        type DecodingFunc = proc(dst, src: ptr UncheckedArray[byte], width, height: int) {.nimcall.}
        const
            decodingFuncs: array[TxTextureFmt, DecodingFunc] = [
                decodeTextureI4,
                decodeTextureI8,
                decodeTextureIA4,
                decodeTextureIA8RGB565,
                decodeTextureIA8RGB565,
                decodeTextureRGB5A3,
                decodeTextureRGBA8,
                nil,
                nil,
                nil,
                nil,
                nil,
                nil,
                nil,
                decodeTextureCmpr,
                nil]
            decodeTexturePaletted = [
                [decodeTextureC4IA8RGB565, decodeTextureC4RGB5A3],
                [decodeTextureC8IA8RGB565, decodeTextureC8RGB5A3]]

        const targetFmtPixelSize: array[TextureFormat, byte] = [1'u8, 1, 2, 4, 2, 2, 3]

        let
            fmtInfo = texFmtProperties[key.fmt]
            roundedWidth = fmtInfo.roundedWidth(key.width)
            roundedHeight = fmtInfo.roundedHeight(key.height)

        var data = newSeq[byte](roundedWidth*roundedHeight*uint32(targetFmtPixelSize[getTargetFmt(key.fmt, key.palFmt)]))

        if key.fmt notin PalettedTexFmts:
            decodingFuncs[key.fmt](cast[ptr UncheckedArray[byte]](addr data[0]),
                cast[ptr UncheckedArray[byte]](addr mainRAM[texmap.adr]),
                int(roundedWidth), int(roundedHeight))
        else:
            decodeTexturePaletted[int(key.fmt == txTexfmtC8)][int(key.palFmt == txLutfmtRGB5A3)](
                cast[ptr UncheckedArray[byte]](addr data[0]),
                cast[ptr UncheckedArray[byte]](addr mainRAM[texmap.adr]),
                int(roundedWidth), int(roundedHeight),
                cast[ptr UncheckedArray[uint16]](addr tmem[texpalAdr]))

        rasterogl.uploadTexture(texture.native, 0, 0, 0, int(key.width), int(key.height), int(roundedWidth), addr data[0])

    rasterogl.bindTexture(n, texture.native)
