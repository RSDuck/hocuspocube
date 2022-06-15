import
    hashes, tables, strformat,
    xxhash,

    rasterinterfacecommon,
    opengl/rasterogl,
    bpcommon,
    ../gekko/memory,
    texturedecode

var palHashCache: Table[(uint32, uint32), uint64]

proc clearPalHashCache*() =
    palHashCache.clear()

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
                    mainRamReadPtr(texmap.adr, texture.dataSize),
                    int(roundedWidth), int(roundedHeight))
        else:
            decodeTexturePaletted[int(key.fmt == txTexfmtC8)][int(key.palFmt == txLutfmtRGB5A3)](
                cast[ptr UncheckedArray[byte]](addr data[0]),
                mainRamReadPtr(texmap.adr, texture.dataSize),
                int(roundedWidth), int(roundedHeight),
                cast[ptr UncheckedArray[uint16]](addr tmem[texpalAdr]))

        rasterogl.uploadTexture(texture.native, 0, 0, 0, int(key.width), int(key.height), int(roundedWidth), addr data[0])

    rasterogl.bindTexture(n, texture.native)
