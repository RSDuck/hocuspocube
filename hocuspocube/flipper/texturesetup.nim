import
    hashes, tables, strformat,
    stew/endians2,
    xxhash,

    ../util/bitstruct,

    rasterinterfacecommon,
    opengl/rasterogl,
    bpcommon,
    ../gekko/gekko

var palHashCache: Table[(uint32, uint32), uint64]

proc clearPalHashCache*() =
    palHashCache.clear()

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

proc calculateTexSize(fmt: TxTextureFmt, width, height, levels: uint32): (uint32, uint32, uint32) =        
    let (tileW, tileH, lineBytes) = case fmt
        of txTexfmtI4, txTexfmtC4, txTexfmtCmp:
            (3'u32, 3'u32, 32'u32)
        of txTexfmtI8, txTexfmtIA4, txTexfmtC8:
            (3'u32, 2'u32, 32'u32)
        of txTexfmtRGB565, txTexfmtRGB5A3, txTexfmtIA8, txTexfmtC14X2:
            (2'u32, 2'u32, 32'u32)
        of txTexfmtRGBA8:
            (2'u32, 2'u32, 64'u32)
        else: raiseAssert("invalid texture fmt")

    let
        numTilesX = (width + (1'u32 shl tileW) - 1) shr tileW
        numTilesY = (height + (1'u32 shl tileH) - 1) shr tileH

    result[0] = numTilesX * numTilesY * lineBytes
    result[1] = numTilesX shl tileW
    result[2] = numTilesY shl tileH

    for i in 1..<levels:
        result[0] += (numTilesX shr i) * (numTilesY shr i) * lineBytes

iterator doTileLoop(width, height, tileW, tileH: int, stepW = 1, stepH = 1, secondLine = 0): (int, int) =
    let
        roundedWidth = (width + tileW - 1) and not(tileW - 1)
        roundedHeight = (height + tileH - 1) and not(tileH - 1)
    var tileSrc = 0
    for y in countup(0, roundedHeight - 1, tileH):
        for x in countup(0, roundedWidth - 1, tileW):
            let tileOffset = x + y * roundedWidth
            for j in countup(0, tileH - 1, stepH):
                let lineOffset = tileOffset + j * roundedWidth
                for i in countup(0, tileW - 1, stepW):
                    yield (lineOffset + i, tileSrc)
                    tileSrc += 1
            tileSrc += secondLine

proc decodeTextureI4(dst, src: ptr UncheckedArray[byte], width, height: int) =
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 8, 2):
        let srcPixels = src[srcIdx]
        dst[dstIdx] = srcPixels and 0xF0
        dst[dstIdx + 1] = srcPixels shl 4

proc decodeTextureI8(dst, src: ptr UncheckedArray[byte], width, height: int) =
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 4):
        dst[dstIdx] = src[srcIdx]

proc decodeTextureIA4(dst, src: ptr UncheckedArray[byte], width, height: int) =
    let dst = cast[ptr UncheckedArray[uint16]](dst)
    for (dstIdx, srcIdx) in doTileLoop(width, height, 8, 4):
        let
            intensity = uint16((src[srcIdx] and 0xF) shl 4)
            alpha = uint16(src[srcIdx] and 0xF0)
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
        ((color and 0x1F) shl 19) or
            ((color and 0x3E0) shl 6) or
            ((color and 0x7C00) shr 7) or
            0xFF000000'u32
    else:
        # color is RGB4A3
        ((color and 0xF) shl 20) or
            ((color and 0xF0) shl 8) or
            ((color and 0xF00) shr 4) or
            ((color and 0x7000) shl 17)


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
    (r shl (0+3)) or (g shl (8+2)) or (b shl (16+3)) or ((if a == 0: 0'u32 else: 255'u32) shl 24)

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

            colors = [packRGB565A1ToRGBA8(cmprBlock.r0, cmprBlock.g0, cmprBlock.b0, 1),
                packRGB565A1ToRGBA8(cmprBlock.r1, cmprBlock.g1, cmprBlock.b1, 1),
                packRGB565A1ToRGBA8(r2, g2, b2, a2),
                packRGB565A1ToRGBA8(r3, g3, b3, a3)]

        for j in 0..<4:
            for i in 0..<4:
                let idx = cmprBlock.indices((3 - i) + (3 - j) * 4)

                # for compressed textures the width has to be a multiple of the block size
                # so it's ok that we use width instead of the rounded width here
                dst[dstIdx + j * width + i] = colors[idx]

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
            targetFmt = getTargetFmt(key.fmt, texmap.setLut.fmt)

            (dataSize, _, _) = calculateTexSize(key.fmt, key.width, key.height, 1)

        for i in 0..<dataSize div 32:
            mainRAMTagging[(key.adr shr 5) + i] = memoryTagTexture

        texture.dataSize = dataSize
        texture.native = rasterogl.createTexture(int key.width, int key.height, 1, targetFmt)
        textures[key] = texture
        texture.invalid = true

        echo "creating texture"

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

        let (_, roundedWidth, roundedHeight) = calculateTexSize(key.fmt, key.width, key.height, 1)

        var data = newSeq[byte](roundedWidth*roundedHeight*uint32(targetFmtPixelSize[getTargetFmt(key.fmt, key.palFmt)]))

        if key.fmt notin PalettedTexFmts:
            decodingFuncs[key.fmt](cast[ptr UncheckedArray[byte]](addr data[0]),
                cast[ptr UncheckedArray[byte]](addr mainRAM[texmap.adr]),
                int(key.width), int(key.height))
        else:
            decodeTexturePaletted[int(key.fmt == txTexfmtC8)][int(key.palFmt == txLutfmtRGB5A3)](
                cast[ptr UncheckedArray[byte]](addr data[0]),
                cast[ptr UncheckedArray[byte]](addr mainRAM[texmap.adr]),
                int(key.width), int(key.height),
                cast[ptr UncheckedArray[uint16]](addr tmem[texpalAdr]))

        rasterogl.uploadTexture(texture.native, 0, 0, 0, int(key.width), int(key.height), int(roundedWidth), addr data[0])

    rasterogl.bindTexture(n, texture.native)

proc invalidateTexture*(adr: uint32) =
    # pretty inefficient
    for key, entry in mpairs textures:
        if adr >= key.adr and adr < key.adr + entry.dataSize:
            if not entry.invalid:
                echo &"invalidating texture at {key.adr:08X} (write to {adr:08X})"
            entry.invalid = true
