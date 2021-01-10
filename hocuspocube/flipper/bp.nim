import
    strformat,

    ../util/bitstruct,

    ../gecko/gecko,

    rasterinterface,
    pe

type
    CopyMode = enum
        copyTexture
        copyXfb

makeBitStruct uint32, CopyExecute:
    mode[14]: CopyMode
    clear[11]: bool

makeBitStruct uint32, EfbCoordPair:
    x[0..9]: uint32
    y[10..24]: uint32

makeBitStruct uint32, EfbCopyStride:
    stride[0..9]: uint32

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

var
    clearR, clearG, clearB, clearA: uint8
    clearZ: uint32

    efbCopyDst, efbCopyDstStride: uint32
    efbCopySrcX, efbCopySrcY, efbCopyW, efbCopyH: uint32

proc bpWrite*(adr, val: uint32) =
    case adr
    of 0x49:
        let val = EfbCoordPair val
        efbCopySrcX = val.x
        efbCopySrcY = val.y
    of 0x4A:
        let val = EfbCoordPair val
        efbCopyW = val.x + 1
        efbCopyH = val.y + 1
    of 0x4B:
        efbCopyDst = val shl 5
    of 0x4D:
        efbCopyDstStride = EfbCopyStride(val).stride shl 5
    of 0x45:
        if (val and 0x2) != 0:
            pe.flagFinish()
            echo "pe finish!"
    of 0x4F:
        clearR = uint8(val)
        clearA = uint8(val shr 8)
    of 0x50:
        clearB = uint8(val)
        clearG = uint8(val shr 8)
    of 0x51:
        clearZ = val
    of 0x52:
        let val = CopyExecute val

        echo &"copy execute {efbCopySrcX}, {efbCopySrcY} {efbCopyW}x{efbCopyH} to {efbCopyDst:08X} stride: {efbCopyDstStride}"

        assert val.mode == copyXfb

        var efbContent = newSeq[uint32](efbCopyW * efbCopyH)
        retrieveFrame(efbContent, efbCopySrcX, efbCopySrcY, efbCopyW, efbCopyH)

        var adr = efbCopyDst
        for i in 0..<efbCopyH:
            convertLineRgbToYuv(cast[ptr UncheckedArray[uint32]](addr MainRAM[adr]),
                toOpenArray(efbContent, int (efbCopyH-i-1)*efbCopyW, int (efbCopyH-i-1+1)*efbCopyW-1),
                int efbCopyW)
            adr += efbCopyDstStride

        if val.clear:
            rasterinterface.clear(clearR, clearG, clearB, clearA, clearZ)
    else: echo &"unknown bp write {adr:02X} {val:06X}"
