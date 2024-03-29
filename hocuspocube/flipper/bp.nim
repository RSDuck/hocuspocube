import
    strformat,
    stew/endians2,
    bpcommon,
    ../gekko/[gekko, memory],
    ../vi,
    pe,
    texturedecode,
    rasterinterfacecommon

template bpLog(msg: string): untyped = discard

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

    efbCopyDst: uint32
    efbCopyDstStride: EfbCopyStride
    efbCopySrcPos, efbCopySize: EfbCoordPair

    scissorTL, scissorBR: ScissorCoords
    scissorOffset: ScissorOffset

    copyExecute: CopyExecute

    zmode*: ZMode

    sSize*: array[8, SuSize]
    tSize*: array[8, SuSize]

    texMaps*: array[8, TexMap]

    peCMode0*: PeCMode0
    peCMode1*: PeCMode1

    indMat*: array[9, IndMatElement]
    indCmd*: array[16, IndCmd]

    ras1ss*: array[2, Ras1SS]
    ras1iref*: Ras1Iref

    colorEnv*: array[16, TevColorEnv]
    alphaEnv*: array[16, TevAlphaEnv]
    ras1Tref*: array[8, Ras1Tref]

    zenv0*: uint32
    zenv1*: TevZEnv1

    tevRegister*: array[8, TevRegister]
    konstants*: array[8, TevRegister]

    tevKSel*: array[8, TevKSel]

    genMode*: GenMode
    peCntrl*: PeCntrl

    alphaCompare*: AlphaCompare

    efbCopyStepY: uint32

    loadLut0: uint32
    loadLut1: TxLoadTLut1

    tmem*: array[1024*1024, byte]

    bpMask: uint32

proc getScissor*(): (int32, int32, int32, int32) =
    result[0] = int32(scissorTL.x) - 342
    result[1] = int32(scissorTL.y) - 342
    result[2] = int32(scissorBR.x) - int32(scissorTL.x) + 1
    result[3] = int32(scissorBR.y) - int32(scissorTL.y) + 1

proc getScissorOffset*(): (int32, int32) =
    result[0] = int32(scissorOffset.x) * 2 - 342
    result[1] = int32(scissorOffset.y) * 2 - 342

import
    rasterinterface, texturesetup

proc maskedWrite[T](register: var T, val: uint32): bool {.discardable.} =
    let prevValue = register
    register = T((uint32(register) and not(bpMask)) or (val and bpMask))
    register != prevValue

proc bpWrite*(adr, val: uint32) =
    case adr
    of 0x00:
        if genMode.maskedWrite val:
            fragmentShaderDirty = true
            rasterStateDirty = true
    of 0x01..0x04:
        # copy filter stuff
        discard
    of 0x06..0x0E:
        if indMat[adr - 0x06].maskedWrite val:
            registerUniformDirty = true
    of 0x0F:
        # indmask
        # it's just a bitmap of which textures are used in indirect stages
        discard
    of 0x10..0x1F:
        if indCmd[adr - 0x10].maskedWrite val:
            fragmentShaderDirty = true
    of 0x20:
        if scissorTL.maskedWrite val:
            rasterStateDirty = true
    of 0x21:
        if scissorBR.maskedWrite val:
            rasterStateDirty = true
    of 0x22:
        # lpWidth
        # line and point emulation is currently completely bad anyway
        discard
    of 0x25..0x26:
        if ras1ss[adr - 0x25].maskedWrite val:
            registerUniformDirty = true
    of 0x27:
        if ras1iref.maskedWrite val:
            fragmentShaderDirty = true
    of 0x28..0x2F:
        if ras1Tref[adr - 0x28].maskedWrite val:
            fragmentShaderDirty = true
    of 0x30..0x3F:
        if
            (if (adr mod 2) == 0:
                sSize[(adr - 0x30) div 2].maskedWrite val
            else:
                tSize[(adr - 0x30) div 2].maskedWrite val):
            registerUniformDirty = true
    of 0x40:
        if zmode.maskedWrite val:
            rasterStateDirty = true
    of 0x41:
        if peCMode0.maskedWrite val:
            rasterStateDirty = true
    of 0x42:
        if peCMode1.maskedWrite val:
            registerUniformDirty = true
            rasterStateDirty = true
    of 0x43:
        if peCntrl.maskedWrite val:
            fragmentShaderDirty = true
    of 0x49:
        efbCopySrcPos.maskedWrite val
    of 0x4A:
        efbCopySize.maskedWrite val
    of 0x4B:
        efbCopyDst.maskedWrite val
    of 0x4D:
        efbCopyDstStride.maskedWrite val
    of 0x45:
        if ((val and bpMask) and 0x2) != 0:
            finishFrame()

            pe.flagFinish()
            bpLog "pe finish!"
    of 0x4E:
        efbCopyStepY.maskedWrite val
    of 0x4F:
        clearR = uint8(val)
        clearA = uint8(val shr 8)
    of 0x50:
        clearB = uint8(val)
        clearG = uint8(val shr 8)
    of 0x51:
        clearZ.maskedWrite val
    of 0x52:
        copyExecute.maskedWrite val

        let
            srcX = efbCopySrcPos.x
            srcY = efbCopySrcPos.y
            width = efbCopySize.x+1
            height = efbCopySize.y+1
            stride = efbCopyDstStride.stride shl 5
            dstAdr = HwPtr(efbCopyDst shl 5).adr

            # not accurate!!!
            xfbHeight = uint32(1f+256f/float32(efbCopyStepY)*float32(height-1))

        assert not copyExecute.intensity, "intensity copies are not supported"

        let (fb, finalHeight, shader, depthCopy) = case copyExecute.mode
            of copyXfb:
                #[var efbContent = newSeq[uint32](width * height)
                rasterinterface.retrieveFrame(efbContent, srcX, srcY, width, height)

                let xfbLines = uint32(float32(height) * (256f / float32(stride div 2)))
                withMainRamWritePtr(HwPtr(efbCopyDst shl 5).adr, stride*xfbLines):
                    #echo &"dispcopy {srcX}, {srcY} {width}x{height} to {efbCopyDst:08X} stride: {stride} {efbCopyStepY}"
                    var
                        offset = 0'u32
                        line = efbCopyStepY - 1 # in .8 fix point like efbCopyStepY
                    for i in 0..<height:
                        let uniqueOffset = offset
                        convertLineRgbToYuv(cast[ptr UncheckedArray[uint32]](addr ramPtr[offset]),
                            toOpenArray(efbContent, int (height-i-1)*width, int (height-i-1+1)*width-1),
                            int width)
                        offset += stride
                        line += efbCopyStepY

                        while (line shr 8) == i:
                            copyMem(addr ramPtr[offset], unsafeAddr ramPtr[uniqueOffset], width*2)
                            line += efbCopyStepY
                            offset += stride]#

                (vi.getOrCreateXfbFramebuffer(dstAdr, width, xfbHeight, stride),
                    xfbHeight,
                    efbColorCopyShaders[copyTexfmtRGBA8][false],
                    false)
            of copyTexture:
                const
                    resultColorFmt: array[CopyTexFmt, TextureFormat] = [
                        texfmtI8, # R4
                        texfmtI8,
                        texfmtIA8, # RA4
                        texfmtIA8, # RA8
                        texfmtRGBA8, # RGB565
                        texfmtRGBA8, # RGB5A3
                        texfmtRGBA8, # RGBA8
                        texfmtI8, # A8
                        texfmtI8, # R8
                        texfmtI8, # G8
                        texfmtI8, # B8
                        texfmtIA8, # RG8
                        texfmtIA8, # GB8
                        texfmtI8,
                        texfmtI8,
                        texfmtI8]
                    resultDepthFmt: array[CopyTexZFmt, TextureFormat] = [
                        texfmtI8, # Z4
                        texfmtI8, # Z8
                        texfmtI8,
                        texfmtI8,
                        texfmtI8,
                        texfmtI8,
                        texfmtRGBA8, # Z24X
                        texfmtI8,
                        texfmtI8,
                        texfmtI8, # Z8M
                        texfmtI8, # Z8L
                        texfmtI8,
                        texfmtIA8, # Z16L
                        texfmtI8,
                        texfmtI8,
                        texfmtI8]

                let
                    depthCopy = peCntrl.fmt == peFmtZ24
                
                    targetTex = texturesetup.getOrCreateXfbFramebuffer(dstAdr,
                        width, height,
                        if depthCopy: resultDepthFmt[CopyTexZFmt copyExecute.fmt]
                        else: resultColorFmt[CopyTexFmt copyExecute.fmt])

                    shader =
                        if depthCopy:
                            efbDepthCopyShaders[CopyTexZFmt copyExecute.fmt]
                        else:
                            efbColorCopyShaders[CopyTexFmt copyExecute.fmt][peCntrl.fmt == peFmtRGBA6Z24]
                (targetTex, height, shader, depthCopy)
                #[let
                    fmt = CopyTexFmt(copyExecute.fmt)
                    fmtInfo = texCopyFmtProperties[fmt]
                    dstAdr = HwPtr(efbCopyDst shl 5).adr

                    roundedWidth = fmtInfo.roundedWidth(width)
                    roundedHeight = fmtInfo.roundedHeight(height)

                invalidateTexture(dstAdr)

                withMainRamWritePtr(dstAdr, fmtInfo.textureDataSize(roundedWidth, roundedHeight)):
                    var efbContent = newSeq[uint32](roundedWidth * roundedHeight)
                    rasterinterface.retrieveFrame(efbContent, srcX, srcY, roundedWidth, roundedHeight)

                    echo &"texcopy {srcX}, {srcY} {width}x{height} (rounded to {roundedWidth}x{roundedHeight}) to {efbCopyDst:08X} stride: {stride} {fmt}"

                    if fmt == copyTexfmtRGB565:
                        let dst = cast[ptr UncheckedArray[uint16]](ramPtr)

                        for (srcIdx, dstIdx) in doTileLoop(int roundedWidth, int roundedHeight, 4, 4, flipY = true, tileStride = int(stride) div 2):
                            let src = efbContent[srcIdx]
                            dst[dstIdx] = toBE uint16(((src and 0xF8) shl 8) or ((src and 0xFC00) shr 5) or ((src and 0xF80000) shr 19))
                    else:
                        raiseAssert(&"unimplemented texcopy format {fmt}")
                (nil, 0'u32)]#

        #echo &"efb copy {dstAdr:08X} {copyExecute.fmt} {depthCopy} {width} {height} {stride} {efbCopyStepY} {copyExecute.yscale}"

        assert shader != nil, &"{depthCopy} {peCntrl.fmt == peFmtRGBA6Z24} {copyExecute.fmt}"
        rasterinterface.copyEfb(fb,
            shader,
            int width, int finalHeight,
            float32 srcX, float32 srcY, float32 width, float32 height,
            depthCopy)

        if copyExecute.clear:
            rasterinterface.clear(clearR, clearG, clearB, clearA, clearZ, peCMode0.colorUpdate, peCMode0.alphaUpdate, zmode.update)
    of 0x53, 0x54:
        discard # also copy filter
    of 0x59:
        if scissorOffset.maskedWrite val:
            rasterStateDirty = true
    of 0x64:
        loadLut0.maskedWrite val
    of 0x65:
        loadLut1.maskedWrite val

        let
            src = HwPtr(loadLut0 shl 5).adr
            dst = (loadLut1.tmemAdr shl 9) + 0x80000
            count = loadLut1.size*32

        assert dst + count <= uint32(sizeof(tmem))
        #echo &"uploading tlut {src:08X} {dst:X} {count}"
        readMainRam(src, addr tmem[dst], count)
        samplerStateDirty.incl {range[0..7](0)..7}
        clearPalHashCache()
    of 0x66:
        # invalidate tex cache
        discard
    of 0x80..0x83:
        let idx = adr - 0x80
        if texMaps[idx].setMode0.maskedWrite val:
            samplerStateDirty.incl idx
    of 0x84..0x87:
        let idx = adr - 0x84
        if texMaps[idx].setMode1.maskedWrite val:
            samplerStateDirty.incl idx
    of 0x88..0x8B:
        let idx = adr - 0x88
        if texMaps[idx].setImage0.maskedWrite val:
            imageStateDirty.incl idx
            registerUniformDirty = true
    of 0x8C..0x8F:
        let idx = adr - 0x8C
        if texMaps[idx].setImage1.maskedWrite val:
            imageStateDirty.incl idx
    of 0x90..0x93:
        let idx = adr - 0x90
        if texMaps[idx].setImage2.maskedWrite val:
            imageStateDirty.incl idx
    of 0x94..0x97:
        let idx = adr - 0x94
        if texMaps[idx].setImage3.maskedWrite val:
            imageStateDirty.incl idx
    of 0x98..0x9B:
        let idx = adr - 0x98
        if texMaps[idx].setLut.maskedWrite val:
            imageStateDirty.incl idx
    of 0xA0..0xA3:
        let idx = adr - 0xA0 + 4
        if texMaps[idx].setMode0.maskedWrite val:
            samplerStateDirty.incl idx
    of 0xA4..0xA7:
        let idx = adr - 0xA4 + 4
        if texMaps[idx].setMode1.maskedWrite val:
            samplerStateDirty.incl idx
    of 0xA8..0xAB:
        let idx = adr - 0xA8 + 4
        if texMaps[idx].setImage0.maskedWrite val:
            imageStateDirty.incl idx
            registerUniformDirty = true
    of 0xAC..0xAF:
        let idx = adr - 0xAC + 4
        if texMaps[idx].setImage1.maskedWrite val:
            imageStateDirty.incl idx
    of 0xB0..0xB3:
        let idx = adr - 0xB0 + 4
        if texMaps[idx].setImage2.maskedWrite val:
            imageStateDirty.incl idx
    of 0xB4..0xB7:
        let idx = adr - 0xB4 + 4
        if texMaps[idx].setImage3.maskedWrite val:
            imageStateDirty.incl idx
    of 0xB9..0xBB:
        let idx = adr - 0xB9 + 4
        if texMaps[idx].setLut.maskedWrite val:
            imageStateDirty.incl idx
    of 0xC0..0xDF:
        if (adr mod 2) == 0:
            if colorEnv[(adr - 0xC0) div 2].maskedWrite val:
                fragmentShaderDirty = true
        else:
            if alphaEnv[(adr - 0xC1) div 2].maskedWrite val:
                fragmentShaderDirty = true
    of 0xE0..0xE7:
        var dirty = false
        # TODO: figure out how exactly this works
        if TevRegister(val).setKonst:
            dirty = konstants[adr - 0xE0].maskedWrite uint32(val)
        else:
            dirty = tevRegister[adr - 0xE0].maskedWrite uint32(val)
        registerUniformDirty = registerUniformDirty or dirty
    of 0xE8..0xEF:
        discard # fog stuff
    of 0xF0..0xF2:
        discard # more fog
    of 0xF3:
        let val = AlphaCompare val
        if alphaCompare.ref0 != val.ref0 or alphaCompare.ref1 != val.ref1:
            registerUniformDirty = true
        if alphaCompare.maskedWrite uint32(val):
            fragmentShaderDirty = true
    of 0xF4:
        if zenv0.maskedWrite(val):
            registerUniformDirty = true
    of 0xF5:
        if zenv1.maskedWrite val:
            fragmentShaderDirty = true
    of 0xF6..0xFD:
        if tevKSel[adr - 0xF6].maskedWrite val:
            fragmentShaderDirty = true
    of 0xFE:
        bpMask = val
        return
    else: echo &"unknown bp write {adr:02X} {val:06X}"

    bpMask = 0xFFFFFF'u32