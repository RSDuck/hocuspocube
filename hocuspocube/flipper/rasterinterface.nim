import
    strformat, tables, hashes,
    opengl/rasterogl,
    rasterinterfacecommon,
    bpcommon

var
    curVertexBuffer*: VertexBuffer

    xfMemoryDirty*: bool
    xfMemoryUniform*: XfMemoryUniform
    registerUniform*: RegistersUniform
    registerUniformDirty* = false
    rasterStateDirty* = true
    imageStateDirty*: set[0..7]
    samplerStateDirty*: set[0..7]

    currentVertexShader, currentFragmentShader: NativeShader
    vertexShaderDirty*, fragmentShaderDirty*: bool

    samplers: array[8, NativeSampler]

    batchPrimitive: PrimitiveKind
    batchFmt: DynamicVertexFmt
    batchNumVertices: seq[int]

proc endDraw() =
    if batchNumVertices.len > 0:
#        echo &"end draw {numVertices} vertices"
        rasterogl.draw(batchPrimitive, batchNumVertices, batchFmt, curVertexBuffer.data)
        batchNumVertices.setLen(0)
        curVertexBuffer.clear()

proc finishFrame*() =
    discard

proc retrieveFrame*(data: var openArray[uint32], x, y, width, height: uint32) =
    endDraw()

    rasterogl.retrieveFrame(data, x, y, width, height)


proc copyEfb*(framebuffer: NativeFramebuffer, width, height: int, offsetX, offsetY, scaleX, scaleY: float32) =
    endDraw()

    rasterogl.copyEfb(framebuffer, width, height, offsetX, offsetY, scaleX, scaleY)
    #rasterogl.retrieveFrame(data, x, y, width, height)

proc clear*(r, g, b, a: uint8, depth: uint32, clearColor, clearAlpha, clearDepth: bool) =
    rasterogl.clear(r, g, b, a, depth, clearColor, clearAlpha, clearDepth)

import
    bp, xf,
    shadergen, texturesetup

var
    vertexShaders: Table[VertexShaderKey, NativeShader]
    fragmentShaders: Table[FragmentShaderKey, NativeShader]

proc getVertexShader(key: VertexShaderKey): NativeShader =
    vertexShaders.withValue(key, value):
        return value[]
    result = compileShader(shaderStageVertex, genVertexShader(key))
    vertexShaders[key] = result
proc getFragmentShader(key: FragmentShaderKey): NativeShader =
    fragmentShaders.withValue(key, value):
        return value[]
    result = compileShader(shaderStageFragment, genFragmentShader(key))
    fragmentShaders[key] = result

proc setupUniforms() =
    if registerUniformDirty:
        translateProj(registerUniform.projection)
        registerUniform.matIndices0 = uint32 matIdxLo
        registerUniform.matInidces1 = uint32 matIdxHi
        for i in 0..<8:
            # texture coordinates are .7 fixpoint
            # we put the factor here in to save the multiplications in the shader
            registerUniform.texcoordScale[i*2+0] = float32((sSize[i].size + 1) * 128)
            registerUniform.texcoordScale[i*2+1] = float32((tSize[i].size + 1) * 128)

            registerUniform.textureSizes[i*4+0] = float32(texMaps[i].width)
            registerUniform.textureSizes[i*4+1] = float32(texMaps[i].height)
            registerUniform.textureSizes[i*4+2] = 1f / float32(texMaps[i].width * 128)
            registerUniform.textureSizes[i*4+3] = 1f / float32(texMaps[i].height * 128)

            registerUniform.regValues[i] = uint32 tevRegister[i]
            registerUniform.konstants[i] = uint32 konstants[i]
        registerUniform.matColors[0] = uint32 ambColorsRegs[0]
        registerUniform.matColors[1] = uint32 ambColorsRegs[1]
        registerUniform.matColors[2] = uint32 matColorsRegs[0]
        registerUniform.matColors[3] = uint32 matColorsRegs[1]

        registerUniform.dualTexMatIndices0 = 0
        registerUniform.dualTexMatIndices1 = 0
        for i in 0..<4:
            registerUniform.dualTexMatIndices0 = registerUniform.dualTexMatIndices0 or
                (dualTex[i].dualMtx shl (i*8))
            registerUniform.dualTexMatIndices1 = registerUniform.dualTexMatIndices1 or
                (dualTex[i+4].dualMtx shl (i*8))

        for i in 0..<9:
            registerUniform.indMat[i] = uint32 indMat[i]
        for i in 0..<3:
            let val = (indMat[i].s or (indMat[i+3].s shl 2) or (indMat[i+6].sTop shl 4))-17
            registerUniform.indMat[i] =
                (registerUniform.indMat[i] and 0x3FFFFF'u32) or (val shl 22)
            #assert (val shl 22) == (uint32(int(val)) shl 22), &"difference {val} {(val shl 22)} {(uint32(int(val)) shl 22)}"
        for i in 0..<2:
            registerUniform.ras1ss[i] = uint32 ras1ss[i]

        registerUniform.alphaRefs = alphaCompare.ref0 or (alphaCompare.ref1 shl 8)  
        registerUniform.zenvBias = zenv0

        rasterogl.uploadRegisters(registerUniform)
        registerUniformDirty = false
    if xfMemoryDirty:
        rasterogl.uploadXfMemory(xfMemoryUniform)

        xfMemoryDirty = false

proc usedTextures(): set[0..7] =
    for i in 0..<genMode.ntev+1:
        let (texmap, _, texmapEnable, _) = ras1Tref.getRas1Tref(i)
        if texmapEnable:
            result.incl texmap
    for i in 0..<genMode.nbmp:
        result.incl ras1iref.texmap(int i)

proc setupTextures() =
    let dirtyImagesInUse = imageStateDirty * usedTextures()
    for i in dirtyImagesInUse:
        #echo &"texture dirty {i} {texMaps[i].setImage0.width} {texMaps[i].setImage0.height} {texMaps[i].setImage0.fmt} {float(texMaps[i].setMode1.minlod)/16.0} {float(texMaps[i].setMode1.maxlod)/16.0} {(texMaps[i].setImage3 shl 5):08X}"
        setupTexture(i)
        imageStateDirty.excl i
    let dirtySamplersInUse = samplerStateDirty * usedTextures()
    for i in dirtySamplersInUse:
        rasterogl.configure(samplers[i], 
            texMaps[i].setMode0.wrapS, texMaps[i].setMode0.wrapT,
            texMaps[i].setMode0.magFilter, texMaps[i].setMode0.minFilter)
        samplerStateDirty.excl i

proc getVtxShaderKey(fmt: DynamicVertexFmt): VertexShaderKey =
    result.enabledAttrs = fmt.enabledAttrs
    result.numTexcoordGen = numTexcoordGen
    result.texcoordGen = texcoordGen
    result.numColors = numColors
    result.lightCtrls = lightCtrls
    result.normalsNBT = vtxAttrNormalNBT in fmt.attrSizes
    result.enableDualTex = enableDualTex
    for i in 0..<8:
        if dualTex[i].normalise:
            result.normaliseDualTex.incl i

proc getFragShaderKey(): FragmentShaderKey =
    result.numTevStages = genMode.ntev+1
    result.numIndTevStages = genMode.nbmp
    result.indCmd = indCmd
    result.ras1iref = ras1iref
    result.colorEnv = colorEnv
    result.alphaEnv = alphaEnv
    result.ras1Tref = ras1Tref
    result.ksel = tevKSel
    result.alphaCompLogic = alphaCompare.logic
    result.alphaComp0 = alphaCompare.comp0
    result.alphaComp1 = alphaCompare.comp1
    result.zenv1 = zenv1

proc init*() =
    rasterogl.init()

    for i in 0..<8:
        samplers[i] = rasterogl.createSampler()
        rasterogl.bindSampler(i, samplers[i])

proc startDraw(kind: PrimitiveKind) =
    assert batchNumVertices.len == 0
    if rasterStateDirty:
        # TODO: clamp scissor to 264 in height when AA is enabled
        let
            (viewportX, viewportY, viewportW, viewportH, viewportNear, viewportFar) = getViewport()
            (scissorX, scissorY, scissorW, scissorH) = getScissor()
            (offsetX, offsetY) = getScissorOffset()
        #echo &"setup viewport {viewportX}, {viewportY} {viewportW}x{viewportH} {viewportNear} {viewportFar}"
        #echo &"setup scissor {scissorX}, {scissorY} {scissorW}x{scissorH} (offset: {offsetX} {offsetY})"
        rasterogl.setViewport(viewportX - float32(offsetX), viewportY - float32(offsetY), viewportW, viewportH, viewportNear, viewportFar)
        rasterogl.setScissor(true, scissorX - offsetX, scissorY - offsetY, scissorW, scissorH)
        if peCMode0.blendOp == blendSub:
            rasterogl.setBlendState(peCMode0.blendEnable, blendSub, blendFactorOne, blendFactorOne)
        else:
            rasterogl.setBlendState(peCMode0.blendEnable, blendAdd,
                case peCMode0.srcFactor
                of blendFactorSrcColor: blendFactorDstColor
                of blendFactorInvSrcColor: blendFactorInvDstColor
                else: peCMode0.srcFactor,
                peCMode0.dstFactor)
        rasterogl.setColorAlphaUpdate(peCMode0.colorUpdate, peCMode0.alphaUpdate)
        rasterogl.setCullFace(genMode.cullmode)
        rasterogl.setZMode(zmode.enable, zmode.fun, zmode.update and zmode.enable)
        rasterStateDirty = false

    setupTextures()
    setupUniforms()

    if vertexShaderDirty:
        currentVertexShader = getVertexShader(getVtxShaderKey(curVertexBuffer.curFmt))
    if fragmentShaderDirty:
        currentFragmentShader = getFragmentShader(getFragShaderKey())
    if vertexShaderDirty or fragmentShaderDirty:
        rasterogl.bindShader(currentVertexShader, currentFragmentShader)
        vertexShaderDirty = false
        fragmentShaderDirty = false

    batchPrimitive = kind
    batchFmt = curVertexBuffer.curFmt

#    echo "starting draw"

proc draw*(kind: PrimitiveKind, count: int) =
    if count > 0:
        if rasterStateDirty or
            registerUniformDirty or
            xfMemoryDirty or
            vertexShaderDirty or
            fragmentShaderDirty or
            (imageStateDirty * usedTextures()) != {} or
            (samplerStateDirty * usedTextures()) != {} or
            batchPrimitive != kind or
            batchFmt != curVertexBuffer.curFmt:
            endDraw()

            startDraw(kind)

    #    echo &"adding {count} vertices"
        batchNumVertices.add count
