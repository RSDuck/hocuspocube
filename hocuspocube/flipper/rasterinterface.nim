import
    tables, hashes,
    opengl/rasterogl,
    rasterinterfacecommon

var
    curVertexBuffer*: VertexBuffer

    xfMemoryDirty*: bool
    xfMemoryUniform*: XfMemoryUniform
    registerUniform*: RegistersUniform
    registerUniformDirty* = false
    rasterStateDirty* = true
    imageStateDirty*: set[0..7]
    samplerStateDirty*: set[0..7]

    samplers: array[8, NativeSampler]

proc retrieveFrame*(data: var openArray[uint32], x, y, width, height: uint32) =
    rasterogl.retrieveFrame(data, x, y, width, height)

proc finishFrame*() =
    #rasterogl.dispatchBatch()
    discard

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
            registerUniform.texcoordScale[i*2+0] = float32(sSize[i].size + 1)
            registerUniform.texcoordScale[i*2+1] = float32(tSize[i].size + 1)

            registerUniform.textureSizes[i*4+0] = float32(texMaps[i].width)
            registerUniform.textureSizes[i*4+1] = float32(texMaps[i].height)
            registerUniform.textureSizes[i*4+2] = 1f / float32(texMaps[i].width)
            registerUniform.textureSizes[i*4+3] = 1f / float32(texMaps[i].height)

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

proc draw*(kind: PrimitiveKind, count: int, fmt: DynamicVertexFmt) =
    assert(genMode.nbmp == 0)

    if rasterStateDirty:
        # TODO: clamp scissor to 264 in height when AA is enabled
        let
            (viewportX, viewportY, viewportW, viewportH) = getViewport()
            (scissorX, scissorY, scissorW, scissorH) = getScissor()
            (offsetX, offsetY) = getScissorOffset()
        #echo &"setup viewport {viewportX}, {viewportY} {viewportW}x{viewportH}"
        #echo &"setup scissor {scissorX}, {scissorY} {scissorW}x{scissorH} (offset: {offsetX} {offsetY})"
        rasterogl.setViewport(int32(viewportX) - offsetX, int32(viewportY) - offsetY, int32(viewportW), int32(viewportH))
        rasterogl.setScissor(true, scissorX - offsetX, scissorY - offsetY, scissorW, scissorH)
        rasterogl.setBlendState(peCMode0.blendEnable, peCMode0.blendOp, peCMode0.srcFactor, peCMode0.dstFactor)
        rasterogl.setColorAlphaUpdate(peCMode0.colorUpdate, peCMode0.alphaUpdate)
        rasterogl.setCullFace(genMode.cullmode)
        rasterogl.setZMode(zmode.enable, zmode.fun, zmode.update and zmode.enable)
        rasterStateDirty = false

    #echo "drawing ", kind, " ", count
    setupTextures()
    setupUniforms()
    rasterogl.bindShader(getVertexShader(getVtxShaderKey(fmt)), getFragmentShader(getFragShaderKey()))
    rasterogl.draw(kind, count, fmt, curVertexBuffer.data)
    curVertexBuffer.clear()