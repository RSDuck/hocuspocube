import
    std/setutils, math, strformat,
    ../rasterinterfacecommon, ../bpcommon,

    opengl

const
    VertexBufferSegments = 3
    VertexBufferSegmentSize = 1024*1024
    VertexBufferSize = VertexBufferSegmentSize*VertexBufferSegments

    fullscreenQuadVtxShaderSource = """
#version 430 core

layout (location = 0) out vec2 outTexcoord;

const vec2 Coords[4] = vec2[]
(
    vec2(0.0, 0.0),
    vec2(1.0, 0.0),
    vec2(0.0, 1.0),
    vec2(1.0, 1.0)
);

layout (location = 0) uniform vec2 PositionScale;
layout (location = 1) uniform vec2 PositionOffset;
layout (location = 2) uniform vec2 TexcoordScale;
layout (location = 3) uniform vec2 TexcoordOffset;

out gl_PerVertex
{
vec4 gl_Position;
float gl_PointSize;
float gl_ClipDistance[];
};

void main()
{
    gl_Position = vec4((Coords[gl_VertexID] * PositionScale + PositionOffset) * 2.0 - vec2(1.0, 1.0), 0.5, 1.0);
    outTexcoord = Coords[gl_VertexID];
    outTexcoord *= TexcoordScale;
    outTexcoord += TexcoordOffset;
}
    """
    fullscreenQuadFragShaderSource = """
#version 430 core

layout (location = 0) in vec2 inTexcoord;

layout (location = 0) out vec4 outColor;

layout (binding = 0) uniform sampler2D inXfb;

void main()
{
    outColor = texture(inXfb, inTexcoord);
}
    """
    copyEfbShaderSource = """
#version 430 core

layout (location = 0) in vec2 inTexcoord;

layout (location = 0) out vec4 outColor;

layout (location = 0) uniform sampler2D inEfb;

void main()
{
    outColor = texture(inEfb, inTexcoord);
}
    """

const
    fullscreenQuadVtxShaderUniformPositionScale = 0
    fullscreenQuadVtxShaderUniformPositionOffset = 1
    fullscreenQuadVtxShaderUniformTexcoordScale = 2
    fullscreenQuadVtxShaderUniformTexcoordOffset = 3

type
    OGLVertexShader = ref object of NativeShader
        handle: GLuint
    OGLFragmentShader = ref object of NativeShader
        handle: GLuint
    OGLGeometryShader = ref object of NativeShader
        handle: GLuint

    OGLTexture = ref object of NativeTexture
        handle: GLuint

    OGLSampler = ref object of NativeSampler
        handle: GLuint

    OGLFramebuffer = ref object of NativeFramebuffer
        handle: GLuint

    RenderStateEnable = enum
        enableDepthTest
        enableDepthWrite
        enableScissor
        enableCulling
        enableBlending
        enableColorWrite
        enableAlphaWrite

    RenderState = object
        textures: array[8, NativeTexture]
        samplers: array[8, NativeSampler]
        textureUnit: int32
        enable: set[RenderStateEnable]
        depthFunc: CompareFunction
        vertexShader, fragmentShader, geometryShader: NativeShader
        framebuffer: NativeFramebuffer
        viewport: (float32, float32, float32, float32)
        depthRange: (float32, float32)
        scissorBox: (int32, int32, int32, int32)
        blendOp: BlendOp
        blendSrcFactor, blendDstFactor: BlendFactor
        cullface: CullFace

var
    vtxBuffer, idxBuffer, flipperVao, metaVao: GLuint

    shaderPipeline: GLuint

    vtxBufferPtr: pointer
    vtxBufferLocks: array[VertexBufferSegments, GLsync]
    curVtxBufferIdx = 0
    curVtxBufferOffset = 0

    idxBufferPtr: pointer
    idxBufferLocks: array[VertexBufferSegments, GLsync]
    curIdxBufferIdx = 0
    curIdxBufferOffset = 0

    registerUniform, xfMemory: GLuint

    curBoundFormat: DynamicVertexFmt
    # as long as the same format is bound we must specify the offset via glDrawArrays
    formatVertexOffset: int

    efb: NativeFramebuffer
    rawXfbTexture: NativeTexture

    copyEfbShader: NativeShader
    fullscreenQuadVtxShader, fullscreenQuadFragShader: NativeShader

    currentRenderstate: RenderState

    flipperRenderstate: RenderState
    clearRenderstate: RenderState
    metaRenderstate: RenderState
    copyEfbRenderstate: RenderState


proc editTexture(texture: NativeTexture) =
    for i in 0..<8:
        if currentRenderstate.textures[i] == texture:
            if currentRenderstate.textureUnit != i:
                glActiveTexture(cast[GLenum](cast[int](GL_TEXTURE0)+i))
                currentRenderstate.textureUnit = int32(i)
            return
    currentRenderstate.textures[currentRenderstate.textureUnit] = texture
    glBindTexture(GL_TEXTURE_2D, OGLTexture(texture).handle)

proc createTexture*(width, height, miplevels: int, fmt: TextureFormat): NativeTexture =
    let texture = OGLTexture(
        width: width, height: height,
        miplevels: miplevels,
        fmt: fmt)

    glGenTextures(1, addr texture.handle)
    editTexture(texture)

    const translateFmt: array[TextureFormat, GLenum] = [
        GL_R8,
        GL_R8,
        GL_RG8,
        GL_RGBA8,
        GL_RGB5_A1,
        GL_RGB565,
        GL_DEPTH_COMPONENT24]
    glTexStorage2D(GL_TEXTURE_2D, GLsizei miplevels, translateFmt[fmt], GLsizei width, GLsizei height)

    if fmt in {texfmtI8, texfmtL8, texfmtIA8}:
        let
            swizzleI8 = [GLint(GL_RED), GLint(GL_RED), GLint(GL_RED), GLint(GL_RED)]
            swizzleIA8 = [GLint(GL_RED), GLint(GL_RED), GLint(GL_RED), GLint(GL_GREEN)]
            swizzleL8 = [GLint(GL_RED), GLint(GL_RED), GLint(GL_RED), GLint(GL_ONE)]
            swizzleRGB5A1 = [GLint(GL_BLUE), GLint(GL_GREEN), GLint(GL_RED), GLint(GL_ALPHA)]
        glTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_SWIZZLE_RGBA,
            case fmt
            of texfmtI8: unsafeAddr(swizzleI8[0])
            of texfmtIA8: unsafeAddr(swizzleIA8[0])
            of texfmtL8: unsafeAddr(swizzleL8[0])
            of texfmtRGB5A1: unsafeAddr(swizzleRGB5A1[0])
            else: raiseAssert("blah"))

    # we still need a better way to define those properties
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)

    texture

proc uploadTexture*(texture: NativeTexture, x, y, level, w, h, stride: int, data: pointer) =
    editTexture(texture)
    const
        format: array[TextureFormat, GLenum] = [
            GL_RED,
            GL_RED,
            GL_RG,
            GL_RGBA,
            GL_RGBA,
            GL_RGB,
            GL_DEPTH_COMPONENT]
        typ: array[TextureFormat, GLenum] = [
            GL_UNSIGNED_BYTE,
            GL_UNSIGNED_BYTE,
            GL_UNSIGNED_BYTE,
            GL_UNSIGNED_BYTE,
            GL_UNSIGNED_SHORT_1_5_5_5_REV,
            GL_UNSIGNED_SHORT_5_6_5,
            GL_UNSIGNED_SHORT]
    glPixelStorei(GL_UNPACK_ROW_LENGTH, GLint stride)
    glTexSubImage2D(GL_TEXTURE_2D, GLint(level), GLint(x), GLint(y),
        GLsizei(w), GLsizei(h),
        format[texture.fmt], typ[texture.fmt],
        data)

proc destroy*(texture: NativeTexture) =
    glDeleteTextures(1, addr OGLTexture(texture).handle)

proc configure*(sampler: NativeSampler, wrapS, wrapT: TextureWrapMode, magFilter: TextureMagFilter, minFilter: TextureMinFilter, force = false) =
    let sampler = OGLSampler(sampler)

    const
        translateWrapMode: array[TextureWrapMode, GLint] = [
            GL_CLAMP_TO_EDGE, GL_REPEAT, GL_MIRRORED_REPEAT, GL_CLAMP_TO_EDGE]
        translateMagFilter: array[TextureMagFilter, GLint] = [
            GL_NEAREST, GL_LINEAR]
        translateMinFilter: array[TextureMinFilter, GLint] = [
            GL_NEAREST, GL_NEAREST, GL_NEAREST, # as we currently only upload the first mip level we can't enable mip mapping yet
            GL_NEAREST,
            GL_LINEAR, GL_LINEAR, GL_LINEAR,
            GL_LINEAR
        ]

    if sampler.wrapS != wrapS or force:
        glSamplerParameteri(sampler.handle, GL_TEXTURE_WRAP_S, translateWrapMode[wrapS])
        sampler.wrapS = wrapS

    if sampler.wrapT != wrapT or force:
        glSamplerParameteri(sampler.handle, GL_TEXTURE_WRAP_T, translateWrapMode[wrapT])
        sampler.wrapT = wrapT

    if sampler.magFilter != magFilter or force:
        glSamplerParameteri(sampler.handle, GL_TEXTURE_MAG_FILTER, translateMagFilter[magFilter])
        sampler.magFilter = magFilter

    if sampler.minFilter != minFilter or force:
        glSamplerParameteri(sampler.handle, GL_TEXTURE_MIN_FILTER, translateMinFilter[minFilter])
        sampler.minFilter = minFilter
    
proc createSampler*(): NativeSampler =
    let sampler = OGLSampler()
    glGenSamplers(1, addr sampler.handle)
    sampler.configure(sampler.wrapS, sampler.wrapT, sampler.magFilter, sampler.minFilter, true)
    sampler

proc createFramebuffer*(width, height: int, depth: bool): NativeFramebuffer =
    let framebuffer = OGLFramebuffer()

    glGenFramebuffers(1, addr framebuffer.handle)
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer.handle)

    framebuffer.colorbuffer = createTexture(width, height, 1, texfmtRGBA8)

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, OGLTexture(framebuffer.colorbuffer).handle, 0)
    if depth:
        framebuffer.depthbuffer = createTexture(width, height, 1, texfmtDepth24)
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, OGLTexture(framebuffer.depthbuffer).handle, 0)

    assert glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE

    currentRenderstate.framebuffer = framebuffer

    framebuffer

proc destroy*(fb: NativeFramebuffer) =
    let fb = OGLFramebuffer(fb)
    glDeleteFramebuffers(1, addr fb.handle)
    fb.colorbuffer.destroy()
    if fb.depthbuffer != nil: fb.depthbuffer.destroy()

proc debugMessage(source: GLenum,
    typ: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: ptr GLchar,
    userParam: pointer) {.stdcall.} =
    echo "debug message ", cast[cstring](message)

proc compileShader*(stage: ShaderStage, source: string): NativeShader =
    let
        sourceArray = allocCStringArray([source])
        shader = glCreateShaderProgramv(case stage
            of shaderStageVertex: GL_VERTEX_SHADER
            of shaderStageFragment: GL_FRAGMENT_SHADER
            of shaderStageGeometry: GL_GEOMETRY_SHADER, 1, sourceArray)
    deallocCStringArray(sourceArray)

    var linkStatus: GLint
    glGetProgramiv(shader, GL_LINK_STATUS, addr linkStatus)
    if linkStatus == 0:
        var
            logLength: GLint
            log: string
        glGetProgramiv(shader, GL_INFO_LOG_LENGTH, addr logLength)
        log.setLen logLength - 1
        glGetProgramInfoLog(shader, GLsizei logLength, addr logLength, log)
        echo "shader compilation failed:"
        echo log
        echo "source:"
        echo source
        doAssert false

    case stage
    of shaderStageVertex:
        OGLVertexShader(handle: shader)
    of shaderStageFragment:
        OGLFragmentShader(handle: shader)
    of shaderStageGeometry:
        OGLGeometryShader(handle: shader)

proc applyRenderstate(state: RenderState, textureUnits = 8, framebufferOnly = false) =
    let
        toEnable = state.enable - currentRenderstate.enable
        toDisable = currentRenderstate.enable - state.enable
        enableDirty = toEnable + toDisable

    if enableDepthWrite in enableDirty:
        glDepthMask(if enableDepthWrite in state.enable: GL_TRUE else: GL_FALSE)
        currentRenderstate.enable[enableDepthWrite] = enableDepthWrite in state.enable

    if (enableDirty * {enableColorWrite, enableAlphaWrite}) != {}:
        let
            colorWrite =
                if enableColorWrite in state.enable: GL_TRUE else: GL_FALSE
            alphaWrite =
                if enableAlphaWrite in state.enable: GL_TRUE else: GL_FALSE
        glColorMask(colorWrite, colorWrite, colorWrite, alphaWrite)
        currentRenderstate.enable[enableColorWrite] = enableColorWrite in state.enable
        currentRenderstate.enable[enableAlphaWrite] = enableAlphaWrite in state.enable

    if enableScissor in toEnable:
        glEnable(GL_SCISSOR_TEST)
        currentRenderstate.enable.incl enableScissor
    if enableScissor in toDisable:
        glDisable(GL_SCISSOR_TEST)
        currentRenderstate.enable.excl enableScissor

    if enableScissor in currentRenderstate.enable and state.scissorBox != currentRenderstate.scissorBox:
        let (x, y, w, h) = state.scissorBox
        glScissor(x, y, w, h)
        currentRenderstate.scissorBox = state.scissorBox

    if state.framebuffer != currentRenderstate.framebuffer:
        glBindFramebuffer(GL_FRAMEBUFFER,
            if state.framebuffer != nil: OGLFramebuffer(state.framebuffer).handle else: 0)
        currentRenderstate.framebuffer = state.framebuffer

    if not framebufferOnly:
        if enableDepthTest in toEnable:
            glEnable(GL_DEPTH_TEST)
        if enableCulling in toEnable:
            glEnable(GL_CULL_FACE)
        if enableBlending in toEnable:
            glEnable(GL_BLEND)

        if enableDepthTest in toDisable:
            glDisable(GL_DEPTH_TEST)
        if enableCulling in toDisable:
            glDisable(GL_CULL_FACE)
        if enableBlending in toDisable:
            glDisable(GL_BLEND)
        currentRenderstate.enable = state.enable

        if enableCulling in currentRenderstate.enable and state.cullface != currentRenderstate.cullface:
            const translateCulling: array[CullFace, GLenum] = [
                GL_FRONT_AND_BACK, # should never appear
                GL_FRONT,
                GL_BACK,
                GL_FRONT_AND_BACK]
            glCullFace(translateCulling[state.cullface])
            currentRenderstate.cullface = state.cullface

        if enableDepthTest in currentRenderstate.enable and state.depthFunc != currentRenderstate.depthFunc:            
            const translateFunc: array[CompareFunction, GLenum] = [
                GL_NEVER,
                GL_LESS,
                GL_EQUAL,
                GL_LEQUAL,
                GL_GREATER,
                GL_NOTEQUAL,
                GL_GEQUAL,
                GL_ALWAYS]
            glDepthFunc(translateFunc[state.depthFunc])
            currentRenderstate.depthFunc = state.depthFunc

        if state.vertexShader != currentRenderstate.vertexShader:
            glUseProgramStages(shaderPipeline, GL_VERTEX_SHADER_BIT, OGLVertexShader(state.vertexShader).handle)
            currentRenderstate.vertexShader = state.vertexShader
        if state.fragmentShader != currentRenderstate.fragmentShader:
            glUseProgramStages(shaderPipeline, GL_FRAGMENT_SHADER_BIT, OGLFragmentShader(state.fragmentShader).handle)
            currentRenderstate.fragmentShader = state.fragmentShader
        if state.geometryShader != currentRenderstate.geometryShader:
            glUseProgramStages(shaderPipeline, GL_GEOMETRY_SHADER_BIT,
                if state.geometryShader != nil: OGLGeometryShader(state.geometryShader).handle else: 0)
            currentRenderstate.geometryShader = state.geometryShader
        
        if state.viewport != currentRenderstate.viewport:
            let (x, y, w, h) = state.viewport
            glViewportIndexedf(0, x, y, w, h)
            currentRenderstate.viewport = state.viewport
        if state.depthRange != currentRenderstate.depthRange:
            let (near, far) = state.depthRange
            glDepthRangef(near, far)
            currentRenderstate.depthRange = state.depthRange

        for i in 0..<textureUnits:
            if state.textures[i] != currentRenderstate.textures[i] and state.textures[i] != nil:
                glBindTextureUnit(GLuint(i), OGLTexture(state.textures[i]).handle)
                currentRenderstate.textures[i] = state.textures[i]
        for i in 0..<textureUnits:
            if state.samplers[i] != currentRenderstate.samplers[i] and state.textures[i] != nil:
                glBindSampler(GLuint(i), if state.samplers[i] != nil: OGLSampler(state.samplers[i]).handle else: 0)
                currentRenderstate.samplers[i] = state.samplers[i]

        if enableBlending in currentRenderstate.enable:
            if state.blendOp != currentRenderstate.blendOp:
                const translateEquation: array[BlendOp, GLenum] = [
                    GL_FUNC_ADD,
                    GL_FUNC_SUBTRACT]
                glBlendEquation(translateEquation[currentRenderstate.blendOp])

                currentRenderstate.blendOp = state.blendOp
            if state.blendSrcFactor != currentRenderstate.blendSrcFactor or
                state.blendDstFactor != currentRenderstate.blendDstFactor:
                
                const translateFactor: array[BlendFactor, GLenum] = [
                    GL_ZERO,
                    GL_ONE,
                    GL_SRC_COLOR,
                    GL_ONE_MINUS_SRC_COLOR,
                    GL_SRC_ALPHA,
                    GL_ONE_MINUS_SRC_ALPHA,
                    GL_DST_ALPHA,
                    GL_ONE_MINUS_DST_ALPHA,
                    GL_DST_COLOR,
                    GL_ONE_MINUS_DST_COLOR]

                glBlendFunc(translateFactor[state.blendSrcFactor], translateFactor[state.blendDstFactor])
                currentRenderstate.blendSrcFactor = state.blendSrcFactor
                currentRenderstate.blendDstFactor = state.blendDstFactor

proc init*() =
    #glDebugMessageCallback(debugMessage, nil)
    #glEnable(GL_DEBUG_OUTPUT)

    glEnable(GL_PRIMITIVE_RESTART)
    glPrimitiveRestartIndex(0xFFFFFFFF'u32)

    glClipControl(GL_LOWER_LEFT, GL_ZERO_TO_ONE)

    glGenVertexArrays(1, addr flipperVao)
    glBindVertexArray(flipperVao)

    glGenVertexArrays(1, addr metaVao)

    glGenBuffers(1, addr vtxBuffer)
    glBindBuffer(GL_ARRAY_BUFFER, vtxBuffer)
    glBufferStorage(GL_ARRAY_BUFFER, VertexBufferSize, nil,
        GL_DYNAMIC_STORAGE_BIT or GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_COHERENT_BIT)

    vtxBufferPtr = glMapBufferRange(GL_ARRAY_BUFFER, 0, VertexBufferSize,
        GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_COHERENT_BIT or GL_MAP_INVALIDATE_BUFFER_BIT)

    glGenBuffers(1, addr idxBuffer)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, idxBuffer)
    glBufferStorage(GL_ELEMENT_ARRAY_BUFFER, VertexBufferSize, nil,
        GL_DYNAMIC_STORAGE_BIT or GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_COHERENT_BIT)

    idxBufferPtr = glMapBufferRange(GL_ELEMENT_ARRAY_BUFFER, 0, VertexBufferSize,
        GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_COHERENT_BIT or GL_MAP_INVALIDATE_BUFFER_BIT)

    glGenProgramPipelines(1, addr shaderPipeline)
    glBindProgramPipeline(shaderPipeline)

    glGenBuffers(1, addr registerUniform)
    glBindBuffer(GL_UNIFORM_BUFFER, registerUniform)
    glBufferStorage(GL_UNIFORM_BUFFER, sizeof(RegistersUniform), nil, GL_DYNAMIC_STORAGE_BIT)

    glGenBuffers(1, addr xfMemory)
    glBindBuffer(GL_UNIFORM_BUFFER, xfMemory)
    glBufferStorage(GL_UNIFORM_BUFFER, sizeof(XfMemoryUniform), nil, GL_DYNAMIC_STORAGE_BIT)

    efb = createFramebuffer(640, 528, true)

    rawXfbTexture = createTexture(1024, 1024, 1, texfmtRGBA8)

    fullscreenQuadVtxShader = compileShader(shaderStageVertex, fullscreenQuadVtxShaderSource)
    fullscreenQuadFragShader = compileShader(shaderStageFragment, fullscreenQuadFragShaderSource)

    copyEfbShader = compileShader(shaderStageFragment, copyEfbShaderSource)

    metaRenderstate = RenderState(
        vertexShader: fullscreenQuadVtxShader,
        fragmentShader: fullscreenQuadFragShader,
        viewport: (0f, 0f, 640f, 480f),
        depthRange: (0f, 1f),
        enable: {enableColorWrite, enableDepthWrite}) # bah
    metaRenderstate.textures[0] = rawXfbTexture

    flipperRenderstate = RenderState(
        viewport: (0f, 0f, 640f, 528f),
        framebuffer: efb)

    clearRenderstate = RenderState(framebuffer: efb)

    copyEfbRenderstate = RenderState(
        vertexShader: fullscreenQuadVtxShader,
        fragmentShader: copyEfbShader,
        enable: {enableColorWrite})

proc bindShader*(vertex, fragment: NativeShader) =
    flipperRenderstate.vertexShader = vertex
    flipperRenderstate.fragmentShader = fragment

    glBindBufferRange(GL_UNIFORM_BUFFER, 0, registerUniform, 0, sizeof(RegistersUniform))
    glBindBufferRange(GL_UNIFORM_BUFFER, 1, xfMemory, 0, sizeof(XfMemoryUniform))

proc uploadXfMemory*(data: XfMemoryUniform) =
    glBindBuffer(GL_UNIFORM_BUFFER, xfMemory)
    glBufferSubData(GL_UNIFORM_BUFFER, 0, sizeof(XfMemoryUniform), unsafeAddr data)
proc uploadRegisters*(data: RegistersUniform) =
    glBindBuffer(GL_UNIFORM_BUFFER, registerUniform)
    glBufferSubData(GL_UNIFORM_BUFFER, 0, sizeof(RegistersUniform), unsafeAddr data)

proc retrieveFrame*(data: var openArray[uint32], x, y, width, height: uint32) =
    applyRenderstate flipperRenderstate, framebufferOnly = true
    glReadPixels(GLint x, GLint(528 - (y + height)), GLsizei width, GLsizei height, GL_RGBA, GL_UNSIGNED_BYTE, addr data[0])

proc copyEfb*(framebuffer: NativeFramebuffer, dstWidth, dstHeight: int, offsetX, offsetY, srcWidth, srcHeight: float32) =
    copyEfbRenderstate.framebuffer = framebuffer
    copyEfbRenderstate.viewport = (0f, 0f, float32 dstWidth, float32 dstHeight)
    copyEfbRenderstate.textures[0] = efb.colorbuffer
    applyRenderstate copyEfbRenderstate, textureUnits = 1
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformPositionScale, 1f, 1f)
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformPositionOffset, 0f, 0f)
    let
        scaleX = srcWidth / 640f
        scaleY = srcHeight / 528f
        offsetX = offsetX / 640f
        offsetY = 1f - (offsetY / 528f + scaleY)
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformTexcoordScale, scaleX, scaleY)
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformTexcoordOffset, offsetX, offsetY)

    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)

proc clear*(r, g, b, a: uint8, depth: uint32, clearColor, clearAlpha, clearDepth: bool) =
    clearRenderstate.enable[enableColorWrite] = clearColor
    clearRenderstate.enable[enableAlphaWrite] = clearAlpha
    if clearDepth: clearRenderstate.enable.incl enableDepthWrite
    applyRenderstate clearRenderstate, framebufferOnly = true
    glClearDepth(float(depth) / float((1'u32 shl 24) - 1))
    glClearColor(float32(r) / 255, float32(g) / 255, float32(b) / 255, float32(a) / 255)
    var mask: GLbitfield
    if clearColor or clearAlpha: mask = GL_COLOR_BUFFER_BIT
    if clearDepth: mask = mask or GL_DEPTH_BUFFER_BIT
    glClear(mask)

proc setViewport*(x, y, w, h, near, far: float32) =
    flipperRenderstate.viewport = (x, 528f - (y + h), w, h)
    flipperRenderstate.depthRange = (near, far)
proc setScissor*(enable: bool, x, y, w, h: int32) =
    flipperRenderstate.scissorBox = (x, 528 - (y + h), w, h)
    flipperRenderstate.enable[enableScissor] = enable
proc setZMode*(enable: bool, fun: CompareFunction, update: bool) =
    flipperRenderstate.depthFunc = fun
    flipperRenderstate.enable[enableDepthTest] = enable
    flipperRenderstate.enable[enableDepthWrite] = update
proc bindTexture*(unit: int, texture: NativeTexture) =
    flipperRenderstate.textures[unit] = texture
proc setBlendState*(enable: bool, op: BlendOp, srcFactor, dstFactor: BlendFactor) =
    flipperRenderstate.enable[enableBlending] = enable
    flipperRenderstate.blendOp = op
    flipperRenderstate.blendSrcFactor = srcFactor
    flipperRenderstate.blendDstFactor = dstFactor
proc setCullFace*(mode: CullFace) =
    if mode == cullNone:
        flipperRenderstate.enable.excl enableCulling
    else:
        flipperRenderstate.enable.incl enableCulling
        flipperRenderstate.cullface = mode
proc bindSampler*(i: int, sampler: NativeSampler) =
    flipperRenderstate.samplers[i] = sampler
proc setColorAlphaUpdate*(updateColor, updateAlpha: bool) =
    flipperRenderstate.enable[enableColorWrite] = updateColor
    flipperRenderstate.enable[enableAlphaWrite] = updateAlpha

proc draw*(kind: PrimitiveKind, counts: seq[int], fmt: DynamicVertexFmt, data: openArray[byte]) =
    applyRenderstate(flipperRenderstate)

    if fmt != curBoundFormat:
        let
            numEnabled = curBoundFormat.enabledAttrs.card
            numToEnable = fmt.enabledAttrs.card
        if numEnabled > numToEnable:
            for i in numToEnable..<numEnabled:
                glDisableVertexAttribArray(GLuint i)
        elif numEnabled < numToEnable:
            for i in numEnabled..<numToEnable:
                glEnableVertexAttribArray(GLuint i)

        glBindVertexBuffer(0, vtxBuffer, curVtxBufferOffset + curVtxBufferIdx * VertexBufferSegmentSize, GLsizei fmt.vertexSize)

        var attribIdx = GLuint 0
        template doIdx(attr): untyped =
            if attr in fmt.enabledAttrs:
                glVertexAttribIFormat(attribIdx, 1, GL_UNSIGNED_BYTE, GLuint(fmt.attrOffsets[attr]))
                glVertexAttribBinding(attribIdx, 0)
                attribIdx += 1
        template doCoord(attr, tripple): untyped =
            glVertexAttribFormat(attribIdx, GLint(if tripple in fmt.attrSizes: 3 else: 2), cGL_FLOAT, GL_FALSE, GLuint(fmt.attrOffsets[attr]))
            glVertexAttribBinding(attribIdx, 0)
            attribIdx += 1
        template doColor(attr): untyped =
            if attr in fmt.enabledAttrs:
                glVertexAttribFormat(attribIdx, 4, GL_UNSIGNED_BYTE, GL_TRUE, GLuint(fmt.attrOffsets[attr]))
                glVertexAttribBinding(attribIdx, 0)
                attribIdx += 1
        template doTexcoord(attr, double): untyped =
            if attr in fmt.enabledAttrs:
                glVertexAttribFormat(attribIdx, GLint(if double in fmt.attrSizes: 2 else: 1), cGL_FLOAT, GL_FALSE, GLuint(fmt.attrOffsets[attr]))
                glVertexAttribBinding(attribIdx, 0)
                attribIdx += 1
        doCoord(vtxAttrPosition, vtxAttrPosition3)
        doIdx(vtxAttrPosNrmMat)
        for i in 0..<8:
            doIdx(vtxAttrTexMat0.succ(i))

        if vtxAttrNormal in fmt.enabledAttrs:
            glVertexAttribFormat(attribIdx, 3, cGL_FLOAT, GL_FALSE, GLuint(fmt.attrOffsets[vtxAttrNormal]))
            glVertexAttribBinding(attribIdx, 0)
            attribIdx += 1
            if vtxAttrNormalNBT in fmt.attrSizes:
                glVertexAttribFormat(attribIdx, 3, cGL_FLOAT, GL_FALSE, GLuint(fmt.attrOffsets[vtxAttrNormal] + 3*4))
                glVertexAttribBinding(attribIdx, 0)
                attribIdx += 1
                glVertexAttribFormat(attribIdx, 3, cGL_FLOAT, GL_FALSE, GLuint(fmt.attrOffsets[vtxAttrNormal] + 6*4))
                glVertexAttribBinding(attribIdx, 0)
                attribIdx += 1
        for i in 0..<2:
            doColor(vtxAttrColor0.succ(i))
        for i in 0..<8:
            doTexcoord(vtxAttrTexCoord0.succ(i), vtxAttrTexCoord0ST.succ(i))

        curBoundFormat = fmt
        formatVertexOffset = 0

    if curVtxBufferOffset + data.len > VertexBufferSegmentSize:
        vtxBufferLocks[curVtxBufferIdx] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, GLbitfield 0)
        curVtxBufferIdx += 1
        if curVtxBufferIdx >= VertexBufferSegments: curVtxBufferIdx = 0
        curVtxBufferOffset = 0
        if pointer(vtxBufferLocks[curVtxBufferIdx]) != nil:
            discard glClientWaitSync(vtxBufferLocks[curVtxBufferIdx], GL_SYNC_FLUSH_COMMANDS_BIT, GL_TIMEOUT_IGNORED)
            glDeleteSync(vtxBufferLocks[curVtxBufferIdx])
            vtxBufferLocks[curVtxBufferIdx] = nil

        formatVertexOffset = 0
        glBindVertexBuffer(0, vtxBuffer, curVtxBufferOffset + curVtxBufferIdx * VertexBufferSegmentSize, GLsizei fmt.vertexSize)

    assert data.len > 0, &"{counts} {data.len}"
    copyMem(cast[pointer](cast[ByteAddress](vtxBufferPtr) + VertexBufferSegmentSize * curVtxBufferIdx + curVtxBufferOffset),
        unsafeAddr data[0],
        data.len)
    curVtxBufferOffset += data.len

    let oglPrim = case kind
        of primitiveTriangles: GL_TRIANGLES
        of primitiveTriangleStrips, primitiveQuads, primitiveQuads2: GL_TRIANGLE_STRIP
        of primitiveTriangleFan: GL_TRIANGLE_FAN
        of primitivePoints: GL_POINTS
        of primitiveLines: GL_LINES
        of primitiveLineStrip: GL_LINE_STRIP

    let totalCount = sum(counts)
    if kind in {primitiveQuads, primitiveQuads2} or (kind in {primitiveTriangleFan, primitiveTriangleStrips, primitiveLineStrip} and counts.len > 1):
        let indicesCount =
                if kind in {primitiveQuads, primitiveQuads2}:
                    (totalCount div 4) * 5
                else:
                    totalCount + counts.len

        if curIdxBufferOffset + indicesCount*4 > VertexBufferSegmentSize:
            idxBufferLocks[curIdxBufferIdx] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, GLbitfield 0)
            curIdxBufferIdx += 1
            if curIdxBufferIdx >= VertexBufferSegments: curIdxBufferIdx = 0
            curIdxBufferOffset = 0
            if pointer(idxBufferLocks[curIdxBufferIdx]) != nil:
                discard glClientWaitSync(idxBufferLocks[curIdxBufferIdx], GL_SYNC_FLUSH_COMMANDS_BIT, GL_TIMEOUT_IGNORED)
                glDeleteSync(idxBufferLocks[curIdxBufferIdx])
                idxBufferLocks[curIdxBufferIdx] = nil

        let
            indexBufferOffset = VertexBufferSegmentSize * curIdxBufferIdx + curIdxBufferOffset
            indicesPtr = cast[ptr UncheckedArray[uint32]](cast[ByteAddress](idxBufferPtr) + indexBufferOffset)
        if kind in {primitiveQuads, primitiveQuads2}:
            generateQuadIndices(indicesPtr, 0, totalCount)
        else:
            generateSeparatingIndices(indicesPtr, 0, counts)

        curIdxBufferOffset += indicesCount*4

        glDrawElementsBaseVertex(oglPrim, GLsizei indicesCount, GL_UNSIGNED_INT, cast[pointer](indexBufferOffset), GLint formatVertexOffset)
    else:
        glDrawArrays(oglPrim, GLint formatVertexOffset, GLsizei totalCount)

    formatVertexOffset += totalCount

proc presentFrame*(width, height: int, pixelData: openArray[uint32]) =
    uploadTexture(rawXfbTexture, 0, 0, 0, width, height, width, unsafeAddr pixelData[0])

    metaRenderstate.textures[0] = rawXfbTexture
    applyRenderstate metaRenderstate, textureUnits = 1

    glClear(GL_COLOR_BUFFER_BIT)

    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformPositionScale, 1f, 1f)
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformPositionOffset, 0f, 0f)
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformTexcoordScale, float32(width) / 1024f, -float32(height) / 1024f)
    glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformTexcoordOffset, 0f, float32(height) / 1024f)

    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)

proc presentFrame*(totalLines: int, textures: seq[(int, int, NativeTexture)]) =
    applyRenderstate metaRenderstate, framebufferOnly = true

    glClear(GL_COLOR_BUFFER_BIT)

    for (y, height, texture) in textures:
        metaRenderstate.textures[0] = texture
        applyRenderstate metaRenderstate, textureUnits = 1

        let
            normHeight = float32(height) / float32(totalLines)
            normY = 1f - (float32(y) / float32(totalLines) + normHeight)

        glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformPositionScale, 1f, normHeight)
        glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformPositionOffset, 0f, normY)
        glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformTexcoordScale, 1f, 1f)
        glProgramUniform2f(OGLVertexShader(fullscreenQuadVtxShader).handle, fullscreenQuadVtxShaderUniformTexcoordOffset, 0f, 0f)

        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)

proc presentBlankFrame*() =
    applyRenderstate metaRenderstate, framebufferOnly = true

    glClearColor(0f, 0f, 0f, 1f)
    glClear(GL_COLOR_BUFFER_BIT)