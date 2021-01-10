import
    ../rasterinterfacecommon,

    opengl

const
    VertexBufferSegments = 3
    VertexBufferSegmentSize = 1024*128
    VertexBufferSize = VertexBufferSegmentSize*VertexBufferSegments

    fullscreenQuadVtxShaderSource = """
        #version 420 core

        layout (location = 0) out vec2 outTexcoord;

        const vec4 Positions[4] = vec4[]
        (
            vec4(-1.0, -1.0, 0.5, 1.0),
            vec4(1.0, -1.0, 0.5, 1.0),
            vec4(-1.0, 1.0, 0.5, 1.0),
            vec4(1.0, 1.0, 0.5, 1.0)
        );
        const vec2 Texcoords[4] = vec2[]
        (
            vec2(0.0, 0.0),
            vec2(1.0, 0.0),
            vec2(0.0, 1.0),
            vec2(1.0, 1.0)
        );

        uniform vec2 PositionScale;
        uniform vec2 TexcoordScale;

        void main()
        {
            gl_Position = Positions[gl_VertexID] * vec4(PositionScale, 1.0, 1.0);
            outTexcoord = Texcoords[gl_VertexID];
            outTexcoord.y = (1.0 - outTexcoord.y);
            outTexcoord *= TexcoordScale;
        }
    """
    fullscreenQuadFragShaderSource = """
        #version 420 core

        layout (location = 0) in vec2 inTexcoord;

        layout (location = 0) out vec4 outColor;

        uniform sampler2D inXfb;

        void main()
        {
            outColor = texture(inXfb, inTexcoord);
        }
    """

    quadGeometryShaderSource = """
        #version 420 core

        layout (lines_adjacency) in;
        layout (triangle_strip, max_vertices = 4) out;

        layout (location = 0) in vec4 inColor0[];

        layout (location = 0) out vec4 outColor0;

        void main()
        {
            gl_Position = gl_in[1].gl_Position;
            outColor0 = inColor0[1];
            EmitVertex();
            outColor0 = inColor0[2];
            gl_Position = gl_in[2].gl_Position;
            EmitVertex();
            outColor0 = inColor0[0];
            gl_Position = gl_in[0].gl_Position;
            EmitVertex();
            outColor0 = inColor0[3];
            gl_Position = gl_in[3].gl_Position;
            EmitVertex();
        }
    """

var
    vtxBuffer, flipperVao, metaVao: GLuint

    flipperShaderPipeline, metaShaderPipeline: GLuint

    vtxBufferPtr: array[VertexBufferSegments, pointer]
    vtxBufferLocks: array[VertexBufferSegments, GLsync]
    curWriteBufferIdx = 0
    curWriteBufferOffset = 0

    xfRegisters, xfMemory: GLuint

    curBoundFormat: DynamicVertexFmt
    lastPrimitve = primitiveTriangles
    # as long as the same format is bound we must specify the offset via glDrawArrays
    formatVertexOffset: int

    efb, efbColorBuffer, efbDepthBuffer: GLuint

    quadGeometryShader: GLuint

    rawXfbTexture: GLuint

    fullscreenQuadVtxShader, fullscreenQuadFragShader: GLuint

    currentRenderstateMeta = false

type
    OGLVertexShader = ref object of NativeShader
        handle: GLuint
        xfRegistersBlockIdx, xfMemoryBlockIdx: GLuint
    OGLFragmentShader = ref object of NativeShader
        handle: GLuint

proc debugMessage(source: GLenum,
    typ: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: ptr GLchar,
    userParam: pointer) {.stdcall.} =
    echo "debug message ", cast[cstring](message)

proc internalShaderCompile(stage: ShaderStage, source: string): GLuint =
    let sourceArray = allocCStringArray([source])
    result = glCreateShaderProgramv(case stage
            of shaderStageVertex: GL_VERTEX_SHADER
            of shaderStageFragment: GL_FRAGMENT_SHADER
            of shaderStageGeometry: GL_GEOMETRY_SHADER, 1, sourceArray)
    deallocCStringArray(sourceArray)

    var linkStatus: GLint
    glGetProgramiv(result, GL_LINK_STATUS, addr linkStatus)
    if linkStatus == 0:
        var
            logLength: GLint
            log: string
        glGetProgramiv(result, GL_INFO_LOG_LENGTH, addr logLength)
        log.setLen logLength - 1
        glGetProgramInfoLog(result, GLsizei logLength, addr logLength, log)
        echo "shader compilation failed:"
        echo log
        echo "source:"
        echo source
        doAssert false

proc compileShader*(stage: ShaderStage, source: string): NativeShader =
    let shader = internalShaderCompile(stage, source)

    case stage
    of shaderStageVertex:
        let vtxShader = OGLVertexShader(handle: shader)
        vtxShader.xfRegistersBlockIdx = glGetUniformBlockIndex(shader, "xfRegisters")
        vtxShader.xfMemoryBlockIdx = glGetUniformBlockIndex(shader, "xfMemory")

        glUniformBlockBinding(shader, vtxShader.xfRegistersBlockIdx, 0)
        glUniformBlockBinding(shader, vtxShader.xfMemoryBlockIdx, 1)

        vtxShader
    of shaderStageFragment:
        OGLFragmentShader(handle: shader)
    of shaderStageGeometry:
        raise newException(ValueError, "not yet")

proc ensureRenderstate(meta: bool, force = false) =
    if currentRenderstateMeta == meta and not force:
        return

    currentRenderstateMeta = meta
    if meta:
        glBindFramebuffer(GL_FRAMEBUFFER, 0)
        glBindProgramPipeline(metaShaderPipeline)
        glBindVertexArray(metaVao)
        glDisable(GL_DEPTH_TEST)
        glDepthMask(GL_FALSE)
    else:
        glBindFramebuffer(GL_FRAMEBUFFER, efb)
        glBindProgramPipeline(flipperShaderPipeline)
        glBindVertexArray(flipperVao)
        glEnable(GL_DEPTH_TEST)
        glDepthMask(GL_TRUE)

proc init*() =
    glDebugMessageCallback(debugMessage, nil)
    glEnable(GL_DEBUG_OUTPUT)

    glGenVertexArrays(1, addr flipperVao)
    glBindVertexArray(flipperVao)

    glGenVertexArrays(1, addr metaVao)

    glGenBuffers(1, addr vtxBuffer)
    glBindBuffer(GL_ARRAY_BUFFER, vtxBuffer)
    glBufferStorage(GL_ARRAY_BUFFER, VertexBufferSize, nil,
        GL_DYNAMIC_STORAGE_BIT or GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_COHERENT_BIT)

    vtxBufferPtr[0] = glMapBufferRange(GL_ARRAY_BUFFER, 0, VertexBufferSize,
        GL_MAP_WRITE_BIT or GL_MAP_PERSISTENT_BIT or GL_MAP_COHERENT_BIT or GL_MAP_INVALIDATE_BUFFER_BIT)
    for i in 1..<VertexBufferSegments:
        vtxBufferPtr[i] = cast[pointer](cast[ByteAddress](vtxBufferPtr[0]) + i * VertexBufferSegmentSize)

    glGenProgramPipelines(1, addr flipperShaderPipeline)
    glBindProgramPipeline(flipperShaderPipeline)

    glGenBuffers(1, addr xfRegisters)
    glBindBuffer(GL_UNIFORM_BUFFER, xfRegisters)
    glBufferStorage(GL_UNIFORM_BUFFER, sizeof(XfRegistersUniform), nil, GL_DYNAMIC_STORAGE_BIT)

    glGenBuffers(1, addr xfMemory)
    glBindBuffer(GL_UNIFORM_BUFFER, xfMemory)
    glBufferStorage(GL_UNIFORM_BUFFER, sizeof(XfMemoryUniform), nil, GL_DYNAMIC_STORAGE_BIT)

    glGenTextures(1, addr rawXfbTexture)
    glBindTexture(GL_TEXTURE_2D, rawXfbTexture)
    # 1024*1024 is the theoretical maximum size of an xfb
    glTexStorage2D(GL_TEXTURE_2D, 1, GL_RGB8, 1024, 1024)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

    glGenTextures(1, addr efbColorBuffer)
    glBindTexture(GL_TEXTURE_2D, efbColorBuffer)
    glTexStorage2D(GL_TEXTURE_2D, 1, GL_RGBA8, 640, 480)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

    glGenTextures(1, addr efbDepthBuffer)
    glBindTexture(GL_TEXTURE_2D, efbDepthBuffer)
    glTexStorage2D(GL_TEXTURE_2D, 1, GL_DEPTH_COMPONENT24, 640, 480)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

    glGenFramebuffers(1, addr efb)
    glBindFramebuffer(GL_FRAMEBUFFER, efb)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, efbColorBuffer, 0)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, efbDepthBuffer, 0)
    assert glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE

    quadGeometryShader = internalShaderCompile(shaderStageGeometry, quadGeometryShaderSource)

    fullscreenQuadVtxShader = internalShaderCompile(shaderStageVertex, fullscreenQuadVtxShaderSource)
    fullscreenQuadFragShader = internalShaderCompile(shaderStageFragment, fullscreenQuadFragShaderSource)

    glGenProgramPipelines(1, addr metaShaderPipeline)
    glUseProgramStages(metaShaderPipeline, GL_VERTEX_SHADER_BIT, fullscreenQuadVtxShader)
    glUseProgramStages(metaShaderPipeline, GL_FRAGMENT_SHADER_BIT, fullscreenQuadFragShader)

    ensureRenderstate true

proc bindShader*(vertex, fragment: NativeShader) =
    glUseProgramStages(flipperShaderPipeline, GL_VERTEX_SHADER_BIT, OGLVertexShader(vertex).handle)
    glUseProgramStages(flipperShaderPipeline, GL_FRAGMENT_SHADER_BIT, OGLFragmentShader(fragment).handle)

    glBindBufferRange(GL_UNIFORM_BUFFER, 0, xfRegisters, 0, sizeof(XfRegistersUniform))
    glBindBufferRange(GL_UNIFORM_BUFFER, 1, xfMemory, 0, sizeof(XfMemoryUniform))

proc uploadXfMemory*(data: XfMemoryUniform) =
    glBindBuffer(GL_UNIFORM_BUFFER, xfMemory)
    glBufferSubData(GL_UNIFORM_BUFFER, 0, sizeof(XfMemoryUniform), unsafeAddr data)
proc uploadXfRegisters*(data: XfRegistersUniform) =
    glBindBuffer(GL_UNIFORM_BUFFER, xfRegisters)
    glBufferSubData(GL_UNIFORM_BUFFER, 0, sizeof(XfRegistersUniform), unsafeAddr data)

proc retrieveFrame*(data: var openArray[uint32], x, y, width, height: uint32) =
    ensureRenderstate false
    glReadPixels(GLint x, GLint(480 - (y + height)), GLsizei width, GLsizei height, GL_RGBA, GL_UNSIGNED_BYTE, addr data[0])

proc clear*(r, g, b, a: uint8, depth: uint32) =
    glClearDepth(float(depth) / float((1'u32 shl 24) - 1))
    glClearColor(float32(r) / 255, float32(g) / 255, float32(b) / 255, float32(a) / 255)
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

proc draw*(kind: PrimitiveKind, count: int, fmt: DynamicVertexFmt, data: openArray[byte]) =
    ensureRenderstate false

    if kind == primitiveQuads and lastPrimitve != primitiveQuads:
        glUseProgramStages(flipperShaderPipeline, GL_GEOMETRY_SHADER_BIT, quadGeometryShader)
    elif kind != primitiveQuads and lastPrimitve == primitiveQuads:
        glUseProgramStages(flipperShaderPipeline, GL_GEOMETRY_SHADER_BIT, 0)
    lastPrimitve = kind

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

        glBindVertexBuffer(0, vtxBuffer, curWriteBufferOffset + curWriteBufferIdx * VertexBufferSegmentSize, GLsizei fmt.vertexSize)

        var attribIdx = GLuint 0
        template doCoord(attr, tripple): untyped =
            glVertexAttribFormat(attribIdx, GLint(if tripple in fmt.attrSizes: 3 else: 2), cGL_FLOAT, GL_FALSE, GLuint(fmt.attrOffsets[attr]))
            glVertexAttribBinding(attribIdx, 0)
            attribIdx += 1
        template doColor(attr): untyped =
            glVertexAttribFormat(attribIdx, 4, GL_UNSIGNED_BYTE, GL_TRUE, GLuint(fmt.attrOffsets[attr]))
            glVertexAttribBinding(attribIdx, 0)
            attribIdx += 1
        doCoord(vtxAttrPosition, vtxAttrPosition3)
        doColor(vtxAttrColor0)
        doColor(vtxAttrColor1)

        curBoundFormat = fmt
        formatVertexOffset = 0

    if curWriteBufferOffset + data.len > VertexBufferSegmentSize:
        vtxBufferLocks[curWriteBufferIdx] = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, GLbitfield 0)
        curWriteBufferIdx += 1
        if curWriteBufferIdx >= 3: curWriteBufferIdx = 0
        curWriteBufferOffset = 0
        if pointer(vtxBufferLocks[curWriteBufferIdx]) != nil:
            glWaitSync(vtxBufferLocks[curWriteBufferIdx], GLbitfield 0, GL_TIMEOUT_IGNORED)
            glDeleteSync(vtxBufferLocks[curWriteBufferIdx])
        
        formatVertexOffset = 0
        glBindVertexBuffer(0, vtxBuffer, curWriteBufferOffset + curWriteBufferIdx * VertexBufferSegmentSize, GLsizei fmt.vertexSize)

    copyMem(cast[pointer](cast[ByteAddress](vtxBufferPtr[curWriteBufferIdx]) + curWriteBufferOffset), unsafeAddr data[0], data.len)
    curWriteBufferOffset += data.len

    glDrawArrays(case kind
        of primitiveTriangles: GL_TRIANGLES
        of primitiveQuads, primitiveQuads2: GL_LINES_ADJACENCY
        of primitiveTriangleStrips: GL_TRIANGLE_STRIP
        of primitiveTriangleFan: GL_TRIANGLE_FAN
        of primitivePoints: GL_POINTS
        of primitiveLines: GL_LINES
        of primitiveLineStrip: GL_LINE_STRIP,
        GLint formatVertexOffset,
        GLsizei count)

    formatVertexOffset += count

proc presentFrame*(width, height: int, pixelData: openArray[uint32]) =
    ensureRenderstate true

    glClear(GL_COLOR_BUFFER_BIT)

    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE2D, rawXfbTexture)
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, GLsizei width, GLsizei height, GL_RGBA, GL_UNSIGNED_BYTE, unsafeAddr pixelData[0])

    # urgh those uniforms neeed to be cached
    glProgramUniform2f(fullscreenQuadVtxShader, glGetUniformLocation(fullscreenQuadVtxShader, "PositionScale"), 1f, 1f)
    glProgramUniform2f(fullscreenQuadVtxShader, glGetUniformLocation(fullscreenQuadVtxShader, "TexcoordScale"), float32(width) / 1024f, float32(height) / 1024f)

    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)

proc presentBlankFrame*() =
    ensureRenderstate true

    glClear(GL_COLOR_BUFFER_BIT)