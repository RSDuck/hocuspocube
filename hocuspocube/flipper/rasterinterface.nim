import
    tables, hashes,

    opengl/rasterogl,

    shadergen,

    rasterinterfacecommon

var
    curVertexBuffer*: VertexBuffer

    fragmentShader: NativeShader

type
    VertexShaderKey* = object
        enabledAttrs*: set[VertexAttrKind]

proc hash(key: VertexShaderKey): Hash =
    result = result !& hash(key.enabledAttrs)
    result = !$result

var
    vertexShaders: Table[VertexShaderKey, NativeShader]

proc getVertexShader*(key: VertexShaderKey): NativeShader =
    vertexShaders.withValue(key, value):
        return value[]

    result = compileShader(shaderStageVertex, genVertexShader(key.enabledAttrs))
    vertexShaders[key] = result

proc setXfRegisters*(registers: XfRegistersUniform) =
    uploadXfRegisters(registers)

proc setXfMemory*(memory: XfMemoryUniform) =
    uploadXfMemory(memory)

proc retrieveFrame*(data: var openArray[uint32], x, y, width, height: uint32) =
    rasterogl.retrieveFrame(data, x, y, width, height)

proc clear*(r, g, b, a: uint8, depth: uint32) =
    rasterogl.clear(r, g, b, a, depth)

proc init*() =
    rasterogl.init()

    fragmentShader = compileShader(shaderStageFragment, genFragmentShader())

proc draw*(kind: PrimitiveKind, count: int, fmt: DynamicVertexFmt) =
    rasterogl.bindShader(getVertexShader(VertexShaderKey(enabledAttrs: fmt.enabledAttrs)), fragmentShader)
    rasterogl.draw(kind, count, fmt, curVertexBuffer.data)
    curVertexBuffer.clear()