import
    strformat,
    rasterinterfacecommon

template line(str: string) =
    result &= str
    result &= '\n'

proc genVertexShader*(attrs: set[VertexAttrKind]): string =
    line "#version 420 core"

    block:
        line "layout (location = 0) in vec3 inPosition;"
        var location = 1
        if vtxAttrColor0 in attrs:
            line &"layout (location = {location}) in vec4 inColor0;"
            inc location
        if vtxAttrColor1 in attrs:
            line &"layout (location = {location}) in vec4 inColor1;"
            inc location

    line "layout (location = 0) out vec4 outColor0;"

    line "layout (std140) uniform xfRegisters {"
    line "mat4 Projection;"
    line "uvec4 DefaultMats;"
    line "};"

    line "layout (std140) uniform xfMemory {"
    line "vec4 PosTexMats[64];"
    line "vec4 NrmMats[32];" # stupid padding
    line "vec4 PostTexMats[64];"
    line "};"

    line "void main() {"

    line "uint pnmatIdx = DefaultMats.x;"
    line "vec4 position4 = vec4(inPosition, 1.0);"
    line """vec3 transformedPos = vec3(dot(position4, PosTexMats[pnmatIdx]),
                                        dot(position4, PosTexMats[pnmatIdx + 1U]),
                                        dot(position4, PosTexMats[pnmatIdx + 2U]));"""

    line "gl_Position = Projection * vec4(transformedPos, 1.0);"

    if vtxAttrColor0 in attrs:
        line "outColor0 = inColor0;"
    else:
        line "outColor0 = vec4(0.0, 0.0, 0.0, 1.0);"

    line "}"

proc genFragmentShader*(): string =
    line "#version 420 core"

    line "layout (location = 0) in vec4 inColor0;"

    line "out vec4 outColor;"

    line "void main() {"
    line "outColor = inColor0;"
    line "}"
