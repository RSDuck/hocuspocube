import
    strformat, hashes,
    rasterinterfacecommon,
    xf, bp

type
    VertexShaderKey* = object
        enabledAttrs*: set[VertexAttrKind]
        normalsNBT*: bool

        numTexcoordGen*: uint32
        texcoordGen*: array[8, TexcoordGen]

        numColors*: uint32
        lightCtrls*: array[LightCtrlKind, LightCtrl]
    
    FragmentShaderKey* = object
        numTevStages*: uint32
        colorEnv*: array[16, TevColorEnv]
        alphaEnv*: array[16, TevAlphaEnv]
        ras1Tref*: array[8, Ras1Tref]
        ksel*: array[8, TevKSel]

proc `==`*(a, b: VertexShaderKey): bool =
    result = a.enabledAttrs == b.enabledAttrs and
        a.numTexcoordGen == b.numTexcoordGen and
        a.numColors == b.numColors

    if result:
        for i in 0..<a.numTexcoordGen:
            if a.texcoordGen[i] != b.texcoordGen[i]:
                return false

        if a.numColors >= 1 and 
            a.lightCtrls[lightCtrlColor0] != b.lightCtrls[lightCtrlColor0] or
            a.lightCtrls[lightCtrlAlpha0] != b.lightCtrls[lightCtrlAlpha0]:
            return false
        if a.numColors == 2 and 
            a.lightCtrls[lightCtrlColor1] != b.lightCtrls[lightCtrlColor1] or
            a.lightCtrls[lightCtrlAlpha1] != b.lightCtrls[lightCtrlAlpha1]:
            return false

proc hash*(key: VertexShaderKey): Hash =
    result = result !& hash(key.enabledAttrs)
    result = result !& hash(key.numTexcoordGen)
    for i in 0..<key.numTexcoordGen:
        result = result !& hash(key.texcoordGen[i])
    result = result !& hash(key.numColors)
    if key.numColors >= 1:
        result = result !&
            hash(key.lightCtrls[lightCtrlColor0]) !&
            hash(key.lightCtrls[lightCtrlAlpha0])
    if key.numColors == 2:
        result = result !&
            hash(key.lightCtrls[lightCtrlColor1]) !&
            hash(key.lightCtrls[lightCtrlAlpha1])
    result = !$result

proc `==`*(a, b: FragmentShaderKey): bool =
    result = a.numTevStages == b.numTevStages

    if result:
        for i in 0..<a.numTevStages:
            if a.colorEnv[i] != b.colorEnv[i]:
                return false
            if a.alphaEnv[i] != b.alphaEnv[i]:
                return false
            if a.ras1Tref.getRas1Tref(i) != b.ras1Tref.getRas1Tref(i):
                return false
            if a.ksel.getTevKSel(i) != b.ksel.getTevKSel(i):
                return false

proc hash*(key: FragmentShaderKey): Hash =
    result = result !& hash(key.numTevStages)
    for i in 0..<key.numTevStages:
        result = result !& hash(key.colorEnv[i])
        result = result !& hash(key.alphaEnv[i])
        result = result !& hash(key.ras1Tref.getRas1Tref(i))
        result = result !& hash(key.ksel.getTevKSel(i))
    result = !$result

template line(str: string) =
    result &= str
    result &= '\n'

const
    registerUniformSource = """layout (std140, binding = 0) uniform Registers {
mat4 Projection;
uint MatIndices0, MatIndices1;
vec4 TexcoordScale[4];
vec4 TextureSizes[8];
ivec4 Konstants[2];
uvec4 MatColor;
};"""

func mapPackedArray2(n: uint32): (uint32, string) =
    if (n mod 2) == 0:
        (n div 2, "xy")
    else:
        (n div 2, "zw")

proc genVertexShader*(key: VertexShaderKey): string =
    line "#version 430 core"

    block:
        line "layout (location = 0) in vec3 inPosition;"
        var location = 1
        if vtxAttrNormal in key.enabledAttrs:
            line &"layout (location = {location}) in vec3 inNormal;"
            location += 1
            if key.normalsNBT:
                line &"layout (location = {location}) in vec3 inBinormal;"
                location += 1
                line &"layout (location = {location}) in vec3 inTangent;"
                location += 1
        for i in 0..<2:
            if vtxAttrColor0.succ(i) in key.enabledAttrs:
                line &"layout (location = {location}) in vec4 inColor{i};"
                location += 1
        for i in 0..<8:
            if vtxAttrTexCoord0.succ(i) in key.enabledAttrs:
                line &"layout (location = {location}) in vec2 inTexcoord{i};"
                location += 1

    line "layout (location = 0) out vec4 outColor0;"
    line "layout (location = 1) out vec4 outColor1;"
    line "layout (location = 2) out vec3 outTexcoord0;"

    line registerUniformSource

    line "layout (std140, binding = 1) uniform xfMemory {"
    line "vec4 PosTexMats[64];"
    line "vec4 NrmMats[32];" # stupid padding
    line "vec4 PostTexMats[64];"
    line "uvec4 LightColor[2];"
    line "vec4 LightPositionA1[4];"
    line "vec4 LightDirectionA0[4];"
    line "vec4 LightRemainingFactors[4];"
    line "};"

    line "void main() {"

    line "uint pnmatIdx = bitfieldExtract(MatIndices0, 0, 6);"
    line "vec4 position4 = vec4(inPosition, 1.0);"
    line """vec3 transformedPos = vec3(dot(position4, PosTexMats[pnmatIdx]),
                                        dot(position4, PosTexMats[pnmatIdx + 1U]),
                                        dot(position4, PosTexMats[pnmatIdx + 2U]));"""

    line "gl_Position = Projection * vec4(transformedPos, 1.0);"

    for i in 0..<2:
        let swizzle = if i == 0: 'x' else: 'y'
        line &"vec4 ambientReg{i} = unpackUnorm4x8(MatColor.{swizzle}).abgr;"
    for i in 0..<2:
        let swizzle = if i == 0: 'z' else: 'w'
        line &"vec4 materialReg{i} = unpackUnorm4x8(MatColor.{swizzle}).abgr;"

    if vtxAttrNormal in key.enabledAttrs:
        line """vec3 transformedNormal = normalize(vec3(dot(inNormal, NrmMats[pnmatIdx].xyz),
                                            dot(inNormal, NrmMats[pnmatIdx + 1U].xyz),
                                            dot(inNormal, NrmMats[pnmatIdx + 2U].xyz)));"""

    for i in 0..<key.numColors:
        line "{"

        let
            colorLightCtrl = key.lightCtrls[lightCtrlColor0.succ(int i)]
            alphaLightCtrl = key.lightCtrls[lightCtrlAlpha0.succ(int i)]

            ambientColor = case colorLightCtrl.ambSrc
                of matColorSrcPerVertex: &"inColor{i}.rgb"
                of matColorSrcRegister: &"ambientReg{i}.rgb"
            ambientAlpha = case alphaLightCtrl.ambSrc
                of matColorSrcPerVertex: &"inColor{i}.a"
                of matColorSrcRegister: &"ambientReg{i}.a"
            materialColor = case colorLightCtrl.matSrc
                of matColorSrcPerVertex: &"inColor{i}.rgb"
                of matColorSrcRegister: &"materialReg{i}.rgb"
            materialAlpha = case alphaLightCtrl.matSrc
                of matColorSrcPerVertex: &"inColor{i}.a"
                of matColorSrcRegister: &"materialReg{i}.a"

        line &"vec4 finalColor;"
        if colorLightCtrl.enableLighting:
            line &"finalColor.rgb = {ambientColor};"
        else:
            line &"finalColor.rgb = {materialColor};"

        if alphaLightCtrl.enableLighting:
            line &"finalColor.a = {ambientAlpha};"
        else:
            line &"finalColor.a = {materialAlpha};"

        proc calculateAtten(result: var string, light: int, ctrl: LightCtrl) =
            line &"vec3 lightPos = LightPositionA1[{light}].xyz;"
            line &"vec3 lightDir = LightDirectionA0[{light}].xyz;"
            line "vec3 torwardsLight = lightPos - transformedPos;"
            line "float torwardsLightLen = length(torwardsLight);"
            line "torwardsLight /= torwardsLightLen;"

            if ctrl.attenEnable:
                case ctrl.attenSelect
                of attenSelectDiffSpotlight:
                    line "float aattn = dot(torwardsLight, lightDir);"
                    line "float d = torwardsLightLen;"
                of attenSelectSpecular:
                    line "float aattn = dot(transformedNormal, lightDir);"
                    line "float d = aattn;"

                line &"""float atten = max(LightDirectionA0[{light}].w + LightPositionA1[{light}].w * aattn + aattn * aattn * LightRemainingFactors[{light}].x) /
                                        (LightRemainingFactors[{light}].y + LightRemainingFactors[{light}].z * aattn + aattn * aattn * LightRemainingFactors[{light}].w), 0);"""
            else:
                line "float atten = 1.0;"

            let diff =
                    case ctrl.diffAtten
                    of diffuseAtten1: "1.0"
                    of diffuseAttenNL: "dot(transformedNormal, torwardsLight)"
                    of diffuseAttenNLClamped: "max(dot(transformedNormal, torwardsLight), 0)"
                    else: raiseAssert("diffuse attenuation reserved value!")
            line &"atten *= {diff};"

        for j in 0..<8:
            line "{"
            let
                lightColorIdx = if j >= 4: 1 else: 0
                lightColorSwizzle = case range[0..3](j mod 4)
                    of 0: 'x'
                    of 1: 'y'
                    of 2: 'z'
                    of 3: 'w'
            line &"vec4 lightColor = unpackUnorm4x8(LightColor[{lightColorIdx}].{lightColorSwizzle}).abgr;"
            if colorLightCtrl.enableLighting and colorLightCtrl.lights(j):
                line "{"
                calculateAtten(result, j, colorLightCtrl)
                line "finalColor.rgb += atten * lightColor.rgb;"
                line "}"
            if alphaLightCtrl.enableLighting and alphaLightCtrl.lights(j):
                line "{"
                calculateAtten(result, j, alphaLightCtrl)
                line "finalColor.a += atten * lightColor.a;"
                line "}"
            line "}"

        if colorLightCtrl.enableLighting:
            line &"finalColor.rgb = clamp(finalColor.rgb * {materialColor}, 0, 1);"
        if alphaLightCtrl.enableLighting:
            line &"finalColor.a = clamp(finalColor * {materialAlpha}, 0, 1);"

        line &"outColor{i} = finalColor;"

        line "}"

    for i in 0..<key.numTexcoordGen:
        doAssert key.texcoordGen[i].kind == texcoordGenKindRegular, "only regular texcoords implemented"

        line "{"
        let src =
            case key.texcoordGen[i].src
            of texcoordGenSrcGeom: "transformedPos"
            of texcoordGenSrcNrm: "vec4(transformedNormal, 1.0)"
            of texcoordGenSrcTex0..texcoordGenSrcTex7:
                let n = ord(texcoordGen[i].src) - ord(texcoordGenSrcTex0)
                if vtxAttrTexCoord0.succ(n) in key.enabledAttrs:
                    &"vec4(inTexcoord{n}, 1.0, 1.0)"
                else:
                    "vec4(0.0, 0.0, 0.0, 1.0)" # what happens then?
            else: raiseAssert(&"texcoord source {key.texcoordGen[i].src} not implemented yet")

        line &"vec4 texcoordSrc = {src};"
        let
            matVar = if i >= 4: 0 else: 1
            matShift = 6*(i - (if i >= 4: 4 else: 0))
        line &"uint matIdx = bitfieldExtract(MatIndices{matVar}, {matShift}, 6);"

        case key.texcoordGen[i].proj
        of texcoordProjStq:
            line """vec3 transformedTexcoord = vec3(dot(texcoordSrc, PosTexMats[matIdx]),
                                                    dot(texcoordSrc, PosTexMats[matIdx+1U]),
                                                    dot(texcoordSrc, PosTexMats[matIdx+2U]));"""
        of texcoordProjSt:
            line """vec3 transformedTexcoord = vec3(dot(texcoordSrc, PosTexMats[matIdx]),
                                        dot(texcoordSrc, PosTexMats[matIdx+1U]),
                                        1.0);"""

        let
            (scaleIdx, scaleSwizzle) = mapPackedArray2(i)
        line &"outTexcoord0 = transformedTexcoord * vec3(TexcoordScale[{scaleIdx}].{scaleSwizzle} * 128.0, 1.0);"

        line "}"

    line "}"

proc signedExtract(val: string, start, bits: int): string =
    &"(({val} << {32 - bits - start}) >> {32 - bits})"

proc genFragmentShader*(key: FragmentShaderKey): string =
    line "#version 430 core"

    line "layout (location = 0) in vec4 inColor0;"
    line "layout (location = 1) in vec4 inColor1;"
    line "layout (location = 2) in vec3 inTexcoord0;"

    line "layout (binding = 0) uniform sampler2D Textures[8];"

    line "out vec4 outColor;"

    line registerUniformSource

    line "void main() {"

    for i in 0..<4:
        let
            swizzle0 = if (i mod 2) == 0: "x" else: "z"
            swizzle1 = if (i mod 2) == 0: "y" else: "w"
            r = signedExtract(&"Konstants[{i div 2}].{swizzle0}", 0, 11)
            g = signedExtract(&"Konstants[{i div 2}].{swizzle1}", 12, 11)
            b = signedExtract(&"Konstants[{i div 2}].{swizzle1}", 0, 11)
            a = signedExtract(&"Konstants[{i div 2}].{swizzle0}", 12, 11)
        line &"ivec4 konst{i} = ivec4({r}, {g}, {b}, {a});"

    line "ivec4 reg0 = konst0, reg1 = konst1, reg2 = konst2, reg3 = konst3;"

    line "ivec3 texcoord0 = ivec3(inTexcoord0);"
    for i in 0..<key.numTevStages:
        line "{"
        const
            mapColorOperand: array[TevColorEnvSel, string] =
                ["reg0.rgb", "reg0.aaa", "reg1.rgb", "reg1.aaa", "reg2.rgb", "reg2.aaa", "reg3.rgb", "reg3.aaa",
                    "texcolor.rgb", "texcolor.aaa", "rascolor.rgb", "rascolor.aaa",
                    "ivec3(255, 255, 255)", "ivec3(128, 128, 128)", "konstant.rgb", "ivec3(0, 0, 0)"]
            mapAlphaOperand: array[TevAlphaEnvSel, string] =
                ["reg0.a", "reg1.a", "reg2.a", "reg3.a", "texcolor.a", "rascolor.a", "konstant.a", "0"]

            mapColorKonstant: array[TevKColorSel, string] =
                ["ivec3(255)", "ivec3(255*7/8)", "ivec3(255*3/4)", "ivec3(255*5/8)", "ivec3(255*1/2)", "ivec3(255*3/8)",
                    "ivec3(255*1/4)", "ivec3(255*1/8)",
                    "konst0.rgb", "konst1.rgb", "konst2.rgb", "konst3.rgb",
                    "konst0.rrr", "konst1.rrr", "konst2.rrr", "konst3.rrr",
                    "konst0.ggg", "konst1.ggg", "konst2.ggg", "konst3.ggg",
                    "konst0.bbb", "konst1.bbb", "konst2.bbb", "konst3.bbb",
                    "konst0.aaa", "konst1.aaa", "konst2.aaa", "konst3.aaa"]
            mapAlphaKonstant: array[TevKAlphaSel, string] =
                ["255", "(255*7/8)", "(255*3/4)", "(255*5/8)", "(255*1/2)", "(255*3/8)",
                    "(255*1/4)", "(255*1/8)",
                    "0", "0", "0", "0", "0", "0", "0", "0",
                    "konst0.r", "konst1.r", "konst2.r", "konst3.r",
                    "konst0.g", "konst1.g", "konst2.g", "konst3.g",
                    "konst0.b", "konst1.b", "konst2.b", "konst3.b",
                    "konst0.a", "konst1.a", "konst2.a", "konst3.a"]

        let
            colorEnv = key.colorEnv[i]
            alphaEnv = key.alphaEnv[i]

            (texmap, _, texmapEnable, color) = key.ras1tref.getRas1Tref(i)
            (kselColor, kselAlpha) = key.ksel.getTevKSel(i)

            colorDst = &"reg{colorEnv.dst}.rgb"
            alphaDst = &"reg{alphaEnv.dst}.a"

            colorOp = if colorEnv.sub: "-" else: "+"
            alphaOp = if alphaEnv.sub: "-" else: "+"

            colorA = mapColorOperand[colorEnv.sela]
            colorB = mapColorOperand[colorEnv.selb]
            colorC = mapColorOperand[colorEnv.selc]
            colorD = mapColorOperand[colorEnv.seld]
            alphaA = mapAlphaOperand[alphaEnv.sela]
            alphaB = mapAlphaOperand[alphaEnv.selb]
            alphaC = mapAlphaOperand[alphaEnv.selc]
            alphaD = mapAlphaOperand[alphaEnv.seld]

            colorKonstant = mapColorKonstant[kselColor]
            alphaKonstant = mapAlphaKonstant[kselAlpha]

            rascolor = case color
                of ras1trefColorColor0: "inColor0"
                of ras1trefColorColor1: "inColor1"
                of ras1trefColorZero: "vec4(0)"
                else: "unimplemented"

        line &"ivec4 rascolor = ivec4({rascolor} * 255.0);"

        line &"ivec4 konstant = ivec4({colorKonstant}, {alphaKonstant});"

        if texmapEnable:
            line &"ivec4 texcolor = ivec4(texture(Textures[{texmap}], vec2(texcoord0.xy) * TextureSizes[{texmap}].zw / 128.0) * 255.0);"
        else:
            # welp what happens here?
            line &"ivec4 texcolor = ivec4(255, 255, 255, 255);"

        line &"uvec3 colorA8 = uvec3({colorA}) & uvec3(0xFF);"
        line &"uvec3 colorB8 = uvec3({colorB}) & uvec3(0xFF);"
        line &"uvec3 colorC8 = uvec3({colorC}) & uvec3(0xFF);"

        line &"uint alphaA8 = uint({alphaA}) & 0xFF;"
        line &"uint alphaB8 = uint({alphaB}) & 0xFF;"
        line &"uint alphaC8 = uint({alphaC}) & 0xFF;"

        line &"""ivec3 colorVal = ({colorD} << 8) {colorOp} ivec3((255 - colorC8) * colorA8 + colorC8 * colorB8);"""
        line &"""int alphaVal = ({alphaD} << 8) {alphaOp} int((255 - alphaC8) * alphaA8 + alphaC8 * alphaB8);"""

        if colorEnv.clamp:
            line &"{colorDst} = clamp(colorVal >> 8, 0, 255);"
        else:
            line &"{colorDst} = clamp(colorVal >> 8, 0, 1023);"

        if alphaEnv.clamp:
            line &"{alphaDst} = clamp(alphaVal >> 8, 0, 255);"
        else:
            line &"{alphaDst} = clamp(alphaVal >> 8, 0, 1023);"

        line "}"

    line "outColor = vec4(reg0) / 255.0;"
    line "}"
