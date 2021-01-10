import
    strformat,

    rasterinterfacecommon,
    rasterinterface,

    ../util/bitstruct

type    
    ProjMatKind = enum
        projPerspective
        projOrthographic

makeBitStruct uint32, MatIndexLo:
    geometryIdx[0..5]: uint32
    tex0[6..11]: uint32
    tex1[12..17]: uint32
    tex2[18..23]: uint32
    tex3[24..29]: uint32

makeBitStruct uint32, MatIndexHi:
    tex4[0..5]: uint32
    tex5[6..11]: uint32
    tex6[12..17]: uint32
    tex7[18..23]: uint32

var
    # it's weird that those registers need to be specified twice
    # this needs some investigation, would be interesting to know what happens if only one of them is set
    matIdxLo: MatIndexLo
    matIdxHi: MatIndexHi

    viewport: array[6, float32]
    projMat: array[6, float32]
    projMatKind: ProjMatKind

    xfRegistersDirty = true
    xfMemoryDirty = true

    xfMemoryUniform*: XfMemoryUniform
    xfRegistersUniform*: XfRegistersUniform

proc translateProj() =
    case projMatKind
    of projPerspective:
        for i in 0..<16:
            xfRegistersUniform.projection[i] = 0f
        xfRegistersUniform.projection[0+0*4] = projMat[0]
        xfRegistersUniform.projection[0+2*4] = projMat[1]
        xfRegistersUniform.projection[1+1*4] = projMat[2]
        xfRegistersUniform.projection[1+2*4] = projMat[3]
        xfRegistersUniform.projection[2+2*4] = projMat[4]
        xfRegistersUniform.projection[2+3*4] = projMat[5]
        xfRegistersUniform.projection[3+2*4] = -1f
    else: discard

proc setupXf*() =
    if xfRegistersDirty:
        translateProj()
        setXfRegisters(xfRegistersUniform)
    if xfMemoryDirty:
        setXfMemory(xfMemoryUniform)

proc xfWrite*(adr, val: uint32) =
    case adr
    of 0..0xFF:
        xfMemoryUniform.posTexMats[adr] = cast[float32](val)
        xfMemoryDirty = true
    of 0x400..0x45F:
        let offset = adr - 0x400'u32
        xfMemoryUniform.nrmMats[(offset div 3) * 4 + (offset mod 3)] = cast[float32](val)
        xfMemoryDirty = true
    of 0x1018: matIdxLo = MatIndexLo val
    of 0x1019: matIdxHi = MatIndexHi val
    of 0x101A..0x101F: viewport[adr - 0x101A] = cast[float32](val)
    of 0x1020..0x1025:
        projMat[adr - 0x1020] = cast[float32](val)
        xfRegistersDirty = true
    of 0x1026:
        projMatKind = ProjMatKind(val and 1)
        xfRegistersDirty = true
    else: echo &"unknown xf write {adr:04X} {val:08X}"
