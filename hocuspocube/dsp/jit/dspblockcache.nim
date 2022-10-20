import
    strformat, sets,
    ../dspstate

type BlockEntryFunc* = proc(state: ptr DspState): int32 {.cdecl.}

var
    blockEntries: array[0x1000 + 0x1000, BlockEntryFunc]
    loopEnds: HashSet[uint16]

proc mapBlockEntryAdr*(adr: uint16): uint32 =
    case adr
    of 0x0'u16..0xFFF: uint32(adr)
    of 0x8000'u16..0x8FFF: uint32(adr) - 0x8000 + 0x1000
    else:
        raiseAssert(&"unknown dsp block adr {adr:04X}")

proc lookupBlock*(adr: uint16): BlockEntryFunc =
    #echo &"look up block {adr:04X}"
    blockEntries[mapBlockEntryAdr(adr)]

proc setBlock*(adr: uint16, f: BlockEntryFunc) =
    blockEntries[mapBlockEntryAdr(adr)] = f

proc markLoopEnd*(adr: uint16) =
    loopEnds.incl adr

proc isLoopEnd*(adr: uint16): bool =
    adr in loopEnds

proc invalidateByAdr*(adr: uint16) =
    if adr <= 0xFFF'u16:
        loopEnds.excl adr
        blockEntries[adr] = nil

proc clearBlockCache*() =
    loopEnds.clear()
    for entry in mitems blockEntries:
        entry = nil
