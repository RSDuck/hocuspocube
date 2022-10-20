import
    strformat,
    ".."/[ppcstate, memory]

type BlockEntryFunc* = proc(ppcstate: ptr PpcState): int32 {.cdecl.}

var
    blockEntries: array[(0x1800000 + 0x900) div 4, BlockEntryFunc]

proc mapBlockEntryAdr*(adr: uint32): uint32 =
    if adr >= 0xFFF00000'u32:
        assert adr >= 0xFFF00000'u32 and adr < 0xFFF00000'u32+0x900
        (adr - 0xFFF00000'u32 + MainRamSize) div 4
    else:
        assert adr < MainRamSize
        adr div 4

proc lookupBlock*(adr: uint32): BlockEntryFunc =
    blockEntries[mapBlockEntryAdr(adr)]

proc setBlock*(adr: uint32, f: BlockEntryFunc) =
    blockEntries[mapBlockEntryAdr(adr)] = f

proc invalidateBlockCacheCode*(adr: uint32) =
    for i in countup(adr, adr+31, 4):
        blockEntries[i div 4] = nil

proc clearBlockCache*() =
    zeroMem(addr blockEntries, sizeof(blockEntries))

proc compileBlockPpc(adr: uint32): BlockEntryFunc {.importc: "compileBlockPpc".}

proc nextBlock*(state: var PpcState, adr: uint32): BlockEntryFunc =
    result = lookupBlock(state.translateInstrAddr(adr))
    if unlikely(result == nil):
        result = compileBlockPpc(adr)
    #echo &"lookup and translate {adr:08X} {repr(result)}"
