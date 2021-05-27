import ppcfrontendcommon

var
    blockEntries*: array[(0x1800000 + 0x900) div 4, BlockEntryFunc]

proc mapBlockEntryAdr*(adr: uint32): uint32 =
    if adr >= 0xFFF00000'u32:
        (adr - 0xFFF00000'u32 + 0x1800000) div 4
    else:
        adr div 4

proc invalidateBlockCacheCode*(adr: uint32) =
    for i in countup(adr, adr+31, 4):
        blockEntries[i div 4] = nil

proc clearBlockCache*() =
    zeroMem(addr blockEntries, sizeof(blockEntries))