import
    strformat,
    memory

const TotalMemorySize = int64(MainRamSize + LcSize)

when defined(windows):
    import winim/inc/[windef, winbase, winimbase]

    # some things which aren't wrapped by winim yet:
    const
        MEM_RESERVE_PLACEHOLDER = 0x00040000
        MEM_REPLACE_PLACEHOLDER = 0x00004000
        MEM_COALESCE_PLACEHOLDERS = 0x1
        MEM_PRESERVE_PLACEHOLDER = 0x2

    proc MapViewOfFile3*(FileMapping, Process: HANDLE, BaseAddress: PVOID, Offset: ULONG64, ViewSize: SIZE_T, AllocationType, PageProtection: ULONG, ExtendedParameters: pointer, ParameterCount: ULONG): LPVOID {.winapi, stdcall, dynlib: "kernelbase", importc.}
    proc VirtualAlloc2*(Process: HANDLE, BaseAddress: PVOID, Size: SIZE_T, AllocationType, PageProtection: ULONG, ExtendedParameters: pointer, ParameterCount: ULONG): LPVOID {.winapi, stdcall, dynlib: "kernelbase", importc.}
    proc UnmapViewOfFile2*(Process: HANDLE, BaseAddress: PVOID, UnmapFlags: ULONG): BOOL {.winapi, stdcall, dynlib: "kernelbase", importc.}

    let
        allMemory = CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, DWORD TotalMemorySize, nil)

        adrSpaces = VirtualAlloc2(GetCurrentProcess(), nil, SIZE_T (1'i64 shl 32)*2, MEM_RESERVE or MEM_RESERVE_PLACEHOLDER, PAGE_NOACCESS, nil, 0)

        memBase = VirtualAlloc2(GetCurrentProcess(), nil, TotalMemorySize, MEM_RESERVE or MEM_RESERVE_PLACEHOLDER, PAGE_NOACCESS, nil, 0)

    assert allMemory != INVALID_HANDLE_VALUE

    doAssert MapViewOfFile3(allMemory, GetCurrentProcess(), memBase, 0, TotalMemorySize, MEM_REPLACE_PLACEHOLDER, PAGE_READWRITE, nil, 0) == memBase

    var placeholders = @[(cast[int64](adrSpaces), (1'i64 shl 32)*2)]
elif defined(nintendoswitch):
    let
        memBase = alloc0(TotalMemorySize)
        adrSpaces = pointer nil
else:
    import posix

    proc memfd_create(name: cstring, flags: cuint): cint {.importc, header: "<sys/mman.h>".}

    let allMemory = memfd_create("hocuspocube_mem", 0)
    assert allMemory != -1
    doAssert ftruncate(allMemory, Off TotalMemorySize) == 0

    let adrSpaces = mmap(nil, int((1'i64 shl 32)*2), PROT_NONE, MAP_ANONYMOUS or MAP_PRIVATE, -1, 0)
    assert adrSpaces != MAP_FAILED, &"{strerror(errno)}"

    let memBase = mmap(nil, int(TotalMemorySize), PROT_READ or PROT_WRITE, MAP_SHARED, allMemory, 0)
    assert memBase != MAP_FAILED, &"{strerror(errno)}"

let
    physicalAdrSpace* = cast[pointer](adrSpaces)
    translatedAdrSpace* = cast[pointer]((cast[ByteAddress](adrSpaces) + 1'i64 shl 32))

mainRAM = cast[ptr UncheckedArray[byte]](memBase)
lockedCache = cast[ptr UncheckedArray[byte]](cast[int](memBase) + MainRamSize)

const regions = [
    (0'i64, int64 MainRamSize),
    (0xE0000000'i64, int64 LcSize)]

iterator allRegions(translatedStart, physicalStart, totalSize: int64): (int64, int64, int64) =
    var allRegionsOffset = 0'i64

    for (start, size) in regions:
        if start + size > physicalStart and start < physicalStart + totalSize:
            var
                translatedAdr = start - physicalStart + translatedStart
                mapOffset = allRegionsOffset
                mapSize = size
            if start < physicalStart:
                translatedAdr = translatedStart
                mapOffset += physicalStart - start
                mapSize -= physicalStart - start
            if start + size > physicalStart + totalSize:
                mapSize -= (start + size) - (physicalStart + totalSize)

            yield (mapOffset, mapSize, translatedAdr)

        allRegionsOffset += size

proc changeRegionMapping*(adrSpace: pointer, translatedStart, physicalStart, totalSize: int64, map: bool) =
    echo &"remapping for region {translatedStart:08X} {physicalStart:08X} {totalSize:08X}"
    for (mapOffset, mapSize, adr) in allRegions(translatedStart, physicalStart, totalSize):
        let mapDst = cast[pointer](cast[ByteAddress](adrSpace) + adr)

        echo &"changing mapping {repr(mapDst)} {mapOffset:08X} {mapSize:08X}: {adr:08X} {map}"

        if map:
            when defined(windows):
                for i in 0..<placeholders.len:
                    let placeholder = placeholders[i]
                    if cast[int](mapDst) >= placeholder[0] and
                        cast[int](mapDst) < placeholder[0] + placeholder[1]:
                        if cast[int](mapDst) > placeholder[0]:
                            placeholders.add((placeholder[0], cast[int](mapDst)-placeholder[0]))
                            #echo &"adding placeholder {placeholders[^1]}"
                        if cast[int](mapDst)+mapSize < placeholder[0]+placeholder[1]:
                            placeholders.add((cast[int64](mapDst)+mapSize, placeholder[0]+placeholder[1]-(cast[int64](mapDst)+mapSize)))
                            #echo &"adding placeholder {placeholders[^1]}"
                        #echo &"deleting {placeholders[i]}"
                        placeholders.del(i)
                        break
                doAssert VirtualFreeEx(GetCurrentProcess(), mapDst, mapSize, MEM_RELEASE or MEM_PRESERVE_PLACEHOLDER) != FALSE, &"{GetLastError()}"
                doAssert MapViewOfFile3(allMemory,
                    GetCurrentProcess(),
                    mapDst,
                    ULONG mapOffset,
                    SIZE_T mapSize,
                    MEM_REPLACE_PLACEHOLDER,
                    PAGE_READWRITE,
                    nil, 0) == mapDst, &"error: {GetLastError()} {repr(mapDst)}"
            elif defined(nintendoswitch):
                discard
            else:
                doAssert mmap(mapDst, int mapSize, PROT_READ or PROT_WRITE, MAP_FIXED or MAP_SHARED, allMemory, Off mapOffset) != MAP_FAILED
        else:
            when defined(windows):
                doAssert UnmapViewOfFile2(GetCurrentProcess(), mapDst, MEM_PRESERVE_PLACEHOLDER) != FALSE, &"error: {GetLastError()}"

                var
                    coalesceStart = cast[int64](mapDst)
                    coalesceSize = mapSize
                    i = 0
                while i < placeholders.len:
                    if cast[int64](mapDst) == placeholders[i][0]+placeholders[i][1]:
                        coalesceStart = placeholders[i][0]
                        coalesceSize += placeholders[i][1]
                        placeholders.del(i)
                    elif cast[int64](mapDst)+mapSize == placeholders[i][0]:
                        coalesceSize += placeholders[i][1]
                        placeholders.del(i)
                    else:
                        i += 1
                #echo &"coalescing {repr(cast[pointer](coalesceStart))} {coalesceSize:08X}"
                doAssert VirtualFree(cast[pointer](coalesceStart), coalesceSize, MEM_RELEASE or MEM_COALESCE_PLACEHOLDERS) != FALSE, &"error: {GetLastError()}"
                placeholders.add((coalesceStart, coalesceSize))
            elif defined(nintendoswitch):
                discard
            else:
                doAssert munmap(mapDst, int mapSize) == 0
                doAssert mmap(mapDst, int mapSize, PROT_NONE, MAP_FIXED or MAP_SHARED or MAP_ANONYMOUS, -1, 0) != MAP_FAILED

changeRegionMapping(physicalAdrSpace, 0, 0, 1 shl 32, true)

when not defined(nintendoswitch):
    import ../util/jit/codegenx64

when defined(windows):
    proc exceptionHandler(info: ptr EXCEPTION_POINTERS): LONG {.stdcall.} =
        var rip = cast[pointer](info.ContextRecord.Rip)
        if info.ExceptionRecord.ExceptionCode == EXCEPTION_ACCESS_VIOLATION and
            handleSegfault(rip):
                info.ContextRecord.Rip = cast[DWORD64](rip)
                #echo &"invalid access to {cast[int](info.ExceptionRecord.ExceptionInformation[1])-cast[int](info.ContextRecord.R15):08X}"
                #echo cast[pointer](info.ContextRecord.R15) == translatedAdrSpace
                #echo cast[ptr PpcState](info.ContextRecord.Rbp).msr.dr
                #echo &"{cast[ptr PpcState](info.ContextRecord.Rbp).pc:08X}"
                EXCEPTION_CONTINUE_EXECUTION
        else:
            echo "it all returns to nothing"
            EXCEPTION_CONTINUE_SEARCH

    AddVectoredExceptionHandler(1, exceptionHandler)
elif defined(nintendoswitch):
    discard
else:
    var sa, oldSa: Sigaction

    proc exceptionHandler(sig: cint, siginfo: ptr SigInfo, rawCtx: pointer) {.noconv.} =
        if sig != SIGSEGV:
            return

        let ctx = cast[ptr Ucontext](rawCtx)
        var pc: pointer
        {.emit: [pc, " = ", ctx, "->uc_mcontext.gregs[REG_RIP];"].}

        if handleSegfault(pc):
            {.emit: [ctx, "->uc_mcontext.gregs[REG_RIP] = ", pc, ";"].}
            return
    
        if (oldSa.sa_flags and SA_SIGINFO) != 0:
            oldSa.sa_sigaction(sig, siginfo, rawCtx)
            return
        if oldSa.sa_handler == SIG_DFL:
            signal(sig, SIG_DFL)
            return
        if oldSa.sa_handler == SIG_IGN:
            return
        oldSa.sa_handler(sig)

    sa.sa_handler = nil
    sa.sa_sigaction = exceptionHandler
    sa.sa_flags = SA_SIGINFO
    discard sigemptyset(sa.sa_mask)
    discard sigaction(SIGSEGV, sa, oldSa)
