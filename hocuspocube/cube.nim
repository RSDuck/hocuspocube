import
    streams, strformat,
    gekko/[interpreter/ppcinterpreter, gekko, ppcstate],
    gekko/jit/ppcfrontend,
    dsp/interpreter/dspinterpreter,
    flipper/[rasterinterface, cp],
    util/dolfile,
    cycletiming

proc loadDol*(input: Stream) =
    let file = dolfile.loadDol(input)

    proc writeSection(section: Section) =
        copyMem(addr mainRAM[section.start - 0x80000000'u32], unsafeAddr section.data[0], section.data.len)
    for section in file.text:
        echo &".text at {section.start:X}"
        writeSection section
    for section in file.data:
        echo &".data at {section.start:X}"
        writeSection section

    gekkoState.pc = file.entrypoint - 0x80000000'u32

proc boot*() =
    gekkoState.msr.ip = true
    gekkoState.pendingExceptions.incl exceptionSystemReset

proc run*() =
    rasterinterface.init()
    while true:
        gekkoTarget = min(gekkoTimestamp + gekkoMaxSlice, nearestEvent())
        ppcfrontend.gekkoRun gekkoTimestamp, gekkoTarget
        dspRun dspTimestamp, gekkoTimestamp

        cpRun()

        processEvents()