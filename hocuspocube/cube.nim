import
    streams, strformat,
    cycletiming,
    gekko/[interpreter/ppcinterpreter, gekko, ppcstate, memory],
    dsp/[dsp, dspstate, interpreter/dspinterpreter]

when not defined(nintendoswitch):
    import gekko/jit/ppcfrontend, dsp/jit/dspfrontend

import
    flipper/[rasterinterface, cp],
    util/dolfile,

    vi,

    std/monotimes, times

when defined(nintendoswitch):
    import frontend/switch
else:
    import frontend/sdl

proc loadDol*(input: Stream) =
    let file = dolfile.loadDol(input)

    proc writeSection(section: Section) =
        writeMainRAM(section.start - 0x80000000'u32, section.data)
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

var gekkoTimeStart: MonoTime

# the exportc is  a bad workaround
proc systemStep*(): bool {.exportc.} =
    gekkoTime += getMonoTime() - gekkoTimeStart
    let curTime = gekkoTimestamp()

    mDspState.negativeCycles = int32(dspTimestamp - curTime)
    dspTimestamp = curTime
    let dspStartTime = getMonoTime()
    dspfrontend.dspRun()
    dspTime += getMonoTime() - dspStartTime
    cpRun()
    processEvents(curTime)

    gekkoTarget = min(curTime + gekkoMaxSlice, nearestEvent())
    gekkoState.negativeCycles = int32(curTime - gekkoTarget)

    gekkoTimeStart = getMonoTime()

    frontendRunning

proc run*() =
    rasterinterface.init()
    discard systemStep()
    gekkoTimeStart = getMonoTime()
    ppcfrontend.gekkoRun()
