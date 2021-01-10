import
    streams, strformat,
    gecko/[interpreter/ppcinterpreter, gecko],
    dsp/interpreter/dspinterpreter,
    flipper/[rasterinterface, cp],
    util/dolfile,
    cycletiming

proc loadDol*(input: Stream) =
    let file = dolfile.loadDol(input)

    proc writeSection(section: Section) =
        copyMem(addr MainRAM[section.start - 0x80000000'u32], unsafeAddr section.data[0], section.data.len)
    for section in file.text:
        echo &".text at {section.start:X}"
        writeSection section
    for section in file.data:
        echo &".data at {section.start:X}"
        writeSection section

    geckoState.pc = file.entrypoint - 0x80000000'u32

proc run*() =
    rasterinterface.init()
    while true:
        let sliceEnd = min(geckoTimestamp + geckoMaxSlice, nearestEvent())

        geckoRun geckoTimestamp, sliceEnd
        dspRun dspTimestamp, geckoTimestamp

        cpRun()

        processEvents()