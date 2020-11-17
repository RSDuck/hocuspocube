import
    stew/endians2, streams

type
    DolFile* = object
        entrypoint*: uint32
        text*: seq[Section]
        data*: seq[Section]
        bssStart*, bssSize*: uint32

    Section* = object
        start*: uint32
        data*: seq[byte]

    DolHeader {.packed.} = object
        textOffsets: array[7, uint32]
        dataOffsets: array[11, uint32]
        textStarts: array[7, uint32]
        dataStarts: array[11, uint32]
        textSizes: array[7, uint32]
        dataSizes: array[11, uint32]
        bssStart, bssSize: uint32
        entrypoint: uint32

proc loadDol*(input: Stream): DolFile =
    var header: DolHeader
    doAssert input.readData(addr header, sizeof(header)) == sizeof(header)

    result.entrypoint = fromBE header.entrypoint
    result.bssStart = fromBE header.bssStart
    result.bssSize = fromBE header.bssSize

    template doSection(name: untyped) =
        for i in 0..<len(header.`name Offsets`):
            let
                offset = fromBE header.`name Offsets`[i]
                start = fromBE header.`name Starts`[i]
                size = fromBE header.`name Sizes`[i]

            if size > 0:
                var section = Section(start: start, data: newSeq[byte](size))
                input.setPosition(int offset)
                doAssert input.readData(addr section.data[0], int size) == int size

                result.name.add(section)
    doSection(text)
    doSection(data)