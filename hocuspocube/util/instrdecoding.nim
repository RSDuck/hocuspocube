import
    stew/bitops2,
    strformat, parseutils, macros, bitops, options, tables

proc parseFields*(pattern: string): OrderedTable[string, Slice[int]] =
    var i = 0
    while i < pattern.len:
        case pattern[i]
        of '0', '1':
            i += skipUntil(pattern, {'a'..'z'}, i)
        of 'a'..'z':
            var name: string
            let iStart = i
            i += parseWhile(pattern, name, {pattern[i]}, i)
            result[name] = (pattern.len - iStart - name.len)..(pattern.len - iStart - 1)
        of '_':
            while i < pattern.len and pattern[i] == '_': i += 1
        else:
            echo "invalid char ", pattern[i], " in parsing pattern ", pattern
            break

proc decodePattern(pattern: string): tuple[mask: uint32, fixbits: uint32] =
    for i in 0..<pattern.len:
        let idx = pattern.len - 1 - i
        if pattern[i] in {'0', '1'}:
            result[0].setMask(1'u32 shl idx)
            if pattern[i] == '1':
                result[1].setMask(1'u32 shl idx)

iterator validEncodings*(mask, fixbits: uint32, len: int): uint32 =
    var
        stack = @[(0, 0'u32)]
    while stack.len > 0:
        let (index, value) = stack.pop()

        if index < len:
            let
                isFix = mask.getBit(index)
                pattern = fixbits.getBit(index)

            if not isFix or pattern == false:
                stack.add((index + 1, value))
            if not isFix or pattern == true:
                stack.add((index + 1, value or (1'u32 shl index)))
        else:
            yield value

proc generateFields*(instr, letBlock: NimNode, patterns: OrderedTable[string, Slice[int]], existingIdents: seq[NimNode] = @[]): seq[NimNode] =
    var i = 0
    for name, slice in pairs patterns:
        result.add(if existingIdents.len == 0: nskLet.genSym name else: existingIdents[i])
        i += 1

        let
            idx = slice.a
            mask = toMask[uint32](0..slice.len-1)

        letBlock.add(nnkIdentDefs.newTree(nnkPragmaExpr.newTree(result[^1],
            nnkPragma.newTree(ident"used")),
            newEmptyNode(),
            (quote do: (`instr` shr `idx`) and cast[typeof(`instr`)](`mask`))))

func extractBits[T](x: T, slice: Slice[int]): T {.inline.} =
    (x and slice.toMask[:T]) shr slice.a

func insertBits*[T](x: var T, slice: Slice[int], val: T) =
    let mask = slice.toMask[:T]
    x = (x and not(mask)) or ((val shl slice.a) and mask)

macro matchSparseInstrs*[T](instr: T, patterns: static[OrderedTable[string, string]], body: varargs[untyped]): untyped =
    result = nnkIfStmt.newTree()
    for branch in body:
        if branch.kind == nnkOfBranch:
            branch[0].expectKind nnkStrLit
            let
                pattern = patterns[$branch[0]]
                (mask, fixbits) = decodePattern(pattern)
                fields = parseFields(pattern)

                matches = quote do: ((`instr` and cast[typeof(`instr`)](`mask`)) == cast[typeof(`instr`)](`fixbits`))

                letBlock = nnkLetSection.newTree()

            discard generateFields(instr, letBlock, fields, branch[1..^1])

            branch[^1].insert(0, letBlock)
            result.add nnkElifBranch.newTree(matches, branch[^1])
        else:
            result.add nnkElse.newTree(branch[0])
    #echo result.repr

proc generateShortDecoder*[bits: static[Slice[int]]](patterns: openArray[(string, string)],
    instrWidth: int,
    instr, state, undefinedInstr: NimNode): NimNode =

    const
        casesCount = 1 shl bits.len

    let caseStmt = nnkCaseStmt.newTree(nil)

    var
        cases: array[casesCount, int]

    for i, instrPattern in pairs patterns:
        let
            (name, pattern) = instrPattern

            branch = nnkOfBranch.newTree(newStmtList(nnkLetSection.newNimNode()))

            fields = parseFields(pattern)
            syms = generateFields(instr, branch[0][0], fields)

            (mask, fixbits) = decodePattern(pattern)

        branch[0].add(nnkCall.newTree(ident name, state))
        for sym in syms:
            branch[0][^1].add sym

        for encoding in validEncodings(mask.extractBits(bits), fixbits.extractBits(bits), bits.len):
            if cases[int encoding] != 0:
                let
                    name = patterns[i][0]
                    otherName = patterns[cases[int encoding] - 1][0]
                error(&"Conflict between {name} and {otherName}")

            branch.insert(branch.len - 1, newLit(encoding))
            cases[int encoding] = i + 1

        caseStmt.add branch

    let undefinedInstrBranch = nnkOfBranch.newTree()
    for encoding, association in cases:
        if association == 0:
            undefinedInstrBranch.add(newLit(uint32 encoding))

    if undefinedInstrBranch.len > 0:
        undefinedInstrBranch.add(quote do: `undefinedInstr`(`state`, `instr`))
        caseStmt.add undefinedInstrBranch

    caseStmt[0] = quote do: range[0..`casesCount`-1](extractBits(`instr`, `bits`))

    caseStmt

proc generateDecoder*[primaryBits, secondaryBits: static[Slice[int]]](patterns: openArray[(string, string)],
        excludeList: Table[string, seq[(string, uint32)]],
        instrWidth: int,
        instr, state, undefinedInstr: NimNode): NimNode =
    const
        primaryCasesCount = 1 shl primaryBits.len
        secondaryCasesCount = 1 shl secondaryBits.len
        secondaryCasesShift = secondaryBits.len

        decodingBits = primaryBits.toMask[:uint32] or secondaryBits.toMask[:uint32]

    var
        primaryCases: array[primaryCasesCount, seq[int]]
        indexedExclusionList: seq[seq[(uint32, uint32)]]
        decodedPatterns: seq[(uint32, uint32, NimNode)]

    let caseStmt = nnkCaseStmt.newTree(nil)

    for i, instrPattern in pairs patterns:
        let
            (name, pattern) = instrPattern

            branch = nnkOfBranch.newTree(newStmtList(nnkLetSection.newNimNode()))
        
            fields = parseFields(pattern)
            syms = generateFields(instr, branch[0][0], fields)

            (mask, fixbits) = decodePattern(pattern)

        decodedPatterns.add((mask, fixbits, branch))

        if pattern.len != instrWidth:
            error(&"pattern for instruction {name} is either too long or too short (want {instrWidth} got {pattern.len})")

        #if instrWidth == 16:
        #    branch[0].add(quote do: echo "executing dsp instr ", `name`, " ", `instr`)
        branch[0].add(nnkCall.newTree(ident name, state))
        for sym in syms:
            branch[0][^1].add sym

        var exclusions: seq[(uint32, uint32)]
        for exclusion in excludeList.getOrDefault(name):
            let
                field = fields[exclusion[0]]
                mask = field.toMask[:uint32]()
            exclusions.add((mask, (exclusion[1] shl field.a) and mask))
            #echo &"exclusion for {name} {exclusions[^1][0]:08X} {exclusions[^1][1]:08X} {fields[exclusion[0]]}"
        indexedExclusionList.add exclusions

        for encoding in validEncodings(mask.extractBits(primaryBits), fixbits.extractBits(primaryBits), primaryBits.len):
            primaryCases[int encoding].add(i)

        caseStmt.add branch

    let undefinedInstrBranch = nnkOfBranch.newTree()

    var
        curOffset = 0
        offsetTable: array[primaryCasesCount, uint8]
    for i in 0..<primaryCasesCount: offsetTable[i] = 255'u8
    for primEncoding, primCase in pairs primaryCases:
        var
            caseMapping: array[secondaryCasesCount, int]
            mappedInstrs = 0
            unfilledCases = secondaryCasesCount

        for patternIdx in primCase:
            let (mask, fixbits, _) = decodedPatterns[patternIdx]
            var mappedAtleastOne = false
            for secEncoding in validEncodings(mask.extractBits(secondaryBits), fixbits.extractBits(secondaryBits), secondaryBits.len):
                block skip:
                    var fullEncoding = 0'u32
                    fullEncoding.insertBits(primaryBits, uint32 primEncoding)
                    fullEncoding.insertBits(secondaryBits, uint32 secEncoding)

                    for exclusion in indexedExclusionList[patternIdx]:
                        if (fullEncoding and (exclusion[0] and decodingBits)) == exclusion[1]:
                            break skip

                    if caseMapping[int secEncoding] != 0:
                        let
                            name = patterns[patternIdx][0]
                            otherName = patterns[caseMapping[int secEncoding] - 1][0]
                        error(&"Conflict between {name} and {otherName} {primEncoding:08X}")

                    caseMapping[int secEncoding] = patternIdx + 1

                    mappedAtleastOne = true
                    unfilledCases -= 1

            if mappedAtleastOne:
                mappedInstrs += 1

        if mappedInstrs == 1 and unfilledCases == 0:
            #echo &"instr {patterns[int caseMapping[0] - 1][0]} fills entire prefix space of {primEncoding:08X}"
            let branch = decodedPatterns[caseMapping[0] - 1][2]
            branch.insert(branch.len - 1, newLit(primEncoding))
        elif mappedInstrs == 0:
            undefinedInstrBranch.add(newLit(primEncoding))
        else:
            for secEncoding in 0..<secondaryCasesCount:
                let val = curOffset + primaryCasesCount + secEncoding
                if caseMapping[secEncoding] != 0:
                    let branch = decodedPatterns[caseMapping[secEncoding] - 1][2]
                    branch.insert(branch.len - 1, newLit(val))
                else:
                    undefinedInstrBranch.add(newLit(val))

            offsetTable[primEncoding] = uint8(curOffset shr secondaryCasesShift)
            curOffset += secondaryCasesCount

            undefinedInstrBranch.add(newLit(primEncoding))

    if undefinedInstrBranch.len > 0:
        undefinedInstrBranch.add(quote do: `undefinedInstr`(`state`, `instr`))
        caseStmt.add undefinedInstrBranch

    # For ppc where the first identifier is only 6 bit (thus we can store whether a primary encoding
    # needs the second encoding in a single 64-bit register) a solution using popcount
    # could be used if it was more common.
    let
        totalValues = curOffset + primaryCasesCount

        offsetTableSym = nskConst.genSym("offsetTable")
        offsetTableDefinition = nnkConstSection.newTree(nnkConstDef.newTree(offsetTableSym, newEmptyNode(), nnkBracket.newTree()))
    for i in 0..<primaryCasesCount:
        offsetTableDefinition[0][2].add newlit offsetTable[i]

    caseStmt[0] = quote do: range[0..`totalValues`-1]((block:
        let
            primaryBits = extractBits(`instr`, `primaryBits`)
            secondaryBits = extractBits(`instr`, `secondaryBits`)
            offset = uint16(`offsetTableSym`[primaryBits])

        if offset == 255: primaryBits
        else: (`primaryCasesCount` + secondaryBits + (offset shl `secondaryCasesShift`))
        ))

    result = newStmtList(offsetTableDefinition, caseStmt)
