import
    parseutils, macros, bitops

iterator parseFields*(pattern: string): (int, string) =
    var i = 0
    while i < pattern.len:
        case pattern[i]
        of '0', '1':
            i += skipUntil(pattern, {'a'..'z'}, i)
        of 'a'..'z':
            var name: string
            let iStart = i
            i += parseWhile(pattern, name, {pattern[i]}, i)
            yield (pattern.len - iStart - name.len, name)
        else:
            echo "invalid char ", pattern[i], " in parsing pattern ", pattern
            break

iterator validEncodings*(pattern: string): uint32 =
    var
        stack = @[(0, 0'u32)]
    while stack.len > 0:
        let (index, value) = stack.pop()

        if index < pattern.len:
            let
                isOne = pattern[pattern.len - index - 1] == '1'
                isZero = pattern[pattern.len - index - 1] == '0'

                isNone = not isOne and not isZero

            if isZero or isNone:
                stack.add((index + 1, value))
            if isOne or isNone:
                stack.add((index + 1, bitor(value, 1'u32 shl index)))
        else:
            yield value

proc generateFields*(instr, letBlock: NimNode, pattern: string): seq[NimNode] =
    for (idx, name) in parseFields pattern:
        result.add(nskLet.genSym name)

        let mask = toMask[uint32](0..name.len-1)

        letBlock.add(nnkIdentDefs.newTree(result[^1], newEmptyNode(), (quote do: (`instr` shr `idx`) and `mask`)))

func extractBits[T](x: T, slice: Slice[int]): T =
    (x and slice.toMask[:T]) shr slice.a

func flipSlice(x: Slice[int], max: int): Slice[int] =
    max-1-x.b..max-1-x.a

proc generateDecoder*[primaryBits, secondaryBits: static[Slice[int]]](patterns: openArray[(string, string)],
        instrWidth: int,
        instr, state, undefinedInstr: NimNode): NimNode =
    # in both ppc as well as the dsp instruction set a lot of bits are relevant for
    # decoding an instruction. So one solution would be to just go with a 2^16 entry table.
    # But notice how in both cases the bits necessary to decode the instruction can be split up
    # into fields (for ppc this is an official notation) and that most instructions which
    # do use both parts use the same few first parts.
    # This means if we can split up the lookup into two steps, the table size can be reduced
    # significantly down to
    # 2^(primary bits count)+2^(secondary bits count)*(instructions patterns which can't be decoded
    #   from the primary identifier alone)
    # We also want to avoid too complex branching, so instead of nesting the decoding based on the second identifier
    # inside the branch from the primary identifier we linearise the indices using a look up table.
    const
        primaryCasesCount = 1 shl primaryBits.len
        secondaryCasesCount = 1 shl secondaryBits.len
        secondaryCasesShift = secondaryBits.len
    var primaryCases: array[primaryCasesCount, seq[(NimNode, int)]]
    let caseStmt = nnkCaseStmt.newTree(nil)

    for i, instrPattern in pairs patterns:
        let
            (name, pattern) = instrPattern

            branch = nnkOfBranch.newTree(newStmtList(nnkLetSection.newNimNode()))
        
            syms = generateFields(instr, branch[0][0], pattern)

        branch[0].add(nnkCall.newTree(ident name, state))
        for sym in syms:
            branch[0][^1].add sym

        for encoding in validEncodings(pattern[flipSlice(primaryBits, instrWidth)]):
            primaryCases[encoding].add (branch, i)

        caseStmt.add branch

    let undefinedInstrBranch = nnkOfBranch.newTree()

    var
        curOffset = 0
        offsetTable: array[primaryCasesCount, uint8]
    for i in 0..<primaryCasesCount: offsetTable[i] = 255'u8
    for primEncoding, primCase in pairs primaryCases:
        if primCase.len == 1:
            primCase[0][0].insert 0, newLit(uint32 primEncoding)
        elif primCase.len > 1:
            # these cases need to be distinguished by their secondary value
            let offset = curOffset
            offsetTable[primEncoding] = uint8(offset shr secondaryCasesShift)
            curOffset += secondaryCasesCount

            var notHandledCases = {range[0..secondaryCasesCount-1](0)..secondaryCasesCount-1}
            for secondaryCase in primCase:
                for secondEncoding in validEncodings(patterns[secondaryCase[1]][1][flipSlice(secondaryBits, instrWidth)]):
                    secondaryCase[0].insert secondaryCase[0].len - 1, newLit(uint32(primaryCasesCount + offset) + secondEncoding)
                    notHandledCases.excl secondEncoding

            for remainingCase in notHandledCases:
                undefinedInstrBranch.add newLit(uint32(primaryCasesCount + remainingCase + offset))

            # note that this branch should never be called
            undefinedInstrBranch.add(newLit uint32(primEncoding))
        else:
            undefinedInstrBranch.add newLit(uint32 primEncoding)

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