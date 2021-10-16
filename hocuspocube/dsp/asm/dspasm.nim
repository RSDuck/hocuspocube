#[
    in progress dsp assembler/disassembler
]#

import
    ".."/dspdef, ../../util/instrdecoding,

    cligen,

    stew/endians2,
    bitops, options, sets, sugar,
    streams, os,
    macros,
    tables, deques,
    strformat, parseutils, strutils, lexbase,

    fusion/matching

{.experimental: "caseStmtMacros".}
    
macro generateAssemblyFuncs(suffix: static string, patterns: static openArray[(string, string)]): untyped =
    result = newStmtList()
    for def in patterns:
        let
            (name, pattern) = def

            params = nnkFormalParams.newTree(bindSym"uint16")

            constantPattern = block:
                var pat: uint16
                for i, val in pairs pattern:
                    if val == '1':
                        pat.setMask(1'u16 shl (15-i))
                pat

            procResult = ident "result"
            body = newStmtList(quote do: `procResult` = `constantPattern`)

        for (fieldName, slice) in pairs parseFields(pattern):
            let sym = ident fieldName
            params.add nnkIdentDefs.newTree(sym, bindSym "uint16", newEmptyNode())
            body.add quote do:
                `procResult`.insertBits(`slice`, `sym`)

        result.add nnkProcDef.newTree(ident &"gen{name}_{suffix}",
            newEmptyNode(),
            newEmptyNode(),
            params,
            newEmptyNode(),
            newEmptyNode(),
            body)

generateAssemblyFuncs("main", DspPatterns)
generateAssemblyFuncs("parallel", DspSecondaryPatterns)

type
    InstrParam = enum
        adr0 = "r0"
        adr1 = "r1"
        adr2 = "r2"
        adr3 = "r3"
        inc0 = "m0"
        inc1 = "m1"
        inc2 = "m2"
        inc3 = "m3"
        wrap0 = "l0"
        wrap1 = "l1"
        wrap2 = "l2"
        wrap3 = "l3"
        callStack = "pcs"
        statusStack = "pss"
        loopAdrStack = "eas"
        loopCountStack = "lcs"
        a2 = "a2"
        b2 = "b2"
        dpp = "dpp"
        status = "psr"
        ps0 = "ps0"
        ps1 = "ps1"
        ps2 = "ps2"
        pc1 = "pc1"
        x0 = "x0"
        y0 = "y0"
        x1 = "x1"
        y1 = "y1"
        a0 = "a0"
        b0 = "b0"
        a1 = "a1"
        b1 = "b1"

        a = "a"
        b = "b"

        x = "x"
        y = "y"

        p = "p"

        im = "im"
        dp = "dp"
        xl = "xl"

        tb = "tb"
        sv = "sv"
        te0 = "te0"
        te1 = "te1"
        te2 = "te2"
        te3 = "te3"
        et = "et"

        ge
        lt
        gt
        le
        nz
        z
        nc
        c
        ne
        e
        nm
        m
        nt
        t
        v
        uncond = ""

    ParserError = object of CatchableError
    SemcheckError = object of CatchableError

    Location = object
        file: string
        line: int

    TokenKind = enum
        invalid
        eof
        ident
        intlit
        strlit
        comma
        colon
        newline
        parOpen
        parClose
        lSqrBracket
        rSqrBracket
        vBar
        dot
        at
        plus
        minus
        star
        slash
    Token = object
        case kind: TokenKind
        of invalid, eof, newline,
            comma, colon,
            parOpen, parClose,
            lSqrBracket, rSqrBracket,
            vBar, dot, at,
            plus, minus, star, slash:
            discard
        of ident, strlit:
            str: string
        of intlit:
            integer: int
        line: int

    NodeKind = enum
        intlit
        ident
        keyword
        instr
        label
        instrList
        directive
        deref
        negate
        add
        sub
        mul
        divi
        aand
        oor
        nnot
        xxor
        sshl
        sshr

const InstrParamTable = collect(initTable()):
    for par in adr0..et:
        {$par: par}

const NodeKindWithChildren = {instr, label, directive, instrList, deref,
    add, sub, mul, divi, negate, aand, nnot, oor, xxor, sshl, sshr}
type
    Node = ref object
        case kind: NodeKind
        of intlit:
            intlit: int64
        of ident:
            ident: string
        of keyword:
            keyword: InstrParam
        of NodeKindWithChildren:
            children: seq[Node]
        location: Location

    Parser = object of BaseLexer
        tokens: Deque[Token]

        file: string

    Assembler = object
        labels: Table[string, uint16]

        result: seq[uint16]

proc `$`(token: Token): string =
    case token.kind
    of ident: token.str
    of strlit: &"\"{token.str}\""
    of eof: "[end of file]"
    of comma: ","
    of colon: ":"
    of newline: "[new line]"
    of lSqrBracket: "["
    of rSqrBracket: "]"
    of vBar: "|"
    of dot: "."
    of at: "@"
    of plus: "+"
    of minus: "-"
    of star: "*"
    of slash: "/"
    of parOpen: "("
    of parClose: ")"
    of intlit: $token.integer
    of invalid: "[invalid token]"

proc skip(parser: var Parser) =
    var pos = parser.bufpos
    while true:
        case parser.buf[pos]
        of ' ', '\t':
            pos += 1
        of '#', ';':
            while not (parser.buf[pos] in {'\c', '\L', lexbase.EndOfFile}):
                pos += 1
        else:
            break
    parser.bufpos = pos

template `[]`(node: Node, i: int): Node = node.children[i]
template `[]=`(node: Node, i: int, val: Node) = node.children[i] = val
template `[]`(node: Node, i: BackwardsIndex): Node = node.children[i]
template `[]=`(node: Node, i: BackwardsIndex, val: Node) = node.children[i] = val
template len(node: Node): int = node.children.len
template add(node: Node, val: Node) = node.children.add val
iterator items(node: Node): Node =
    for item in node.children:
        yield item

proc raiseLexerError(parser: var Parser, message: string) =
    raise newException(ParserError, &"{parser.file}({parser.lineNumber}): {message}")

proc raiseParserError(parser: var Parser, token: Token, message: string) =
    raise newException(ParserError, &"{parser.file}({token.line}): {message}")

proc raiseSemcheckError(node: Node, message: string) =
    raise newException(SemcheckError, &"{node.location.file}({node.location.line}): {message}")

proc getTokRaw(parser: var Parser): Token =
    skip(parser)

    case parser.buf[parser.bufpos]
    of '0'..'9':
        var integer = 0
        if parser.buf[parser.bufpos] == '0' and parser.buf[parser.bufpos + 1] == 'x':
            parser.bufpos += 2
            parser.bufpos += parseHex(parser.buf, integer, parser.bufpos)
        else:
            parser.bufpos += parseInt(parser.buf, integer, parser.bufpos)
        result = Token(kind: TokenKind.intlit, integer: integer)
    of Letters:
        var token: string
        parser.bufpos += parseIdent(parser.buf, token, parser.bufpos)
        result = Token(kind: ident, str: token)
    of '"':
        parser.bufpos += 1
        var str: string

        var done = false
        while not done:
            case parser.buf[parser.bufpos]
            of '"':
                done = true
            of '\c', '\L':
                parser.raiseLexerError("strings have to be terminated on the same line")
            of '\\':
                parser.bufpos += 1
                case parser.buf[parser.bufpos]
                of 'n': str &= '\n'
                of 'c': str &= '\c'
                of '"': str &= '"'
                of '0': str &= '0'
                else: parser.raiseLexerError("invalid escape sequence")
            else:
                str &= parser.buf[parser.bufpos]

            parser.bufpos += 1

        result = Token(kind: strlit, str: str)
    of ',', ':', '[', ']', '(', ')', '|', '.', '@', '+', '-', '*', '/':
        result = Token(kind: (case parser.buf[parser.bufpos]
            of ',': TokenKind.comma
            of ':': TokenKind.colon
            of '[': TokenKind.lSqrBracket
            of ']': TokenKind.rSqrBracket
            of '|': TokenKind.vBar
            of '.': TokenKind.dot
            of '@': TokenKind.at
            of '+': TokenKind.plus
            of '-': TokenKind.minus
            of '*': TokenKind.star
            of '/': TokenKind.slash
            of '(': TokenKind.parOpen
            of ')': TokenKind.parClose
            else: raiseAssert("should not happen")))
        parser.bufpos += 1
    of '\c':
        result = Token(kind: newline)
        parser.bufpos = lexbase.handleCR(parser, parser.bufpos)
    of '\L':
        result = Token(kind: newline)
        parser.bufpos = lexbase.handleLF(parser, parser.bufpos)
    of EndOfFile:
        result = Token(kind: eof)
    else:
        parser.raiseLexerError(&"unexpected token {parser.buf[parser.bufpos]}")
    
    result.line = parser.lineNumber

proc getTok(parser: var Parser, idx: int): Token =
    while idx >= parser.tokens.len:
        parser.tokens.addLast parser.getTokRaw
    parser.tokens[idx]

proc advance(parser: var Parser, tokens: int) =
    parser.tokens.shrink tokens

proc newTree(kind: NodeKind, loc: Location, children: varargs[Node]): Node =
    case kind
    of NodeKindWithChildren:
        Node(kind: kind, location: loc, children: @children)
    else:
        raiseAssert(&"{kind} cannot be constructed with this method")

proc getLocation(parser: Parser, tok: Token): Location =
    Location(file: parser.file, line: tok.line)

proc newIntLit(intlit: int64, loc: Location): Node =
    Node(kind: NodeKind.intlit, intlit: intlit, location: loc)

proc newIdent(ident: string, loc: Location): Node =
    Node(kind: NodeKind.ident, ident: ident, location: loc)

proc newKeyword(keyword: InstrParam, loc: Location): Node =
    Node(kind: NodeKind.keyword, keyword: keyword, location: loc)

proc treeRepr(node: Node): string =
    var stack = @[(node, 0)]
    while stack.len > 0:
        let (topnode, depth) = stack.pop()
        result &= repeat(' ', depth*2)
        case topnode.kind
        of intlit:
            result.addInt topnode.intlit
            result &= ": intlit"
        of ident:
            result &= topnode.ident
            result &= ": ident"
        of keyword:
            result &= $topnode.keyword
            result &= ": keyword"
        of NodeKindWithChildren:
            result &= $topnode.kind
            for i in countdown(topnode.len-1, 0):
                stack.add((topnode[i], depth + 1))
        if stack.len > 0:
            result &= "\n"

proc classifyOperator(parser: var Parser, token: Token): Option[tuple[kind: NodeKind, precedence: int]] =
    case token.kind
    of plus: some((NodeKind.add, 1))
    of minus: some((NodeKind.sub, 1))
    of star: some((NodeKind.mul, 2))
    of slash: some((NodeKind.divi, 2))
    of ident:
        if token.str == "and":
            some((NodeKind.aand, 0))
        elif token.str == "or":
            some((NodeKind.oor, 0))
        elif token.str == "xor":
            some((NodeKind.xxor, 0))
        elif token.str == "shl":
            some((NodeKind.sshl, 3))
        elif token.str == "shr":
            some((NodeKind.sshr, 3))
        else:
            none((NodeKind, int))
    else: none((NodeKind, int))

proc parseExpr(parser: var Parser, lhs: Node, minPrecedence: int): Node

proc parseAtom(parser: var Parser): Node =
    let tok = parser.getTok(0)
    parser.advance(1)
    case tok.kind
    of ident:
        if tok.str == "not":
            newTree(NodeKind.nnot, parser.getLocation(tok), parser.parseAtom())
        else:
            newIdent(tok.str, parser.getLocation(tok))
    of intlit:
        newIntLit(tok.integer, parser.getLocation(tok))
    of minus:
        newTree(NodeKind.negate, parser.getLocation(tok), parser.parseAtom())
    of parOpen:
        let parNode = parser.parseExpr(parser.parseAtom(), 0)
        if parser.getTok(0).kind != parClose:
            parser.raiseLexerError("expected closing `)`")
        parser.advance(1)
        parNode
    else:
        parser.raiseParserError(tok, &"expected integer or identifier instead of {tok}")
        raiseAssert("unreachable")

proc parseExpr(parser: var Parser, lhs: Node, minPrecedence: int): Node =
    result = lhs
    while (let operator = parser.classifyOperator(parser.getTok(0)); operator.isSome and operator.get.precedence >= minPrecedence):
        parser.advance 1
        var rhs = parser.parseAtom()

        while (let nextOperator = parser.classifyOperator(parser.getTok(0));
            nextOperator.isSome and nextOperator.get.precedence > operator.get.precedence):
            rhs = parser.parseExpr(rhs, nextOperator.get.precedence)

        result = newTree(operator.get[0], parser.getLocation(parser.getTok(0)), result, rhs)

proc parseParams(parser: var Parser, addToNode: Node, endTokens: set[TokenKind]) =
    if parser.getTok(0).kind in endTokens:
        return

    while true:
        if parser.getTok(0).kind == lSqrBracket:
            parser.advance 1
            addToNode.add NodeKind.deref.newTree(parser.getLocation(parser.getTok(0)),
                parser.parseExpr(parser.parseAtom(), 0))
            if parser.getTok(0).kind != rSqrBracket:
                parser.raiseParserError(parser.getTok(0), &"expected closing `]` for dererencing expression {parser.getTok(0)}")
            parser.advance 1
        else:
            addToNode.add parser.parseExpr(parser.parseAtom(), 0)

        if parser.getTok(0).kind != comma:
            break
        parser.advance 1

proc parseFile(parser: var Parser): Node =
    result = instrList.newTree(parser.getLocation(parser.getTok(0)))
    while parser.getTok(0).kind != eof:
        let firstTok = parser.getTok(0)
        if firstTok.kind == newline:
            parser.advance 1
            continue

        if firstTok.kind in {Tokenkind.ident, intlit} and parser.getTok(1).kind == colon:
            result.add NodeKind.label.newTree(parser.getLocation(firstTok), newIdent($firstTok, parser.getLocation(firstTok)))
            parser.advance 2
        elif firstTok.kind == ident:
            parser.advance 1

            let instr = NodeKind.instr.newTree(parser.getLocation(firstTok), newIdent(firstTok.str, parser.getLocation(firstTok)))
            result.add instr
            parseParams(parser, instr, {newline, eof, vBar})
            if parser.getTok(0).kind == vBar:
                # parse parallel instruction
                let secondaryName = parser.getTok(1)

                if secondaryName.kind != ident:
                    parser.raiseParserError(parser.getTok(1), "expected identifier for parallel instruction")

                let parallelInstr = NodeKind.instr.newTree(parser.getLocation(firstTok),
                    newIdent(secondaryName.str, parser.getLocation(secondaryName)))

                instr.add parallelInstr
                parser.advance 2
                parseParams(parser, parallelInstr, {newline, eof})
        elif firstTok.kind == dot and (let name = parser.getTok(1); name.kind == ident):
            let directive =
                NodeKind.directive.newTree(parser.getLocation(name),
                    newIdent(name.str, parser.getLocation(name)))

            parser.advance 2
            parseParams(parser, directive, {newline, eof})
            result.add directive
        else:
            parser.raiseParserError(firstTok, &"expected instruction, directive or label instead of {firstTok}")

        if parser.getTok(0).kind notin {newline, eof}:
            parser.raiseParserError(parser.getTok(0), "expected newline")

proc isLongInstr(node: Node): bool =
    # pretty terrible hack but we save ourselves from having to do a
    # painful first pass to identify all the instructions

    const
        jmp = ["jmpge", "jmplt", "jmpgt", "jmple",
            "jmpnz", "jmpnc", "jmpc", "jmpne", "jmpe", "jmpnm",
            "jmpm", "jmpnt", "jmpt", "jmpv", "jmp"].toHashSet()
        longInstrs = ["callge", "calllt", "callgt", "callle",
            "callnz", "callnc", "callc", "callne", "calle", "callnm",
            "callm", "callnt", "callt", "callv", "call",
            "loop",
            "adli",
            "cmpli",
            "xorli",
            "anli",
            "orli",
            "ldla",
            "stla",
            "mvli",
            "stli",
            "btstl",
            "btsth"].toHashSet()

    if node[0].ident in jmp:
        node.len == 2 and node[1].kind != keyword
    else:
        node[0].ident in longInstrs

proc identifyRegs(node: var Node) =
    if node.kind == ident and InstrParamTable.hasKey(node.ident):
        node = newKeyword(InstrParamTable[node.ident], node.location)
    elif node.kind in NodeKindWithChildren:
        for child in mitems node.children:
            identifyRegs(child)

proc initParser(stream: Stream, file: string): Parser =
    open(result, stream)
    result.file = file

proc parseFile(filename: string): Node =
    let stream = newFileStream(filename, fmRead)
    var parser = initParser(stream, filename)
    result = parser.parseFile()
    stream.close()

proc evalImm(s: Assembler, node: Node): Option[int64] =
    case node.kind
    of intlit: some(node.intlit)
    of ident:
        if s.labels.hasKey(node.ident):
            some int64(s.labels[node.ident])
        else:
            node.raiseSemcheckError(&"unknown label {node.ident}")
            raiseAssert("should not happen")
    of add, sub, mul, divi, aand, oor, xxor, sshl, sshr:
        let
            a = s.evalImm(node[0])
            b = s.evalImm(node[1])

        if a.isSome and b.isSome:
            case node.kind
            of add: some(a.get + b.get)
            of sub: some(a.get - b.get)
            of mul: some(a.get * b.get)
            of divi: some(a.get div b.get)
            of aand: some(a.get and b.get)
            of oor: some(a.get or b.get)
            of xxor: some(a.get xor b.get)
            of sshl: some(a.get shl b.get)
            of sshr: some(a.get shr b.get)
            else: raiseAssert("should not happen")
        else:
            none(int64)
    of negate, nnot:
        let val = s.evalImm(node[0])
        if val.isSome:
            case node.kind
            of negate: some(-val.get)
            of nnot: some(not val.get)
            else: raiseAssert("should not happen")
        else:
            none(int64)
    else:
        none(int64)

macro genInstrAssembler(s: Assembler, instr: Node, ret: static[bool], body: untyped): untyped =
    var instrs: Table[string, NimNode]

    result = nnkCaseStmt.newTree(quote do: `instr`[0].ident)

    for entry in body:
        case entry
            of nnkCall[@name is nnkStrLit(), until @params(it == entry[^1]), nnkStmtList[nnkCall[@callName is nnkIdent(), all @callParams(it.kind == nnkExprEqExpr)], opt @li]]:
                var entry = instrs.getOrDefault($name)
                if entry == nil:
                    entry = nnkOfBranch.newTree(name, nnkStmtList.newTree())
                    result.add entry
                    instrs[$name] = entry

                let unpackIdents = nnkLetSection.newTree()
                for i, ident in pairs params:
                    unpackIdents.add nnkIdentDefs.newTree(ident, newEmptyNode(), quote do: `instr`[`i`+1])

                let
                    call = nnkCall.newTree(callName)
                    abortTry = nskLabel.genSym("abort")
                for param in callParams:
                    var letVal: NimNode
                    if param[0].kind == nnkTupleConstr:
                        letVal = nnkVarTuple.newTree()
                        for unpackElem in param[0]:
                            let sym = nskLet.genSym($unpackElem)
                            letVal.add sym
                            if $unpackElem != "_":
                                call.add nnkExprEqExpr.newTree(unpackElem, sym)
                    else:
                        let sym = nskLet.genSym($param[0])
                        letVal = nnkIdentDefs.newTree(sym)
                        if $param[0] != "_":
                            call.add nnkExprEqExpr.newTree(param[0], sym)

                    unpackIdents.add letVal
                    letVal.add newEmptyNode()
                    let eval = param[1]
                    letVal.add (quote do:
                        if (let val = `eval`; val.isSome):
                            val.get
                        else:
                            break `abortTry`)

                var addLi = newEmptyNode()
                if li.isSome:
                    if ret:
                        error("Cannot add a long immediate to small instruction")

                    let
                        li = li.get
                        val = nskLet.genSym("li")
                        letVal = nnkIdentDefs.newTree(val, newEmptyNode(), quote do:
                            if (let val = `li`; val.isSome):
                                val.get
                            else:
                                break `abortTry`)

                    unpackIdents.add letVal

                    addLi = quote do: `s`.result.add `val`

                let 
                    paramsCount = params.len+1

                    add =
                        if ret:
                            quote do: return some(`call`)
                        else:
                            quote do: `s`.result.add `call`

                    finalRet = if ret: newEmptyNode() else: (quote do: return)

                entry[^1].add(quote do:
                    if `instr`.len == `paramsCount`:
                        block `abortTry`:
                            `unpackIdents`
                            `add`
                            `addLi`
                            `finalRet`)
            else:
                error("Could not parse", instr)

    result.add nnkElse.newTree(quote do: `instr`.raiseSemcheckError(&"unknown instruction name {`instr`[0].ident}"); raiseAssert("should not happen"))

proc encodeKeywordRange[T](node: Node, first, last: T): Option[uint16] =
    if node.kind == keyword and node.keyword in first..last:
        some(uint16(node.keyword.ord - first.ord))
    else:
        none(uint16)

proc encodeSmallReg(node: Node): Option[uint16] =
    node.encodeKeywordRange(adr0, b1)

proc encodeMainAccum(node: Node): Option[uint16] =
    node.encodeKeywordRange(a, b)

proc encodeA1B1(node: Node): Option[uint16] =
    node.encodeKeywordRange(a1, b1)

proc encodeAddSub(d, s: Node): Option[(uint16, uint16)] =
    if d.kind == keyword and d.keyword in {a, b} and s.kind == keyword and d.keyword != s.keyword:
        let d = uint16(d.keyword.ord - a.ord)
        case s.keyword
        of x0: some((d, 0'u16))
        of y0: some((d, 1'u16))
        of x1: some((d, 2'u16))
        of y1: some((d, 3'u16))
        of x: some((d, 4'u16))
        of y: some((d, 5'u16))
        of a: some((d, 6'u16))
        of b: some((d, 6'u16))
        of p: some((d, 7'u16))
        else: none((uint16, uint16))
    else:
        none((uint16, uint16))

proc encodeXl(node: Node): Option[uint16] =
    if node.kind == keyword and node.keyword == xl:
        some(0'u16)
    else:
        none(uint16)

proc encodeImmS16(s: Assembler, node: Node): Option[uint16] =
    let integer = s.evalImm(node)
    if integer.isSome and integer.get in int64(low(int16))..int64(high(int16)):
        some(uint16(integer.get))
    else:
        none(uint16)

proc encodeImmU16(s: Assembler, node: Node): Option[uint16] =
    let integer = s.evalImm(node)
    if integer.isSome and integer.get in int64(low(uint16))..int64(high(uint16)):
        some(uint16(integer.get))
    else:
        none(uint16)

proc encodeImmU8(s: Assembler, node: Node): Option[uint16] =
    let integer = s.evalImm(node)
    if integer.isSome and integer.get in int64(low(uint8))..int64(high(uint8)):
        some(uint16 uint8(integer.get))
    else:
        none(uint16)

proc encodeImmS8(s: Assembler, node: Node): Option[uint16] =
    let integer = s.evalImm(node)
    if integer.isSome and integer.get in low(int8)..high(int8):
        some(uint16 uint8(integer.get))
    else:
        none(uint16)

proc encodeDerefU8(s: Assembler, node: Node): Option[uint16] =
    if node.kind == deref:
        s.encodeImmU8(node[0])
    else:
        none(uint16)

proc encodeDerefU16(s: Assembler, node: Node): Option[uint16] =
    if node.kind == deref:
        s.encodeImmU16(node[0])
    else:
        none(uint16)

proc encodeCond(node: Node): Option[uint16] =
    if node.kind == keyword and node.keyword in ge..uncond:
        some(uint16(node.keyword.ord - ge.ord))
    else:
        none(uint16)

proc encodeAdrReg(node: Node): Option[uint16] =
    if node.kind == keyword and node.keyword in adr0..adr3:
        some(uint16(node.keyword.ord - adr0.ord))
    else:
        none(uint16)

proc encodeModAdr(s: Assembler, rn, mn: Node): Option[(uint16, uint16)] =
    if rn.kind == keyword and rn.keyword in adr0..adr3:
        let adrReg = uint16(rn.keyword.ord - adr0.ord)
        if mn.kind == keyword and mn.keyword in inc0..inc3:
            if uint16(mn.keyword.ord - inc0.ord) == adrReg:
                return some((adrReg, 3'u16))
        elif (let inc = s.evalImm(mn); inc.isSome):
            return (case inc.get
                of 0: some((adrReg, 0'u16))
                of -1: some((adrReg, 1'u16))
                of 1: some((adrReg, 2'u16))
                else: none((uint16, uint16)))
    return none((uint16, uint16))

proc encodeModAdrDeref(s: Assembler, rn, mn: Node): Option[(uint16, uint16)] =
    if rn.kind == deref:
        s.encodeModAdr(rn[0], mn)
    else:
        none((uint16, uint16))

proc encodeUpperShortReg(node: Node): Option[uint16] =
    node.encodeKeywordRange(x0, b1)

proc encodeStsaSrc(node: Node): Option[uint16] =
    if node.kind == keyword:
        case node.keyword
        of a2: some(0'u16)
        of b2: some(1'u16)
        of a0: some(4'u16)
        of b0: some(5'u16)
        of a1: some(6'u16)
        of b1: some(7'u16)
        else: none(uint16)
    else:
        none(uint16)

proc encodeLargeMr(s: Assembler, rn, mn: Node): Option[(uint16, uint16)] =
    if rn.kind == keyword and rn.keyword in adr0..adr3:
        let adrReg = uint16(rn.keyword.ord - adr0.ord)
        if mn.kind == keyword and mn.keyword in inc0..inc3:
            return some((adrReg, 4'u16 + uint16(mn.keyword.ord-inc0.ord)))
        elif mn.kind == negate and mn[0].kind == keyword and mn[0].keyword in inc0..inc3 and
            uint16(mn[0].keyword.ord-inc0.ord) == adrReg:
            return some((adrReg, 3'u16))
        elif (let inc = s.evalImm(mn); inc.isSome):
            return (case inc.get
                of 0: some((adrReg, 0'u16))
                of -1: some((adrReg, 1'u16))
                of 1: some((adrReg, 2'u16))
                else: none((uint16, uint16)))
    return none((uint16, uint16))

proc encodeSecondaryInstr(asmb: Assembler, instr: Node): Option[uint16] =
    if instr.kind == NodeKind.instr:
        asmb.genInstrAssembler instr, true:
            "nop"():
                genmr_parallel((rr, mm) = some((0'u16, 0'u16)))
            "mr"(rn, mn):
                genmr_parallel((rr, mm) = asmb.encodeModAdr(rn, mn))
            "mv"(d, s):
                genmv_parallel(dd = encodeKeywordRange(d, x0, y1), ss = encodeKeywordRange(s, a0, b1))

    return none(uint16)

proc encodeSecondary8(asmb: Assembler, instr: Node): Option[uint16] =
    result = asmb.encodeSecondaryInstr(instr)
    if result.isSome and (result.get and 0x80) != 0:
        result = none(uint16)

proc encodeMainInstr(asmb: var Assembler, instr: Node) =
    const condInstrs = collect(initTable()):
        for instr in ["jmp", "call", "rets", "reti", "exec"]:
            for cond in InstrParam.ge..uncond:
                {(instr & $cond): (instr, cond)}

    if (let (name, cond) = condInstrs.getOrDefault(instr[0].ident); name.len > 0):
        instr[0] = newIdent(name, instr[0].location)
        instr.children.insert(newKeyword(cond, instr[0].location), 1)

    asmb.genInstrAssembler instr, false:
        "jmp"(cc, ta):
            genjmp_main(cccc = encodeCond(cc))
            asmb.encodeImmU16(ta)
        "jmp"(cc, rn):
            genjmpr_main(cccc = encodeCond(cc), rr = encodeAdrReg(rn))
        "call"(cc, ta):
            gencall_main(cccc = encodeCond(cc))
            asmb.encodeImmU16(ta)
        "call"(cc, rn):
            gencallr_main(cccc = encodeCond(cc), rr = encodeAdrReg(rn))
        "rets"(cc):
            genrets_main(cccc = encodeCond(cc))
        "reti"(cc):
            genreti_main(cccc = encodeCond(cc))
        "trap"():
            gentrap_main()
        "wait"():
            genwait_main()
        "exec"(cc):
            genexec_main(cccc = encodeCond(cc))
        "loop"(lc, ea):
            genloopi_main(cccccccc = asmb.encodeImmU8(lc))
            asmb.encodeImmU16(ea)
        "loop"(reg, ea):
            genloop_main(rrrrr = encodeSmallReg(reg))
            asmb.encodeImmU16(ea)
        "rep"(rc):
            genrepi_main(cccccccc = asmb.encodeImmU8(rc))
        "rep"(reg):
            genrep_main(rrrrr = encodeSmallReg(reg))
        "pld"(d, rn, mn):
            genpld_main(d = encodeA1B1(d), (rr, mm) = asmb.encodeModAdrDeref(rn, mn))
        "nop"():
            genmr_main((mmm, rr) = some((0'u16, 0'u16)))
        "nop"(secondary):
            genpnop_main(xxxxxxxx = asmb.encodeSecondary8(secondary))

        "mv"(d, s):
            genmv_main(ddddd = encodeSmallReg(d), sssss = encodeSmallReg(s))

        "cmpsi"(s, si):
            gencmpsi_main(s = encodeMainAccum(s), iiiiiiii = asmb.encodeImmS8(si))
        "cmpli"(s, li):
            gencmpli_main(s = encodeMainAccum(s))
            asmb.encodeImmS16(li)

        "adsi"(d, si):
            genadsi_main(d = encodeMainAccum(d), iiiiiiii = asmb.encodeImmS8(si))
        "adli"(d, li):
            genadli_main(d = encodeMainAccum(d))
            asmb.encodeImmS16(li)

        "clr"(d, secondary):
            genclra_main(d = encodeMainAccum(d), xxxxxxxx = asmb.encodeSecondary8(secondary))

        "mr"(rn, mn):
            genmr_main((rr, mmm) = asmb.encodeLargeMr(rn, mn))

        "xorli"(d, li):
            genxorli_main(d = encodeMainAccum(d))
            asmb.encodeImmU16(li) 
        "anli"(d, li):
            genanli_main(d = encodeMainAccum(d))
            asmb.encodeImmU16(li)
        "orli"(d, li):
            genorli_main(d = encodeMainAccum(d))
            asmb.encodeImmU16(li)

        "add"(d, s, secondary):
            genadd_main((sss, d) = encodeAddSub(d, s), xxxxxxxx = asmb.encodeSecondary8(secondary))
        "clr"(reg, secondary):
            genclra_main(d = encodeMainAccum(reg), xxxxxxxx = asmb.encodeSecondary8(secondary))
        "clr"(xl, secondary):
            genclrxl_main(_ = xl.encodeXl(), xxxxxxxx = asmb.encodeSecondary8(secondary))
        "set"(xl, secondary):
            gensetxl_main(_ = xl.encodeXl(), xxxxxxxx = asmb.encodeSecondary8(secondary))

        "adli"(d, li):
            genadli_main(d = encodeMainAccum(d))
            asmb.encodeImmS16(li)

        "ldsa"(d, sa):
            genldsa_main(ddd = encodeUpperShortReg(d), aaaaaaaa = asmb.encodeDerefU8(sa))
        "stsa"(sa, s):
            genstsa_main(sss = encodeStsaSrc(s), aaaaaaaa = asmb.encodeDerefU8(sa))

        "ldla"(d, la):
            genldla_main(ddddd = encodeSmallReg(d))
            asmb.encodeDerefU16(la)
        "stla"(la, s):
            genstla_main(sssss = encodeSmallReg(s))
            asmb.encodeDerefU16(la)

        "stli"(sa, li):
            genstli_main(aaaaaaaa = asmb.encodeDerefU8(sa))
            asmb.encodeImmU16(li)

        "btstl"(d, bs):
            genbtstl_main(d = encodeA1B1(d))
            asmb.encodeImmU16(bs)
        "btsth"(d, bs):
            genbtsth_main(d = encodeA1B1(d))
            asmb.encodeImmU16(bs)

        "mvsi"(d, si):
            genmvsi_main(ddd = encodeUpperShortReg(d), iiiiiiii = asmb.encodeImmS8(si))
        "mvli"(d, li):
            genmvli_main(ddddd = encodeSmallReg(d))
            asmb.encodeImmS16(li)

    instr.raiseSemcheckError(&"parameters mismatch for instr {instr[0].ident}")

proc expectParams(node: Node, n: int) =
    if node.len != n + 1:
        node.raiseSemcheckError(&"expected {n} parameters")

proc assemble(ast: Node, startAdr: uint16): seq[uint16] =
    # process directives
    for node in ast.children:
        if node.kind == directive:
            case node[0].ident:
            of "dw", "equ": discard # will be handled during code generation
            #of "include": discard
            else: node.raiseSemcheckError(&"unknown directive {node[0].ident}")

    # identify all special words
    for node in ast:
        if node.kind == instr:
            for i in 1..<node.len:
                identifyRegs(node[i])

    var s: Assembler
    s.labels["CMBH"] = 0xFFFE'u16
    s.labels["CMBL"] = 0xFFFF'u16
    s.labels["DMBH"] = 0xFFFC'u16
    s.labels["DMBL"] = 0xFFFD'u16

    s.labels["DSMAH"] = 0xFFCE'u16
    s.labels["DSMAL"] = 0xFFCF'u16
    s.labels["DSPA"] = 0xFFCD'u16
    s.labels["DSCR"] = 0xFFC9'u16
    s.labels["DSBL"] = 0xFFCB'u16

    s.labels["ACSAH"] = 0xFFD4'u16
    s.labels["ACSAL"] = 0xFFD5'u16
    s.labels["ACEAH"] = 0xFFD6'u16
    s.labels["ACEAL"] = 0xFFD7'u16
    s.labels["ACCAH"] = 0xFFD8'u16
    s.labels["ACCAL"] = 0xFFD9'u16
    s.labels["ACDAT"] = 0xFFDD'u16
    s.labels["DIRQ"] = 0xFFFB'u16

    # collect all labels and calculate their addresses
    block:
        var offset = startAdr
        for node in ast:
            case node.kind
            of label:
                if node[0].ident in InstrParamTable:
                    node[0].raiseSemcheckError(&"{node[0].ident} is a reserved parameter")
                if node[0].ident in s.labels:
                    node[0].raiseSemcheckError("duplicate label")
                s.labels[node[0].ident] = offset
            of instr:
                offset += (if node.isLongInstr(): 2 else: 1)
            of directive:
                case node[0].ident
                of "dw": offset += 1
                else: discard
            else: discard

    # generate code
    for node in ast:
        if node.kind == directive:
            case node[0].ident
            of "dw":
                node.expectParams 1
                if (let param = s.encodeImmU16(node[1]); param.isSome):
                    s.result.add param.get
                else:
                    node.raiseSemcheckError("expected integer")
            of "equ":
                node.expectParams 2
                let name = node[1]
                if name.kind != ident:
                    name.raiseSemcheckError("expected identifier")
                let value = s.encodeImmU16(node[2])
                if value.isNone:
                    node[2].raiseSemcheckError("expected integer")
                s.labels[name.ident] = value.get
        elif node.kind == instr:
            s.encodeMainInstr(node)

    s.result

proc dspasm(startAdr = 0'u16, cHeader = false, input, output: string) =
    var bin = assemble(parseFile(input), startAdr)

    let outputFile = newFileStream(output, fmWrite)
    if cHeader:
        let varName = output.splitFile().name
        outputFile.writeLine(&"u16 {varName}[] __attribute__((aligned(32))) = {'{'}")
        var lineBreak = 10
        for word in bin:
            outputFile.write(&"0x{word:04X},") 
            lineBreak -= 1
            if lineBreak == 0:
                outputFile.writeLine("")
                lineBreak = 20
        outputFile.writeLine("\n};")
    else:
        # convert to big endian
        for word in mitems bin:
            word = toBE word
        outputFile.writeData(unsafeAddr bin[0], 2*bin.len)
    outputFile.close()

dispatch(dspasm)
