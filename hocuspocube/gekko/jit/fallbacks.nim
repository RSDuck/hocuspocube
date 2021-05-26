import
    macros,

    ../../util/instrdecoding,
    ../ppcdef, ../ppcstate,
    ../interpreter/ppcinterpreter_int,
    ../interpreter/ppcinterpreter_loadstore,
    ../interpreter/ppcinterpreter_system,
    ../interpreter/ppcinterpreter_branch,
    ../interpreter/ppcinterpreter_float

{.experimental: "dynamicBindSym".}

macro generateFallbacks(): untyped =
    result = newStmtList()

    for instrPattern in PpcPatterns:
        let
            state = nskParam.genSym"state"
            instr = nskParam.genSym"instr"

            body = newStmtList(nnkLetSection.newNimNode())
            (name, pattern) = instrPattern
            fields = parseFields(pattern)
            syms = generateFields(instr, body[0], fields)

        # pretty evil
        body.add(nnkCall.newTree(bindSym name, state))
        for sym in syms:
            body[^1].add sym
        body.add(quote do: `state`.pc += 4)

        result.add newProc(nnkPostfix.newTree(ident"*", ident name),
            [newEmptyNode(),
                newIdentDefs(state, nnkVarTy.newTree(bindSym"PpcState")),
                newIdentDefs(instr, bindSym"uint32")],
            body)


generateFallbacks()