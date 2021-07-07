import
    macros,

    instrdecoding

macro generateFallbacks*(patterns: static[seq[(string, string)]], stateTyp: typedesc, instrTyp, module, pcStep: untyped): untyped =
    result = newStmtList()

    for instrPattern in patterns:
        let
            state = nskParam.genSym"state"
            instr = nskParam.genSym"instr"

            body = newStmtList(nnkLetSection.newNimNode())
            (name, pattern) = instrPattern
            fields = parseFields(pattern)
            syms = generateFields(instr, body[0], fields)

        body.add(nnkCall.newTree(nnkDotExpr.newTree(module, ident name), state))
        for sym in syms:
            body[^1].add sym
        body.add (quote do: `state`.pc += `pcStep`)

        result.add newProc(nnkPostfix.newTree(ident"*", ident name),
            [newEmptyNode(),
                newIdentDefs(state, nnkVarTy.newTree(stateTyp.getType()[1])),
                newIdentDefs(instr, instrTyp)],
            body)
