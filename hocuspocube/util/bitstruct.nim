import
    macros, bitops, tables

proc exportifyIdent(node: NimNode, `export`: bool): NimNode =
    if `export`:
        nnkPostfix.newTree(ident"*", node)
    else:
        node

macro makeBitStruct*(baseTyp: typedesc, name, body: untyped): untyped =
    name.expectKind {nnkPrefix, nnkIdent}
    if name.kind == nnkPrefix and $name[0] != "*":
        error("for exporting prefix with a star(*)!", name)
    let
        `export` = name.kind == nnkPrefix
        name = if `export`: name[1] else: name
        nameDeclaration = exportifyIdent(name, `export`)

    result = newStmtList(quote do:
        type `nameDeclaration` = distinct `baseTyp`)

    result.add((quote("@") do:
        proc `==`*(a, b: @name): bool {.borrow.}))

    var
        tagMasks: Table[string, uint64]

    for field in body:
        field.expectKind nnkCall

        let
            typ = field[1][0]
            location = if field[0].kind == nnkPragmaExpr:
                field[0][0] else:
                field[0]

        location.expectKind nnkBracketExpr

        if field[0].len > 2:
            # assume it's parameterised
            let
                procName = exportifyIdent(location[0], `export`)
                paramName = location[1]

                isRange = location[2].kind == nnkInfix and $location[2][0] == ".."
                a = if isRange: location[2][1] else: location[2]
                b = if isRange: location[2][2] else: a

            result.add(quote do:
                proc `procName`(struct: `name`, `paramName`: int): `typ` {.inline, used.} =
                    let
                        a = `a`
                        b = `b`
                        mask = toMask[`baseTyp`](Slice[int](a: a, b: b))
                    `typ`(bitand(`baseTyp`(struct), mask) shr a)
                proc `procName`(struct: var `name`, `paramName`: int, newVal: `typ`) {.inline, used.} =
                    let
                        a = `a`
                        b = `b`
                        mask = toMask[`baseTyp`](Slice[int](a: a, b: b))
                    struct = `name` bitor(bitand(`baseTyp`(struct), bitnot mask), bitand(`baseTyp`(newVal) shl a, mask)))
        else:
            location[1].expectKind {nnkInfix, nnkIntLit}
            if location[1].kind == nnkInfix and $location[1][0] != "..":
                error("expected either a bit range or a single bit")

            let
                slice = if location[1].kind == nnkInfix:
                    Slice[int](a: int location[1][1].intVal, b: int location[1][2].intVal) else:
                    Slice[int](a: int location[1].intVal, b: int location[1].intVal)

                mask = toMask[uint64](slice)

            if field[0].kind == nnkPragmaExpr:
                for tag in field[0][1]:
                    tagMasks.mgetOrPut($tag, 0'u64).setMask(uint64 mask)

            if $location[0] != "_":
                let
                    getterName = exportifyIdent(location[0], `export`)
                    setterName = exportifyIdent(ident($location[0] & "="), `export`)

                    shift = slice.a
                    invMask = newLit(not mask)

                result.add(quote do:
                    proc `getterName`(struct: `name`): `typ` {.inline, used.} =
                        `typ`((`baseTyp`(struct) and `mask`) shr `shift`)
                    proc `setterName`(struct: var `name`, newVal: `typ`) {.inline, used.} =
                        struct = `name`((`baseTyp`(struct) and `invMask`) or ((`baseTyp`(newVal) shl `shift`) and `mask`)))

    for tagName, mask in tagMasks:
        let
            getterName = exportifyIdent(ident tagName, `export`)
            setterName = exportifyIdent(ident(tagName & "="), `export`)

            invMask = newLit(not mask)

        result.add(quote do:
            proc `getterName`(struct: `name`): `baseTyp` {.inline, used.} =
                `baseTyp`(struct) and `mask`
            proc `setterName`(struct: var `name`, newVal: `baseTyp`) {.inline, used.} =
                struct = `name`((`baseTyp`(struct) and `invMask`) or (newVal and `mask`)))