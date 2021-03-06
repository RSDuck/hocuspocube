import
    stew/endians2,
    macros, strformat, strutils, bitops

proc sizeToType(size: uint32): NimNode {.compiletime.} =
    case size:
    of 1: bindSym"uint8"
    of 2: bindSym"uint16"
    of 4: bindSym"uint32"
    of 8: bindSym"uint64"
    else: error("invalid size (only 1/2/4 allowed)"); nil

macro ioBlock*(name: untyped, size: static[uint32], regs: varargs[untyped]): untyped =
    var
        associations = newSeq[(uint32, int)](size)

        writeRegistersProcs = @[NimNode(nil)]
        readRegistersProc = @[NimNode(nil)]

        regNames = @["unknown"]

    result = newStmtList()

    for i, reg in pairs regs:
        reg.expectLen 4, 6
        reg[0].expectKind nnkIdent
        reg[1].expectKind nnkIntLit
        reg[2].expectKind nnkIntLit

        var
            adr = reg[1].intVal
            writeProc = NimNode nil
            readProc = NimNode nil

        let
            name = $reg[0]
            regSize = reg[2].intVal

            repeats = if reg.len >= 5: (reg[3].expectKind nnkIntLit; reg[3].intVal) else: 1
            stride = if reg.len == 6: (reg[4].expectKind nnkIntLit; reg[4].intVal) else: regSize

            underlyingTyp = sizeToType(uint32 regSize)

        for child in reg[^1]:
            child.expectKind nnkCall
            child.expectLen 2
            child[0].expectKind nnkIdent

            let
                body = child[^1]
                idxIdent = ident"idx"

            case $child[0]
            of "read":
                let
                    procName = nskProc.genSym(name & "Read")

                result.add(quote do:
                    proc `procName`(addrFull: uint32): `underlyingTyp` =
                        let `idxIdent` {.used.} = (addrFull - `adr`) div `stride`
                        `body`)

                readProc = procName
            of "write":
                let
                    valueIdent = ident"val"
                    procName = nskProc.genSym(name & "Write")

                result.add(quote do:
                    proc `procName`(addrFull: uint32, `valueIdent`: `underlyingTyp`) =
                        let `idxIdent` {.used.} = (addrFull - `adr`) div `stride`
                        `body`)

                writeProc = procName
            else: error("invalid register access (allowed are read and write)", child[0])

        if readProc == nil:
            error(&"register {name} has no read implementation", reg)

        for repeat in 0..<repeats:
            readRegistersProc.add readProc
            writeRegistersProcs.add writeProc

            if (adr and (regSize - 1)) != 0:
                error("unaligned register", reg)

            for j in adr..<adr+regSize:
                if associations[j][1] != 0:
                    error("conflict between " & name & " and " & regNames[associations[j][1]], reg)
                associations[j] = (uint32 regSize, regNames.len)

            if repeats == 1:
                regNames.add name
            else:
                regNames.add name & $repeat

            adr += stride

    let
        readName = ident($name & "Read")
        writeName = ident($name & "Write")

        addrSymRead = nskParam.genSym("adr")
        addrSymWrite = nskParam.genSym("adr")
        valueSym = nskParam.genSym("value")

        readCases = [nnkCaseStmt.newNimNode(), nnkCaseStmt.newNimNode(), nnkCaseStmt.newNimnode(), nnkCaseStmt.newNimNode()]
        writeCases = [nnkCaseStmt.newNimNode(), nnkCaseStmt.newNimNode(), nnkCaseStmt.newNimNode(), nnkCaseStmt.newNimNode()]

        regBankName = $name

    for i in 0..<4:
        let
            accessSize = 1'u32 shl i
            mask = accessSize - 1
            addressMask = (not mask) and (size - 1)

            accessTyp = sizeToType(accessSize)
        readCases[i].add(nnkInfix.newTree(bindSym"and", addrSymRead, newLit addressMask))
        writeCases[i].add(nnkInfix.newTree(bindSym"and", addrSymWrite, newLit addressMask))

        # endianess makes this all complicated
        # because the values in/out are in pseudo little endian
        var adr = 0'u32
        while adr < size:
            if associations[adr][0] >= accessSize:
                let
                    idx = associations[adr][1]
                    readProc = readRegistersProc[idx]
                    writeProc = writeRegistersProcs[idx]

                    regSize = associations[adr][0]
                    regMask = regSize - 1
                if regSize == accessSize:
                    # it's a match, how lovely
                    if readProc != nil:
                        readCases[i].add(nnkOfBranch.newTree(newLit adr,
                            quote do: toBE `readProc`(`adr`)))
                    if writeProc != nil:
                        writeCases[i].add(nnkOfBranch.newTree(newLit adr,
                            quote do: `writeProc`(`adr`, fromBE `valueSym`)))
                    adr += accessSize
                else:
                    # only a part of the register is accessed
                    if readProc != nil:
                        readCases[i].add nnkOfBranch.newTree(nnkInfix.newTree(bindSym"..", newLit adr, newLit(adr + regSize - 1)),
                            quote do: `accessTyp`(toBE(`readProc`(`adr`)) shr ((`addrSymRead` and `regMask`) * 8)))
                    if writeProc != nil:
                        let
                            sizeMask = toMask[uint32](0..int(accessSize)*8-1)
                            regTyp = sizeToType(regSize)
                        writeCases[i].add nnkOfBranch.newTree(nnkInfix.newTree(bindSym"..", newLit adr, newLit(adr + regSize - 1)),
                            quote do:
                                let
                                    writeShift = `regTyp`((`addrSymWrite` and `regMask` xor (`regMask` and not(`mask`))) * 8)
                                    writeMask = `regTyp`(`sizeMask`) shl writeShift

                                `writeProc`(`adr`, (`readProc`(`adr`) and not(writeMask)) or (`regTyp`(fromBE(`valueSym`)) shl writeShift)))
                    adr += regSize
            else:
                # multiple registers are accessed at once

                let
                    readBranch = nnkOfBranch.newTree(newLit adr)
                    writeBranch = nnkOfBranch.newTree(newLit adr, newStmtList())

                var
                    readValue = newLit(0)

                    offset = 0'u32
                    invOffset = accessSize * 8

                    atleastOneRead = false
                    atleastOneWrite = false
                    partiallyUnknownRead = false
                    partiallyUnknownWrite = false
                while offset < accessSize:
                    let
                        (regSize, regIdx) = associations[adr]
                    
                        readProc = readRegistersProc[regIdx]
                        writeProc = writeRegistersProcs[regIdx]

                        regTyp = if regSize > 0: sizeToType(regSize) else: nil

                    invOffset -= regSize * 8

                    if readProc != nil:
                        atleastOneRead = true
                        readValue = quote do: `readValue` or (`accessTyp`(`readProc`(`adr`)) shl `invOffset`)
                    else:
                        partiallyUnknownRead = true
                    if writeProc != nil:
                        atleastOneWrite = true
                        writeBranch[1].add quote do: `writeProc`(`adr`, `regTyp`(fromBE(`valueSym`) shr `invOffset`))
                    else:
                        partiallyUnknownWrite = true

                    offset += max(regSize, 1)
                    adr += max(regSize, 1)

                let bitsize = accessSize * 8

                if atleastOneRead:
                    readBranch.add(if partiallyUnknownRead: (quote do:
                            echo "partially unknown ", `regBankName`, " read ", `bitsize`, " ",
                                toHex`addrSymRead`; toBE(`readValue`)) else:
                        quote do: toBE `readValue`)

                    readCases[i].add readBranch
                if atleastOneWrite:
                    if partiallyUnknownWrite:
                        writeBranch[1].add quote do:
                            echo "partially unknown ", `regBankName`, " write ", `bitsize` ," ",
                                toHex`addrSymWrite`, " ", `valueSym`
                    writeCases[i].add writeBranch
    for i in 0..<4:
        let bitsize = 8 shl i
        readCases[i].add(nnkElse.newTree(quote do:
            echo "unknown ", `regBankName`, " read ", `bitsize`, " ", toHex`addrSymRead`; 0))
        writeCases[i].add(nnkElse.newTree(quote do:
            echo "unknown ", `regBankName`, " write ", `bitsize` ," ", toHex`addrSymWrite`, " ", toHex fromBE(`valueSym`)))

    block:
        let
            read8 = readCases[0]
            read16 = readCases[1]
            read32 = readCases[2]
            read64 = readCases[3]

            write8 = writeCases[0]
            write16 = writeCases[1]
            write32 = writeCases[2]
            write64 = writeCases[3]

        result.add(quote do:
            proc `readName`*[T](`addrSymRead`: uint32): T =
                when T is uint64:
                    `read64`
                elif T is uint32:
                    `read32`
                elif T is uint16:
                    `read16`
                elif T is uint8:
                    `read8`

            proc `writeName`*[T](`addrSymWrite`: uint32, `valueSym`: T) =
                when T is uint64:
                    `write64`
                elif T is uint32:
                    `write32`
                elif T is uint16:
                    `write16`
                elif T is uint8:
                    `write8`)
    #echo result.repr

when isMainModule:
    ioBlock test, 0x20:
    of blargcsr, 0x00, 4:
        read: 0
        write: echo val
    of blargbox, 0x10, 1:
        read: 42
        write: discard
    of blargbox2, 0x11, 1:
        read: 43
        write: discard
    of blargbox3, 0x12, 1:
        read: 45
    of dadada, 0x6, 2:
        read: 0xFEED
    of blargnarg, 0x04, 2:
        read: 32
        write: discard

    for i in countup(0, 0x20 - 1, 4):
        let
            val32 = testRead[uint32](uint32 i)
            val16 = (uint32(testRead[uint16](uint32 i)) shl 0) or (uint32(testRead[uint16](uint32 i + 2)) shl 16)
            val8 = (uint32(testRead[uint8](uint32 i)) shl 0) or (uint32(testRead[uint8](uint32 i + 1)) shl 8) or
                (uint32(testRead[uint8](uint32 i + 2)) shl 16) or (uint32(testRead[uint8](uint32 i + 3)) shl 24)

        doAssert val32 == val16, &"failure at address {i:X} {val32:X} != {val16:X}"
        doAssert val32 == val8, &"failure at address {i:X} {val32:X} != {val8:X}"