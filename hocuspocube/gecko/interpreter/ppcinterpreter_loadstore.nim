import
    ../ppcstate, ../memory, ../../util/aluhelper,
    ppcinterpreter_aux,
    options, stew/endians2,
    strformat

using state: var PpcState

template r(num: uint32): uint32 {.dirty.} = state.r[num]
template fr(num: uint32): PairedSingle {.dirty.} = state.fr[num]

template calcAddrImm(update: bool, body: untyped): untyped {.dirty.} =
    let ea = (if not update and a == 0: 0'u32 else: r(a)) + signExtend(imm, 16)

    body

    if update:
        r(a) = ea

template calcAddr(update: bool, body: untyped): untyped {.dirty.} =
    let ea = (if not update and a == 0: 0'u32 else: r(a)) + r(b)
    
    body

    if update:
        r(a) = ea

template doMemOp(body: untyped): untyped {.dirty.} =
    if (let `addr` = state.translateDataAddr(ea); `addr`.isSome):
        body
    else:
        return

template loadByte: untyped {.dirty.} =
    doMemOp:
        r(d) = uint32 readPhysical[uint8](`addr`.get)

template loadHalf(rev = false): untyped {.dirty.} =
    doMemOp:
        let val = readPhysical[uint16](`addr`.get)
        if rev:
            r(d) = val
        else:
            r(d) = fromBE val

template loadHalfAlgebraic: untyped {.dirty.} =
    doMemOp:
        r(d) = signExtend(uint32 fromBE readPhysical[uint16](`addr`.get), 16)

template loadWord(rev = false): untyped {.dirty.} =
    doMemOp:
        let val = readPhysical[uint32](`addr`.get)
        if rev:
            r(d) = val
        else:
            r(d) = fromBE val

template storeByte: untyped {.dirty.} =
    doMemOp:
        writePhysical[uint8](`addr`.get, uint8 r(s))

template storeHalf(rev = false): untyped {.dirty.} =
    doMemOp:
        writePhysical[uint16](`addr`.get, if rev: uint16 r(s) else: toBE uint16 r(s))

template storeWord(rev = false): untyped {.dirty.} =
    doMemOp:
        writePhysical[uint32](`addr`.get, if rev: uint32 r(s) else: toBE uint32 r(s))

proc lbz*(state; d, a, imm: uint32) =
    calcAddrImm false:
        loadByte

proc lbzu*(state; d, a, imm: uint32) =
    calcAddrImm true:
        loadByte

proc lbzux*(state; d, a, b: uint32) =
    calcAddr true:
        loadByte

proc lbzx*(state; d, a, b: uint32) =
    calcAddr false:
        loadByte

proc lha*(state; d, a, imm: uint32) =
    calcAddrImm false:
        loadHalfAlgebraic

proc lhau*(state; d, a, imm: uint32) =
    calcAddrImm true:
        loadHalfAlgebraic

proc lhaux*(state; d, a, b: uint32) =
    calcAddr true:
        loadHalfAlgebraic

proc lhax*(state; d, a, b: uint32) =
    calcAddr false:
        loadHalfAlgebraic

proc lhz*(state; d, a, imm: uint32) =
    calcAddrImm false:
        loadHalf

proc lhzu*(state; d, a, imm: uint32) =
    calcAddrImm true:
        loadHalf

proc lhzux*(state; d, a, b: uint32) =
    calcAddr true:
        loadHalf

proc lhzx*(state; d, a, b: uint32) =
    calcAddr false:
        loadHalf

proc lwz*(state; d, a, imm: uint32) =
    calcAddrImm false:
        loadWord

proc lwzu*(state; d, a, imm: uint32) =
    calcAddrImm true:
        loadWord

proc lwzux*(state; d, a, b: uint32) =
    calcAddr true:
        loadWord

proc lwzx*(state; d, a, b: uint32) =
    calcAddr false:
        loadWord

proc stb*(state; s, a, imm: uint32) =
    calcAddrImm false:
        storeByte

proc stbu*(state; s, a, imm: uint32) =
    calcAddrImm true:
        storeByte

proc stbux*(state; s, a, b: uint32) =
    calcAddr true:
        storeByte

proc stbx*(state; s, a, b: uint32) =
    calcAddr false:
        storeByte

proc sth*(state; s, a, imm: uint32) =
    calcAddrImm false:
        storeHalf

proc sthu*(state; s, a, imm: uint32) =
    calcAddrImm true:
        storeHalf

proc sthux*(state; s, a, b: uint32) =
    calcAddr true:
        storeHalf

proc sthx*(state; s, a, b: uint32) =
    calcAddr false:
        storeHalf

proc stw*(state; s, a, imm: uint32) =
    calcAddrImm false:
        storeWord

proc stwu*(state; s, a, imm: uint32) =
    calcAddrImm true:
        storeWord

proc stwux*(state; s, a, b: uint32) =
    calcAddr true:
        storeWord

proc stwx*(state; s, a, b: uint32) =
    calcAddr false:
        storeWord

proc lhbrx*(state; d, a, b: uint32) =
    calcAddr false:
        loadHalf true

proc lwbrx*(state; d, a, b: uint32) =
    calcAddr false:
        loadWord true

proc sthbrx*(state; s, a, b: uint32) =
    calcAddr false:
        storeHalf true

proc stwbrx*(state; s, a, b: uint32) =
    calcAddr false:
        storeWord true

template calcAddrMultiple(start: uint32, body: untyped): untyped {.dirty.} =
    var
        ea = (if a == 0: 0'u32 else: r(a)) + signExtend(imm, 16)
        r = start
    while r <= 31:
        body

        inc r
        ea += 4

proc lmw*(state; d, a, imm: uint32) =
    # TODO alignment exception
    calcAddrMultiple d:
        r(r) = fromBE readPhysical[uint32](state.translateDataAddr(ea).get)

proc stmw*(state; s, a, imm: uint32) =
    calcAddrMultiple s:
        writePhysical[uint32](state.translateDataAddr(ea).get, toBE r(r))

proc lswi*(state; d, a, nb: uint32) =
    doAssert false, "instr not implemented"

proc lswx*(state; d, a, b: uint32) =
    doAssert false, "instr not implemented"

proc stswi*(state; s, a, nb: uint32) =
    doAssert false, "instr not implemented"

proc stswx*(state; s, a, b: uint32) =
    doAssert false, "instr not implemented"

# Float

template loadDouble: untyped {.dirty.} =
    doMemOp:
        fr(d).double = cast[float64](fromBE readPhysical[uint64](`addr`.get))

template loadSingle: untyped {.dirty.} =
    doMemOp:
        fr(d).ps0 = float64 cast[float32](fromBE readPhysical[uint32](`addr`.get))
        fr(d).ps1 = fr(d).ps0

template storeDouble: untyped {.dirty.} =
    doMemOp:
        writePhysical[uint64](`addr`.get, toBE cast[uint64](fr(s).double))

template storeSingle: untyped {.dirty.} =
    doMemOp:
        writePhysical[uint32](`addr`.get, toBE cast[uint32](fr(s).ps0))

template loadQuant: untyped {.dirty.} =
    doMemOp:
        assert state.gqr[i].ldType == 0, "quantisised load/store integer conversion not implemented"

        fr(d).ps0 = cast[float32](fromBE readPhysical[uint32](`addr`.get))
        if w == 0:
            fr(d).ps1 = float64 cast[float32](fromBE readPhysical[uint32](`addr`.get + 4))
        else:
            fr(d).ps1 = 1.0

template storeQuant: untyped {.dirty.} =
    doMemOp:
        assert state.gqr[i].stType == 0, "quantisised load/store integer conversion not implemented"

        writePhysical[uint32](`addr`.get, toBE cast[uint32](fr(s).ps0))
        if w == 0:
            writePhysical[uint32](`addr`.get + 4, toBE cast[uint32](fr(s).ps1))

proc lfd*(state; d, a, imm: uint32) =
    calcAddrImm false:
        loadDouble

proc lfdu*(state; d, a, imm: uint32) =
    calcAddrImm true:
        loadDouble

proc lfdux*(state; d, a, b: uint32) =
    calcAddr true:
        loadDouble

proc lfdx*(state; d, a, b: uint32) =
    calcAddr false:
        loadDouble

proc lfs*(state; d, a, imm: uint32) =
    calcAddrImm false:
        loadSingle

proc lfsu*(state; d, a, imm: uint32) =
    calcAddrImm true:
        loadSingle

proc lfsux*(state; d, a, b: uint32) =
    calcAddr true:
        loadSingle

proc lfsx*(state; d, a, b: uint32) =
    calcAddr false:
        loadSingle

proc stfd*(state; s, a, imm: uint32) =
    calcAddrImm false:
        storeDouble

proc stfdu*(state; s, a, imm: uint32) =
    calcAddrImm true:
        storeDouble

proc stfdux*(state; s, a, b: uint32) =
    calcAddr true:
        storeDouble

proc stfdx*(state; s, a, b: uint32) =
    calcAddr false:
        storeDouble

proc stfiwx*(state; s, a, b: uint32) =
    calcAddr false:
        doMemOp:
            writePhysical[uint32](`addr`.get, uint32 cast[uint64](fr(s).double))

proc stfs*(state; s, a, imm: uint32) =
    calcAddrImm false:
        storeSingle

proc stfsu*(state; s, a, imm: uint32) =
    calcAddrImm true:
        storeSingle

proc stfsux*(state; s, a, b: uint32) =
    calcAddr true:
        storeSingle

proc stfsx*(state; s, a, b: uint32) =
    calcAddr false:
        storeSingle

proc psq_lx*(state; d, a, b, w, i: uint32) =
    calcAddr false:
        loadQuant

proc psq_stx*(state; s, a, b, w, i: uint32) =
    calcAddr false:
        storeQuant

proc psq_lux*(state; d, a, b, w, i: uint32) =
    calcAddr true:
        loadQuant

proc psq_stux*(state; s, a, b, w, i: uint32) =
    calcAddr true:
        storeQuant

proc psq_l*(state; d, a, w, i, imm: uint32) =
    calcAddrImm false:
        loadQuant

proc psq_lu*(state; d, a, w, i, imm: uint32) =
    calcAddrImm true:
        loadQuant

proc psq_st*(state; s, a, w, i, imm: uint32) =
    calcAddrImm false:
        storeQuant

proc psq_stu*(state; s, a, w, i, imm: uint32) =
    calcAddrImm true:
        storeQuant
