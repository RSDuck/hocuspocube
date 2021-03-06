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

template calcAddrImmQuant(update: bool, body: untyped): untyped {.dirty.} =
    let ea = (if not update and a == 0: 0'u32 else: r(a)) + signExtend(imm, 12)

    body

    if update:
        r(a) = ea

template calcAddr(update: bool, body: untyped): untyped {.dirty.} =
    let ea = (if not update and a == 0: 0'u32 else: r(a)) + r(b)

    body

    if update:
        r(a) = ea

template doMemOp(body: untyped): untyped {.dirty.} =
    if (let adr = state.translateDataAddr(ea); adr.isSome):
        body
    else:
        echo "memory translation failed"

template loadByte: untyped {.dirty.} =
    doMemOp:
        r(d) = uint32 state.readMemory[:uint8](adr.get)

template loadHalf(rev = false): untyped {.dirty.} =
    doMemOp:
        let val = state.readMemory[:uint16](adr.get)
        if rev:
            r(d) = val
        else:
            r(d) = fromBE val

template loadHalfAlgebraic: untyped {.dirty.} =
    doMemOp:
        r(d) = signExtend(uint32 fromBE state.readMemory[:uint16](adr.get), 16)

template loadWord(rev = false): untyped {.dirty.} =
    doMemOp:
        let val = state.readMemory[:uint32](adr.get)
        if rev:
            r(d) = val
        else:
            r(d) = fromBE val

template storeByte: untyped {.dirty.} =
    doMemOp:
        state.writeMemory[:uint8](adr.get, uint8 r(s))

template storeHalf(rev = false): untyped {.dirty.} =
    doMemOp:
        state.writeMemory[:uint16](adr.get, if rev: uint16 r(s) else: toBE uint16 r(s))

template storeWord(rev = false): untyped {.dirty.} =
    doMemOp:
        state.writeMemory[:uint32](adr.get, if rev: uint32 r(s) else: toBE uint32 r(s))

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

        r += 1
        ea += 4

proc lmw*(state; d, a, imm: uint32) =
    # TODO alignment exception
    calcAddrMultiple d:
        doMemOp:
            r(r) = fromBE state.readMemory[:uint32](adr.get)

proc stmw*(state; s, a, imm: uint32) =
    calcAddrMultiple s:
        doMemOp:
            state.writeMemory[:uint32](adr.get, toBE r(r))

proc lswi*(state; d, a, nb: uint32) =
    raiseAssert "instr not implemented"

proc lswx*(state; d, a, b: uint32) =
    raiseAssert "instr not implemented"

proc stswi*(state; s, a, nb: uint32) =
    raiseAssert "instr not implemented"

proc stswx*(state; s, a, b: uint32) =
    raiseAssert "instr not implemented"

# Float

template loadDouble: untyped {.dirty.} =
    doMemOp:
        fr(d).double = cast[float64](fromBE state.readMemory[:uint64](adr.get))
        if state.pc == 0x81374408'u32:
            echo &"reading {fr(d).double} {cast[uint64](fr(d).double)} {state.r[3]:04X} {state.r[4]:04X} {fr(2).double}"

template loadSingle: untyped {.dirty.} =
    doMemOp:
        fr(d).ps0 = cast[float32](fromBE state.readMemory[:uint32](adr.get))
        checkNan(fr(d).ps0)
        fr(d).ps1 = fr(d).ps0

template storeDouble: untyped {.dirty.} =
    doMemOp:
        state.writeMemory[:uint64](adr.get, toBE cast[uint64](fr(s).double))

template storeSingle: untyped {.dirty.} =
    doMemOp:
        checkNan(fr(s).ps0)
        state.writeMemory[:uint32](adr.get, toBE cast[uint32](float32(fr(s).ps0)))

template loadQuant: untyped {.dirty.} =
    doMemOp:
        assert state.gqr[i].ldType == 0, "quantisised load/store integer conversion not implemented"

        fr(d).ps0 = cast[float32](fromBE state.readMemory[:uint32](adr.get))
        checkNan(fr(d).ps0)
        if w == 0:
            fr(d).ps1 = float64 cast[float32](fromBE state.readMemory[:uint32](adr.get + 4))
            checkNan(fr(d).ps1)
        else:
            fr(d).ps1 = 1.0

template storeQuant: untyped {.dirty.} =
    doMemOp:
        assert state.gqr[i].stType == 0, "quantisised load/store integer conversion not implemented"

        checkNan(fr(s).ps0)
        state.writeMemory[:uint32](adr.get, toBE cast[uint32](float32(fr(s).ps0)))
        if w == 0:
            checkNan(fr(s).ps1)
            state.writeMemory[:uint32](adr.get + 4, toBE cast[uint32](float32(fr(s).ps1)))

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
            state.writeMemory[:uint32](adr.get, toBE uint32 cast[uint64](fr(s).double))

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
    calcAddrImmQuant false:
        loadQuant

proc psq_lu*(state; d, a, w, i, imm: uint32) =
    calcAddrImmQuant true:
        loadQuant

proc psq_st*(state; s, a, w, i, imm: uint32) =
    calcAddrImmQuant false:
        storeQuant

proc psq_stu*(state; s, a, w, i, imm: uint32) =
    calcAddrImmQuant true:
        storeQuant
