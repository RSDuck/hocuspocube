import
    ../../util/aluhelper,
    ir, ppcfrontendcommon

using builder: var IrBlockBuilder[PpcIrRegState]

proc calcAdrImm(builder; a, imm: uint32, update: bool): IrInstrRef =
    result =
        (if not update and a == 0:
            builder.imm(signExtend(imm, 16))
        else:
            builder.biop(irInstrIAdd, builder.loadreg(a), builder.imm(signExtend(imm, 16))))

    if update:
        builder.storereg a, result

proc calcAdr(builder; a, b: uint32, update: bool): IrInstrRef =
    result = (if not update and a == 0:
                builder.loadreg(a)
            else:
                builder.biop(irInstrIAdd, builder.loadreg(a), builder.loadreg(b)))

    if update:
        builder.storereg a, result

proc lbz*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdrImm(a, imm, false))

proc lbzu*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdrImm(a, imm, true))

proc lbzux*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdr(a, b, true))

proc lbzx*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdr(a, b, false))

proc lha*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdrImm(a, imm, false))

proc lhau*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdrImm(a, imm, true))

proc lhaux*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdr(a, b, true))

proc lhax*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdr(a, b, false))

proc lhz*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdrImm(a, imm, false))

proc lhzu*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdrImm(a, imm, true))

proc lhzux*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdr(a, b, true))

proc lhzx*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdr(a, b, false))

proc lwz*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdrImm(a, imm, false))

proc lwzu*(builder; d, a, imm: uint32) =
    builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdrImm(a, imm, true))

proc lwzux*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdr(a, b, true))

proc lwzx*(builder; d, a, b: uint32) =
    builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdr(a, b, false))

proc stb*(builder; s, a, imm: uint32) =
    discard builder.biop(irInstrStore8, builder.calcAdrImm(a, imm, false), builder.loadreg(s))

proc stbu*(builder; s, a, imm: uint32) =
    discard builder.biop(irInstrStore8, builder.calcAdrImm(a, imm, true), builder.loadreg(s))

proc stbux*(builder; s, a, b: uint32) =
    discard builder.biop(irInstrStore8, builder.calcAdr(a, b, true), builder.loadreg(s))

proc stbx*(builder; s, a, b: uint32) =
    discard builder.biop(irInstrStore8, builder.calcAdr(a, b, false), builder.loadreg(s))

proc sth*(builder; s, a, imm: uint32) =
    discard builder.biop(irInstrStore16, builder.calcAdrImm(a, imm, false), builder.loadreg(s))

proc sthu*(builder; s, a, imm: uint32) =
    discard builder.biop(irInstrStore16, builder.calcAdrImm(a, imm, true), builder.loadreg(s))

proc sthux*(builder; s, a, b: uint32) =
    discard builder.biop(irInstrStore16, builder.calcAdr(a, b, true), builder.loadreg(s))

proc sthx*(builder; s, a, b: uint32) =
    discard builder.biop(irInstrStore16, builder.calcAdr(a, b, false), builder.loadreg(s))

proc stw*(builder; s, a, imm: uint32) =
    discard builder.biop(irInstrStore32, builder.calcAdrImm(a, imm, false), builder.loadreg(s))

proc stwu*(builder; s, a, imm: uint32) =
    discard builder.biop(irInstrStore32, builder.calcAdrImm(a, imm, true), builder.loadreg(s))

proc stwux*(builder; s, a, b: uint32) =
    discard builder.biop(irInstrStore32, builder.calcAdr(a, b, true), builder.loadreg(s))

proc stwx*(builder; s, a, b: uint32) =
    discard builder.biop(irInstrStore32, builder.calcAdr(a, b, false), builder.loadreg(s))

proc lhbrx*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lhbrx")

proc lwbrx*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lwbrx")

proc sthbrx*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr sthbrx")

proc stwbrx*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr stwbrx")

template calcAddrMultiple(start: uint32, body: untyped): untyped {.dirty.} =
    var
        ea = builder.calcAdrImm(a, imm, false)
        r = start
        offset = 0'u32
    while r <= 31:
        ea = builder.biop(irInstrIAdd, ea, builder.imm(offset))

        body

        r += 1
        offset += 4

proc lmw*(builder; d, a, imm: uint32) =
    calcAddrMultiple d:
        builder.storereg r, builder.unop(irInstrLoad32, ea)

proc stmw*(builder; s, a, imm: uint32) =
    calcAddrMultiple s:
        discard builder.biop(irInstrStore32, ea, builder.loadreg(r))

proc lswi*(builder; d, a, nb: uint32) =
    raiseAssert("unimplemented instr lswi")

proc lswx*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lswx")

proc stswi*(builder; s, a, nb: uint32) =
    raiseAssert "instr not implemented stswi"

proc stswx*(builder; s, a, b: uint32) =
    raiseAssert "instr not implemented stswx"

# Float

proc lfd*(builder; d, a, imm: uint32) =
    discard

proc lfdu*(builder; d, a, imm: uint32) =
    raiseAssert("unimplemented instr lfdu")

proc lfdux*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lfdux")

proc lfdx*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lfdx")

proc lfs*(builder; d, a, imm: uint32) =
    raiseAssert("unimplemented instr lfs")

proc lfsu*(builder; d, a, imm: uint32) =
    raiseAssert("unimplemented instr lfsu")

proc lfsux*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lfsux")

proc lfsx*(builder; d, a, b: uint32) =
    raiseAssert("unimplemented instr lfsx")

proc stfd*(builder; s, a, imm: uint32) =
    raiseAssert("unimplemented instr stfd")

proc stfdu*(builder; s, a, imm: uint32) =
    raiseAssert("unimplemented instr stfdu")

proc stfdux*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr stfdux")

proc stfdx*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr stfdx")

proc stfiwx*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr stfixw")

proc stfs*(builder; s, a, imm: uint32) =
    raiseAssert("unimplemented instr stfs")

proc stfsu*(builder; s, a, imm: uint32) =
    raiseAssert("unimplemented instr stfsu")

proc stfsux*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr stfsux")

proc stfsx*(builder; s, a, b: uint32) =
    raiseAssert("unimplemented instr stfsx")

proc psq_lx*(builder; d, a, b, w, i: uint32) =
    raiseAssert("unimplemented instr psq_lx")

proc psq_stx*(builder; s, a, b, w, i: uint32) =
    raiseAssert("unimplemented instr psq_stx")

proc psq_lux*(builder; d, a, b, w, i: uint32) =
    raiseAssert("unimplemented instr psq_lux")

proc psq_stux*(builder; s, a, b, w, i: uint32) =
    raiseAssert("unimplemented instr psq_stux")

proc psq_l*(builder; d, a, w, i, imm: uint32) =
    discard

proc psq_lu*(builder; d, a, w, i, imm: uint32) =
    raiseAssert("unimplemented instr psq_lu")

proc psq_st*(builder; s, a, w, i, imm: uint32) =
    raiseAssert("unimplemented instr psq_st")

proc psq_stu*(builder; s, a, w, i, imm: uint32) =
    raiseAssert("unimplemented instr psq_stu")

# not really a load/store operation
proc dcbz*(builder; a, b: uint32) =
    raiseAssert("unimplemented instr dcbz")

proc dcbz_l*(builder; a, b: uint32) =
    raiseAssert("unimplemented instr dcbz_l")