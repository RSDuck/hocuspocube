import
    ../../util/aluhelper,
    ir, ppcfrontendcommon,
    fallbacks

using builder: var IrBlockBuilder[PpcIrRegState]

proc calcAdrImm(builder; a, imm: uint32, update: bool): IrInstrRef =
    result =
        (if not update and a == 0:
            builder.imm(signExtend(imm, 16))
        else:
            builder.biop(irInstrIAdd, builder.loadreg(a), builder.imm(signExtend(imm, 16))))

    if update:
        builder.storereg a, result

proc calcAdrImmQuant(builder; a, imm: uint32, update: bool): IrInstrRef =
    result =
        (if not update and a == 0:
            builder.imm(signExtend(imm, 12))
        else:
            builder.biop(irInstrIAdd, builder.loadreg(a), builder.imm(signExtend(imm, 12))))

    if update:
        builder.storereg a, result

proc calcAdr(builder; a, b: uint32, update: bool): IrInstrRef =
    result = (if not update and a == 0:
                builder.loadreg(b)
            else:
                builder.biop(irInstrIAdd, builder.loadreg(a), builder.loadreg(b)))

    if update:
        builder.storereg a, result

const
    interpretLoadStore = false
    interpretRegularFloatMem = false
    interpretQuantLoadStore = true

    interpretLoads = false
    interpretStores = false

    interpretLoads8 = false
    interpretLoadsS16 = false
    interpretLoadsU16 = false

    interpretLoadStoreUpdate = false

proc lbz*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoads8:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lbz)
    else:
        builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdrImm(a, imm, false))

proc lbzu*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoads8 or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lbzu)
    else:
        builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdrImm(a, imm, true))

proc lbzux*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoads8 or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lbzux)
    else:
        builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdr(a, b, true))

proc lbzx*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoads8:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lbzx)
    else:
        builder.storereg d, builder.unop(irInstrLoadU8, builder.calcAdr(a, b, false))

proc lha*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsS16:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lha)
    else:
        builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdrImm(a, imm, false))

proc lhau*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsS16 or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhau)
    else:
        builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdrImm(a, imm, true))

proc lhaux*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsS16 or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhaux)
    else:
        builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdr(a, b, true))

proc lhax*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsS16:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhax)
    else:
        builder.storereg d, builder.unop(irInstrLoadS16, builder.calcAdr(a, b, false))

proc lhz*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsU16:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhz)
    else:
        builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdrImm(a, imm, false))

proc lhzu*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsU16 or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhzu)
    else:
        builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdrImm(a, imm, true))

proc lhzux*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsU16 or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhzux)
    else:
        builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdr(a, b, true))

proc lhzx*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadsU16:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lhzx)
    else:
        builder.storereg d, builder.unop(irInstrLoadU16, builder.calcAdr(a, b, false))

proc lwz*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lwz)
    else:
        builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdrImm(a, imm, false))

proc lwzu*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lwzu)
    else:
        builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdrImm(a, imm, true))

proc lwzux*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lwzux)
    else:
        builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdr(a, b, true))

proc lwzx*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretLoads:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lwzx)
    else:
        builder.storereg d, builder.unop(irInstrLoad32, builder.calcAdr(a, b, false))

proc stb*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stb)
    else:
        discard builder.biop(irInstrStore8, builder.calcAdrImm(a, imm, false), builder.loadreg(s))

proc stbu*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretStores or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stbu)
    else:
        discard builder.biop(irInstrStore8, builder.calcAdrImm(a, imm, true), builder.loadreg(s))

proc stbux*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretStores or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stbux)
    else:
        discard builder.biop(irInstrStore8, builder.calcAdr(a, b, true), builder.loadreg(s))

proc stbx*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stbx)
    else:
        discard builder.biop(irInstrStore8, builder.calcAdr(a, b, false), builder.loadreg(s))

proc sth*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.sth)
    else:
        discard builder.biop(irInstrStore16, builder.calcAdrImm(a, imm, false), builder.loadreg(s))

proc sthu*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.sthu)
    else:
        discard builder.biop(irInstrStore16, builder.calcAdrImm(a, imm, true), builder.loadreg(s))

proc sthux*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretStores or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.sthux)
    else:
        discard builder.biop(irInstrStore16, builder.calcAdr(a, b, true), builder.loadreg(s))

proc sthx*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.sthx)
    else:
        discard builder.biop(irInstrStore16, builder.calcAdr(a, b, false), builder.loadreg(s))

proc stw*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stw)
    else:
        discard builder.biop(irInstrStore32, builder.calcAdrImm(a, imm, false), builder.loadreg(s))

proc stwu*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretStores or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stwu)
    else:
        discard builder.biop(irInstrStore32, builder.calcAdrImm(a, imm, true), builder.loadreg(s))

proc stwux*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretStores or interpretLoadStoreUpdate:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stwux)
    else:
        discard builder.biop(irInstrStore32, builder.calcAdr(a, b, true), builder.loadreg(s))

proc stwx*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretStores:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stwx)
    else:
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
    while r <= 31:
        body

        r += 1
        ea = builder.biop(irInstrIAdd, ea, builder.imm(4))

proc lmw*(builder; d, a, imm: uint32) =
    when interpretLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lmw)
    else:
        calcAddrMultiple d:
            builder.storereg r, builder.unop(irInstrLoad32, ea)

proc stmw*(builder; s, a, imm: uint32) =
    when interpretLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stmw)
    else:
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

proc expandAndDup(builder; instr: IrInstrRef): IrInstrRef =
    builder.unop(irInstrFSwizzleD00, builder.unop(irInstrCvtss2sd, instr))

proc lfd*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfd)
    else:
        builder.storefreg d, builder.unop(irInstrLoadFsd, builder.calcAdrImm(a, imm, false))
    builder.regs.floatInstr = true

proc lfdu*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfdu)
    else:
        builder.storefreg d, builder.unop(irInstrLoadFsd, builder.calcAdrImm(a, imm, true))
    builder.regs.floatInstr = true

proc lfdux*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfdux)
    else:
        builder.storefreg d, builder.unop(irInstrLoadFsd, builder.calcAdr(a, b, true))
    builder.regs.floatInstr = true

proc lfdx*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfdx)
    else:
        builder.storefreg d, builder.unop(irInstrLoadFsd, builder.calcAdr(a, b, false))
    builder.regs.floatInstr = true

proc lfs*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfs)
    else:
        builder.storefregp d, builder.expandAndDup(builder.unop(irInstrLoadFss, builder.calcAdrImm(a, imm, false)))
    builder.regs.floatInstr = true

proc lfsu*(builder; d, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfsu)
    else:
        builder.storefregp d, builder.expandAndDup(builder.unop(irInstrLoadFss, builder.calcAdrImm(a, imm, true)))
    builder.regs.floatInstr = true

proc lfsux*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfsux)
    else:
        builder.storefregp d, builder.expandAndDup(builder.unop(irInstrLoadFss, builder.calcAdr(a, b, true)))
    builder.regs.floatInstr = true

proc lfsx*(builder; d, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.lfsx)
    else:
        builder.storefregp d, builder.expandAndDup(builder.unop(irInstrLoadFss, builder.calcAdr(a, b, false)))
    builder.regs.floatInstr = true

proc stfd*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfd)
    else:
        discard builder.biop(irInstrStoreFsd, builder.calcAdrImm(a, imm, false), builder.loadfreg(s))
    builder.regs.floatInstr = true

proc stfdu*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfdu)
    else:
        discard builder.biop(irInstrStoreFsd, builder.calcAdrImm(a, imm, true), builder.loadfreg(s))
    builder.regs.floatInstr = true

proc stfdux*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfdux)
    else:
        discard builder.biop(irInstrStoreFsd, builder.calcAdr(a, b, true), builder.loadfreg(s))
    builder.regs.floatInstr = true

proc stfdx*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfdx)
    else:
        discard builder.biop(irInstrStoreFsd, builder.calcAdr(a, b, false), builder.loadfreg(s))
    builder.regs.floatInstr = true

proc stfiwx*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfiwx)
    else:
        discard builder.biop(irInstrStoreFss, builder.calcAdr(a, b, false), builder.loadfreg(s))
    builder.regs.floatInstr = true

proc stfs*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfs)
    else:
        discard builder.biop(irInstrStoreFss, builder.calcAdrImm(a, imm, false), builder.unop(irInstrCvtsd2ss, builder.loadfreg(s)))
    builder.regs.floatInstr = true

proc stfsu*(builder; s, a, imm: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfsu)
    else:
        discard builder.biop(irInstrStoreFss, builder.calcAdrImm(a, imm, true), builder.unop(irInstrCvtsd2ss, builder.loadfreg(s)))
    builder.regs.floatInstr = true

proc stfsux*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfsux)
    else:
        discard builder.biop(irInstrStoreFss, builder.calcAdr(a, b, true), builder.unop(irInstrCvtsd2ss, builder.loadfreg(s)))
    builder.regs.floatInstr = true

proc stfsx*(builder; s, a, b: uint32) =
    when interpretLoadStore or interpretRegularFloatMem:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.stfsx)
    else:
        discard builder.biop(irInstrStoreFss, builder.calcAdr(a, b, false), builder.unop(irInstrCvtsd2ss, builder.loadfreg(s)))
    builder.regs.floatInstr = true

proc quantload(builder; adr: IrInstrRef, d, w, i: uint32) =
    builder.storefregp d, builder.unop(irInstrCvtps2pd,
        builder.biop(if w == 1: irInstrLoadFsq else: irInstrLoadFpq,
            adr,
            builder.loadctx(irInstrLoadSpr, irSprNumGqr0.succ(int w).uint32)))

proc quantstore(builder; adr: IrInstrRef, s, w, i: uint32) =
    let
        storeval =
            if w == 1:
                builder.unop(irInstrCvtsd2ss, builder.loadfreg(s))
            else:
                builder.unop(irInstrCvtpd2ps, builder.loadfregp(s))

    discard builder.triop(if w == 1: irInstrStoreFsq else: irInstrStoreFpq,
        adr,
        storeval,
        builder.loadctx(irInstrLoadSpr, irSprNumGqr0.succ(int w).uint32))

proc psq_lx*(builder; d, a, b, w, i: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_lx)
    else:
        builder.quantload builder.calcAdr(a, b, false), d, w, i
    builder.regs.floatInstr = true

proc psq_stx*(builder; s, a, b, w, i: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_stx)
    else:
        builder.quantstore builder.calcAdr(a, b, false), s, w, i
    builder.regs.floatInstr = true

proc psq_lux*(builder; d, a, b, w, i: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_lux)
    else:
        builder.quantload builder.calcAdr(a, b, true), d, w, i
    builder.regs.floatInstr = true

proc psq_stux*(builder; s, a, b, w, i: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_stux)
    else:
        builder.quantstore builder.calcAdr(a, b, true), s, w, i
    builder.regs.floatInstr = true

proc psq_l*(builder; d, a, w, i, imm: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_l)
    else:
        builder.quantload builder.calcAdrImmQuant(a, imm, false), d, w, i
    builder.regs.floatInstr = true

proc psq_lu*(builder; d, a, w, i, imm: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_lu)
    else:
        builder.quantload builder.calcAdrImmQuant(a, imm, true), d, w, i
    builder.regs.floatInstr = true

proc psq_st*(builder; s, a, w, i, imm: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_st)
    else:
        builder.quantstore builder.calcAdrImmQuant(a, imm, false), s, w, i
    builder.regs.floatInstr = true

proc psq_stu*(builder; s, a, w, i, imm: uint32) =
    when interpretLoadStore or interpretQuantLoadStore:
        builder.interpreter(builder.regs.instr, builder.regs.pc, fallbacks.psq_stu)
    else:
        builder.quantstore builder.calcAdrImmQuant(a, imm, true), s, w, i
    builder.regs.floatInstr = true

# not really a load/store operation
proc dcbz*(builder; a, b: uint32) =
    discard

proc dcbz_l*(builder; a, b: uint32) =
    raiseAssert("unimplemented instr dcbz_l")