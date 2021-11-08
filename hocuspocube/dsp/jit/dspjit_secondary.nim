import
    ../dspstate, ../dspdef, dspfrontendcommon,
    fallbacks_secondary,
    ../../util/jit/ir,
    strformat

using builder: var IrBlockBuilder[DspIrState]

const interpretSecondary = false

proc undefinedSecondary(builder; instr: uint16) =
    raiseAssert(&"undefined secondary dsp instr {builder.regs.pc:02X} {instr:02X}")

proc incAdrReg(builder; adr: IrInstrRef, reg, m: uint32) =
    if m == 0:
        builder.writeAdr reg, builder.incAdr(adr, builder.readAdrLen reg)
    else:
        builder.writeAdr reg, builder.incAdr(adr, builder.readAdrLen reg, builder.readAdrMod reg)

proc mr(builder; m, r: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.mr)
    else:
        builder.loadStoreAdrInc(builder.readAdr(r), m, r)

proc mv(builder; d, s: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.mv)
    else:
        builder.writeReg x0.succ(int d),
            case range[0..3](s)
            of 0..1: builder.readReg(a0.succ(int s))
            of 2..3: builder.storeAccum(s - 2)

proc st(builder; s, m, r: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.st)
    else:
        let
            val = case range[0..3](s)
                of 0..1: builder.readReg(a0.succ(int s))
                of 2..3: builder.storeAccum(s - 2)
            adr = builder.readAdr(r)
        discard builder.biop(dspStoreDMem, adr, val)

        builder.incAdrReg(adr, r, m)

proc ld(builder; d, m, r: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.ld)
    else:
        let
            adr = builder.readAdr(r)
            val = builder.unop(dspLoadDMem, adr)

        builder.incAdrReg(adr, r, m)

        case range[0..7](d)
        of 0..5: builder.writeReg(x0.succ(int d), val)
        of 6..7: builder.loadAccum(d - 6, val)

proc ls(builder; d, m, n, k, s: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.ls)
    else:
        let
            (loadReg, storeReg) = if k == 0: (0'u32, 3'u32) else: (3'u32, 0'u32)
            storeVal = builder.storeAccum(s)

            loadAdr = builder.readAdr(loadReg)
            storeAdr = builder.readAdr(storeReg)

        builder.writeReg(x0.succ(int d), builder.unop(dspLoadDMem, loadAdr))
        discard builder.biop(dspStoreDMem, storeAdr, storeVal)

        builder.incAdrReg(builder.readAdr(0), 0, n)
        builder.incAdrReg(builder.readAdr(3), 3, m)

proc ldd(builder; d, m, n, r: uint16) =
    when interpretSecondary:
        builder.interpretdsp(builder.regs.instr, builder.regs.pc, fallbacks_secondary.ldd)
    else:
        let
            (d1, d2, adr) =
                if r == 3:
                    # ldd2
                    if (d and 1) == 0:
                        (x1, x0, d shr 1)
                    else:
                        (y1, y0, d shr 1)
                else:
                    (x0.succ(int(d shr 1) * 2), y0.succ(int(d and 1) * 2), r)
            adrVal = builder.readAdr(adr)
            r3 = builder.readAdr(3)

        builder.writeReg(d1, builder.unop(dspLoadDMem, adrVal))
        builder.writeReg(d2, builder.unop(dspLoadDMem, r3))

        builder.incAdrReg(adrVal, adr, n)
        builder.incAdrReg(r3, 3, m)

proc dispatchSecondary*(builder; x: uint16) =
    dspSecondaryDispatch(x, builder, undefinedSecondary)
