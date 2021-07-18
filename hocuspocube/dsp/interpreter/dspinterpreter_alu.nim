import
    bitops, stew/bitops2,
    ../dspstate, ../dsp,
    ../../util/aluhelper,
    dspinterpreter_aux,
    dspinterpreter_secondary,
    strformat

using state: var DspState

proc mr*(state; m, r: uint16) =
    let r = int r
    case range[0..7](m)
    of 0: discard # mr with m=0 is also known as nop
    of 1: state.adrReg[r] = decAdr(state.adrReg[r], state.wrapReg[r])
    of 2: state.adrReg[r] = incAdr(state.adrReg[r], state.wrapReg[r])
    of 3: state.adrReg[r] = decAdr(state.adrReg[r], state.wrapReg[r], cast[int16](state.incReg[r]))
    of 4..7: state.adrReg[r] = incAdr(state.adrReg[r], state.wrapReg[r], cast[int16](state.incReg[m - 4]))

template addAccumOp(accumReg, addend: untyped, parallel: untyped): untyped =
    let
        addendVal = addend
        accum = state.readAccum(accumReg)
        sum = accum + addendVal

    parallel

    state.writeAccum(accumReg, sum)
    state.setC1(cast[uint64](accum), cast[uint64](addendVal))
    state.setV1(cast[uint64](sum), cast[uint64](accum), cast[uint64](addendVal))
    state.setZ1(cast[uint64](sum))
    state.setN1(cast[uint64](sum))
    state.setE1(cast[uint64](sum))
    state.setU1(cast[uint64](sum))

proc adsi*(state; d, i: uint16) =
    addAccumOp(int d, int64(cast[int8](i)) shl 16):
        discard

proc adli*(state; d: uint16) =
    addAccumOp(int d, int64(cast[int16](fetchFollowingImm)) shl 16):
        discard

proc cmpsi*(state; s, i: uint16) =
    let
        subtrahend = int64(cast[int8](i)) shl 16
        accum = state.readAccum(int s)
        diff = accum - subtrahend

    state.setC2(cast[uint64](accum), cast[uint64](subtrahend))
    state.setV2(cast[uint64](diff), cast[uint64](accum), cast[uint64](subtrahend))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))

proc cmpli*(state; s: uint16) =
    let
        subtrahend = int64(cast[int16](fetchFollowingImm)) shl 16

        a = state.readAccum(int s)
        diff = a - subtrahend

    state.setC2(cast[uint64](a), cast[uint64](subtrahend))
    state.setV2(cast[uint64](diff), cast[uint64](a), cast[uint64](subtrahend))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))

template shiftOp(logical, shift: untyped, parallel: untyped): untyped =
    let
        accum = state.readAccum(int d)
        shiftAmount = shift
        val =
            if shiftAmount < 0:
                if logical:
                    accum shr -shiftAmount
                else:
                    cast[int64](cast[uint64](accum) shr -shiftAmount)
            else:
                (accum shl shiftAmount)

    parallel

    state.writeAccum(int d, val)

    state.status.ca = false
    state.status.ov = false
    state.setZ1(cast[uint64](val))
    state.setN1(cast[uint64](val))
    state.setE1(cast[uint64](val))
    state.setU1(cast[uint64](val))


proc lsfi*(state; d, i: uint16) =
    shiftOp(true, cast[int16](signExtend(i, 7))):
        discard

proc asfi*(state; d, i: uint16) =
    shiftOp(false, cast[int16](signExtend(i, 7))):
        discard

template logicOp(doOp, parallel: untyped) =
    let
        reg {.inject.} = state.readReg(dspRegA1.succ(int d))
        val = doOp
        top = state.readReg(dspRegA2.succ(int d))

    parallel

    state.writeReg(dspRegA1.succ(int d), val)

    state.status.ca = false
    state.status.ov = false
    state.setZ2(val)
    state.setN2(val)
    state.setE1(top, val)
    state.setU1(val)

proc xorli*(state; d: uint16) =
    logicOp(reg xor fetchFollowingImm):
        discard

proc anli*(state; d: uint16) =
    logicOp(reg and fetchFollowingImm):
        discard

proc orli*(state; d: uint16) =
    logicOp(reg or fetchFollowingImm):
        discard

proc norm*(state; d, r: uint16) =
    raiseAssert "unimplemented dsp instr norm"

proc ddiv*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr ddiv"

proc addc*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr addc"

proc subc*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr subc"

proc negc*(state; d: uint16) =
    raiseAssert "unimplemented dsp instr negc"

proc max*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr max"

proc lsfn*(state; d, s: uint16) =
    shiftOp(true, -cast[int16](state.readReg(dspRegX1.succ(int s)))):
        discard

proc lsfn2*(state; d: uint16) =
    shiftOp(true, -cast[int16](state.readReg(dspRegB1.pred(int d)))):
        discard

proc asfn*(state; d, s: uint16) =
    shiftOp(false, -cast[int16](state.readReg(dspRegX1.succ(int s)))):
        discard

proc asfn2*(state; d: uint16) =
    shiftOp(false, -cast[int16](state.readReg(dspRegB1.pred(int d)))):
        discard

proc mv*(state; d, s: uint16) =
    state.writeReg DspReg(d), state.readReg(DspReg(s))

proc mvsi*(state; d, i: uint16) =
    if d <= 5:
        state.writeReg(dspRegX0.succ(int d), signExtend(i, 8))
    else:
        state.loadAccum(int d - 6, signExtend(i, 8))
    #echo &"loading short {d} {signExtend(i, 8)} {state.pc:02X} {state.r[dspRegB1]:02X}"

proc mvli*(state; d: uint16) =
    state.writeReg DspReg(d), fetchFollowingImm

proc clrpsr*(state; b: uint16) =
    state.status.bit(6 + int(b), false)

proc setpsr*(state; b: uint16) =
    state.status.bit(6 + int(b), true)

proc btstl*(state; d: uint16) =
    let mask = fetchFollowingImm
    state.status.tb = (state.readReg(dspRegA1.succ(int d)) and mask) == 0

proc btsth*(state; d: uint16) =
    let mask = fetchFollowingImm
    state.status.tb = (state.readReg(dspRegA1.succ(int d)) and mask) == mask

proc add*(state; s, d, x: uint16) =
    addAccumOp(int d, case range[0..7](s)
            of 0..3: int64(cast[int16](state.readReg(dspRegX0.succ(int s)))) shl 16
            of 4..5: state.readAuxAccum(int s - 4)
            of 6: state.readAccum(int(1 - d))
            of 7: state.readProduct()):
        state.dispatchSecondary(x)

proc addl*(state; s, d, x: uint16) =
    addAccumOp(int d, int64(state.readReg(dspRegX0.succ(int s)))):
        state.dispatchSecondary(x)

proc sub*(state; s, d, x: uint16) =
    let
        subtrahend = case range[0..7](s)
            of 0..3: int64(cast[int16](state.readReg(dspRegX0.succ(int s)))) shl 16
            of 4..5: state.readAuxAccum(int s - 4)
            of 6: state.readAccum(int(1 - d))
            of 7: state.readProduct()
        accum = state.readAccum(int d)
        diff = accum - subtrahend

    state.dispatchSecondary(x)

    state.writeAccum(int d, diff)
    state.setC2(cast[uint64](accum), cast[uint64](subtrahend))
    state.setV2(cast[uint64](diff), cast[uint64](accum), cast[uint64](subtrahend))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))

proc amv*(state; s, d, x: uint16) =
    let
        val = case range[0..7](s)
            of 0..3: int64(cast[int16](state.readReg(dspRegX0.succ(int s)))) shl 16
            of 4..5: int64(state.readAuxAccum(int s - 4))
            of 6: state.readAccum(int(1 - d))
            of 7: state.readProduct()
    state.dispatchSecondary(x)

    state.writeAccum(int d, val)
    state.status.ca = false
    state.status.ov = false
    state.setZ1(cast[uint64](val))
    state.setN1(cast[uint64](val))
    state.setE1(cast[uint64](val))
    state.setU1(cast[uint64](val))

proc cmp*(state; s, d, x: uint16) =
    let
        a = state.readAccum(int d)
        b = int64(cast[int16](state.readReg(dspRegX1.succ(int s)))) shl 16
        diff = a - b

    state.dispatchSecondary(x)

    state.setC2(cast[uint64](a), cast[uint64](b))
    state.setV2(cast[uint64](diff), cast[uint64](a), cast[uint64](b))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))

proc cmpa*(state; x: uint16) =
    let
        a = state.readAccum(0)
        b = state.readAccum(1)
        diff = a - b

    state.dispatchSecondary(x)

    state.setC2(cast[uint64](a), cast[uint64](b))
    state.setV2(cast[uint64](diff), cast[uint64](a), cast[uint64](b))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))
    #echo &"comparing {a:04X} by {b:04X} {diff:04X} {state.status.ca} {state.status.ov} {state.status.zr} {state.status.mi} {state.status.ext} {state.status.unnorm}"

proc inc*(state; d, x: uint16) =
    addAccumOp(int(d.getBit(0)), if d.getBit(1): 1 else: 0x10000):
        state.dispatchSecondary(x)

proc dec*(state; d, x: uint16) =
    let
        accum = state.readAccum(int(d.getBit(0)))
        subtrahend = if d.getBit(1): 1 else: 0x10000
        diff = accum - subtrahend

    state.dispatchSecondary(x)

    state.setC2(cast[uint64](accum), cast[uint64](subtrahend))
    state.setV2(cast[uint64](diff), cast[uint64](accum), cast[uint64](subtrahend))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))
    state.writeAccum(int(d.getBit(0)), diff)

proc abs*(state; d, x: uint16) =
    let absVal = abs(state.readAccum(int(d)))

    state.dispatchSecondary(x)

    state.status.ca = false
    state.setV5(cast[uint64](absVal))
    state.setZ1(cast[uint64](absVal))
    state.setN1(cast[uint64](absVal))
    state.setE1(cast[uint64](absVal))
    state.setU1(cast[uint64](absVal))
    state.writeAccum(int(d), absVal)

proc neg*(state; d, x: uint16) =
    let
        accum = state.readAccum(int d)
        diff = -accum

    state.dispatchSecondary(x)

    state.setC2(0'u64, cast[uint64](accum))
    state.setV2(cast[uint64](diff), 0'u64, cast[uint64](accum))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))
    state.writeAccum(int d, diff)

proc negp*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr negp"

proc clra*(state; d, x: uint16) =
    state.dispatchSecondary(x)

    state.writeAccum int(d), 0
    state.status.ca = false
    state.status.ov = false
    state.status.zr = true
    state.status.mi = false
    state.status.ext = false
    state.status.unnorm = true

proc clrp*(state; x: uint16) =
    state.dispatchSecondary(x)

    state.prod = 0xFFFF_FFFF_FFF0_0000'u64
    state.prodcarry = 0x0010

proc round(x: int64): int64 =
    result = x
    if (result and 0xFFFF) > 0x8000 or ((result and 0xFFFF) == 0x8000 and (x and 0x10000) == 0x10000):
        result += 0x10000
    result.clearMask(0xFFFF'i64)

proc rnd*(state; d, x: uint16) =
    raiseAssert &"unimplemented dsp instr {state.pc:04X}"

proc rndp*(state; d, x: uint16) =
    let
        prod = state.readProduct()
        roundedProd = round(prod)

    state.dispatchSecondary(x)

    state.setC7(cast[uint64](prod), cast[uint64](roundedProd))
    state.setV6(cast[uint64](prod), cast[uint64](roundedProd))
    state.setZ1(cast[uint64](roundedProd))
    state.setN1(cast[uint64](roundedProd))
    state.setE1(cast[uint64](roundedProd))
    state.setU1(cast[uint64](roundedProd))
    state.writeAccum(int d, roundedProd)

proc tst*(state; s, x: uint16) =
    let accum = state.readAccum(int s)

    state.dispatchSecondary(x)

    state.status.ca = false
    state.status.ov = false
    state.setZ1(cast[uint64](accum))
    state.setN1(cast[uint64](accum))
    state.setE1(cast[uint64](accum))
    state.setU1(cast[uint64](accum))

proc tst2*(state; s, x: uint16) =
    let accum = int64(cast[int16](state.readReg(dspRegX1.succ(int s)))) shl 16

    state.dispatchSecondary(x)

    state.status.ca = false
    state.status.ov = false
    state.setZ1(cast[uint64](accum))
    state.setN1(cast[uint64](accum))
    state.setE1(cast[uint64](accum))
    state.setU1(cast[uint64](accum))

proc tstp*(state; x: uint16) =
    raiseAssert "unimplemented dsp instr tstp"

proc lsl16*(state; d, x: uint16) =
    shiftOp(true, 16):
        state.dispatchSecondary(x)

proc lsr16*(state; d, x: uint16) =
    shiftOp(true, -16):
        state.dispatchSecondary(x)

proc asr16*(state; d, x: uint16) =
    shiftOp(false, -16):
        state.dispatchSecondary(x)

proc addp*(state; s, d, x: uint16) =
    let
        prod = cast[int64](signExtend(cast[uint64](state.readProduct()), 32))
        addend = int64(cast[int32](state.readReg(dspRegX1.succ(int s)))) shl 16
        sum = prod + addend

    state.dispatchSecondary(x)

    state.setC1(cast[uint64](prod), cast[uint64](addend))
    state.setV1(cast[uint64](sum), cast[uint64](prod), cast[uint64](addend))
    state.setZ1(cast[uint64](sum))
    state.setN1(cast[uint64](sum))
    state.setE1(cast[uint64](sum))
    state.setU1(cast[uint64](sum))
    state.writeAccum(int d, prod)

proc pnop*(state; x: uint16) =
    state.dispatchSecondary(x)

proc clrim*(state; x: uint16) =
    state.dispatchSecondary(x)
    state.status.im = false

proc clrdp*(state; x: uint16) =
    state.dispatchSecondary(x)
    state.status.dp = false

proc clrxl*(state; x: uint16) =
    state.dispatchSecondary(x)
    state.status.xl = false

proc setim*(state; x: uint16) =
    state.dispatchSecondary(x)
    state.status.im = true

proc setdp*(state; x: uint16) =
    state.dispatchSecondary(x)
    state.status.dp = true

proc setxl*(state; x: uint16) =
    state.dispatchSecondary(x)
    state.status.xl = true

proc getMulOperands(state; s: uint16): (uint16, uint16, bool, bool) =
    case range[2..11](s)
    of 2: (state.readReg(dspRegX1), state.readReg(dspRegX0), false, false)
    of 3: (state.readReg(dspRegY1), state.readReg(dspRegY0), false, false)
    of 4: (state.readReg(dspRegX0), state.readReg(dspRegY0), true, true)
    of 5: (state.readReg(dspRegX0), state.readReg(dspRegY1), true, false)
    of 6: (state.readReg(dspRegX1), state.readReg(dspRegY0), false, true)
    of 7: (state.readReg(dspRegX1), state.readReg(dspRegY1), false, false)
    of 8: (state.readReg(dspRegA1), state.readReg(dspRegX1), false, false)
    of 9: (state.readReg(dspRegA1), state.readReg(dspRegY1), false, false)
    of 10: (state.readReg(dspRegB1), state.readReg(dspRegX1), false, false)
    of 11: (state.readReg(dspRegB1), state.readReg(dspRegY1), false, false)

proc mpy*(state; s, x: uint16) =
    let
        (a, b, aImUnsigned, bImUnsigned) = state.getMulOperands(s)

        aSigned = if state.status.dp and aImUnsigned: int64(a) else: int64(cast[int16](a))
        bSigned = if state.status.dp and bImUnsigned: int64(b) else: int64(cast[int16](b))

        prod = aSigned * (if state.status.im: bSigned else: bSigned * 2)

    state.dispatchSecondary(x)

    state.writeProduct(prod)

proc mpy2*(state; x: uint16) =
    let
        factor = int64(cast[int16](state.readReg(dspRegX1)))
        prod = factor * (if state.status.im: factor * 2 else: factor)

    state.dispatchSecondary(x)

    state.writeProduct(prod)

template macOp(a, b: untyped, negate: bool): untyped =
    let
        aVal = int64(cast[int16](a))
        bVal = int64(cast[int16](b))

        prod = aVal * (if state.status.im: bVal else: bVal * 2)
        sum = state.readProduct() + (if negate: -prod else: prod)

    state.dispatchSecondary(x)

    state.writeProduct(sum)

proc mac*(state; s, x: uint16) =
    macOp(state.readReg(dspRegX0.succ(int(s.getBit(1))*2)), state.readReg(dspRegY0.succ(int(s.getBit(0))*2)), false)

proc mac2*(state; s, x: uint16) =
    macOp(state.readReg(dspRegA1.succ(int s.getBit(1))), state.readReg(dspRegX1.succ(int s.getBit(0))), false)

proc mac3*(state; s, x: uint16) =
    macOp(state.readReg(dspRegX1.succ(int s)), state.readReg(dspRegX0.succ(int s)), false)

proc macn*(state; s, x: uint16) =
    macOp(state.readReg(dspRegX0.succ(int(s.getBit(1))*2)), state.readReg(dspRegY0.succ(int(s.getBit(0))*2)), true)

proc macn2*(state; s, x: uint16) =
    macOp(state.readReg(dspRegA1.succ(int s.getBit(1))), state.readReg(dspRegX1.succ(int s.getBit(0))), true)

proc macn3*(state; s, x: uint16) =
    macOp(state.readReg(dspRegX1.succ(int s)), state.readReg(dspRegX0.succ(int s)), true)

proc mvmpy*(state; s, d, x: uint16) =
    let
        (a, b, aImUnsigned, bImUnsigned) = state.getMulOperands(s)

        aSigned = if state.status.dp and aImUnsigned: int64(a) else: int64(cast[int16](a))
        bSigned = if state.status.dp and bImUnsigned: int64(b) else: int64(cast[int16](b))

        oldProd = state.readProduct()
        prod = aSigned * (if state.status.im: bSigned else: bSigned * 2)

    state.dispatchSecondary(x)

    state.writeAccum(int d, oldProd)
    state.writeProduct(prod)

proc rnmpy*(state; s, d, x: uint16) =
    let
        oldProd = state.readProduct()
        oldProdRounded = round(oldProd)

        (a, b, aImUnsigned, bImUnsigned) = state.getMulOperands(s)

        aSigned = if state.status.dp and aImUnsigned: int64(a) else: int64(cast[int16](a))
        bSigned = if state.status.dp and bImUnsigned: int64(b) else: int64(cast[int16](b))

        prod = aSigned * (if state.status.im: bSigned else: bSigned * 2)

    state.dispatchSecondary(x)

    state.setC7(cast[uint64](oldProd), cast[uint64](oldProdRounded))
    state.setV6(cast[uint64](oldProd), cast[uint64](oldProdRounded))
    state.setZ1(cast[uint64](oldProdRounded))
    state.setN1(cast[uint64](oldProdRounded))
    state.setE1(cast[uint64](oldProdRounded))
    state.setU1(cast[uint64](oldProdRounded))
    state.writeAccum(int d, oldProdRounded)
    state.writeProduct(prod)

proc admpy*(state; s, d, x: uint16) =
    let
        accum = state.readAccum(int d)
        oldProd = state.readProduct()
        sum = accum + oldProd

        (a, b, aImUnsigned, bImUnsigned) = state.getMulOperands(s)

        aSigned = if state.status.dp and aImUnsigned: int64(a) else: int64(cast[int16](a))
        bSigned = if state.status.dp and bImUnsigned: int64(b) else: int64(cast[int16](b))

        prod = aSigned * (if state.status.im: bSigned else: bSigned * 2)

    state.dispatchSecondary(x)

    state.writeProduct(prod)
    state.setC1(cast[uint64](accum), cast[uint64](oldProd))
    state.setV1(cast[uint64](sum), cast[uint64](accum), cast[uint64](oldProd))
    state.setZ1(cast[uint64](sum))
    state.setN1(cast[uint64](sum))
    state.setE1(cast[uint64](sum))
    state.setU1(cast[uint64](sum))
    state.writeAccum(int d, sum)

proc nnot*(state; d, x: uint16) =
    logicOp(not reg):
        state.dispatchSecondary(x)

proc xxor*(state; s, d, x: uint16) =
    logicOp(reg xor state.readReg(dspRegX1.succ(int s))):
        state.dispatchSecondary(x)

proc xxor2*(state; d, x: uint16) =
    logicOp(reg xor state.readReg(dspRegB1.pred(int d))):
        state.dispatchSecondary(x)

proc aand*(state; s, d, x: uint16) =
    logicOp(reg and state.readReg(dspRegX1.succ(int s))):
        state.dispatchSecondary(x)

proc aand2*(state; d, x: uint16) =
    logicOp(reg and state.readReg(dspRegB1.pred(int d))):
        state.dispatchSecondary(x)

proc oor*(state; s, d, x: uint16) =
    logicOp(reg or state.readReg(dspRegX1.succ(int s))):
        state.dispatchSecondary(x)

proc oor2*(state; d, x: uint16) =
    logicOp(reg or state.readReg(dspRegB1.pred(int d))):
        state.dispatchSecondary(x)

proc lsf*(state; s, d, x: uint16) =
    shiftOp(true, cast[int16](state.readReg(dspRegX1.succ(int s)))):
        state.dispatchSecondary(x)

proc lsf2*(state; d, x: uint16) =
    shiftOp(true, cast[int16](state.readReg(dspRegB1.pred(int d)))):
        state.dispatchSecondary(x)

proc asf*(state; s, d, x: uint16) =
    shiftOp(false, cast[int16](state.readReg(dspRegX1.succ(int s)))):
        state.dispatchSecondary(x)

proc asf2*(state; d, x: uint16) =
    shiftOp(false, cast[int16](state.readReg(dspRegB1.pred(int d)))):
        state.dispatchSecondary(x)