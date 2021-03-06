import
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
    of 1: state.writeReg dspRegAdr0.succ(r), decAdr(adrReg(r), wrapReg(r))
    of 2: state.writeReg dspRegAdr0.succ(r), incAdr(adrReg(r), wrapReg(r))
    of 3: state.writeReg dspRegAdr0.succ(r), decAdr(adrReg(r), wrapReg(r), cast[int16](incReg(r)))
    of 4..7: state.writeReg dspRegAdr0.succ(r), incAdr(adrReg(r), wrapReg(r), cast[int16](incReg(int m - 4)))

proc adsi*(state; d, i: uint16) =
    raiseAssert "unimplemented dsp instr"

proc adli*(state; d: uint16) =
    let
        imm = int64(fetchFollowingImm) shl 16

        accum = state.readAccum(int d)
        sum = accum + imm

    state.writeAccum(int d, sum)
    state.setC1(cast[uint64](accum), cast[uint64](imm))
    state.setV1(cast[uint64](sum), cast[uint64](accum), cast[uint64](imm))
    state.setZ1(cast[uint64](sum))
    state.setN1(cast[uint64](sum))
    state.setE1(cast[uint64](sum))
    state.setU1(cast[uint64](sum))

proc cmpsi*(state; s, i: uint16) =
    raiseAssert "unimplemented dsp instr"

proc cmpli*(state; s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lsfi*(state; d, i: uint16) =
    let 
        shift = cast[int16](signExtend(i, 7))
        val = (if shift < 0:
            state.readAccum(int d) shr -shift
        else:
            state.readAccum(int d) shl shift)

    state.writeAccum(int d, val)

    state.status.ca = false
    state.status.ov = false
    state.setZ1(cast[uint64](val))
    state.setN1(cast[uint64](val))
    state.setE1(cast[uint64](val))
    state.setU1(cast[uint64](val))

proc asfi*(state; d, i: uint16) =
    raiseAssert "unimplemented dsp instr"

proc xorli*(state; d: uint16) =
    raiseAssert "unimplemented dsp instr"

proc anli*(state; d: uint16) =
    let
        mask = fetchFollowingImm
        val = state.readReg(dspRegA1.succ(int d)) and mask

    state.writeReg(dspRegA1.succ(int d), val)

    state.status.ca = false
    state.status.ov = false
    state.setZ2(val)
    state.setN2(val)
    state.setE1(state.readReg(dspRegA2.succ(int d)))
    state.setU1(val)

proc orli*(state; d: uint16) =
    raiseAssert "unimplemented dsp instr"

proc norm*(state; d, r: uint16) =
    raiseAssert "unimplemented dsp instr"

proc ddiv*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc addc*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc subc*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc negc*(state; d: uint16) =
    raiseAssert "unimplemented dsp instr"

proc max*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lfs*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lfs2*(state; d: uint16) =
    raiseAssert "unimplemented dsp instr"

proc asf*(state; d, s: uint16) =
    raiseAssert "unimplemented dsp instr"

proc asf2*(state; d: uint16) =
    raiseAssert "unimplemented dsp instr"

proc mv*(state; d, s: uint16) =
    state.writeReg DspReg(d), state.r[DspReg(s)]

proc mvsi*(state; d, i: uint16) =
    if d < 6:
        state.writeReg(dspRegX0.succ(int d), i)
    else:
        state.loadAccum(int d - 6, i)

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
    let
        addend = case range[0..7](s)
            of 0..3: int64(state.readReg(dspRegX0.succ(int s))) shl 16
            of 4..5: state.readAuxAccum(int s - 4)
            of 6: state.readAccum((int s - 6) xor 1)
            of 7: state.readProduct()
        accum = state.readAccum(int d)
        sum = addend + accum

    state.dispatchSecondary(x)

    state.writeAccum(int d, sum)
    state.setC1(cast[uint64](accum), cast[uint64](addend))
    state.setV1(cast[uint64](sum), cast[uint64](accum), cast[uint64](addend))
    state.setZ1(cast[uint64](sum))
    state.setN1(cast[uint64](sum))
    state.setE1(cast[uint64](sum))
    state.setU1(cast[uint64](sum))

proc addl*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc sub*(state; s, d, x: uint16) =
    let
        subtrahend = case range[0..7](s)
            of 0..3: int64(state.readReg(dspRegX0.succ(int s))) shl 16
            of 4..5: state.readAuxAccum(int s - 4)
            of 6: state.readAccum((int s - 6) xor 1)
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
    raiseAssert "unimplemented dsp instr"

proc cmp*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc cmpa*(state; x: uint16) =
    let
        a = state.readAccum(0)
        b = state.readAccum(1)

    state.dispatchSecondary(x)

    let diff = a - b
    state.setC2(cast[uint64](a), cast[uint64](b))
    state.setV2(cast[uint64](diff), cast[uint64](a), cast[uint64](b))
    state.setZ1(cast[uint64](diff))
    state.setN1(cast[uint64](diff))
    state.setE1(cast[uint64](diff))
    state.setU1(cast[uint64](diff))
    echo &"comparing {a:04X} by {b:04X} {diff:04X} {state.status.ca} {state.status.ov} {state.status.zr} {state.status.mi} {state.status.ext} {state.status.unnorm}"

proc inc*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc dec*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc abs*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc neg*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc negp*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc clra*(state; d, x: uint16) =
    state.writeAccum int(d), 0
    state.status.ca = false
    state.status.ov = false
    state.status.zr = true
    state.status.mi = false
    state.status.ext = false
    state.status.unnorm = false

proc clrp*(state; x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc rnd*(state; d, x: uint16) =
    raiseAssert &"unimplemented dsp instr {state.pc:04X}"

proc rndp*(state; d, x: uint16) =
    raiseAssert &"unimplemented dsp instr {state.pc:04X}"

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
    raiseAssert "unimplemented dsp instr"

proc tstp*(state; x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lsl16*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lsr16*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc asr16*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc addp*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc pnop*(state; x: uint16) =
    state.dispatchSecondary(x)

proc clrim*(state; x: uint16) =
    raiseAssert "unimplemented dsp instr"

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

proc mpy*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc mpy2*(state; x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc mac*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc mac2*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc mac3*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc macn*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc macn2*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc macn3*(state; s, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc mvmpy*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc rnmpy*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc admpy*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc nnot*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc xxor*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc xxor2*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc aand*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc aand2*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc oor*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc oor2*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lsf*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc lsf2*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc asf*(state; s, d, x: uint16) =
    raiseAssert "unimplemented dsp instr"

proc asf2*(state; d, x: uint16) =
    raiseAssert "unimplemented dsp instr"