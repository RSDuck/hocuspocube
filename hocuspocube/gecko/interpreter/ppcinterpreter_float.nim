import
    ../ppcstate, ppcinterpreter_aux,

    math

using state: var PpcState

template fr(num: uint32): PairedSingle {.dirty.} = state.fr[num]

# TODO: handle more of those thousands of float flags and exceptions

template setFprf(x: float64) {.dirty.} =
    state.fpscr.fprf = (case classify(x)
        of fcNormal: (if x > 0.0: 0b00100'u32 else: 0b01000'u32)
        of fcSubnormal: (if x > 0.0: 0b10100'u32 else: 0b11000'u32)
        of fcZero: 0b00010'u32
        of fcNegZero: 0b10010'u32
        of fcNan: 0b10001'u32
        of fcInf: 0b00101'u32
        of fcNegInf: 0b01001'u32)

func msub(a, b, c: float64): float64 =
    if isNan(a): a
    elif isNan(b): b
    elif isNan(c): c
    else: a * c - b

func madd(a, b, c: float64): float64 =
    if isNan(a): a
    elif isNan(b): b
    elif isNan(c): c
    else: a * c + b

func nmsub(a, b, c: float64): float64 =
    if isNan(a): a
    elif isNan(b): b
    elif isNan(c): c
    else: -(a * c - b)

func nmadd(a, b, c: float64): float64 =
    if isNan(a): a
    elif isNan(b): b
    elif isNan(c): c
    else: -(a * c + b)

template handleRc {.dirty.} =
    state.cr.ox = state.fpscr.ox
    state.cr.vx = state.fpscr.vx
    state.cr.fex = state.fpscr.fex
    state.cr.fx = state.fpscr.fx

proc faddx*(state; d, a, b, rc: uint32) =
    fr(d).double = fr(a).double + fr(b).double
    setFprf fr(d).double
    handleRc

proc faddsx*(state; d, a, b, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 + fr(b).ps0
    fr(d).ps1 = fr(d).ps1
    setFprf fr(d).ps0
    handleRc

proc fdivx*(state; d, a, b, rc: uint32) =
    fr(d).double = fr(a).double / fr(b).double
    setFprf fr(d).double
    handleRc

proc fdivsx*(state; d, a, b, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 / fr(b).ps0
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fmulx*(state; d, a, c, rc: uint32) =
    fr(d).double = fr(a).double * fr(c).double
    setFprf fr(d).double
    handleRc

proc fmulsx*(state; d, a, c, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 * fr(c).ps0
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fresx*(state; d, b, rc: uint32) =
    raiseAssert "instr not implemented"

proc frsqrtex*(state; d, b, rc: uint32) =
    fr(d).double = 1.0 / sqrt(fr(b).double)
    setFprf fr(d).double
    handleRc

proc fsubx*(state; d, a, b, rc: uint32) =
    fr(d).double = fr(a).double - fr(b).double
    setFprf fr(d).double
    handleRc

proc fsubsx*(state; d, a, b, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 - fr(b).ps0
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fselx*(state; d, a, b, c, rc: uint32) =
    raiseAssert "instr not implemented"

proc fmaddx*(state; d, a, b, c, rc: uint32) =
    fr(d).double = madd(fr(a).double, fr(b).double, fr(c).double)
    setFprf fr(d).double
    handleRc

proc fmaddsx*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = madd(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fmsubx*(state; d, a, b, c, rc: uint32) =
    fr(d).double = msub(fr(a).double, fr(b).double, fr(c).double)
    setFprf fr(d).double
    handleRc

proc fmsubsx*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = msub(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fnmaddx*(state; d, a, b, c, rc: uint32) =
    fr(d).double = nmadd(fr(a).double, fr(b).double, fr(c).double)
    setFprf fr(d).double
    handleRc

proc fnmaddsx*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = nmadd(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fnmsubx*(state; d, a, b, c, rc: uint32) =
    fr(d).double = nmsub(fr(a).double, fr(b).double, fr(c).double)
    setFprf fr(d).double
    handleRc

proc fnmsubsx*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = nmsub(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = fr(d).ps0
    setFprf fr(d).ps0
    handleRc

proc fctiwx*(state; d, b, rc: uint32) =
    raiseAssert "instr not implemented"

proc fctiwzx*(state; d, b, rc: uint32) =
    if fr(b).double > float64(high(int32)):
        fr(d).double = cast[float64](cast[uint64](high(int32)))
    elif fr(b).double < float64(low(int32)):
        fr(d).double = cast[float64](cast[uint64](low(int32)))
    else:
        fr(d).double = cast[float64](cast[uint64](int32(fr(b).double)))
    handleRc

proc frspx*(state; d, b, rc: uint32) =
    fr(d).ps0 = fr(b).double
    setFprf fr(d).ps0
    handleRc

proc fcmpo*(state; crfD, a, b: uint32) =
    var cr = 0'u32
    if isNan(fr(a).double) or isNan(fr(b).double):
        cr = 0b0001
    else:
        if fr(a).double < fr(b).double:
            cr = 0b1000
        elif fr(a).double > fr(b).double:
            cr = 0b0100
        else:
            cr = 0b0010
    state.cr.crf int crfD, cr
    state.fpscr.fprf = cr

proc fcmpu*(state; crfD, a, b: uint32) =
    var cr = 0'u32
    if isNan(fr(a).double) or isNan(fr(b).double):
        cr = 0b0001
    else:
        if fr(a).double < fr(b).double:
            cr = 0b1000
        elif fr(a).double > fr(b).double:
            cr = 0b0100
        else:
            cr = 0b0010
    state.cr.crf int crfD, cr
    state.fpscr.fprf = cr

proc mffsx*(state; d, rc: uint32) =
    fr(d).double = cast[float64](uint64(state.fpscr))
    handleRc

proc mtfsb0x*(state; crbD, rc: uint32) =
    state.fpscr.bit int crbD, false
    handleRc

proc mtfsb1x*(state; crbD, rc: uint32) =
    state.fpscr.bit int crbD, true
    handleRc

proc mtfsfx*(state; fm, b, rc: uint32) =
    let mask = makeFieldMask(fm)
    # TODO: fex and vx shouldn't be setable by themselves
    state.fpscr = Fpscr(uint32(state.fpscr) and not(mask) or (uint32(cast[uint64](fr(b).double)) and mask))

proc mtfsfix*(state; crfD, imm, rc: uint32) =
    # TODO: same thing here
    state.fpscr.crf int crfD, imm
    handleRc

proc fabsx*(state; d, b, rc: uint32) =
    fr(d).double = abs(fr(b).double)
    handleRc

proc fmrx*(state; d, b, rc: uint32) =
    fr(d).double = fr(b).double
    handleRc

proc fnabsx*(state; d, b, rc: uint32) =
    raiseAssert "instr not implemented"

proc fnegx*(state; d, b, rc: uint32) =
    fr(d).double = -fr(b).double
    handleRc

proc ps_div*(state; d, a, b, rc: uint32) =
    raiseAssert "instr not implemented"

proc ps_sub*(state; d, a, b, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 - fr(b).ps0
    fr(d).ps1 = fr(a).ps1 - fr(b).ps1
    setFprf fr(d).ps0
    handleRc

proc ps_add*(state; d, a, b, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 + fr(b).ps0
    fr(d).ps1 = fr(a).ps1 + fr(b).ps1
    setFprf fr(d).ps0
    handleRc

proc ps_sel*(state; d, a, b, c, rc: uint32) =
    raiseAssert "instr not implemented"

proc ps_res*(state; d, b, rc: uint32) =
    fr(d).ps0 = 1f / fr(b).ps0
    fr(d).ps1 = 1f / fr(b).ps1
    setFprf fr(d).ps0
    handleRc

proc ps_mul*(state; d, a, c, rc: uint32) =
    fr(d).ps0 = fr(a).ps0 * fr(c).ps0
    fr(d).ps1 = fr(a).ps1 * fr(c).ps1
    setFprf fr(d).ps0
    handleRc

proc ps_rsqrte*(state; d, b, rc: uint32) =
    fr(d).ps0 = 1f / sqrt(fr(b).ps0)
    fr(d).ps1 = 1f / sqrt(fr(b).ps1)
    setFprf fr(d).ps0
    handleRc

proc ps_msub*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = msub(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = msub(fr(a).ps1, fr(b).ps1, fr(c).ps1)
    setFprf fr(d).ps0
    handleRc

proc ps_madd*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = madd(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = madd(fr(a).ps1, fr(b).ps1, fr(c).ps1)
    setFprf fr(d).ps0
    handleRc

proc ps_nmsub*(state; d, a, b, c, rc: uint32) =
    fr(d).ps0 = nmsub(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = nmsub(fr(a).ps1, fr(b).ps1, fr(c).ps1)
    setFprf fr(d).ps0
    handleRc

proc ps_nmadd*(state; d, a, b, c, rc : uint32) =
    fr(d).ps0 = nmadd(fr(a).ps0, fr(b).ps0, fr(c).ps0)
    fr(d).ps1 = nmadd(fr(a).ps1, fr(b).ps1, fr(c).ps1)
    setFprf fr(d).ps0
    handleRc

proc ps_neg*(state; d, b, rc: uint32) =
    fr(d).ps0 = -fr(b).ps0
    fr(d).ps1 = -fr(b).ps1
    handleRc

proc ps_mr*(state; d, b, rc: uint32) =
    fr(d).ps0 = fr(b).ps0
    fr(d).ps1 = fr(b).ps1
    handleRc

proc ps_nabs*(state; d, b, rc: uint32) =
    raiseAssert "instr not implemented"

proc ps_abs*(state; d, b, rc: uint32) =
    raiseAssert "instr not implemented"

# for horizontal vector operations we need to be careful
# for the case when a destination register is also a source register

proc ps_sum0*(state; d, a, b, c, rc: uint32) =
    let
        fra = fr(a)
        frb = fr(b)
        frc = fr(c)
    fr(d).ps0 = fra.ps0 + frb.ps1
    fr(d).ps1 = frc.ps1
    setFprf fr(d).ps0
    handleRc

proc ps_sum1*(state; d, a, b, c, rc: uint32) =
    let
        fra = fr(a)
        frb = fr(b)
        frc = fr(c)
    fr(d).ps0 = frc.ps0
    fr(d).ps1 = fra.ps0 + frb.ps1
    setFprf fr(d).ps0
    handleRc

proc ps_muls0*(state; d, a, c, rc: uint32) =
    let frc = fr(c).ps0
    fr(d).ps0 = fr(a).ps0 * frc
    fr(d).ps1 = fr(a).ps1 * frc
    setFprf fr(d).ps0
    handleRc

proc ps_muls1*(state; d, a, c, rc: uint32) =
    let frc = fr(c).ps1
    fr(d).ps0 = fr(a).ps0 * frc
    fr(d).ps1 = fr(a).ps1 * frc
    setFprf fr(d).ps0
    handleRc

proc ps_madds0*(state; d, a, b, c, rc: uint32) =
    let frc = fr(c).ps0
    fr(d).ps0 = madd(fr(a).ps0, fr(b).ps0, frc)
    fr(d).ps1 = madd(fr(a).ps1, fr(b).ps1, frc)
    setFprf fr(d).ps0
    handleRc

proc ps_madds1*(state; d, a, b, c, rc: uint32) =
    let frc = fr(c).ps1
    fr(d).ps0 = madd(fr(a).ps0, fr(b).ps0, frc)
    fr(d).ps1 = madd(fr(a).ps1, fr(b).ps1, frc)
    setFprf fr(d).ps0
    handleRc

proc ps_cmpu0*(state; crfD, a, b: uint32) =
    var cr = 0'u32
    if isNan(fr(a).ps0) or isNan(fr(b).ps0):
        cr = 0b0001
    else:
        if fr(a).ps0 < fr(b).ps0:
            cr = 0b1000
        elif fr(a).ps0 > fr(b).ps0:
            cr = 0b0100
        else:
            cr = 0b0010
    state.cr.crf int crfD, cr
    state.fpscr.fprf = cr

proc ps_cmpo0*(state; crfD, a, b: uint32) =
    var cr = 0'u32
    if isNan(fr(a).ps0) or isNan(fr(b).ps0):
        cr = 0b0001
    else:
        if fr(a).ps0 < fr(b).ps0:
            cr = 0b1000
        elif fr(a).ps0 > fr(b).ps0:
            cr = 0b0100
        else:
            cr = 0b0010
    state.cr.crf int crfD, cr
    state.fpscr.fprf = cr

proc ps_cmpu1*(state; crfD, a, b: uint32) =
    var cr = 0'u32
    if isNan(fr(a).ps1) or isNan(fr(b).ps1):
        cr = 0b0001
    else:
        if fr(a).ps1 < fr(b).ps1:
            cr = 0b1000
        elif fr(a).ps1 > fr(b).ps1:
            cr = 0b0100
        else:
            cr = 0b0010
    state.cr.crf int crfD, cr
    state.fpscr.fprf = cr

proc ps_cmpo1*(state; crfD, a, b: uint32) =
    var cr = 0'u32
    if isNan(fr(a).ps1) or isNan(fr(b).ps1):
        cr = 0b0001
    else:
        if fr(a).ps1 < fr(b).ps1:
            cr = 0b1000
        elif fr(a).ps1 > fr(b).ps1:
            cr = 0b0100
        else:
            cr = 0b0010
    state.cr.crf int crfD, cr
    state.fpscr.fprf = cr

proc ps_merge00*(state; d, a, b, rc: uint32) =
    let
        fra = fr(a)
        frb = fr(b)
    fr(d).ps0 = fra.ps0
    fr(d).ps1 = frb.ps0
    handleRc

proc ps_merge01*(state; d, a, b, rc: uint32) =
    let
        fra = fr(a)
        frb = fr(b)
    fr(d).ps0 = fra.ps0
    fr(d).ps1 = frb.ps1
    handleRc

proc ps_merge10*(state; d, a, b, rc: uint32) =
    let
        fra = fr(a)
        frb = fr(b)
    fr(d).ps0 = fra.ps1
    fr(d).ps1 = frb.ps0
    handleRc

proc ps_merge11*(state; d, a, b, rc: uint32) =
    let
        fra = fr(a)
        frb = fr(b)
    fr(d).ps0 = fra.ps1
    fr(d).ps1 = frb.ps1
    handleRc
