import
    ../ppcstate, ../../util/aluhelper,
    ppcinterpreter_aux,
    bitops, stew/bitops2

using state: var PpcState

template r(num: uint32): uint32 {.dirty.} = state.r[num]

template handleRc(reg: uint32): untyped {.dirty.} =
    if rc != 0:
        state.setCr(0, cast[int32](r(reg)), 0)

proc addx*(state; d, a, b, oe, rc: uint32) =
    if oe != 0:
        state.xer.ov = overflowAdd(r(a), r(b))
        state.updateSo()

    r(d) = r(a) + r(b)

    handleRc d
    
proc addcx*(state; d, a, b, oe, rc: uint32) =
    if oe != 0:
        state.xer.ov = overflowAdd(r(a), r(b))
        state.updateSo()
    state.xer.ca = carryAdd(r(a), r(b))
    r(d) = r(a) + r(b)

    handleRc d

proc addex*(state; d, a, b, oe, rc: uint32) =
    let
        intermedRes = r(a) + r(b)
        carry = uint32 state.xer.ca

    if oe != 0:
        state.xer.ov = overflowAdd(r(a), r(b)) or
            overflowAdd(intermedRes, carry)
        state.updateSo()
    state.xer.ca = carryAdd(r(a), r(b)) or
        carryAdd(intermedRes, carry)

    r(d) = intermedRes + carry

    handleRc d

proc addi*(state; d, a, imm: uint32) =
    let imm = signExtend(imm, 16)
    if a == 0:
        r(d) = imm
    else:
        r(d) = r(a) + imm

proc addic*(state; d, a, imm: uint32) =
    let imm = signExtend(imm, 16)
    state.xer.ca = carryAdd(r(a), imm)

    r(d) = r(a) + imm

proc addicdot*(state; d, a, imm: uint32) =
    let imm = signExtend(imm, 16)
    state.xer.ca = carryAdd(r(a), imm)
    r(d) = r(a) + imm
    state.setCr(0, cast[int32](r(d)), 0)

proc addis*(state; d, a, imm: uint32) =
    let imm = imm shl 16
    if a == 0:
        r(d) = imm
    else:
        r(d) = r(a) + imm

proc addmex*(state; d, a, oe, rc: uint32) =
    let
        carry = uint32(state.xer.ca)
        intermedRes = r(a) + carry
    state.xer.ca = carryAdd(r(a), carry) or carrySub(intermedRes, 1)
    if oe != 0:
        state.xer.ov = overflowAdd(r(a), carry) or overflowSub(intermedRes, 1)
        state.updateSo()

    r(d) = intermedRes - 1
    handleRc d

proc addzex*(state; d, a, oe, rc: uint32) =
    let carry = uint32(state.xer.ca)
    state.xer.ca = carryAdd(r(a), carry)
    if oe != 0:
        state.xer.ov = overflowAdd(r(a), carry)
        state.updateSo()
    r(d) = r(a) + carry
    handleRc d

proc divwx*(state; d, a, b, oe, rc: uint32) =
    if r(b) == 0 or (r(a) == 0x8000_0000'u32 and r(b) == 0xFFFF_FFFF'u32):
        if oe != 0:
            state.xer.ov = true
            state.updateSo()
    else:
        if oe != 0:
            state.xer.ov = false
        r(d) = cast[uint32](cast[int32](r(a)) div cast[int32](r(b)))
        handleRc d

proc divwux*(state; d, a, b, oe, rc: uint32) =
    if r(b) == 0:
        if oe != 0'u32:
            state.xer.ov = true
            state.updateSo()
    else:
        r(d) = r(a) div r(b)

        if oe != 0:
            state.xer.ov = false
        handleRc d

proc mulhwx*(state; d, a, b, rc: uint32) =
    r(d) = uint32((int64(cast[int32](r(a))) * int64(cast[int32](r(b)))) shr 32'u64)
    handleRc d

proc mulhwux*(state; d, a, b, rc: uint32) =
    r(d) = uint32((uint64(r(a)) * uint64(r(b))) shr 32'u64)
    handleRc d

proc mulli*(state; d, a, imm: uint32) =
    r(d) = cast[uint32](int64(cast[int32](r(a))) * int64(cast[int32](signExtend(imm, 16))))

proc mullwx*(state; d, a, b, oe, rc: uint32) =
    let resFull = int64(cast[int32](r(a))) * int64(cast[int32](r(b)))

    if oe != 0:
        state.xer.ov = resFull < int64(low(int32)) or resFull > int64(high(int32))
        state.updateSo()

    r(d) = uint32(resFull)

    handleRc d

proc negx*(state; d, a, oe, rc: uint32) =
    if oe != 0:
        state.xer.ov = r(a) == 0x8000_0000'u32
        state.updateSo()
    r(d) = 0 - r(a)
    handleRc d

proc subfx*(state; d, a, b, oe, rc: uint32) =
    if oe != 0:
        state.xer.ov = overflowSub(r(b), r(a))
        state.updateSo()
    r(d) = r(b) - r(a)
    handleRc d

proc subfcx*(state; d, a, b, oe, rc: uint32) =
    state.xer.ca = carrySub(r(b), r(a))
    if oe != 0:
        state.xer.ov = overflowSub(r(b), r(a))
        state.updateSo()
    r(d) = r(b) - r(a)
    handleRc d

proc subfex*(state; d, a, b, oe, rc: uint32) =
    let
        carry = uint32 state.xer.ca
        immRes = r(b) + not(r(a))
    state.xer.ca = carryAdd(r(b), not r(a)) or carryAdd(immRes, carry)
    if oe != 0:
        state.xer.ov = overflowAdd(r(b), not r(a)) or overflowAdd(immRes, carry)
        state.updateSo()
    r(d) = immRes + carry
    handleRc d

proc subfic*(state; d, a, imm: uint32) =
    let imm = signExtend(imm, 16)
    state.xer.ca = carrySub(imm, r(a))
    r(d) = imm - r(a)

proc subfmex*(state; d, a, oe, rc: uint32) =
    let
        carry = uint32 state.xer.ca
        intermedRes = not(r(a)) + carry
    state.xer.ca = carryAdd(carry, not r(a)) or carrySub(intermedRes, 1)
    if oe != 0:
        state.xer.ov = overflowAdd(carry, not r(a)) or carrySub(intermedRes, 1)
        state.updateSo()
    r(d) = intermedRes - 1
    handleRc d

proc subfzex*(state; d, a, oe, rc: uint32) =
    let carry = uint32 state.xer.ca
    state.xer.ca = carryAdd(not r(a), carry)
    if oe != 0:
        state.xer.ov = overflowAdd(not r(a), carry)
        state.updateSo()
    r(d) = carry + not(r(a))
    handleRc d

proc cmp*(state; crfD, l, a, b: uint32) =
    state.setCr(int crfD, cast[int32](r(a)), cast[int32](r(b)))

proc cmpi*(state; crfD, l, a, imm: uint32) =
    state.setCr(int crfD, cast[int32](r(a)), cast[int32](signExtend(imm, 16)))

proc cmpl*(state; crfD, l, a, b: uint32) =
    state.setCr(int crfD, r(a), r(b))

proc cmpli*(state; crfD, l, a, imm: uint32) =
    state.setCr(int crfD, r(a), imm)

proc andx*(state; s, a, b, rc: uint32) =
    r(a) = r(s) and r(b)
    handleRc a

proc andcx*(state; s, a, b, rc: uint32) =
    r(a) = r(s) and not(r(b))
    handleRc a

proc andidot*(state; s, a, imm: uint32) =
    r(a) = r(s) and imm
    state.setCr(0, cast[int32](r(a)), 0)

proc andisdot*(state; s, a, imm: uint32) =
    r(a) = r(s) and (imm shl 16)
    state.setCr(0, cast[int32](r(a)), 0)

proc cntlzwx*(state; s, a, rc: uint32) =
    r(a) = if r(s) == 0: 32'u32 else: uint32(countLeadingZeroBits(r(s)))
    handleRc a

proc eqvx*(state; s, a, b, rc: uint32) =
    r(a) = not(r(s) xor r(b))
    handleRc a

proc extsbx*(state; s, a, rc: uint32) =
    r(a) = signExtend(r(s), 8)
    handleRc a

proc extshx*(state; s, a, rc: uint32) =
    r(a) = signExtend(r(s), 16)
    handleRc a

proc nandx*(state; s, a, b, rc: uint32) =
    r(a) = not(r(s) and r(b))
    handleRc a

proc norx*(state; s, a, b, rc: uint32) =
    r(a) = not(r(s) or r(b))
    handleRc a

proc orx*(state; s, a, b, rc: uint32) =
    r(a) = r(s) or r(b)
    handleRc a

proc orcx*(state; s, a, b, rc: uint32) =
    r(a) = r(s) or not(r(b))
    handleRc a

proc ori*(state; s, a, imm: uint32) =
    r(a) = r(s) or imm

proc oris*(state; s, a, imm: uint32) =
    r(a) = r(s) or (imm shl 16'u32)

proc xorx*(state; s, a, b, rc: uint32) =
    r(a) = r(s) xor r(b)
    handleRc a

proc xori*(state; s, a, imm: uint32) =
    r(a) = r(s) xor imm

proc xoris*(state; s, a, imm: uint32) =
    r(a) = r(s) xor (imm shl 16'u32)

proc mask(mb, me: uint32): uint32 =
    if mb > me:
        result = not toMask[uint32](int(31-mb+1)..int(31-me-1))
    else:
        result = toMask[uint32](int(31-me)..int(31-mb))

proc rlwimix*(state; s, a, sh, mb, me, rc: uint32) =
    let
        r = rotateLeftBits(r(s), sh)
        m = mask(mb, me)

    r(a) = (r and m) or (r(a) and not(m))

    handleRc a

proc rlwinmx*(state; s, a, sh, mb, me, rc: uint32) =
    let
        r = rotateLeftBits(r(s), sh)
        m = mask(mb, me)
    
    r(a) = r and m

    handleRc a

proc rlwnmx*(state; s, a, b, mb, me, rc: uint32) =
    let
        r = rotateLeftBits(r(s), r(b) and 0x1F'u32)
        m = mask(mb, me)

    r(a) = r and m

    handleRc a

proc slwx*(state; s, a, b, rc: uint32) =
    if r(b).getBit(5):
        r(a) = 0'u32
    else:
        r(a) = r(s) shl (r(b) and 0x1F)

    handleRc a

proc srawx*(state; s, a, b, rc: uint32) =
    if r(b).getBit(5):
        r(a) = cast[uint32](cast[int32](r(s)) shr 31)
        state.xer.ca = r(a).getBit(31)
    else:
        let
            shift = r(b) and 0x1F
            res = cast[int32](r(s)) shr shift
        if shift != 0:
            state.xer.ca = res < 0 and (r(s) and (0..int(shift)-1).toMask[:uint32]()) != 0
        else:
            state.xer.ca = false

        r(a) = cast[uint32](res)
    handleRc a

proc srawix*(state; s, a, sh, rc: uint32) =
    let res = cast[int32](r(s)) shr sh
    if sh != 0:
        state.xer.ca = res < 0 and (r(s) and (0..int(sh)-1).toMask[:uint32]()) != 0
    else:
        state.xer.ca = false
    r(a) = cast[uint32](res)

    handleRc a

proc srwx*(state; s, a, b, rc: uint32) =
    if r(b).getBit(5):
        r(a) = 0'u32
    else:
        r(a) = r(s) shr (r(b) and 0x1F)

    handleRc a

proc crand*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, state.cr.bit(int crbA) and state.cr.bit(int crbB))

proc crandc*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, state.cr.bit(int crbA) and not(state.cr.bit(int crbB)))

proc creqv*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, state.cr.bit(int crbA) == state.cr.bit(int crbB))

proc crnand*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, not(state.cr.bit(int crbA) and state.cr.bit(int crbB)))

proc crnor*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, not(state.cr.bit(int crbA) or state.cr.bit(int crbB)))

proc cror*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, state.cr.bit(int crbA) or state.cr.bit(int crbB))

proc crorc*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, state.cr.bit(int crbA) or not(state.cr.bit(int crbB)))

proc crxor*(state; crbD, crbA, crbB: uint32) =
    state.cr.bit(int crbD, state.cr.bit(int crbA) != state.cr.bit(int crbB))

proc mcrf*(state; crfD, crfS: uint32) =
    state.cr.crf(int crfD, state.cr.crf(int crfS))
