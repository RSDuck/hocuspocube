import
    stew/bitops2, bitops,
    ../../util/aluhelper,
    ../dspstate,
    strformat

using state: var DspState

template fetchFollowingImm*: uint16 {.dirty.} =
    state.pc += 1
    instrRead(state.pc)

func writeReg*(state; n: DspReg, val: uint16) {.inline.} =
    case n
    of r0..r3: state.adrReg[ord(n) - ord(r0)] = val
    of m0..m3: state.incReg[ord(n) - ord(m0)] = val
    of l0..l3: state.wrapReg[ord(n) - ord(l0)] = val
    of pcs: state.callStack.push(val)
    of pss: state.statusStack.push(val)
    of eas:
        state.loopCountStack.sp += 1
        state.loopAddrStack.push(val)
    of lcs:
        state.loopAddrStack.sp += 1
        state.loopCountStack.push(val)
    of a0, b0:
        state.mainAccum[ord(n) - ord(a0)].clearMask 0xFFFF'u64
        state.mainAccum[ord(n) - ord(a0)].setMask uint64(val)
    of a1, b1:
        state.mainAccum[ord(n) - ord(a1)].clearMask 0xFFFF_0000'u64
        state.mainAccum[ord(n) - ord(a1)].setMask uint64(val) shl 16
    of a2, b2:
        state.mainAccum[ord(n) - ord(a2)].clearMask 0xFFFF_FFFF_0000_0000'u64
        state.mainAccum[ord(n) - ord(a2)].setMask signExtend[uint64](val, 8) shl 32
    of x0, y0:
        state.auxAccum[ord(n) - ord(x0)].clearMask 0xFFFF'u32
        state.auxAccum[ord(n) - ord(x0)].setMask uint32(val)
    of x1, y1:
        state.auxAccum[ord(n) - ord(x1)].clearMask 0xFFFF_0000'u32
        state.auxAccum[ord(n) - ord(x1)].setMask uint32(val) shl 16
    of dpp: state.dpp = val
    of psr:
        # TODO: check this, I've done a test about this before, but forgot how it went
        # but not all bits can be changed
        state.status = Status(val)
    of ps0:
        state.prod.clearMask 0xFFFF'u64
        state.prod.setMask uint64(val)
    of ps1:
        state.prod.clearMask 0xFFFF_0000'u64
        state.prod.setMask uint64(val) shl 16
    of ps2:
        state.prod.clearMask 0xFFFF_FFFF_0000_0000'u64
        state.prod.setMask signExtend[uint64](val, 8) shl 32
    of pc1:
        state.prodcarry = val

func readReg*(state; n: DspReg): uint16 {.inline.} =
    case n
    of r0..r3: state.adrReg[ord(n) - ord(r0)]
    of m0..m3: state.incReg[ord(n) - ord(m0)]
    of l0..l3: state.wrapReg[ord(n) - ord(l0)]
    of pcs: state.callStack.pop()
    of pss: state.statusStack.pop()
    of eas:
        state.loopCountStack.sp -= 1
        state.loopAddrStack.pop()
    of lcs:
        state.loopAddrStack.sp -= 1
        state.loopCountStack.pop()
    of a0, b0:
        uint16(state.mainAccum[ord(n) - ord(a0)])
    of a1, b1:
        uint16(state.mainAccum[ord(n) - ord(a1)] shr 16)
    of a2, b2:
        uint16(state.mainAccum[ord(n) - ord(a2)] shr 32)
    of x0, y0:
        uint16(state.auxAccum[ord(n) - ord(x0)])
    of x1, y1:
        uint16(state.auxAccum[ord(n) - ord(x1)] shr 16)
    of dpp: state.dpp
    of psr: uint16(state.status)
    of ps0: uint16(state.prod)
    of ps1: uint16(state.prod shr 16)
    of ps2: uint16(state.prod shr 32)
    of pc1: uint16(state.prodcarry)

func readAccum*(state; n: int): int64 {.inline.} =
    cast[int64](state.mainAccum[n])

func writeAccum*(state; n: int, val: int64) {.inline.} =
    state.mainAccum[n] = signExtend(cast[uint64](val), 40)

func loadAccum*(state; n: int, val: uint16) {.inline.} =
    if state.status.xl:
        state.mainAccum[n] = signExtend(uint64(val) shl 16, 32)
    else: 
        state.writeReg a1.succ(n), val

func storeAccum*(state; n: int): uint16 {.inline.} =
    if not state.status.xl or cast[uint64](int64(cast[int32](state.mainAccum[n]))) == state.mainAccum[n]:
        uint16(state.mainAccum[n] shr 16)
    elif state.mainAccum[n] < 0:
        0x8000'u16
    else:
        0x7FFF'u16            

func readAuxAccum*(state; n: int): int64 {.inline.} =
    cast[int32](state.auxAccum[n])

func writeAuxAccum*(state; n: int, val: int64) {.inline.} =
    state.auxAccum[n] = uint32(cast[uint64](val))

# very inaccutare
func readProduct*(state): int64 {.inline.} =
    cast[int64](signExtend(state.prod + (uint64(state.prodcarry) shl 16), 40))

func writeProduct*(state; val: int64) {.inline.} =
    state.prod = signExtend(cast[uint64](val), 40)
    state.prodcarry = 0

# dsp address increment/decrement
# figured out by kiesel-stein and minified by Mylek
# https://forums.dolphin-emu.org/Thread-patch-dsp-lle-faster-masked-math?page=6
func incAdr*(adr, wrap: uint16): uint16 =
    let
        adr = uint32 adr
        wrap = uint32 wrap
    var nextAdr = adr + 1
    if (nextAdr xor adr) > ((wrap or 1) shl 1):
        nextAdr -= wrap + 1

    uint16 nextAdr

func decAdr*(adr, wrap: uint16): uint16 =
    let
        adr = uint32 adr
        wrap = uint32 wrap
    var nextAdr = adr + wrap
    if ((nextAdr xor adr) and ((wrap or 1) shl 1)) > wrap:
        nextAdr -= wrap + 1

    uint16 nextAdr

func incAdr*(adr, wrap: uint16, inc: int16): uint16 =
    let
        adr = uint32 adr
        wrap = uint32 wrap
        inc = int32 inc
    var nextAdr = adr + cast[uint32](inc)
    let
        mask = (wrap or 1) shl 1
        dadr = (adr xor nextAdr xor cast[uint32](inc)) and mask
    if inc >= 0:
        if dadr > wrap:
            nextAdr -= wrap + 1
    else:
        if (((nextAdr + wrap + 1) xor nextAdr) and dadr) <= wrap:
            nextAdr += wrap + 1

    #debugEcho &"inc adr {adr:02X} {wrap:02X} {inc:02X} {nextAdr:02X}"

    uint16 nextAdr

func decAdr*(adr, wrap: uint16, inc: int16): uint16 =
    let
        adr = uint32 adr
        wrap = uint32 wrap
        inc = int32 inc
    var nextAdr = adr - cast[uint32](inc)
    let
        mask = (wrap or 1) shl 1
        dadr = (adr xor nextAdr xor not(cast[uint32](inc))) and mask
    if cast[uint32](inc) > 0xFFFF8000'u32:
        if dadr > wrap:
            nextAdr -= wrap + 1
    else:
        if (((nextAdr + wrap + 1) xor nextAdr) and dadr) <= wrap:
            nextAdr += wrap + 1

    #debugEcho &"dec adr {adr:02X} {wrap:02X} {inc:02X} {nextAdr:02X}"

    uint16 nextAdr

func loadStoreAdrInc*(state; m: range[0..3], rn: int) =
    case m
    of 0: discard
    of 1: state.adrReg[rn] = decAdr(state.adrReg[rn], state.wrapReg[rn])
    of 2: state.adrReg[rn] = incAdr(state.adrReg[rn], state.wrapReg[rn])
    of 3: state.adrReg[rn] = incAdr(state.adrReg[rn], state.wrapReg[rn], cast[int16](state.incReg[rn]))

func setC1*(state; ds, s: uint64) =
    state.status.ca = (0xFFFF_FFFF_FFFF_FFFF'u64 - ds) < s
    let dd = ds + s
    assert state.status.ca == ((ds.getBit(39) and s.getBit(39)) or (not(dd.getBit(39)) and (ds.getBit(39) or s.getBit(39)))), &"wrong carry? {state.status.ca} {dd:X} {ds:X} {s:X}"
func setC2*(state; ds, s: uint64) =
    state.status.ca = ds >= s
    let dd = ds - s
    assert state.status.ca == ((ds.getBit(39) and not(s.getBit(39))) or (not(dd.getBit(39)) and (ds.getBit(39) or not(s.getBit(39))))), &"wrong carry? {state.status.ca} {dd:X} {ds:X} {s:X}"
    #state.status.ca = (dd.getBit(39) and not(s.getBit(39))) or (not(dd.getBit(39)) and (ds.getBit(39) or not(s.getBit(39))))
func setC7*(state; p, d: uint64) =
    state.status.ca = p.getBit(39) and not(d.getBit(39))

func setV1*(state; dd, ds, s: uint64) =
    state.status.ov = ds.getBit(39) == s.getBit(39) and dd.getBit(39) != ds.getBit(39)
func setV2*(state; dd, ds, s: uint64) =
    state.status.ov = ds.getBit(39) != s.getBit(39) and dd.getBit(39) != ds.getBit(39)
func setV5*(state; dd: uint64) =
    state.status.ov = dd.getBit(39)
func setV6*(state; p, d: uint64) =
    state.status.ov = not(p.getBit(39)) and d.getBit(39)

func setZ1*(state; dd: uint64) =
    state.status.zr = (dd and 0xFF_FFFF_FFFF'u64) == 0
func setZ2*(state; dd: uint16) =
    state.status.zr = dd == 0

func setN1*(state; dd: uint64) =
    state.status.mi = dd.getBit(39)
func setN2*(state; dd: uint16) =
    state.status.mi = dd.getBit(15)

func setE1*(state; full: uint64) =
    let masked = full and 0xFF_8000_0000'u64
    state.status.ext = masked != 0'u64 and masked != 0xFF_8000_0000'u64
func setE1*(state; hi, mid: uint16) =
    # for instructions which may only operate on the middle part
    # the flag will be based on the high part which has to be passed in!
    state.status.ext = if mid.getBit(15): hi != 0xFFFF'u16 else: hi != 0'u16

func setU1*(state; full: uint64) =
    state.status.unnorm = full.getBit(31) == full.getBit(30)
func setU1*(state; mid: uint16) =
    state.status.unnorm = mid.getBit(15) == mid.getBit(14)

func dppAdr*(state; a: uint16): uint16 =
    (state.readReg(dpp) shl 8) or a

proc conditionHolds*(state; cond: uint32): bool =
    let
        equal = state.status.zr
        isLess = state.status.ov != state.status.mi
        isLequal = equal or isLess

    case range[0..15](cond)
    of 0: not isLess # greater or equal
    of 1: isLess # less
    of 2: not isLequal # greater
    of 3: isLequal # less or equal
    of 4: not equal # not equal
    of 5: equal # equal
    of 6: not state.status.ca
    of 7: state.status.ca
    of 8: not state.status.ext
    of 9: state.status.ext
    of 10: not state.status.zr and (state.status.ext or state.status.unnorm)
    of 11: state.status.zr or not(state.status.ext or state.status.unnorm)
    of 12: not state.status.tb
    of 13: state.status.tb
    of 14: state.status.ov
    of 15: true