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
    of dspRegAdr0..dspRegAdr3: state.adrReg[ord(n) - ord(dspRegAdr0)] = val
    of dspRegInc0..dspRegInc3: state.incReg[ord(n) - ord(dspRegInc0)] = val
    of dspRegWrap0..dspRegWrap3: state.wrapReg[ord(n) - ord(dspRegWrap0)] = val
    of dspRegCallStack: state.callStack.push(val)
    of dspRegStatusStack: state.statusStack.push(val)
    of dspRegLoopAdrStack:
        state.loopCountStack.sp += 1
        state.loopAddrStack.push(val)
    of dspRegLoopCountStack:
        state.loopAddrStack.sp += 1
        state.loopCountStack.push(val)
    of dspRegA0, dspRegB0:
        state.mainAccum[ord(n) - ord(dspRegA0)].clearMask 0xFFFF'u64
        state.mainAccum[ord(n) - ord(dspRegA0)].setMask uint64(val)
    of dspRegA1, dspRegB1:
        state.mainAccum[ord(n) - ord(dspRegA1)].clearMask 0xFFFF_0000'u64
        state.mainAccum[ord(n) - ord(dspRegA1)].setMask uint64(val) shl 16
    of dspRegA2, dspRegB2:
        state.mainAccum[ord(n) - ord(dspRegA2)].clearMask 0xFFFF_FFFF_0000_0000'u64
        state.mainAccum[ord(n) - ord(dspRegA2)].setMask signExtend[uint64](val, 8) shl 32
    of dspRegX0, dspRegY0:
        state.auxAccum[ord(n) - ord(dspRegX0)].clearMask 0xFFFF'u32
        state.auxAccum[ord(n) - ord(dspRegX0)].setMask uint32(val)
    of dspRegX1, dspRegY1:
        state.auxAccum[ord(n) - ord(dspRegX1)].clearMask 0xFFFF_0000'u32
        state.auxAccum[ord(n) - ord(dspRegX1)].setMask uint32(val) shl 16
    of dspRegDpp: state.dpp = val
    of dspRegStatus:
        # TODO: check this, I've done a test about this before, but forgot how it went
        # but not all bits can be changed
        state.status = Status(val)
    of dspRegPs0:
        state.prod.clearMask 0xFFFF'u64
        state.prod.setMask uint64(val)
    of dspRegPs1:
        state.prod.clearMask 0xFFFF_0000'u64
        state.prod.setMask uint64(val) shl 16
    of dspRegPs2:
        state.prod.clearMask 0xFFFF_FFFF_0000_0000'u64
        state.prod.setMask signExtend[uint64](val, 8) shl 32
    of dspRegPc1:
        state.prodcarry = val

func readReg*(state; n: DspReg): uint16 {.inline.} =
    case n
    of dspRegAdr0..dspRegAdr3: state.adrReg[ord(n) - ord(dspRegAdr0)]
    of dspRegInc0..dspRegInc3: state.incReg[ord(n) - ord(dspRegInc0)]
    of dspRegWrap0..dspRegWrap3: state.wrapReg[ord(n) - ord(dspRegWrap0)]
    of dspRegCallStack: state.callStack.pop()
    of dspRegStatusStack: state.statusStack.pop()
    of dspRegLoopAdrStack:
        state.loopCountStack.sp -= 1
        state.loopAddrStack.pop()
    of dspRegLoopCountStack:
        state.loopAddrStack.sp -= 1
        state.loopCountStack.pop()
    of dspRegA0, dspRegB0:
        uint16(state.mainAccum[ord(n) - ord(dspRegA0)])
    of dspRegA1, dspRegB1:
        uint16(state.mainAccum[ord(n) - ord(dspRegA1)] shr 16)
    of dspRegA2, dspRegB2:
        uint16(state.mainAccum[ord(n) - ord(dspRegA2)] shr 32)
    of dspRegX0, dspRegY0:
        uint16(state.auxAccum[ord(n) - ord(dspRegX0)])
    of dspRegX1, dspRegY1:
        uint16(state.auxAccum[ord(n) - ord(dspRegX1)] shr 16)
    of dspRegDpp: state.dpp
    of dspRegStatus: uint16(state.status)
    of dspRegPs0: uint16(state.prod)
    of dspRegPs1: uint16(state.prod shr 16)
    of dspRegPs2: uint16(state.prod shr 32)
    of dspRegPc1: uint16(state.prodcarry)

func readAccum*(state; n: int): int64 {.inline.} =
    cast[int64](state.mainAccum[n])

func writeAccum*(state; n: int, val: int64) {.inline.} =
    state.mainAccum[n] = cast[uint64](val)

func loadAccum*(state; n: int, val: uint16) {.inline.} =
    if state.status.xl:
        state.mainAccum[n] = signExtend(uint64(val) shl 16, 32)
    else: 
        state.writeReg dspRegA1.succ(n), val

func storeAccum*(state; n: int): uint16 {.inline.} =
    if state.status.xl and (state.status.ov or state.status.ext):
        if cast[int64](state.mainAccum[n]) > 0xFFFF_FFFF'i64:
            0x8000'u16
        elif cast[int64](state.mainAccum[n]) < -0x1_0000_0000'i64:
            0x7FFF'u16
        else:
            uint16(state.mainAccum[n] shr 16)
    else:
        uint16(state.mainAccum[n] shr 16)

func readAuxAccum*(state; n: int): int64 {.inline.} =
    cast[int32](state.auxAccum[n])

func writeAuxAccum*(state; n: int, val: int64) {.inline.} =
    state.auxAccum[n] = uint32(cast[uint64](val))

# very inaccutare
func readProduct*(state): int64 {.inline.} =
    cast[int64](signExtend(state.prod + (uint64(state.prodcarry) shl 16), 40))

func writeProduct*(state; val: int64) {.inline.} =
    state.prod = cast[uint64](val)
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
    var nextAdr = adr + cast[uint32](inc)
    let
        mask = (wrap or 1) shl 1
        dadr = (adr xor nextAdr xor not(cast[uint32](inc))) and mask
    if cast[uint16](inc) >= 0x8000:
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
    assert state.status.ca == ((ds.getBit(39) and s.getBit(39)) or (not(dd.getBit(39)) and (ds.getBit(39) or s.getBit(39)))), &"wrong carry? {dd:08X}"
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
    (state.readReg(dspRegDpp) shl 8) or a

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