import
    ../../util/aluhelper,
    ../dspstate,
    stew/bitops2,
    strformat

using state: var DspState

template fetchFollowingImm*: uint16 {.dirty.} =
    state.pc += 1
    instrRead(state.pc)

func writeReg*(state; n: DspReg, val: uint16) {.inline.} =
    case n
    of dspRegCallStack: state.callStack.push(val)
    of dspRegStatusStack: state.statusStack.push(val)
    of dspRegLoopAdrStack:
        state.loopCountStack.sp += 1
        state.loopAddrStack.push(val)
    of dspRegLoopCountStack:
        state.loopAddrStack.sp += 1
        state.loopCountStack.push(val)
    of dspRegA2, dspRegB2, dspRegPs2: state.r[n] = signExtend[uint16](val, 8)
    else: state.r[n] = val

func readReg*(state; n: DspReg): uint16 {.inline.} =
    case n
    of dspRegCallStack: state.callStack.pop()
    of dspRegStatusStack: state.statusStack.pop()
    of dspRegLoopAdrStack:
        state.loopCountStack.sp -= 1
        state.loopAddrStack.pop()
    of dspRegLoopCountStack:
        state.loopAddrStack.sp -= 1
        state.loopCountStack.pop()
    else: state.r[n]

template adrReg*(n: int): uint16 {.dirty.} =
    state.r[dspRegAdr0.succ(n)]

template incReg*(n: int): uint16 {.dirty.} =
    state.r[dspRegInc0.succ(n)]

template wrapReg*(n: int): uint16 {.dirty.} =
    state.r[dspRegWrap0.succ(n)]

func readAccum*(state; n: int): int64 {.inline.} =
    int64(state.readReg(dspRegA0.succ(n))) or
        (int64(state.readReg(dspRegA1.succ(n))) shl 16) or
        (int64(state.readReg(dspRegA2.succ(n))) shl 32)

func writeAccum*(state; n: int, val: int64) {.inline.} =
    state.writeReg dspRegA0.succ(n), uint16(val)
    state.writeReg dspRegA1.succ(n), uint16(val shr 16)
    state.writeReg dspRegA2.succ(n), uint16(val shr 32)

func loadAccum*(state; n: int, val: uint16) {.inline.} =
    state.writeReg dspRegA1.succ(n), val
    if state.status.xl:
        state.writeReg dspRegA0.succ(n), 0
        state.writeReg dspRegA2.succ(n), if getBit(val, 15): 0xFFFF else: 0    

func storeAccum*(state; n: int): uint16 {.inline.} =
    if state.status.xl and (state.status.ov or state.status.ext):
        if cast[int16](state.r[dspRegA2.succ(n)]) > 0:
            0x8000'u16
        elif cast[int16](state.r[dspRegA2.succ(n)]) < 0:
            0x7fff'u16
        else:
            state.r[dspRegA1.succ(n)]
    else:
        state.r[dspRegA1.succ(n)]

func readAuxAccum*(state; n: int): int64 {.inline.} =
    int64(cast[int32]((uint32(state.readReg(dspRegX0.succ(n))) or
        (uint32(state.readReg(dspRegX1.succ(n))) shl 16))))

func writeAuxAccum*(state; n: int, val: int64) {.inline.} =
    state.writeReg(dspRegX0.succ(n), cast[uint16](val))
    state.writeReg(dspRegX1.succ(n), cast[uint16](val shr 16))

# very inaccutare
func readProduct*(state): int64 {.inline.} =
    result = int64(state.readReg(dspRegPs0))
    result += int64(state.readReg(dspRegPs1)) shl 16
    result += int64(state.readReg(dspRegPc1)) shl 16
    result += int64(state.readReg(dspRegPs2)) shl 32

func writeProduct*(state; val: int64) {.inline.} =
    state.writeReg dspRegPs0, cast[uint16](val)
    state.writeReg dspRegPs1, cast[uint16](val shr 16)
    state.writeReg dspRegPs2, cast[uint16](val shr 32)
    state.writeReg dspRegPc1, 0

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
    of 1: state.writeReg dspRegAdr0.succ(rn), decAdr(adrReg(rn), wrapReg(rn))
    of 2: state.writeReg dspRegAdr0.succ(rn), incAdr(adrReg(rn), wrapReg(rn))
    of 3: state.writeReg dspRegAdr0.succ(rn), incAdr(adrReg(rn), wrapReg(rn), cast[int16](incReg(rn)))

func setC1*(state; ds, s: uint64) =
    state.status.ca = (0xFF_FFFF_FFFF'u64 - (ds and 0xFF_FFFF_FFFF'u64)) < (s and 0xFF_FFFF_FFFF'u64)
    let dd = ds + s
    assert state.status.ca == ((ds.getBit(39) and s.getBit(39)) or (not(dd.getBit(39)) and (ds.getBit(39) or s.getBit(39)))), &"wrong carry? {dd:08X}"
func setC2*(state; ds, s: uint64) =
    state.status.ca = (ds and 0xFF_FFFF_FFFF'u64) >= (s and 0xFF_FFFF_FFFF'u64)
    let dd = ds - s
    assert state.status.ca == ((ds.getBit(39) and not(s.getBit(39))) or (not(dd.getBit(39)) and (ds.getBit(39) or not(s.getBit(39))))), &"wrong carry? {state.status.ca} {dd:X} {ds:X} {s:X}"
    #state.status.ca = (dd.getBit(39) and not(s.getBit(39))) or (not(dd.getBit(39)) and (ds.getBit(39) or not(s.getBit(39))))
func setC7*(state; p, d: uint64) =
    state.status.ca = p.getBit(39) and not(d.getBit(39))

func setV1*(state; dd, ds, s: uint64) =
    state.status.ov = ds.getBit(39) == s.getBit(39) and dd.getBit(39) != ds.getBit(39)
func setV2*(state; dd, ds, s: uint64) =
    state.status.ov = ds.getBit(39) != s.getBit(39) and dd.getBit(39) != ds.getBit(39)
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
    state.status.ext = (full and 0xFF_8000_0000'u64) != 0'u64 or
        (full and 0xFF_8000_0000'u64) != 0xFF_8000_0000'u64
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
    case range[0..15](cond)
    of 0: state.status.ov == state.status.mi # greater or equal
    of 1: state.status.ov != state.status.mi # less
    of 2: not state.status.zr or state.status.ov == state.status.mi # greater
    of 3: state.status.zr or state.status.ov != state.status.mi # less or equal
    of 4: not state.status.zr
    of 5: state.status.zr
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