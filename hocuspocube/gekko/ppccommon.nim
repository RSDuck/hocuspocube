import
    strformat,
    bitops, stew/bitops2,
    ../cycletiming, ppcstate, gekko

proc ppcmask*(mb, me: uint32): uint32 =
    if mb > me:
        result = not toMask[uint32](int(31-mb+1)..int(31-me-1))
    else:
        result = toMask[uint32](int(31-me)..int(31-mb))

proc decodeSplitSpr*(spr: uint32): uint32 =
    ((spr and 0x1F) shl 5) or (spr shr 5)

proc makeFieldMask*(mask: uint32): uint32 =
    for i in 0..<8:
        if mask.getBit(i):
            result.setMask(0xF'u32 shl (i * 4))

proc currentTb*(state: var PpcState): uint64 =
    uint64((gekkoTimestamp - state.tbInitTimestamp) div gekkoCyclesPerTbCycle) + state.tbInit

proc getDecrementer*(state: var PpcState): uint32 =
    let cyclesPassed = uint32((gekkoTimestamp - state.decInitTimestamp) div gekkoCyclesPerTbCycle)
    # the decrementer go negative
    state.decInit - cyclesPassed

proc setupDecrementer*(state: var PpcState, val: uint32) =
    let topBitChanged = val.getBit(31) and not(state.getDecrementer().getBit(31))

    if state.decDoneEvent != InvalidEventToken:
        cancelEvent state.decDoneEvent

    state.decInit = val
    state.decInitTimestamp = gekkoTimestamp

    let
        cyclesUntilZeroToOne = gekkoCyclesPerTbCycle *
            (if state.decInit.getBit(31):
                int64(state.decInit) + 0xFFFFFFFF'i64 # I doubt this will ever happen
            else:
                int64(state.decInit))

    echo &"setup up decrementer {state.decInit} {state.decInitTimestamp} | {cyclesUntilZeroToOne} | {state.pc:08X}"
    state.decDoneEvent = scheduleEvent(state.decInitTimestamp + cyclesUntilZeroToOne, 0,
        proc(timestamp: int64) =
            echo &"decrementer done {gekkoState.decInit} {gekkoState.decInitTimestamp} {gekkoState.getDecrementer()}"
            gekkoState.pendingExceptions.incl exceptionDecrementer)
    if topBitChanged:
        echo "decrementer interrupt by manually changing top bit"
        state.pendingExceptions.incl exceptionDecrementer

proc handleExceptions*() =
    for exception in gekkoState.pendingExceptions:
        if (exception == exceptionExternal or exception == exceptionDecrementer) and not gekkoState.msr.ee:
            continue
        if exception == exceptionMachineCheck and not gekkoState.msr.me:
            continue

        if exception != exceptionExternal:
            # this is a bit hacky
            gekkoState.pendingExceptions.excl exception

        gekkoState.srr0 = gekkoState.pc
        gekkoState.srr1.exceptionSaved = gekkoState.msr.exceptionSaved

        gekkoState.msr.zeroOnException = 0'u32
        gekkoState.msr.le = gekkoState.msr.ile
        if exception == exceptionMachineCheck:
            gekkoState.msr.me = false

        const exceptionOffsets: array[PpcException, uint32] = [
            0x100'u32, 0x1300, 0x400, 0x200, 0x700, 0xC00, 0x800, 0x500, 0xF00, 0x900, 0x600, 0x300, 0xD00]
        let oldPc = gekkoState.pc
        gekkoState.pc = (if gekkoState.msr.ip: 0xFFF00000'u32 else: 0'u32) + exceptionOffsets[exception]
        echo &"taking interrupt to {gekkoState.pc:08X} from {oldPc:08X} {gekkoState.lr:08X}"
        break

proc stateStr*(): string =
    for i in 0..<32:
        result &= &"r{i}: {gekkoState.r[i]:08X}\n"
    result &= &"lr: {gekkoState.lr:08X}\n"
    result &= &"pc: {gekkoState.pc:08X}\n"