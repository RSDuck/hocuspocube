import
    ../ppcstate, ../../util/aluhelper,
    stew/bitops2, strformat

using state: var PpcState

proc bx*(state; li, aa, lk: uint32) =
    if lk == 1:
        state.lr = state.pc + 4
    let prevPc = state.pc
    if aa == 1:
        state.pc = (signExtend(li, 24) shl 2) - 4
    else:
        state.pc += (signExtend(li, 24) shl 2) - 4
    #echo &"branch to {state.pc+4:X} from {prevPc:08X}"

template handleCondAndCtr: untyped {.dirty.} =
    let 
        ctrOk = bo.getBit(2) or (dec state.ctr; state.ctr != 0 xor bo.getBit(1))
        condOk = bo.getBit(4) or state.cr.bit(int bi) == bo.getBit(3)

proc bcx*(state; bo, bi, bd, aa, lk: uint32) =
    handleCondAndCtr

    if ctrOk and condOk:
        if lk == 1:
            state.lr = state.pc + 4
        if aa == 1:
            state.pc = signExtend(bd, 14) shl 2
        else:
            state.pc += signExtend(bd, 14) shl 2
        state.pc -= 4

proc bcctrx*(state; bo, bi, lk: uint32) =
    if bo.getBit(4) or state.cr.bit(int bi) == bo.getBit(3):
        if lk == 1:
            state.lr = state.pc + 4
        state.pc = (state.ctr and not(3'u32)) - 4

proc bclrx*(state; bo, bi, lk: uint32) =
    handleCondAndCtr

    if ctrOk and condOk:
        let nextAddr = state.lr and not(3'u32)
        #echo &"link to {nextAddr:08X} at {state.pc:08X}"
        if lk == 1:
            state.lr = state.pc + 4
        state.pc = nextAddr - 4