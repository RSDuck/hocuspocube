import
    ../util/bitstruct

# refer to https://github.com/dolphin-emu/dolphin/blob/master/Source/Core/Core/DSP/DSPCore.h#L181
makeBitStruct uint16, *Status:
    ca[0]: bool # carry
    ov[1]: bool # overflow
    zr[2]: bool # zero
    mi[3]: bool # negative

    ext[4]: bool
    unnorm[5]: bool

    bit[n, n]: bool
    tb[6]: bool
    sv[7]: bool

    te0[8]: bool
    te1[9]: bool
    te2[10]: bool
    te3[11]: bool
    et[12]: bool

    im[13]: bool
    xl[14]: bool
    dp[15]: bool

type
    DspReg* = enum
        dspRegAdr0
        dspRegAdr1
        dspRegAdr2
        dspRegAdr3
        dspRegInc0
        dspRegInc1
        dspRegInc2
        dspRegInc3
        dspRegWrap0
        dspRegWrap1
        dspRegWrap2
        dspRegWrap3
        dspRegCallStack
        dspRegStatusStack
        dspRegLoopAdrStack
        dspRegLoopCountStack
        dspRegA2
        dspRegB2
        dspRegDpp
        dspRegStatus
        dspRegPs0
        dspRegPs1
        dspRegPs2
        dspRegPc1
        dspRegX0
        dspRegY0
        dspRegX1
        dspRegY1
        dspRegA0
        dspRegB0
        dspRegA1
        dspRegB1

    Stack*[Size: static[int]] = object
        sp*: int32
        values: array[Size, uint16]

    DspState* = object
        mainAccum*: array[2, uint64]
        auxAccum*: array[2, uint32]
        prod*: uint64
        prodcarry*: uint16
        adrReg*: array[4, uint16]
        incReg*: array[4, uint16]
        wrapReg*: array[4, uint16]
        dpp*: uint16
        pc*: uint16

        status*: Status

        callStack*: Stack[8]
        statusStack*: Stack[4]
        loopAddrStack*: Stack[4]
        loopCountStack*: Stack[4]

proc `[]`*[Size: static[int]](stack: Stack[Size], idx: int): uint16 =
    stack.values[idx]

proc peek*[Size: static[int]](stack: Stack[Size]): uint16 =
    stack.values[stack.sp - 1]

proc peek*[Size: static[int]](stack: var Stack[Size]): var uint16 =
    stack.values[stack.sp - 1]

proc aboutToOverflow*[Size: static[int]](stack: Stack[Size]): bool =
    stack.sp == Size - 1

proc aboutToUnderflow*[Size: static[int]](stack: Stack[Size]): bool =
    stack.sp == 0

proc push*[Size: static[int]](stack: var Stack[Size], val: uint16) =
    stack.values[stack.sp] = val
    stack.sp += 1

proc pop*[Size: static[int]](stack: var Stack[Size]): uint16 =
    stack.sp -= 1
    result = stack.values[stack.sp]
