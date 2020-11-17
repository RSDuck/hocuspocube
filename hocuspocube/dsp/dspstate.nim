import
    ../util/bitstruct

# refer to https://github.com/dolphin-emu/dolphin/blob/master/Source/Core/Core/DSP/DSPCore.h#L181
makeBitStruct uint16, *Status:
    ca[0]: bool
    ov[1]: bool
    zr[2]: bool
    mi[3]: bool

    overs32[4]: bool
    top2bits[5]: bool

    lz[6]: bool
    ovSticky[7]: bool

    ie[9]: bool

    ieExt[11]: bool

    am[13]: bool # TODO: Dolphin and duddie disagree on this one, check this
    mode40bit[14]: bool
    mulUnsigned[15]: bool

type
    Stack*[Size: static[int]] = object
        sp*: int32
        values: array[Size, uint16]

    DspState* = object
        r*: array[32, uint16]

        config*, pc*: uint16
        status*: Status

        callStack*: Stack[8]
        dataStack*: Stack[4]
        loopAddrStack*: Stack[4]
        loopCountStack*: Stack[4]

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
    result = stack.values[stack.sp]
    stack.sp -= 1
