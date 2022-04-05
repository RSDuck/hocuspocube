import
    ../util/bitstruct

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
        r0
        r1
        r2
        r3
        m0
        m1
        m2
        m3
        l0
        l1
        l2
        l3
        pcs
        pss
        eas
        lcs
        a2
        b2
        dpp
        psr
        ps0
        ps1
        ps2
        pc1
        x0
        y0
        x1
        y1
        a0
        b0
        a1
        b1

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

        negativeCycles*: int32

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
