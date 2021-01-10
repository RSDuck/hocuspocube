import
    ../util/[bitstruct, ioregs],

    ../gecko/gecko

makeBitStruct uint16, IntSr:
    tokenInt[0]: bool
    finishInt[1]: bool
    tokenMsk[2] {.mutable.}: bool
    finishMsk[3] {.mutable.}: bool

var intSr: Intsr

proc updateInt() =
    setExtInt extintPeToken, intSr.tokenInt and intSr.tokenMsk
    setExtInt extintPeFinish, intSr.finishInt and intSr.finishMsk

ioBlock pe, 0x100:
of intSr, 0x0A, 2:
    read: uint16 intSr
    write:
        intSr.mutable = val
        let val = IntSr val
        if val.tokenInt:
            intSr.tokenInt = false
        if val.finishInt:
            intSr.finishInt = false
        updateInt()

proc flagFinish*() =
    intSr.finishInt = true
    updateInt()