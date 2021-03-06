import
    util/[bitstruct, ioregs]

makeBitStruct uint32, DiSr:
    brk[0]: bool
    deintmask[1]: bool
    deint[2]: bool
    tcintmask[3]: bool
    tcint[4]: bool
    brkintmask[5]: bool
    brkint[6]: bool

makeBitStruct uint32, DiCvr:
    cvr[0]: bool # whether the cover is open
    cvrintmask[1] {.mutable.}: bool
    cvrint[2]: bool

var
    disr: DiSr
    dicvr: DiCvr

dicvr.cvr = true # we let the cover be open for now

ioBlock di, 0x40:
of dicvr, 0x04, 4:
    read: uint32 dicvr
    write:
        dicvr.mutable = val
        let val = DiCvr val

        if val.cvrint:
            dicvr.cvrint = false

        # int?
of dicfg, 0x24, 4:
    read: 1