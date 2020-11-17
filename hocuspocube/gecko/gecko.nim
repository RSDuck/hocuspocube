import
    stew/endians2,
    ../util/bitstruct, ../util/ioregs,
    ppcstate

var geckoState*: PpcState

type
    ExternalInt* = enum
        extintError
        extintRsw
        extintDi
        extintSi
        extintExi
        extintAi
        extintDsp
        extintMem
        extintVi
        extintPeToken
        extintPeFinish
        extintCp
        extintDebug
        extintHsp

makeBitStruct uint32, Intsr:
    exceptions[0..13]: uint32
    exception[n, n]: bool

    rswst[14]: bool

makeBitStruct uint32, Intmr:
    exceptions[0..13]: uint32
    exception[n, n]: bool

var
    intsr: Intsr
    intmr: Intmr

    MainRAM*: array[0x1800000, byte]

proc updateGeckoException() =
    if (intsr.exceptions and intmr.exceptions) != 0:
        geckoState.pendingExceptions.incl exceptionExternal
    else:
        geckoState.pendingExceptions.excl exceptionExternal

proc triggerInt*(exception: ExternalInt) =
    intsr.exception(int(exception), true)
    updateGeckoException()

ioBlock pi, 0x100:
of intsr, 0x00, 4:
    read:
        result = uint32 intsr

        intsr.exceptions = 0
        updateGeckoException()
of intmr, 0x04, 4:
    read:
        uint32(intmr)
    write:
        intmr.exceptions = val
        updateGeckoException()
of consoletyp, 0x2C, 4:
    read:
        (2'u32 shl 28) # hw2, for now hardcoded as described in yagd
