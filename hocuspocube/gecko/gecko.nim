import
    stew/endians2, strformat,
    ../util/bitstruct, ../util/ioregs,
    ppcstate

var geckoState*: PpcState

template piLog(msg: string): untyped =
    discard

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

makeBitStruct uint32, FifoPtr:
    _[5..25] {.adr.}: uint32
    overflow[26]: bool

type
    MemoryTag* = enum
        memoryTagNone
        memoryTagTexture

var
    intsr: Intsr
    intmr: Intmr

    fifoBase, fifoEnd, fifoCurrent: FifoPtr

    MainRAM*: array[0x1800000, byte]
    MainRAMTagging*: array[0x1800000 div 32, MemoryTag]

proc updateFifo*(): uint32 =
    result = fifoCurrent.adr

    if fifoCurrent.adr == fifoEnd.adr:
        fifoCurrent = fifoBase
        fifoCurrent.overflow = true
    else:
        fifoCurrent.adr = fifoCurrent.adr + 32

proc updateGeckoException() =
    if (intsr.exceptions and intmr.exceptions) != 0:
        geckoState.pendingExceptions.incl exceptionExternal
    else:
        geckoState.pendingExceptions.excl exceptionExternal

proc setExtInt*(exception: ExternalInt, enable: bool) =
    piLog &"extint {exception} {enable}"
    intsr.exception(int(exception), enable)
    updateGeckoException()

ioBlock pi, 0x100:
of intsr, 0x00, 4:
    read: uint32 intsr
of intmr, 0x04, 4:
    read: uint32(intmr)
    write:
        intmr.exceptions = val
        updateGeckoException()
of fifobase, 0xC, 4:
    read: uint32 fifoBase
    write: fifoBase.adr = val
of fifoend, 0x10, 4:
    read: uint32 fifoEnd
    write: fifoEnd.adr = val
of fifocurrent, 0x14, 4:
    read: uint32 fifoCurrent
    write:
        fifoCurrent.adr = val
        fifoCurrent.overflow = false
of consoletyp, 0x2C, 4:
    read:
        (2'u32 shl 28) # hw2, for now hardcoded as described in yagd
