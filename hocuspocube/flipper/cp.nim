import
    ../util/[ioregs, bitstruct],

    ../gekko/gekko,

    cpinternal

makeBitStruct uint16, Sr:
    fifoOverflow[0]: bool # fifo overflow occured
    fifoUnderflow[1]: bool # fifo underflow occured
    readIdle[2]: bool # idle for reading
    commandIdle[3]: bool # idle for commands
    bpHit[4]: bool # breakpoint hit

makeBitStruct uint16, Cr:
    enableFifoRead[0] {.mutable.}: bool
    cpMask[1] {.mutable.}: bool # cp interrupt mask
    fifoUnderflowIntmsk[2] {.mutable.}: bool
    fifoOverflowIntmsk[3] {.mutable.}: bool
    gpLink[4] {.mutable.}: bool # enable linking of fifo and cp
    bpEnable[5]: bool # enable breakpoint

makeBitStruct uint16, ClearRegister:
    clearOverflow[0]: bool
    clearUnderflow[1]: bool

makeBitStruct uint32, FifoPtr:
    _[5..25] {.adr.}: uint32

var
    fifoBase, fifoEnd: FifoPtr
    fifoHighWatermark, fifoLowWatermark: FifoPtr # those actually aren't pointers
    fifoReadWriteDistance: FifoPtr
    fifoWritePointer, fifoReadPointer: FifoPtr

    sr: Sr
    cr: Cr

    cmdlistParser: CmdListParser

proc setIdleByFifoSetup() =
    sr.readIdle = fifoWritePointer.adr == fifoReadPointer.adr
    sr.commandIdle = fifoWritePointer.adr == fifoReadPointer.adr

# who ever came up with the idea to swap the upper and lower 16-bit halves?
func swapHalves(x: uint32): uint32 =
    (x shr 16'u32) or (x shl 16'u32)

ioBlock cp, 0x80:
of cpSr, 0x0, 2:
    read: uint16 sr
    write: discard
of cpCr, 0x2, 2:
    read: uint16 cr
    write:
        cr.mutable = val
of cpClear, 0x4, 2:
    read: 0'u16
    write: discard
of cpFifoBase, 0x20, 4:
    read: swapHalves(uint32 fifoBase)
    write: fifoBase.adr = swapHalves(val)
of cpFifoEnd, 0x24, 4:
    read: swapHalves(uint32 fifoEnd)
    write: fifoEnd.adr = swapHalves(val)
of cpFifoHighWatermark, 0x28, 4:
    read: swapHalves(uint32 fifoHighWatermark)
    write: fifoHighWatermark.adr = swapHalves(val)
of cpFifoLowWatermark, 0x2C, 4:
    read: swapHalves(uint32 fifoLowWatermark)
    write: fifoLowWatermark.adr = swapHalves(val)
of cpFifoReadWriteDistance, 0x30, 4:
    read: swapHalves(uint32 fifoReadWriteDistance)
    write: fifoReadWriteDistance.adr = swapHalves(val)
of cpReadPointer, 0x34, 4:
    read: swapHalves(uint32 fifoReadPointer)
    write:
        fifoReadPointer.adr = swapHalves(val)
        setIdleByFifoSetup()
of cpWritePointer, 0x38, 4:
    read: swapHalves(uint32 fifoWritePointer)
    write:
        fifoWritePointer.adr = swapHalves(val)
        setIdleByFifoSetup()

proc cpNotifyFifoBurst*(data: openArray[uint32]): bool =
    if cr.gpLink:
        assert (data.len mod 8) == 0

        cmdlistParser.queueData(toOpenArray(cast[ptr UncheckedArray[byte]](unsafeAddr data[0]), 0, data.len * 4 - 1))

        sr.readIdle = false
        sr.commandIdle = false

        if fifoWritePointer.adr == fifoEnd.adr:
            fifoWritePointer.adr = fifoBase.adr
        else:
            fifoWritePointer.adr = fifoWritePointer.adr + uint32(data.len) * 4

        fifoReadWriteDistance.adr = fifoReadWriteDistance.adr + uint32(data.len) * 4
        # TODO: check watermarks and generate interrupts

        true
    else:
        echo "fifo burst with gp link disabled"
        false

proc cpRun*() =
    if cr.enableFifoRead:
        if fifoWritePointer.adr == fifoReadPointer.adr:
            sr.readIdle = true
            sr.commandIdle = true
            return

        if not cr.gpLink:
            sr.readIdle = false
            sr.commandIdle = false
            echo "gp link disabled"

            if fifoWritePointer.adr < fifoReadPointer.adr:
                # data wraps around
                cmdlistParser.queueData(toOpenArray(mainRAM, fifoReadPointer.adr, fifoEnd.adr + 31'u32))
                cmdlistParser.queueData(toOpenArray(mainRAM, fifoBase.adr, fifoWritePointer.adr - 1))
            else:
                cmdlistParser.queueData(toOpenArray(mainRAM, fifoReadPointer.adr, fifoWritePointer.adr - 1))

        cmdlistParser.run()

        if not cmdListParser.hasData():
            sr.readIdle = true
            sr.commandIdle = true

        # technically not correct, but we're doing so many things wrong
        # that we only need to start caring about this once we implement proper timings
        fifoReadPointer.adr = fifoWritePointer.adr
        fifoReadWriteDistance.adr = 0