import
    strformat,
    ../util/[ioregs, bitstruct],

    ../gekko/[gekko, memory],

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
    bpEnable[5] {.mutable.}: bool # enable breakpoint

makeBitStruct uint16, ClearRegister:
    clearOverflow[0]: bool
    clearUnderflow[1]: bool

var
    fifoBase, fifoEnd: HwPtr
    fifoHighWatermark, fifoLowWatermark: HwPtr # those actually aren't pointers
    fifoReadWriteDistance: HwPtr
    fifoWritePointer, fifoReadPointer: HwPtr
    fifoBreakPoint: HwPtr

    sr: Sr
    cr: Cr

    cmdlistParser: CmdListParser

proc setIdleByFifoSetup() =
    sr.readIdle = fifoWritePointer.adr == fifoReadPointer.adr
    sr.commandIdle = fifoWritePointer.adr == fifoReadPointer.adr

proc updateInt() =
    setExtInt extintCp, sr.bpHit and cr.cpMask

proc setBreakPointByFifoSetup() =
    sr.bpHit = cr.bpEnable and fifoBreakPoint == fifoReadPointer
    updateInt()

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
        setBreakPointByFifoSetup()
        updateInt()
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
        echo &"set fifo read ptr {fifoReadPointer.adr:08X}"
        setIdleByFifoSetup()
        setBreakPointByFifoSetup()
of cpWritePointer, 0x38, 4:
    read: swapHalves(uint32 fifoWritePointer)
    write:
        fifoWritePointer.adr = swapHalves(val)
        echo &"set fifo write ptr {fifoWritePointer.adr:08X}"
        setIdleByFifoSetup()
of cpBreakPoint, 0x3C, 4:
    read: swapHalves(uint32 fifoBreakPoint)
    write:
        fifoBreakPoint.adr = swapHalves(val)
        echo &"set fifo breakpoint {fifoBreakPoint.adr:08X}"
        setBreakPointByFifoSetup()

proc cpNotifyFifoBurst*(data: openArray[byte]): bool =
    if cr.gpLink:
        assert (data.len mod 32) == 0
        cmdlistParser.queueData(data)

        sr.readIdle = false
        sr.commandIdle = false

        if fifoWritePointer.adr == fifoEnd.adr:
            fifoWritePointer.adr = fifoBase.adr
        else:
            fifoWritePointer.adr = fifoWritePointer.adr + uint32(data.len)

        fifoReadWriteDistance.adr = fifoReadWriteDistance.adr + uint32(data.len)
        # TODO: check watermarks and generate interrupts

        assert fifoReadWriteDistance.adr < fifoHighWatermark.adr, &"dist {fifoReadWriteDistance.adr:08X} watermark {fifoHighWatermark.adr}"

        true
    else:
        #echo "fifo burst with gp link disabled"
        false

proc cpRun*() =
    if cr.enableFifoRead:
        if fifoWritePointer.adr == fifoReadPointer.adr:
            sr.readIdle = true
            sr.commandIdle = true
            return

        # check whether the read pointer will hit the break point
        #[if cr.bpEnable:
            sr.bpHit =
                if fifoWritePointer.adr < fifoReadPointer.adr:
                    # wrap around
                    (fifoBreakPoint.adr >= fifoReadPointer.adr and
                        fifoBreakPoint.adr <= fifoEnd.adr) or
                    (fifoBreakPoint.adr >= fifoBase.adr and
                        fifoBreakPoint.adr <= fifoWritePointer.adr)
                else:
                    fifoBreakPoint.adr >= fifoReadPointer.adr and
                        fifoBreakPoint.adr <= fifoWritePointer.adr
        else:
            sr.bpHit = false
        updateInt()

        let lastAdr = if sr.bpHit: fifoBreakPoint.adr else: fifoWritePointer.adr
        #echo &"cp run {fifoWritePointer.adr:08X} {fifoReadPointer.adr:08X} {lastAdr:08X} {sr.bpHit} {cr.bpEnable} {cr.cpMask}"]#

        if not cr.gpLink:
            sr.readIdle = false
            sr.commandIdle = false

            if fifoWritePointer.adr < fifoReadPointer.adr:
                # data wraps around
                withMainRamOpenArray(fifoReadPointer.adr, fifoEnd.adr + 32'u32 - fifoReadPointer.adr, byte):
                    cmdlistParser.queueData(ramArr)
                withMainRamOpenArray(fifoBase.adr, fifoWritePointer.adr - fifoBase.adr, byte):
                    cmdlistParser.queueData(ramArr)
            else:
                withMainRamOpenArray(fifoReadPointer.adr, fifoWritePointer.adr - fifoReadPointer.adr, byte):
                    cmdlistParser.queueData(ramArr)

        cmdlistParser.run()
        #if not sr.bpHit:
        #    cmdlistParser.run()

        if not cmdListParser.hasData():
            sr.readIdle = true
            sr.commandIdle = true

        # technically not correct, but we're doing so many things wrong
        # that we only need to start caring about this once we implement proper timings
        fifoReadPointer.adr = fifoWritePointer.adr
        fifoReadWriteDistance.adr = 0