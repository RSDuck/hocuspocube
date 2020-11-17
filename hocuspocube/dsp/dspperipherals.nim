import
    dsp, ../gecko/gecko,
    stew/endians2

proc copySwapBytes16(dst, src: ptr UncheckedArray[uint16], length: uint32) =
    for i in 0..<length div 2:
        dst[i] = fromBE src[i]

proc runPeripherals*() =
    if dspCsr.reset:
        # TODO: reset stuff
        dspCsr.reset = false
    
    if dspCsr.busyCopying:
        # copy 1kb payload
        echo "transfered inital dsp payload"
        copySwapBytes16(cast[ptr UncheckedArray[uint16]](addr iram[0]), cast[ptr UncheckedArray[uint16]](addr aram[0]), 1024)
        dspCsr.busyCopying = false

    if arDmaStatus != -1:
        var
            dstPtr = addr aram[arDmaArAddr]
            srcPtr = addr MainRAM[arDmaMmAddr]

        if arDmaCnt.direction:
            swap dstPtr, srcPtr

        copyMem(dstPtr, srcPtr, arDmaCnt.length shl 2)
        echo "copied ", arDmaCnt.length shl 2, "bytes"
        dspCsr.arint = true

        arDmaStatus = -1