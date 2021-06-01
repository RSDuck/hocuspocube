import
    gekko/gekko, cycletiming,
    util/[bitstruct, ioregs],
    strformat,
    streams

template diLog(msg: string): untyped =
    echo msg

makeBitStruct uint32, DiSr:
    brk[0]: bool # request break
    deintmask[1] {.mutable.}: bool # error interrupt mask
    deint[2]: bool # error interrupt
    tcintmask[3] {.mutable.}: bool # transfer interrupt
    tcint[4]: bool # transfer finish interrupt
    brkintmask[5] {.mutable.}: bool # break interrupt
    brkint[6]: bool # break interrupt mask

makeBitStruct uint32, DiCr:
    tstart[0]: bool
    dma[1] {.mutable.}: bool
    write[2] {.mutable.}: bool

makeBitStruct uint32, DiCvr:
    cvr[0]: bool # whether the cover is open
    cvrintmask[1] {.mutable.}: bool
    cvrint[2]: bool

const
    CmdReadDiscId = 0xA8000040'u32
    CmdReadSector = 0xA8000000'u32
    CmdRequestError = 0xE0000000'u32
    CmdAudioEnable = 0xE4010000'u32
    CmdAudioDisable = 0xE4000000'u32
    CmdInquiry = 0x12000000'u32

    ErrorCodeOk = 0'u32
    ErrorCodeNoDisc = 0x023A00'u32

type
    DriveState = enum
        driveStateOk
        driveStateLidOpen
        driveStateDiscChanged
        driveStateNoDisc
        driveStateMotorOff
        driveStateDiscNotInitialized

    DriveIdent = object
        revisionLevel, deviceCode: uint16
        releaseDate: uint32
        pad: array[24, byte]

static:
    assert(sizeof(DriveIdent) == 32)

makeBitStruct uint32, DiError:
    driveState[24..31]: DriveState
    error[0..23]: uint32

var
    diSr: DiSr
    diCvr: DiCvr

    diMar, diLen: HwPtr

    diCmdBuf: array[3, uint32]
    diImmBuf: uint32

    diCr: DiCr

    curErrorCode = ErrorCodeOk
    curDriveState = driveStateDiscNotInitialized

    discFile: FileStream

proc loadDvd*(path: string) =
    discFile = newFileStream(path, fmRead)

    assert discFile != nil

proc updateInt() =
    setExtInt extintDi, (diSr.deint and diSr.deintmask) or
        (diSr.tcint and diSr.tcintmask) or
        (diSr.brkint and diSr.brkintmask) or
        (diCvr.cvrint and diCvr.cvrintmask)

proc transferFinish() =
    diSr.tcint = true
    updateInt()

proc setError(code: uint32) =
    curErrorCode = code
    diSr.deint = true
    updateInt()

proc processCmd() =
    case diCmdBuf[0]
    of CmdReadSector:
        if discFile == nil:
            curDriveState = driveStateNoDisc
            setError(ErrorCodeNoDisc)
        else:
            let
                offset = diCmdBuf[1] shl 2
                len = diCmdBuf[2]
            diLog &"read sector {offset} {len} to {diMar.adr:08X} {diLen.adr}"

            assert diLen.adr == len

            discFile.setPosition(int(offset))

            #if diMar.adr < uint32(mainRAM.len):
            let bytesRead = discFile.readData(addr mainRAM[diMar.adr], int(len))
            assert uint32(bytesRead) == len
            #else:
            #    echo "skipped weird disc access"

            diLen.adr = 0

            transferFinish()
    of CmdReadDiscId:
        if discFile == nil:
            curDriveState = driveStateNoDisc
            setError(ErrorCodeNoDisc)
        else:
            assert diCr.dma
            assert diLen.adr == 0x20

            discFile.setPosition(0)
            let bytesRead = discFile.readData(addr mainRAM[diMar.adr], 0x20)
            assert bytesRead == 0x20

            diLog &"di: read disc id to {diMar.adr:08X}"

            diLen.adr = 0

            curDriveState = driveStateOk

            transferFinish()
    of CmdRequestError:
        assert not(diCr.dma)

        var err: DiError
        err.driveState = curDriveState
        err.error = curErrorCode
        diImmBuf = uint32 err
        transferFinish()
    of CmdAudioDisable:
        diLog &"di: disable audio"
        transferFinish()
    of CmdAudioEnable:
        raiseAssert("dvd audio not supported!")
    of CmdInquiry:
        assert diCr.dma
        assert diLen.adr == 0x20

        var ident: DriveIdent
        ident.deviceCode = 0x1234
        ident.revisionLevel = 0x1234
        ident.releaseDate = 0x12345678
        copyMem(addr mainRAM[diMar.adr], addr ident, 0x20)

        diLog &"di: read drive ident to {diMar.adr:08X}"

        diLen.adr = 0

        transferFinish()
    else:
        raiseAssert(&"invalid disc cmd {diCmdBuf[0]:08X} {diCmdBuf[1]:08X} {diCmdBuf[2]:08X}")

ioBlock di, 0x40:
of diSr, 0x00, 4:
    read: uint32 diSr
    write:
        diSr.mutable = val
        let val = DiSr val

        if val.deint:
            diSr.deint = false
        if val.tcint:
            diSr.tcint = false
        if val.brkint:
            diSr.brkint = false

        updateInt()
of diCvr, 0x04, 4:
    read: uint32 dicvr
    write:
        dicvr.mutable = val
        let val = DiCvr val

        if val.cvrint:
            dicvr.cvrint = false

        updateInt()
of diCmdBuf, 0x08, 4, 3:
    read: diCmdBuf[idx]
    write: diCmdBuf[idx] = val
of diMar, 0x14, 4:
    read: diMar.adr
    write:
        diMar.adr = val
of diLen, 0x18, 4:
    read: diLen.adr
    write: diLen.adr = val
of diCr, 0x1C, 4:
    read: uint32 diCr
    write:
        diCr.mutable = val
        let val = DiCr val

        if val.tstart and not diCr.tstart:
            assert not val.write

            echo &"started transfer from {gekkoState.pc:08X} {gekkoState.lr:08X}"

            #schedCommandDelay = scheduleEvent(gekkoTimestamp + 5*1000, 0, proc(timestamp: int64) =
            #    diCr.tstart = false
            #    processCmd())
            processCmd()
of diImmBuf, 0x20, 4:
    read: diImmBuf
of diCfg, 0x24, 4:
    # hm
    read: 1