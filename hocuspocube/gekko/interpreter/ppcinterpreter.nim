import
    strformat, options, stew/endians2,
    streams,

    ../ppcdef, ../ppcstate,
    ../gekko,

    ppcinterpreter_int, 
    ppcinterpreter_float, 
    ppcinterpreter_loadstore,
    ppcinterpreter_system,
    ppcinterpreter_branch,
    ppcinterpreter_aux,

    ../memory

proc undefinedInstr(state: var PpcState, instr: uint32) =
    let file = newFileStream("mainram2.bin", fmWrite)
    file.writeData(addr MainRAM[0], MainRAM.len)
    file.close()
    echo &"undefined instr {instr:08X} at {state.pc:08X} {state.lr:08X}"
    quit()

proc handleExceptions() =
    for exception in geckoState.pendingExceptions:
        if (exception == exceptionExternal or exception == exceptionDecrementer) and not geckoState.msr.ee:
            continue
        if exception == exceptionMachineCheck and not geckoState.msr.me:
            continue

        if exception != exceptionExternal:
            # this is a bit hacky
            geckoState.pendingExceptions.excl exception

        geckoState.srr0 = geckoState.pc
        geckoState.srr1.exceptionSaved = geckoState.msr.exceptionSaved

        geckoState.msr.zeroOnException = 0'u32
        geckoState.msr.le = geckoState.msr.ile
        if exception == exceptionMachineCheck:
            geckoState.msr.me = false

        const exceptionOffsets: array[PpcException, uint32] = [
            0x100'u32, 0x1300, 0x400, 0x200, 0x700, 0xC00, 0x800, 0x500, 0xF00, 0x900, 0x600, 0x300, 0xD00]
        let oldPc = geckoState.pc
        geckoState.pc = (if geckoState.msr.ip: 0xFFF00000'u32 else: 0'u32) + exceptionOffsets[exception]
        #echo &"taking interrupt to {geckoState.pc:08X} from {oldPc:08X} {geckoState.lr:08X}"
        break

proc stateStr(): string =
    for i in 0..<32:
        result &= &"r{i}: {geckoState.r[i]:08X}\n"
    result &= &"lr: {geckoState.pc:08X}\n"
    result &= &"pc: {geckoState.pc:08X}\n"

var nextPrintTimestamp = 0

proc geckoRun*(timestamp: var int64, target: int64) =
    while true:
        {.computedGoto.}

        if geckoState.pendingExceptions.len > 0:
            handleExceptions()

        # TODO: handle translation or fetch failure
        let instr = fromBE readBus[uint32](geckoState.translateInstrAddr(geckoState.pc).get)

        dispatchPpc instr, geckoState, undefinedInstr

        geckoState.pc += 4
        timestamp += 3

        #[if geckoState.pc == 0x800EF51C'u32:
            let file = newFileStream("mainram4.bin", fmWrite)
            file.writeData(addr MainRAM[0], MainRAM.len)
            file.close()
            raiseAssert("blah")]#
            #discard
        if geckoState.pc == 0x8005ae78'u32:
            var msg: string
            for i in 0..<1000:
                let c = char(geckoState.readMemory[:uint8](geckoState.translateDataAddr(geckoState.r[3] + uint32(i)).get))
                if c != '\0':
                    msg &= c
                else:
                    break
            echo "printf: ", msg

        if timestamp >= target:
            if timestamp >= nextPrintTimestamp:
                nextPrintTimestamp += 100000
                #echo &"executed slice {geckoState.pc:08X} {geckoState.lr:08X} timestamp: {timestamp}"
            return