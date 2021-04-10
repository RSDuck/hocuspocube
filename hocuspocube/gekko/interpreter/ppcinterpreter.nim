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
    file.writeData(addr mainRAM[0], mainRAM.len)
    file.close()
    echo &"undefined instr {instr:08X} at {state.pc:08X} {state.lr:08X}"
    quit()

proc handleExceptions() =
    for exception in gekkoState.pendingExceptions:
        if (exception == exceptionExternal or exception == exceptionDecrementer) and not gekkoState.msr.ee:
            continue
        if exception == exceptionMachineCheck and not gekkoState.msr.me:
            continue

        if exception != exceptionExternal:
            # this is a bit hacky
            gekkoState.pendingExceptions.excl exception

        gekkoState.srr0 = gekkoState.pc
        gekkoState.srr1.exceptionSaved = gekkoState.msr.exceptionSaved

        gekkoState.msr.zeroOnException = 0'u32
        gekkoState.msr.le = gekkoState.msr.ile
        if exception == exceptionMachineCheck:
            gekkoState.msr.me = false

        const exceptionOffsets: array[PpcException, uint32] = [
            0x100'u32, 0x1300, 0x400, 0x200, 0x700, 0xC00, 0x800, 0x500, 0xF00, 0x900, 0x600, 0x300, 0xD00]
        let oldPc = gekkoState.pc
        gekkoState.pc = (if gekkoState.msr.ip: 0xFFF00000'u32 else: 0'u32) + exceptionOffsets[exception]
        #echo &"taking interrupt to {gekkoState.pc:08X} from {oldPc:08X} {gekkoState.lr:08X}"
        break

proc stateStr(): string =
    for i in 0..<32:
        result &= &"r{i}: {gekkoState.r[i]:08X}\n"
    result &= &"lr: {gekkoState.pc:08X}\n"
    result &= &"pc: {gekkoState.pc:08X}\n"

var nextPrintTimestamp = 0

proc gekkoRun*(timestamp: var int64, target: var int64) =
    while true:
        {.computedGoto.}

        if gekkoState.pendingExceptions.len > 0:
            handleExceptions()

        # TODO: handle translation or fetch failure
        let instr = fromBE readCode(gekkoState.translateInstrAddr(gekkoState.pc).get)

        dispatchPpc instr, gekkoState, undefinedInstr

        gekkoState.pc += 4
        timestamp += 1

        if timestamp >= target:
            if timestamp >= nextPrintTimestamp:
                nextPrintTimestamp += 100000
                #echo &"executed slice {gekkoState.pc:08X} {gekkoState.lr:08X} timestamp: {timestamp}"
            return