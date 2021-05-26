import
    strformat, options, stew/endians2,
    streams,

    ../ppcdef, ../ppcstate, ../ppccommon,
    ../gekko,

    ppcinterpreter_int, 
    ppcinterpreter_float, 
    ppcinterpreter_loadstore,
    ppcinterpreter_system,
    ppcinterpreter_branch,

    ../memory

proc undefinedInstr(state: var PpcState, instr: uint32) =
    let file = newFileStream("mainram2.bin", fmWrite)
    file.writeData(addr mainRAM[0], mainRAM.len)
    file.close()
    echo &"undefined instr {instr:08X} at {state.pc:08X} {state.lr:08X}"
    quit()

var nextPrintTimestamp = 0

proc gekkoRun*(timestamp: var int64, target: var int64) =
    while true:
        {.computedGoto.}

        if gekkoState.pendingExceptions != {}:
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