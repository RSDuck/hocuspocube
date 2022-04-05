import
    strformat, stew/endians2,
    streams,

    ../ppcdef, ../ppcstate, ../ppccommon,
    ../gekko,

    ppcinterpreter_int, 
    ppcinterpreter_float, 
    ppcinterpreter_loadstore,
    ppcinterpreter_system,
    ppcinterpreter_branch,

    ../memory

proc systemStep(): bool {.importc.}

proc undefinedInstr(state: var PpcState, instr: uint32) =
    let file = newFileStream("mainram2.bin", fmWrite)
    file.writeData(mainRamReadPtr(0'u32, MainRamSize), MainRamSize)
    file.close()
    echo &"undefined instr {instr:08X} at {state.pc:08X} {state.lr:08X}"
    quit()

proc gekkoRun*() =
    while true:
        {.computedGoto.}

        if gekkoState.pendingExceptions != {}:
            gekkoState.handleExceptions()

        # TODO: handle translation or fetch failure
        let instr = fromBE readCode(gekkoState.translateInstrAddr(gekkoState.pc))

        dispatchPpc instr, gekkoState, undefinedInstr

        gekkoState.pc += 4
        gekkoState.negativeCycles += 1

        if gekkoState.negativeCycles >= 0:
            if not systemStep():
                return
