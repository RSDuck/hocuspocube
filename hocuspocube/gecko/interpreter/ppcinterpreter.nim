import
    strformat, strutils, options, stew/endians2,

    ../ppcdef, ../ppcstate,
    ../gecko,

    ppcinterpreter_int, 
    ppcinterpreter_float, 
    ppcinterpreter_loadstore,
    ppcinterpreter_system,
    ppcinterpreter_branch,
    ppcinterpreter_aux,

    ../memory

proc undefinedInstr(state: var PpcState, instr: uint32) =
    echo &"undefined instr {instr:08X} at {state.pc:08X}"
    quit()

proc geckoRun*(timestamp: var int64, target: int64) =
    let startTimestamp = timestamp

    while true:
        {.computedGoto.}

        # TODO: handle translation or fetch failure
        let instr = fromBE readBus[uint32](geckoState.translateInstrAddr(geckoState.pc).get)

        dispatchPpc instr, geckoState, undefinedInstr
        
        geckoState.pc += 4
        timestamp += 1

        if geckoState.pendingExceptions.len > 0:
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
                    0x100'u32, 0x200, 0x300, 0x400, 0x500, 0x600, 0x700, 0x800, 0x900, 0xC00, 0xD00, 0xF00, 0x1300]
                geckoState.pc = (if geckoState.msr.ip: 0xFFF'u32 else: 0'u32) + exceptionOffsets[exception]
                echo &"taking interrupt to {geckoState.pc:08X}"
                break

        # hardcoded idle loop
        if timestamp >= target:
            geckoState.tb += uint32((timestamp - startTimestamp) div 3)
            #echo &"executed slice {geckoState.pc:08X} timestamp: {timestamp}"
            return