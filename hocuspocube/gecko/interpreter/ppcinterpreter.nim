import
    strformat, options, stew/endians2,

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
            0x100'u32, 0x200, 0x300, 0x400, 0x500, 0x600, 0x700, 0x800, 0x900, 0xC00, 0xD00, 0xF00, 0x1300]
        let oldPc = geckoState.pc
        geckoState.pc = (if geckoState.msr.ip: 0xFFF00000'u32 else: 0'u32) + exceptionOffsets[exception]
        echo &"taking interrupt to {geckoState.pc:08X} from {oldPc:08X}"
        break

proc stateStr(): string =
    for i in 0..<32:
        result &= &"r{i}: {geckoState.r[i]:08X}\n"
    result &= &"lr: {geckoState.pc:08X}\n"
    result &= &"pc: {geckoState.pc:08X}\n"

var logStuff = false

proc geckoRun*(timestamp: var int64, target: int64) =
    while true:
        {.computedGoto.}

        #var prevPc = geckoState.pc
#[
        if geckoState.pc == 0x81362b84'u32:
            echo &"you're now entering stupid function from {geckoState.lr:08X}"
        if geckoState.pc == 0x8136356C'u32:
            echo &"blah {geckoState.lr:08X}"
        if geckoState.pc == 0x813695ec'u32:
            echo &"blah2 {geckoState.lr:08X}"
        if geckoState.pc == 0x813022f4'u32:
            echo &"blah3 {geckoState.lr:08X}"]#

        #if logStuff:
        #    echo &"{geckoState.pc:08X} {fromBE(readBus[uint32](0x15ED948'u32 + 0x26C)):08X}"

        if geckoState.pendingExceptions.len > 0:
            handleExceptions()

        # TODO: handle translation or fetch failure
        let instr = fromBE readBus[uint32](geckoState.translateInstrAddr(geckoState.pc).get)

        dispatchPpc instr, geckoState, undefinedInstr

        geckoState.pc += 4
        timestamp += 1

        if timestamp >= target:
            #echo &"executed slice {geckoState.pc:08X} timestamp: {timestamp}"
            return