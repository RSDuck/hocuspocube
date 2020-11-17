import
    strformat,

    gecko, ../dsp/dsp, ../vi, ../si/si

proc writePhysical*[T](`addr`: uint32, val: T) =
    if `addr` < uint32 MainRAM.len:
        cast[ptr T](addr MainRAM[`addr`])[] = val
    elif (`addr` and 0xFFF0000) == 0xC000000:
        case `addr` and 0xFF00
        of 0x2000: viWrite[T](`addr`, val)
        of 0x3000: piWrite[T](`addr`, val)
        of 0x5000: dspWrite[T](`addr`, val)
        of 0x6400: siWrite[T](`addr`, val)
        else: (let pc = geckoState.pc; echo &"unknown io register write {`addr`:X} {`val`:X} {pc:08X}")
    else:
        echo &"welp write to unknown memory access {`addr`:X} {`val`:X}"

proc readPhysical*[T](`addr`: uint32): T =
    if `addr` < uint32 MainRAM.len:
        cast[ptr T](addr MainRAM[`addr`])[]
    elif (`addr` and 0xFFF0000) == 0xC000000:
        case `addr` and 0xFF00
        of 0x2000: viRead[T](`addr`)
        of 0x3000: piRead[T](`addr`)
        of 0x5000: dspRead[T](`addr`)
        of 0x6400: siRead[T](`addr`)
        else: (let pc = geckoState.pc; echo &"unknown io register read {`addr`:X} {pc:08X}"; T(0))
    else:
        echo &"welp read from unknown memory access {`addr`:X}"
        T(0)