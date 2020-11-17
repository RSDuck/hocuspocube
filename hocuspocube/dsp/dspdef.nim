import
    macros,
    ../util/instrdecoding

const DspPatterns = (block:
    var patterns: seq[(string, string)]

    patterns.add(("nop",    "0000000000000000"))
    patterns.add(("halt",   "0000000000100001"))
    
    patterns.add(("jmp",    "000000101001cccc"))

    patterns.add(("loop",   "00000000010rrrrr"))
    patterns.add(("bloop",  "00000000011rrrrr"))

    patterns.add(("lrri",   "000110010ssddddd"))
    patterns.add(("lri",    "00000000100rrrrr"))
    patterns.add(("lr",     "00000000110rrrrr"))
    patterns.add(("sr",     "00000000111rrrrr"))

    patterns.add(("ilrr",   "0000001d000100ss"))
    patterns.add(("ilrri",  "0000001d000110ss"))

    # also different than duddie
    patterns.add(("andf",   "0000001r10100000"))

    # Dolphin has those the other way around then duddie
    # needs to be checked
    patterns.add(("sbclr",  "0001001000000iii"))
    patterns.add(("sbset",  "0001001100000iii"))

    patterns.add(("clr",    "1000x001xxxxxxxx"))

    patterns.add(("srri",   "000110110ddsssss"))

    patterns.add(("mrr",    "000111dddddsssss"))

    patterns.add(("si",     "00010110iiiiiiii"))
    
    patterns)

macro dspMainDispatch*[T](instr: uint16, state: var T, undefinedInstr: proc(state: var T, instr: uint16)): untyped =
    generateDecoder[8..15, 0..7](DspPatterns, 16, instr, state, undefinedInstr)