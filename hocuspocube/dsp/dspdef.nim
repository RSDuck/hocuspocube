import
    macros, tables,
    ../util/instrdecoding

const
    DspPatterns* = @[
        ("jmp",     "000000101001cccc"),
        ("jmpr",    "000101110rr0cccc"),

        ("call",    "000000101011cccc"),
        ("callr",   "000101110rr1cccc"),

        ("rets",    "000000101101cccc"),
        ("reti",    "000000101111cccc"),

        ("trap",    "0000000000100000"),
        ("wait",    "0000000000100001"),

        ("exec",    "000000100111cccc"),

        ("loopi",   "00010001cccccccc"),
        ("loop",    "00000000011rrrrr"),

        ("repi",    "00010000cccccccc"),
        ("rep",     "00000000010rrrrr"),

        ("pld",     "0000001d0001mmrr"),

        ("mr",      "00000000000mmmrr"),

        ("adsi",    "0000010diiiiiiii"),
        ("adli",    "0000001d00000000"),
        ("cmpsi",   "0000011siiiiiiii"),
        ("cmpli",   "0000001s10000000"),
        ("lsfi",    "0001010d0iiiiiii"),
        ("asfi",    "0001010d1iiiiiii"),
        ("xorli",   "0000001d00100000"),
        ("anli",    "0000001d01000000"),
        ("orli",    "0000001d01100000"),

        ("norm",    "0000001d000001rr"),
        ("ddiv",    "0000001d0ss01000"),

        ("addc",    "0000001d10s01100"),
        ("subc",    "0000001d10s01101"),
        ("negc",    "0000001d00001101"),

        ("max",     "0000001d0ss01001"),

        ("lsfn",    "0000001d01s01010"),
        ("lsfn2",   "0000001d11001010"),
        ("asfn",    "0000001d01s01011"),
        ("asfn2",   "0000001d11001011"),

        ("ld",      "0001100mmrrddddd"),
        ("st",      "0001101mmrrsssss"),
        ("ldsa",    "00100dddaaaaaaaa"),
        ("stsa",    "00101sssaaaaaaaa"),
        ("ldla",    "00000000110ddddd"),
        ("stla",    "00000000111sssss"),

        ("mv",      "000111dddddsssss"),
        ("mvsi",    "00001dddiiiiiiii"),
        ("mvli",    "00000000100ddddd"),

        ("stli",    "00010110aaaaaaaa"),

        ("clrpsr",  "0001001000000bbb"),
        ("setpsr",  "0001001100000bbb"),
        ("btstl",   "0000001d10100000"),
        ("btsth",   "0000001d11000000"),

        ("add",     "0100sssdxxxxxxxx"),
        ("addl",    "011100sdxxxxxxxx"),
        ("sub",     "0101sssdxxxxxxxx"),
        ("amv",     "0110sssdxxxxxxxx"),
        ("cmp",     "110sd001xxxxxxxx"),
        ("cmpa",    "10000010xxxxxxxx"),

        ("inc",     "011101ddxxxxxxxx"),
        ("dec",     "011110ddxxxxxxxx"),

        ("abs",     "1010d001xxxxxxxx"),
        ("neg",     "0111110dxxxxxxxx"),
        ("negp",    "0111111dxxxxxxxx"),

        ("clra",    "1000d001xxxxxxxx"),
        ("clrp",    "10000100xxxxxxxx"),

        ("rnd",     "1111110dxxxxxxxx"),
        ("rndp",    "1111111dxxxxxxxx"),

        ("tst",     "1011s001xxxxxxxx"),
        ("tst2",    "1000011sxxxxxxxx"),
        ("tstp",    "10000101xxxxxxxx"),

        ("lsl16",   "1111000dxxxxxxxx"),
        ("lsr16",   "1111010dxxxxxxxx"),
        ("asr16",   "1001d001xxxxxxxx"),

        ("addp",    "111110sdxxxxxxxx"),

        ("pnop",    "10000000xxxxxxxx"),

        ("clrim",   "10001010xxxxxxxx"),
        ("clrdp",   "10001100xxxxxxxx"),
        ("clrxl",   "10001110xxxxxxxx"),

        ("setim",   "10001011xxxxxxxx"),
        ("setdp",   "10001101xxxxxxxx"),
        ("setxl",   "10001111xxxxxxxx"),

        ("mpy",     "1ssss000xxxxxxxx"),
        ("mpy2",    "10000011xxxxxxxx"),
        ("mac",     "111000ssxxxxxxxx"),
        ("mac2",    "111010ssxxxxxxxx"),
        ("mac3",    "1111001sxxxxxxxx"),
        ("macn",    "111001ssxxxxxxxx"),
        ("macn2",   "111011ssxxxxxxxx"),
        ("macn3",   "1111011sxxxxxxxx"),
        ("mvmpy",   "1ssss11dxxxxxxxx"),
        ("rnmpy",   "1ssss01dxxxxxxxx"),
        ("admpy",   "1ssss10dxxxxxxxx"),

        ("nnot",    "0011001d1xxxxxxx"),
        ("xxor",    "001100sd0xxxxxxx"),
        ("xxor2",   "0011000d1xxxxxxx"),
        ("aand",    "001101sd0xxxxxxx"),
        ("aand2",   "0011110d0xxxxxxx"),
        ("oor",     "001110sd0xxxxxxx"),
        ("oor2",    "0011111d0xxxxxxx"),

        ("lsf",     "001101sd1xxxxxxx"),
        ("lsf2",    "0011110d1xxxxxxx"),
        ("asf",     "001110sd1xxxxxxx"),
        ("asf2",    "0011111d1xxxxxxx")
    ]

    MulAccExclude = @[
        ("ssss", 0'u32),
        ("ssss", 1'u32),
        ("ssss", 12'u32),
        ("ssss", 13'u32),
        ("ssss", 14'u32),
        ("ssss", 15'u32)]
    DspExcludeList = {
        "mpy": MulAccExclude,
        "mvmpy": MulAccExclude,
        "rnmpy": MulAccExclude,
        "admpy": MulAccExclude
    }.toTable()

    DspSecondaryPatterns = @[
        ("mr",  "________0000mmrr"),
        ("mv",  "________0001ddss"),
        ("st",  "________001ssmrr"),
        ("ld",  "________01dddmrr"),
        ("ls",  "________10ddmnks"),
        ("ldd", "________11ddmnrr")
    ]

macro dspSecondaryDispatch*[T](instr: uint16, state: var T, undefinedInstr: proc(state: var T, instr: uint16)) =
    generateShortDecoder[4..7](DspSecondaryPatterns, 16, instr, state, undefinedInstr)

macro dspMainDispatch*[T](instr: uint16, state: var T, undefinedInstr: proc(state: var T, instr: uint16)): untyped =
    generateDecoder[7..15, 0..6](DspPatterns, DspExcludeList, 16, instr, state, undefinedInstr)
