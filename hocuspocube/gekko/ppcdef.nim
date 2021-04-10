import
    strutils, macros, tables,
    ../util/instrdecoding

#[
    PowerPC is both easy and hard to decode. On one hand it's all quite organised.
    If the instruction has an immediate operand the instruction type is determined
    by the first 6 bit, otherwise it's determined by the the last up to 10(!) bit,
    which means you need 16-bit to identify them properly.

    This makes decoding them the way you would decode ARM instructions (which need only
    need 12-bit to be identified, but the way it's organised is way messier) quite hard.

    For this reason we do the decoding in two steps, which isn't optimal as it
    makes the dispatching more complicated, but we'll see how it goes.
]#

const PpcPatterns =
    (block:
        var patterns: seq[(string, string)]

        # Integer Arithmetic
        template intArith(name: string, num: int, oe = true): untyped =
            patterns.add (name, "011111dddddaaaaabbbbb" & (if oe: "o" else: "0") & toBin(num, 9) & "r")
        template intArith2Op(name: string, num: int): untyped =
            patterns.add (name, "011111dddddaaaaa00000o" & toBin(num, 9) & "r")
        template intArithImm(name: string, num: int): untyped =
            patterns.add (name, toBin(num, 6) & "dddddaaaaaiiiiiiiiiiiiiiii")
        intArith    "addx",         266
        intArith    "addcx",        10
        intArith    "addex",        138
        intArithImm "addi",         14
        intArithImm "addic",        12
        intArithImm "addicdot",     13
        intArithImm "addis",        15
        intArith2Op "addmex",       234
        intArith2Op "addzex",       202
        intArith    "divwx",        491
        intArith    "divwux",       459
        intArith    "mulhwx",       75,     false
        intArith    "mulhwux",      11,     false
        intArithImm "mulli",        7
        intArith    "mullwx",       235
        intArith2Op "negx",         104
        intArith    "subfx",        40
        intArith    "subfcx",       8
        intArith    "subfex",       136
        intArithImm "subfic",       8
        intArith2Op "subfmex",      232
        intArith2Op "subfzex",      200

        # Integer Comparison
        patterns.add(("cmp",        "011111ccc0laaaaabbbbb00000000000"))
        patterns.add(("cmpi",       "001011ccc0laaaaaiiiiiiiiiiiiiiii"))
        patterns.add(("cmpl",       "011111ccc0laaaaabbbbb00001000000"))
        patterns.add(("cmpli",      "001010ccc0laaaaaiiiiiiiiiiiiiiii"))

        # Integer Logical
        template intLogic(name: string, num: int): untyped =
            patterns.add (name, "011111sssssaaaaabbbbb" & toBin(num, 10) & "r")
        template intLogicImm(name: string, num: int): untyped =
            patterns.add (name, toBin(num, 6) & "sssssaaaaaiiiiiiiiiiiiiiii")
        template intLogic2Op(name: string, num: int): untyped =
            patterns.add (name, "011111sssssaaaaa00000" & toBin(num, 10) & "r")
        intLogic    "andx",             28
        intLogic    "andcx",            60
        intLogicImm "andidot",          28
        intLogicImm "andisdot",         29
        intLogic2Op "cntlzwx",          26
        intLogic    "eqvx",             284
        intLogic2Op "extsbx",           954
        intLogic2Op "extshx",           922
        intLogic    "nandx",            476
        intLogic    "norx",             124
        intLogic    "orx",              444
        intLogic    "orcx",             412
        intLogicImm "ori",              24
        intLogicImm "oris",             25
        intLogic    "xorx",             316
        intLogicImm "xori",             26
        intLogicImm "xoris",            27

        # Integer Rotate
        patterns.add(("rlwimix",    "010100sssssaaaaahhhhhbbbbbeeeeer"))
        patterns.add(("rlwinmx",    "010101sssssaaaaahhhhhbbbbbeeeeer"))
        patterns.add(("rlwnmx",     "010111sssssaaaaahhhhhbbbbbeeeeer"))

        # Integer Shift
        patterns.add(("slwx",       "011111sssssaaaaabbbbb0000011000r"))
        patterns.add(("srawx",      "011111sssssaaaaabbbbb1100011000r"))
        patterns.add(("srawix",     "011111sssssaaaaahhhhh1100111000r"))
        patterns.add(("srwx",       "011111sssssaaaaabbbbb1000011000r"))

        # Floating Point Arithmetic
        template floatOp(name: string, num1, num2: int): untyped =
            patterns.add((name, toBin(num1, 6) & "dddddaaaaabbbbb00000" & toBin(num2, 5) & "r"))
        floatOp     "faddx",         63,        21
        floatOp     "faddsx",        59,        21
        floatOp     "fdivx",         63,        18
        floatOp     "fdivsx",        59,        18
        patterns.add(("fmulx",      "111111dddddaaaaa00000ccccc11001r"))
        patterns.add(("fmulsx",     "111011dddddaaaaa00000ccccc11001r"))
        patterns.add(("fresx",      "111011ddddd00000bbbbb0000011000r"))
        patterns.add(("frsqrtex",    "111111ddddd00000bbbbb0000011010r"))
        floatOp     "fsubx",         63,        20
        floatOp     "fsubsx",        59,        20
        patterns.add(("fselx",      "111111dddddaaaaabbbbbccccc10111r"))

        # Float Point Multiply Add
        template fmaddOp(name: string, num1, num2: int): untyped =
            patterns.add((name, toBin(num1, 6) & "dddddaaaaabbbbbccccc" & toBin(num2, 5) & "r"))
        fmaddOp     "fmaddx",       63,         29
        fmaddOp     "fmaddsx",     59,         29
        fmaddOp     "fmsubx",       63,         28
        fmaddOp     "fmsubsx",      59,         28
        fmaddOp     "fnmaddx",      63,         31
        fmaddOp     "fnmaddsx",     59,         31
        fmaddOp     "fnmsubx",      63,         30
        fmaddOp     "fnmsubsx",     59,         30

        # Floating Point Round and Conversion
        patterns.add(("fctiwx",     "111111ddddd00000bbbbb0000001110r"))
        patterns.add(("fctiwzx",    "111111ddddd00000bbbbb0000001111r"))
        patterns.add(("frspx",      "111111ddddd00000bbbbb0000001100r"))

        # Floating Point Compare
        patterns.add(("fcmpo",      "111111ccc00aaaaaddddd00001000000"))
        patterns.add(("fcmpu",      "111111ccc00aaaaaddddd00000000000"))

        # Floating Point Status and Control
        patterns.add(("mcrfs",      "111111ccc00sss000000000010000000"))
        patterns.add(("mffsx",      "111111ccc0000000000001001000111r"))
        patterns.add(("mtfsb0x",    "111111ccc0000000000000001000110r"))
        patterns.add(("mtfsb1x",    "111111ccc0000000000000000100110r"))
        patterns.add(("mtfsfx",     "1111110ffffffff0bbbbb1011000111r"))
        patterns.add(("mtfsfix",    "111111ccc0000000iiii00010000110r"))

        # Integer Load
        template load(name: string, num: int): untyped =
            patterns.add((name, "011111dddddaaaaabbbbb" & toBin(num, 10) & "0"))
        template loadImm(name: string, num: int): untyped =
            patterns.add((name, toBin(num, 6) & "dddddaaaaaiiiiiiiiiiiiiiii"))
        template store(name: string, num: int): untyped =
            patterns.add((name, "011111sssssaaaaabbbbb" & toBin(num, 10) & "0"))
        template storeImm(name: string, num: int): untyped =
            patterns.add((name, toBin(num, 6) & "sssssaaaaaiiiiiiiiiiiiiiii"))
        loadImm     "lbz",          34
        loadImm     "lbzu",         35
        load        "lbzux",        119
        load        "lbzx",         87
        loadImm     "lha",          42
        loadImm     "lhau",         43
        load        "lhaux",        375
        load        "lhax",         343
        loadImm     "lhz",          40
        loadImm     "lhzu",         41
        load        "lhzux",        311
        load        "lhzx",         279
        loadImm     "lwz",          32
        loadImm     "lwzu",         33
        load        "lwzux",        55
        load        "lwzx",         23

        # Integer Store
        storeImm    "stb",          38
        storeImm    "stbu",         39
        store       "stbux",        247
        store       "stbx",         215
        storeImm    "sth",          44
        storeImm    "sthu",         45
        store       "sthux",        439
        store       "sthx",         407
        storeImm    "stw",          36
        storeImm    "stwu",         37
        store       "stwux",        183
        store       "stwx",         151

        # Integer Load and Store with Byte Reverse
        load        "lhbrx",        790
        load        "lwbrx",        534
        store       "sthbrx",       918
        store       "stwbrx",       662

        patterns.add(("lmw",        "101110dddddaaaaaiiiiiiiiiiiiiiii"))
        patterns.add(("stmw",       "101111dddddaaaaaiiiiiiiiiiiiiiii"))

        # Integer Load and Store String
        load        "lswi",         597
        load        "lswx",         533
        store       "stswi",        725
        store       "stswx",        661

        # Memory Synchronisation
        patterns.add(("eieio",      "01111100000000000000011010101100"))
        patterns.add(("isync",      "01001100000000000000000100101100"))
        patterns.add(("lwarx",      "011111dddddaaaaabbbbb00000101000"))
        patterns.add(("stwcxdot",    "011111sssssaaaaabbbbb00100101101"))
        patterns.add(("sync",       "01111100000000000000010010101100"))

        # Floating Point Load
        loadImm     "lfd",          50
        loadImm     "lfdu",         51
        load        "lfdux",        631
        load        "lfdx",         599
        loadImm     "lfs",          48
        loadImm     "lfsu",         49
        load        "lfsux",        567
        load        "lfsx",         535

        # Floating Point Store
        storeImm    "stfd",         54
        storeImm    "stfdu",        55
        store       "stfdux",       759
        store       "stfdx",        727
        store       "stfiwx",       983
        storeImm    "stfs",         52
        storeImm    "stfsu",        53
        store       "stfsux",       695
        store       "stfsx",        663

        # Floating Point Move
        template fmovOp(name: string, num: int): untyped =
            patterns.add((name, "111111ddddd00000bbbbb" & toBin(num, 10) & "r"))
        fmovOp      "fabsx",        264
        fmovOp      "fmrx",         72
        fmovOp      "fnabsx",       136
        fmovOp      "fnegx",        40

        # Branch
        patterns.add(("bx",         "010010iiiiiiiiiiiiiiiiiiiiiiiiak"))
        patterns.add(("bcx",        "010000oooooiiiiiddddddddddddddak"))
        patterns.add(("bcctrx",     "010011oooooiiiii000001000010000k"))
        patterns.add(("bclrx",      "010011oooooiiiii000000000010000k"))

        # Condition Register Logical
        template condOp(name: string, num: int): untyped =
            patterns.add((name, "010011dddddaaaaabbbbb" & toBin(num, 10) & "0"))
        condOp      "crand",        257
        condOp      "crandc",       129
        condOp      "creqv",        289
        condOp      "crnand",       225
        condOp      "crnor",        33
        condOp      "cror",         449
        condOp      "crorc",        417
        condOp      "crxor",        193
        patterns.add(("mcrf",       "010011ddd00sss000000000000000000"))

        # System Linkage
        patterns.add(("rfi",        "01001100000000000000000001100100"))
        patterns.add(("sc",         "01000100000000000000000000000010"))

        # Trap
        patterns.add(("tw",         "011111tttttaaaaabbbbb00000001000"))
        patterns.add(("twi",        "000011tttttaaaaaiiiiiiiiiiiiiiii"))

        # Processor Control
        patterns.add(("mcrxr",      "011111sss00000000000001000000000"))
        patterns.add(("mfcr",       "011111ddddd000000000000000100110"))
        patterns.add(("mfmsr",      "011111ddddd000000000000010100110"))
        patterns.add(("mfspr",      "011111dddddpppppppppp01010100110"))
        patterns.add(("mftb" ,      "011111dddddpppppppppp01011100110"))
        patterns.add(("mtcrf",      "011111sssss0cccccccc000100100000"))
        patterns.add(("mtmsr",      "011111sssss000000000000100100100"))
        patterns.add(("mtspr",      "011111dddddpppppppppp01110100110"))

        # Cache Management
        template cache(name: string, num: int): untyped =
            patterns.add((name, "01111100000aaaaabbbbb" & toBin(num, 10) & "0"))
        cache       "dcbf",         86
        cache       "dcbi",         470
        cache       "dcbst",        54
        cache       "dcbt",         278
        cache       "dcbtst",       246
        cache       "dcbz",         1014
        cache       "icbi",         982

        # Segment Register Manipulation
        patterns.add(("mfsr",       "011111ddddd0ssss0000010010100110"))
        patterns.add(("mfsrin",     "011111ddddd00000bbbbb10100100110"))
        patterns.add(("mtsr",       "011111ddddd0ssss0000000110100100"))
        patterns.add(("mtsrin",     "011111ddddd00000bbbbb00111100100"))

        # Lookaside Buffer Management
        patterns.add(("tlbie",      "0111110000000000bbbbb01001100100"))
        patterns.add(("tlbsync",    "01111100000000000000010001101100"))

        # External Control
        patterns.add(("eciwx",      "011111dddddaaaaabbbbb01001101100"))
        patterns.add(("ecowx",      "011111sssssaaaaabbbbb01101101100"))

        # Paired-Single Load and Store
        template psLoad(name: string, num: int): untyped =
            patterns.add((name, "000100dddddaaaaabbbbbwiii" & toBin(num, 6) & "0"))
        template psLoadImm(name: string, num: int): untyped =
            patterns.add((name, toBin(num, 6) & "sssssaaaaawiiidddddddddddd"))
        template psStore(name: string, num: int): untyped =
            patterns.add((name, "000100sssssaaaaabbbbbwiii" & toBin(num, 6) & "0"))
        template psStoreImm(name: string, num: int): untyped =
            patterns.add((name, toBin(num, 6) & "sssssaaaaawiiidddddddddddd"))
        psLoad      "psq_lx",       6
        psStore     "psq_stx",      7
        psLoad      "psq_lux",     38
        psStore     "psq_stux",    39
        psLoadImm   "psq_l",        56
        psLoadImm   "psq_lu",       57
        psStoreImm  "psq_st",       60
        psStoreImm  "psq_stu",      61

        # Paired-Single Floating Point Arithmetic
        template psOp(name: string, num: int, twoOp = false): untyped =
            patterns.add((name, "000100dddddaaaaabbbbb" & (if twoOp: "00000" else: "ccccc") & toBin(num, 5) & "r"))
        template psUnary(name: string, num: int): untyped =
            patterns.add((name, "000100ddddd00000bbbbb" & toBin(num, 10) & "r"))
        psOp        "ps_div",       18,     true
        psOp        "ps_sub",       20,     true
        psOp        "ps_add",       21,     true
        psOp        "ps_sel",       23
        patterns.add(("ps_res",     "000100ddddd00000bbbbb0000011000r"))
        patterns.add(("ps_mul",     "000100dddddaaaaa00000ccccc11001r"))
        patterns.add(("ps_rsqrte",  "000100ddddd00000bbbbb0000011010r"))
        psOp        "ps_msub",      28
        psOp        "ps_madd",      29
        psOp        "ps_nmsub",     30
        psOp        "ps_nmadd",     31
        psUnary     "ps_neg",       40
        psUnary     "ps_mr",        72
        psUnary     "ps_nabs",      136
        psUnary     "ps_abs",       264

        # Misc Pairsed-Single
        psOp        "ps_sum0",      10
        psOp        "ps_sum1",      11
        patterns.add(("ps_muls0",    "000100dddddaaaaa00000ccccc01100r"))
        patterns.add(("ps_muls1",    "000100dddddaaaaa00000ccccc01101r"))
        patterns.add(("ps_madds0",   "000100dddddaaaaabbbbbccccc01110r"))
        patterns.add(("ps_madds1",   "000100dddddaaaaabbbbbccccc01111r"))
        template psCmpOp(name: string, num: int): untyped =
            patterns.add((name, "000100ccc00aaaaabbbbb" & toBin(num, 10) & "0"))
        psCmpOp     "ps_cmpu0",     0
        psCmpOp     "ps_cmpo0",     32
        psCmpOp     "ps_cmpu1",     64
        psCmpOp     "ps_cmpo1",     96
        template psMergeOp(name: string, num: int): untyped =
            patterns.add((name, "000100dddddaaaaabbbbb" & toBin(num, 10) & "r"))
        psMergeOp   "ps_merge00",   528
        psMergeOp   "ps_merge01",   560
        psMergeOp   "ps_merge10",   592
        psMergeOp   "ps_merge11",   624
        patterns.add(("dcbz_l",     "00010000000aaaaabbbbb11111101100"))

        patterns)

macro dispatchPpc*[T](instr: uint32, state: var T, undefinedInstr: proc(state: var T, instr: uint32)) =
    generateDecoder[26..31, 1..10](PpcPatterns, initTable[string, seq[(string, uint32)]](), 32, instr, state, undefinedInstr)