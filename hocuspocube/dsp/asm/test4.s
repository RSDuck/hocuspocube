    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException


    mvli a0, -1
    mvli a1, -1
    mvli a2, 127

    adsi a, 1
    adsi a, 0

    mvli dpp, 0
    set xl | nop
    stsa [0], a1
    clr xl | nop


    ldsa b1, [0]
    mvli dpp, 0xFF
    call transferOneWord


    mvli a0, -128
    mvli a1, 0
    mvli a2, 0

    adsi a, -1
    adsi a, 0

    mvli dpp, 0
    set xl | nop
    stsa [0], a1
    clr xl | nop


    ldsa b1, [0]
    mvli dpp, 0xFF
    call transferOneWord

    wait

transferOneWord:
    # a1 - temporary
    # b1 - value to transfer
    ldsa a1, [DMBH and 0xFF]
    btstl a1, 0x8000
    jmpnt transferOneWord

    stli [DMBH and 0xFF], 0
    stla [DMBL], b1

    rets

handleException:
    mvli b1, 0xEAD
    call transferOneWord
    reti
