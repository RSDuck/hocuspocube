    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException


    mvli r0, 0
    mvli m0, 0
    mvli l0, 0

    mr r0, -m0
    mv b1, r0
    call transferOneWord

    mvli r0, 0
    mvli m0, 0
    mvli l0, 0

    mr r0, m0
    mv b1, r0
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

