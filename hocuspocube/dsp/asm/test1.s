    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException
    jmp handleException

    mvli m0, 2

    mvli r0, dataWords
    loop 5, wordLoop

    mvli r1, flags
    loop 32, flagLoop

    clr xl | nop

    mv r3, r0
    # load word
    pld a1, [r3], 1
    mv a2, a1
    pld a1, [r3], 0

    pld b1, [r1], 1
    mvli dpp, 0

    mv psr, b1
    stsa [0], a1
    ldsa b1, [0]

    mvli dpp, 0xFF
    call transferOneWord
    nop
flagLoop:
    nop

    mr r0, m0

wordLoop:
    nop

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

dataWords:
# zero
.dw 0x0000
.dw 0x0000
# ext positive
.dw 0x0001
.dw 0x0000
# ext negative
.dw 0xFFF0
.dw 0x0000
# no ext positive
.dw 0x0000
.dw 0x0100
# no ext negative
.dw 0xFFFF
.dw 0xF000

.equ flagV, 0x0002
.equ flagZ, 0x0004
.equ flagN, 0x0008
.equ flagE, 0x0010
.equ flagXl, 0x4000
.equ flagSv, 0x0040

flags:
#.dw flagXl
#.dw flagXl or flagV
.dw 0x20FA or flagXl
.dw 0x20E8 or flagXl
.dw flagXl or flagZ
.dw flagXl or flagZ or flagV
.dw flagXl or flagN
.dw flagXl or flagN or flagV
.dw flagXl or flagN or flagZ
.dw flagXl or flagN or flagZ or flagV
.dw flagXl or flagE
.dw flagXl or flagE or flagV
.dw flagXl or flagE or flagZ
.dw flagXl or flagE or flagZ or flagV
.dw flagXl or flagE or flagN
.dw flagXl or flagE or flagN or flagV
.dw flagXl or flagE or flagN or flagZ
.dw flagXl or flagE or flagN or flagZ or flagV
.dw flagSv or flagXl
.dw flagSv or flagXl or flagV
.dw flagSv or flagXl or flagZ
.dw flagSv or flagXl or flagZ or flagV
.dw flagSv or flagXl or flagN
.dw flagSv or flagXl or flagN or flagV
.dw flagSv or flagXl or flagN or flagZ
.dw flagSv or flagXl or flagN or flagZ or flagV
.dw flagSv or flagXl or flagE
.dw flagSv or flagXl or flagE or flagV
.dw flagSv or flagXl or flagE or flagZ
.dw flagSv or flagXl or flagE or flagZ or flagV
.dw flagSv or flagXl or flagE or flagN
.dw flagSv or flagXl or flagE or flagN or flagV
.dw flagSv or flagXl or flagE or flagN or flagZ
.dw flagSv or flagXl or flagE or flagN or flagZ or flagV
