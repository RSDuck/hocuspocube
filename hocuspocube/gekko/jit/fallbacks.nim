import
    ../../util/genfallbacks,
    ../ppcdef, ../ppcstate,
    interpreter

generateFallbacks(PpcPatterns, PpcState, uint32, interpreter, 4)

{.used.}