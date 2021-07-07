# this module only exists so that fallback generation is easier

# for instructions which when decoded take only one parameter (besides the cpu state)
# the signature is idenitcal with the fallback function which calls it.

# Thus when the fallback function should call the interpreter function, it calls
# itself instead, and there is an infinite loop!

# To prevent that we need to explicitly specify the module of the decoded interpreter functions
# but they are in a bunch of different modules.

# This module solves this by rexporting all the modules, so the fallbacks only need to
# specify interpreter.instruction

import
    ../interpreter/ppcinterpreter_int,
    ../interpreter/ppcinterpreter_loadstore,
    ../interpreter/ppcinterpreter_system,
    ../interpreter/ppcinterpreter_branch,
    ../interpreter/ppcinterpreter_float

export
    ppcinterpreter_int,
    ppcinterpreter_loadstore,
    ppcinterpreter_system,
    ppcinterpreter_branch,
    ppcinterpreter_float
