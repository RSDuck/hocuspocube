import
    ../../util/jit/ir,
    ../dsp

type
    DspIrState* = object
        pc*, instr*: uint16
        branch*: bool

proc fetchFollowingImm*(builder: var IrBlockBuilder[DspIrState]): uint16 =
    builder.regs.pc += 1
    instrRead(builder.regs.pc)
