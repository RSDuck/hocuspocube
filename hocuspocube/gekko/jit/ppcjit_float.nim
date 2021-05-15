import
    ir, ppcfrontendcommon

using builder: var IrBlockBuilder[PpcIrRegState]

proc faddx*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr faddx")

proc faddsx*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr faddsx")

proc fdivx*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr fdivx")

proc fdivsx*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr fdivsx")

proc fmulx*(builder; d, a, c, rc: uint32) =
    raiseAssert("unimplemented instr fmulx")

proc fmulsx*(builder; d, a, c, rc: uint32) =
    raiseAssert("unimplemented instr fmulsx")

proc fresx*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr fresx")

proc frsqrtex*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr frsqrtex")

proc fsubx*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr fsubx")

proc fsubsx*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr fsubsx")

proc fselx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert "instr not implemented fselx"

proc fmaddx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fmaddx")

proc fmaddsx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fmaddsx")

proc fmsubx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fmsubx")

proc fmsubsx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fmsubsx")

proc fnmaddx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fnmaddx")

proc fnmaddsx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fnmaddsx")

proc fnmsubx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fnmsubx")

proc fnmsubsx*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr fnmsubsx")

proc fctiwx*(builder; d, b, rc: uint32) =
    raiseAssert "instr not implemented fctiwx"

proc fctiwzx*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr fctiwzx")

proc frspx*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr frspx")

proc fcmpo*(builder; crfD, a, b: uint32) =
    raiseAssert("unimplemented instr fcmpo")

proc fcmpu*(builder; crfD, a, b: uint32) =
    raiseAssert("unimplemented instr fcmpu")

proc mffsx*(builder; d, rc: uint32) =
    raiseAssert("unimplemented instr mffsx")

proc mtfsb0x*(builder; crbD, rc: uint32) =
    raiseAssert("unimplemented instr mtfsb0x")

proc mtfsb1x*(builder; crbD, rc: uint32) =
    discard

proc mtfsfx*(builder; fm, b, rc: uint32) =
    discard

proc mtfsfix*(builder; crfD, imm, rc: uint32) =
    raiseAssert("unimplemented instr mtfsfix")

proc fabsx*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr fabsx")

proc fmrx*(builder; d, b, rc: uint32) =
    discard

proc fnabsx*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr fnabsx")

proc fnegx*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr fnegx")

proc ps_div*(builder; d, a, b, rc: uint32) =
    raiseAssert "instr not implemented ps_div"

proc ps_sub*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_sub")

proc ps_add*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_add")

proc ps_sel*(builder; d, a, b, c, rc: uint32) =
    raiseAssert "instr not implemented ps_sel"

proc ps_res*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_res")

proc ps_mul*(builder; d, a, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_mul")

proc ps_rsqrte*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_rsqrte")

proc ps_msub*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_msub")

proc ps_madd*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_madd")

proc ps_nmsub*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_nmsub")

proc ps_nmadd*(builder; d, a, b, c, rc : uint32) =
    raiseAssert("unimplemented instr ps_nmadd")

proc ps_neg*(builder; d, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_neg")

proc ps_mr*(builder; d, b, rc: uint32) =
    discard

proc ps_nabs*(builder; d, b, rc: uint32) =
    raiseAssert "instr not implemented ps_nabs"

proc ps_abs*(builder; d, b, rc: uint32) =
    raiseAssert "instr not implemented ps_abs"

# for horizontal vector operations we need to be careful
# for the case when a destination register is also a source register

proc ps_sum0*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_sum0")

proc ps_sum1*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_sum1")

proc ps_muls0*(builder; d, a, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_muls0")

proc ps_muls1*(builder; d, a, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_muls1")

proc ps_madds0*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_madds0")

proc ps_madds1*(builder; d, a, b, c, rc: uint32) =
    raiseAssert("unimplemented instr ps_madds1")

proc ps_cmpu0*(builder; crfD, a, b: uint32) =
    raiseAssert("unimplemented instr ps_cmpu0")

proc ps_cmpo0*(builder; crfD, a, b: uint32) =
    raiseAssert("unimplemented instr ps_cmpo0")

proc ps_cmpu1*(builder; crfD, a, b: uint32) =
    raiseAssert("unimplemented instr ps_cmpu1")

proc ps_cmpo1*(builder; crfD, a, b: uint32) =
    raiseAssert("unimplemented instr ps_cmpo1")

proc ps_merge00*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_merge00")

proc ps_merge01*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_merge01")

proc ps_merge10*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_merge10")

proc ps_merge11*(builder; d, a, b, rc: uint32) =
    raiseAssert("unimplemented instr ps_merge11")