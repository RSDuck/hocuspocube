template toSigned(x: uint8): int8 = cast[int8](x)
template toSigned(x: uint16): int16 = cast[int16](x)
template toSigned(x: uint32): int32 = cast[int32](x)
template toSigned(x: uint64): int64 = cast[int64](x)

proc signExtend*[T: SomeUnsignedInt](val, bits: T): T {.inline.} =
    cast[T](toSigned(val) shl (sizeof(T)*8 - bits) shr (sizeof(T)*8 - bits))

proc carryAdd*[T: SomeUnsignedInt](a, b: T): bool {.inline.} =
    (high(T) - a) < b
proc carrySub*[T: SomeUnsignedInt](a, b: T): bool {.inline.} =
    a >= b

proc overflowAdd*[T: SomeUnsignedInt](a, b: T): bool {.inline.} =
    let res = a + b
    const signBit = T(1) shl (sizeof(T)*8-1)
    not(((a xor b) and signBit) != 0) and
        ((a xor res) and signBit) != 0
proc overflowSub*[T: SomeUnsignedInt](a, b: T): bool {.inline.} =
    let res = a - b
    const signBit = T(1) shl (sizeof(T)*8-1)
    ((a xor b) and signBit) != 0 and
        ((a xor res) and signBit) != 0
