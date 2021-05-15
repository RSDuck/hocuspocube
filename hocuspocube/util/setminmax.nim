import bitops

proc min*[T](s: set[T]): T =
    let internalSet = cast[ptr UncheckedArray[byte]](unsafeAddr s)

    const sizeNotByteAligned = ord(high(T)) - ord(low(T)) != 7
    block done:
        var i = 0
        when sizeof(set[T]) >= 8:
            var remaining = sizeof(set[T])
            while remaining >= 8:
                let val64 = cast[ptr uint64](unsafeAddr internalSet[i])[]
                if val64 != 0:
                    let idx = i * 8 + countTrailingZeroBits(val64)
                    if sizeNotByteAligned and idx > ord(high(T)):
                        break done
                    return T(idx)
                i += 8
                remaining -= 8
        when sizeof(set[T]) >= 4 and (sizeof(set[T]) mod 8) >= 4:
            let val32 = cast[ptr uint32](unsafeAddr internalSet[i])[]
            if val32 != 0:
                let idx = i * 8 + countTrailingZeroBits(val32)
                if sizeNotByteAligned and idx > ord(high(T)):
                    break done
                return T(idx)
            i += 4
        when sizeof(set[T]) >= 2 and (sizeof(set[T]) mod 4) >= 2:
            let val16 = uint32 cast[ptr uint16](unsafeAddr internalSet[i])[]
            if val16 != 0:
                let idx = i * 8 + countTrailingZeroBits(val16)
                if sizeNotByteAligned and idx > ord(high(T)):
                    break done
                return T(idx)
            i += 2
        when sizeof(set[T]) >= 1 and (sizeof(set[T]) mod 2) == 1:
            let val8 = uint32 internalSet[i]
            if val8 != 0:
                let idx = i * 8 + countTrailingZeroBits(val8)
                if sizeNotByteAligned and idx > ord(high(T)):
                    break done
                return T(idx)

    raiseAssert("empty set")

proc popMin*[T](s: var set[T]): T =
    result = s.min()
    s.excl result