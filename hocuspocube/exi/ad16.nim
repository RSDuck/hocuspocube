import
    strformat,
    exi

type
    Ad16 = ref object of ExiDevice
        transactionPos: uint32
        cmd: uint8
        reg: array[4, uint8]

proc select(dev: ExiDevice, state: bool) =
    let dev = Ad16 dev
    if state:
        dev.transactionPos = 0

const
    cmdInit = 0x0'u8
    cmdWrite = 0xA0'u8
    cmdRead = 0xA2'u8

proc exchange(dev: ExiDevice, response: var openArray[byte], input: openArray[byte]) =
    let dev = Ad16 dev

    for i in 0..<inbytes():
        let inData = input.readByte(i)

        if dev.transactionPos == 0:
            dev.cmd = inData
            response.writeByte i, 0xFF
        else:
            case dev.cmd
            of cmdInit:
                echo "init"
                if dev.transactionPos == 1:
                    dev.reg = [0x04'u8, 0x12, 0x00, 0x00]

                if dev.transactionPos == 1 or dev.transactionPos >= 6:
                    response.writeByte i, 0xFF
                else:
                    response.writeByte i, dev.reg[dev.transactionPos - 2]
            of cmdWrite:
                if dev.transactionPos < 5:
                    dev.reg[dev.transactionPos - 1] = inData
                    if dev.transactionPos + 1 == 5:
                        echo &"AD16 state: {dev.reg}"
                response.writeByte i, 0xFF
            of cmdRead:
                if dev.transactionPos < 5:
                    response.writeByte i, dev.reg[dev.transactionPos - 1]
                else:
                    response.writeByte i, 0xFF
            else:
                echo &"unknown AD16 command {dev.cmd:02X}"

        dev.transactionPos += 1

let devAd16* = Ad16(select: select, exchange: exchange)