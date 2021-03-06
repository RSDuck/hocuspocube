import
    strformat,
    sdl2,

    ../util/bitstruct,
    ../frontend/sdl,
    si

template controllerLog(msg: string): untyped =
    discard

type
    AnalogFullSet = object
        stickX, stickY: uint8
        substickX, substickY: uint8
        triggerL, triggerR: uint8
        buttonA, buttonB: uint8

    GcController* = ref object of SiDevice
        calibration: AnalogFullSet
        mode: uint8
        rumble: bool
        getOrigin: bool

makeBitStruct uint16, Buttons:
    a[0]: bool
    b[1]: bool
    x[2]: bool
    y[2]: bool
    start[3]: bool
    getOrigin[4]: bool

    left[8]: bool
    right[9]: bool
    down[10]: bool
    up[11]: bool
    z[12]: bool
    r[13]: bool
    l[14]: bool
    useOrigin[15]: bool

    firstByte[0..7]: uint8
    secondByte[8..15]: uint8

makeBitStruct uint8, Status:
    mode[0..2]: uint8
    motor[3..4]: uint8

const
    cmdId = 0x00'u8
    cmdStatus = 0x40'u8
    cmdOrigin = 0x41'u8
    cmdRecalibrate = 0x42'u8
    cmdStatusLong = 0x43'u8
    cmdReset = 0xFF'u8

# currently everything's hardcoded here
# not so great :(
proc makeButtonState(controller: GcController): Buttons =
    result.a = SDL_SCANCODE_V in keysDown
    result.b = SDL_SCANCODE_I in keysDown
    result.x = SDL_SCANCODE_X in keysDown
    result.y = SDL_SCANCODE_U in keysDown
    result.start = SDL_SCANCODE_RETURN in keysDown
    result.left = SDL_SCANCODE_N in keysDown
    result.right = SDL_SCANCODE_T in keysDown
    result.up = SDL_SCANCODE_G in keysDown
    result.down = SDL_SCANCODE_R in keysDown
    result.useOrigin = true

proc getAnalogState(): AnalogFullSet =
    result.stickX = if SDL_SCANCODE_LEFT in keysDown: 0x20
        elif SDL_SCANCODE_RIGHT in keysDown: 0xFF-0x20
        else: 0x80
    result.stickY = if SDL_SCANCODE_UP in keysDown: 0xFF-0x20
        elif SDL_SCANCODE_DOWN in keysDown: 0x20
        else: 0x80
    result.substickX = 0x80
    result.substickY = 0x80

func writeAnalogFullSet(stream: var seq[byte], analogSet: AnalogFullSet) =
    stream.add analogSet.stickX
    stream.add analogSet.stickY
    stream.add analogSet.substickX
    stream.add analogSet.substickY
    stream.add analogSet.triggerL
    stream.add analogSet.triggerR
    stream.add analogSet.buttonA
    stream.add analogSet.buttonB

func writeAnalogShort(stream: var seq[byte], y, x: uint8) =
    stream.add (x shr 4) or (y and 0xF0'u8)

proc transact(device: SiDevice, command: openArray[byte], recvData: var seq[byte]): SiTransferState =
    handleEvents()
    let controller = GcController device

    if command.len >= 1:
        controllerLog &"si transaction type: {command[0]:02X}"
        case command[0]
        of cmdId:
            if command.len > 1:
                return siErrCollision
            # type (regular gc controller without rumble)
            recvData.add 0x29
            recvData.add 0x00
            # status (nothing happening)

            var status: Status
            status.mode = controller.mode
            status.motor = uint8 controller.rumble
            recvData.add uint8(status)

            return siErrNone
        of cmdReset:
            controller.rumble = false
            return siErrNoResponse
        of cmdStatus:
            if command.len < 3:
                return siErrNoResponse
            elif command.len > 3:
                return siErrCollision
            let
                mode = command[1] and 0x7
                motor = command[2] and 0x3

            controller.mode = mode
            controller.rumble = motor == 1

            let
                buttons = makeButtonState controller
                analog = getAnalogState()

            recvData.add buttons.firstByte
            recvData.add buttons.secondByte

            recvData.add analog.stickX
            recvData.add analog.stickY
            case range[0..3](mode)
            of 0:
                recvData.add analog.substickX
                recvData.add analog.substickY
                recvData.writeAnalogShort analog.triggerL, analog.triggerR
                recvData.writeAnalogShort analog.buttonA, analog.buttonB
            of 1:
                recvData.writeAnalogShort analog.substickX, analog.substickY
                recvData.add analog.triggerL
                recvData.add analog.triggerR
                recvData.writeAnalogShort analog.buttonA, analog.buttonB
            of 2:
                recvData.writeAnalogShort analog.substickX, analog.substickY
                recvData.writeAnalogShort analog.triggerL, analog.triggerR
                recvData.add analog.buttonA
                recvData.add analog.buttonB
            of 3:
                recvData.add analog.substickX
                recvData.add analog.substickY
                recvData.add analog.triggerL
                recvData.add analog.triggerR
            of 4:
                recvData.add analog.substickX
                recvData.add analog.substickY
                recvData.add analog.buttonA
                recvData.add analog.buttonB
            else: discard

            return siErrNone
        of cmdOrigin:
            if command.len > 1:
                return siErrCollision
            
            let buttons = makeButtonState controller
            recvData.add buttons.firstByte
            recvData.add buttons.secondByte
            recvData.writeAnalogFullSet controller.calibration

            return siErrNone
        of cmdRecalibrate:
            if command.len < 3:
                return siErrNoResponse
            elif command.len > 3:
                return siErrCollision
            let
                mode = command[1] and 0x7
                motor = command[2] and 0x3

            controller.mode = mode
            controller.rumble = motor == 1

            controller.calibration = getAnalogState()

            let buttons = makeButtonState controller
            recvData.add buttons.firstByte
            recvData.add buttons.secondByte
            recvData.writeAnalogFullSet controller.calibration

            return siErrNone
        of cmdStatusLong:
            if command.len < 3:
                return siErrNoResponse
            elif command.len > 3:
                return siErrCollision
            let
                mode = command[1] and 0x7
                motor = command[2] and 0x3
            
            controller.mode = mode
            controller.rumble = motor == 1

            let 
                buttons = makeButtonState controller
                analog = getAnalogState() 
            recvData.add buttons.firstByte
            recvData.add buttons.secondByte
            recvData.writeAnalogFullSet analog

            return siErrNone
        else:
            echo &"unrecognised controller command {command[0]:02X}"

        siErrNoResponse
    else:
        siErrNoResponse


proc makeGcController*(): GcController =
    GcController(
        transact: transact,
        getOrigin: true,
        calibration: getAnalogState())
