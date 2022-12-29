import
    strformat,

    ../util/bitstruct,
    si

template controllerLog(msg: string): untyped =
    discard

type
    GcControllerButton* = enum
        gcControllerA
        gcControllerB
        gcControllerX
        gcControllerY
        gcControllerLeft
        gcControllerRight
        gcControllerUp
        gcControllerDown
        gcControllerZ
        gcControllerStart

    GcControllerState* = object
        keysDown*: set[GcControllerButton]
        stickX*, stickY*: float32
        substickX*, substickY*: float32
        triggerL*, triggerR*: float32

    GcControllerPollFunc* = proc(): GcControllerState

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

        pollFunc: GcControllerPollFunc

makeBitStruct uint16, Buttons:
    a[0]: bool
    b[1]: bool
    x[2]: bool
    y[3]: bool
    start[4]: bool
    getOrigin[5]: bool

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

proc makeButtonState(state: GcControllerState, getOrigin: bool): Buttons =
    result.a = gcControllerA in state.keysDown
    result.b = gcControllerB in state.keysDown
    result.x = gcControllerX in state.keysDown
    result.y = gcControllerY in state.keysDown
    result.start = gcControllerStart in state.keysDown
    result.left = gcControllerLeft in state.keysDown
    result.right = gcControllerRight in state.keysDown
    result.up = gcControllerUp in state.keysDown
    result.down = gcControllerDown in state.keysDown
    result.z = gcControllerZ in state.keysDown
    result.l = state.triggerL >= 0.9f
    result.r = state.triggerR >= 0.9f
    result.getOrigin = getOrigin
    result.useOrigin = true

proc getAnalogState(state: GcControllerState): AnalogFullSet =
    const stickRange = float32(0x80-0x20)
    result.stickX = uint8(float32(0x80) + state.stickX * stickRange)
    result.stickY = uint8(float32(0x80) + state.stickY * stickRange)
    result.substickX = uint8(float32(0x80) + state.substickX * stickRange)
    result.substickY = uint8(float32(0x80) + state.substickY * stickRange)
    result.triggerL = uint8(float32(0xFF) * state.triggerL)
    result.triggerR = uint8(float32(0xFF) * state.triggerR)

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

method exchange(controller: GcController, command: openArray[byte], recvData: var seq[byte]): SiTransferState =
    if command.len >= 1:
        let curState = controller.pollFunc()

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
                buttons = makeButtonState(curState, controller.getOrigin)
                analog = getAnalogState(curState)

            recvData.add buttons.firstByte
            recvData.add buttons.secondByte

            recvData.add analog.stickX
            recvData.add analog.stickY
            case range[0..7](mode)
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

            controller.getOrigin = false

            let buttons = makeButtonState(curState, controller.getOrigin)
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

            controller.calibration = getAnalogState curState

            let buttons = makeButtonState(curState, controller.getOrigin)
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
                buttons = makeButtonState(curState, controller.getOrigin)
                analog = getAnalogState curState
            recvData.add buttons.firstByte
            recvData.add buttons.secondByte
            recvData.writeAnalogFullSet analog

            return siErrNone
        else:
            echo &"unrecognised controller command {command[0]:02X}"

        siErrNoResponse
    else:
        siErrNoResponse


proc makeGcController*(pollFunc: GcControllerPollFunc): GcController =
    GcController(
        getOrigin: true,
        calibration:
            AnalogFullSet(stickX: 0x80, stickY: 0x80,
                substickX: 0x80, substickY: 0x80),
        pollFunc: pollFunc)
