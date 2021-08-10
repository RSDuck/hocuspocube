import
    sunnynimlibnx/display/native_window,
    sunnynimlibnx/services/[applet, hid],
    sunnynimlibnx/runtime/pad,
    egl, opengl,
    os,

    ../flipper/opengl/rasterogl,
    ../si/gccontroller

var
    eglDisplay: EGLDisplay
    eglSurface: EGLSurface
    eglContext: EGLContext

    frontendRunning* = true

    padState: PadState

proc initEgl =
    eglDisplay = eglGetDisplay(EGL_DEFAULT_DISPLAY)
    doAssert eglInitialize(eglDisplay, nil, nil) == EGL_TRUE

    doAssert eglBindAPI(EGL_OPENGL_API) == EGL_TRUE

    var
        config: EGLConfig
        numConfigs: EGLint
        framebufferAttrs = [
            EGLint EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
            EGL_RED_SIZE, 8,
            EGL_GREEN_SIZE, 8,
            EGL_BLUE_SIZE, 8,
            EGL_ALPHA_SIZE, 8,
            EGL_DEPTH_SIZE, 24,
            EGL_STENCIL_SIZE, 8,
            EGL_NONE]
    discard eglChooseConfig(eglDisplay, addr framebufferAttrs[0], addr config, EGLint 1, addr numConfigs)
    doAssert numConfigs != 0

    eglSurface = eglCreateWindowSurface(eglDisplay, config, nwindowGetDefault(), nil)

    var contextAttrs = [
        EGLint EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
        EGL_CONTEXT_MAJOR_VERSION, 4,
        EGL_CONTEXT_MINOR_VERSION, 3,
        EGL_NONE]
    eglContext = eglCreateContext(eglDisplay, config, EGL_NO_CONTEXT, addr contextAttrs[0])
    doAssert eglContext != nil

    doAssert eglMakeCurrent(eglDisplay, eglSurface, eglSurface, eglContext) == EGL_TRUE

    loadExtensions()

    padConfigureInput(1, HidNpadStyleSet_NpadStandard)
    padInitializeDefault(addr padState)

proc deinitEgl =
    doAssert eglMakeCurrent(eglDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT) == EGL_TRUE
    doAssert eglDestroyContext(eglDisplay, eglContext) == EGL_TRUE
    doAssert eglDestroySurface(eglDisplay, eglSurface) == EGL_TRUE
    doAssert eglTerminate(eglDisplay) == EGL_TRUE

proc initFrontend* =
    initEgl()

proc deinitFrontend* =
    deinitEgl()

proc handleEvents =
    padUpdate(addr padState)

    frontendRunning = appletMainLoop() and
        not((padGetButtons(addr padState) and HidNpadButton_Minus) != 0)

proc handleGcController*(): GcControllerState =
    let buttonsHeld = padGetButtons(addr padState)

    template checkButton(switchButton, gcButton): untyped =
        if (buttonsHeld and switchButton) != 0:
            result.keysDown.incl gcButton
    checkButton(HidNpadButton_A, gcControllerA)
    checkButton(HidNpadButton_B, gcControllerB)
    checkButton(HidNpadButton_X, gcControllerX)
    checkButton(HidNpadButton_Y, gcControllerY)
    checkButton(HidNpadButton_Left, gcControllerLeft)
    checkButton(HidNpadButton_Right, gcControllerRight)
    checkButton(HidNpadButton_Up, gcControllerUp)
    checkButton(HidNpadButton_Down, gcControllerDown)
    checkButton(HidNpadButton_R, gcControllerZ)
    checkButton(HidNpadButton_Plus, gcControllerStart)

    template translateStick(switchStick, gcStick): untyped =
        let stick = switchStick
        result.`gcStick X` = float32(stick.x) / float32(JOYSTICK_MAX)
        result.`gcStick Y` = float32(stick.y) / float32(JOYSTICK_MAX)
    translateStick(padGetStickPos(addr padState, 0), stick)
    translateStick(padGetStickPos(addr padState, 1), substick)

    result.triggerL = if (buttonsHeld and HidNpadButtonZL) != 0: 1f else: 0
    result.triggerR = if (buttonsHeld and HidNpadButtonZR) != 0: 1f else: 0

proc presentFrame*(width, height: int, pixelData: openArray[uint32]) =
    handleEvents()

    rasterogl.presentFrame(width, height, pixelData)

    discard eglSwapBuffers(eglDisplay, eglSurface)

proc presentBlankFrame*() =
    handleEvents()

    rasterogl.presentBlankFrame()

    discard eglSwapBuffers(eglDisplay, eglSurface)
