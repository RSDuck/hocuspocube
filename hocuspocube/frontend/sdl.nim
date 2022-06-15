import
    sdl2, opengl,

    ../flipper/opengl/rasterogl,
    ../si/gccontroller

var
    window: WindowPtr
    context: GlContextPtr

    keysDown*: set[Scancode]

    frontendRunning* = true

proc initFrontend* =
    sdl2.init(INIT_VIDEO or INIT_EVENTS)

    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE)
    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4)
    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0)
    #discard sdl2.glSetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG)

    window = sdl2.createWindow("hocuspocube", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, 
        SDL_WINDOW_SHOWN or SDL_WINDOW_OPENGL)

    context = window.glCreateContext()
    discard window.glMakeCurrent(context)

    discard glSetSwapInterval(0)

    loadExtensions()

    window.glSwapWindow()

proc deinitFrontend* =
    destroy window
    sdl2.quit()

proc handleEvents* =
    var event = sdl2.Event()
    while sdl2.pollEvent(event):
        if event.kind == QuitEvent:
            frontendRunning = false
        elif event.kind == KeyDown:
            keysDown.incl event.evKeyboard.keysym.scancode
        elif event.kind == KeyUp:
            keysDown.excl event.evKeyboard.keysym.scancode

proc endFrame*() =
    window.glSwapWindow()

    handleEvents()

proc presentBlankFrame*() =
    handleEvents()

    rasterogl.presentBlankFrame()

    window.glSwapWindow()

proc handleGcController*(): GcControllerState =
    template checkButton(sdlButton, gcButton): untyped =
        if sdlButton in keysDown:
            result.keysDown.incl gcButton
    checkButton(SDL_SCANCODE_V, gcControllerA)
    checkButton(SDL_SCANCODE_I, gcControllerB)
    checkButton(SDL_SCANCODE_X, gcControllerX)
    checkButton(SDL_SCANCODE_U, gcControllerY)
    checkButton(SDL_SCANCODE_N, gcControllerLeft)
    checkButton(SDL_SCANCODE_T, gcControllerRight)
    checkButton(SDL_SCANCODE_G, gcControllerUp)
    checkButton(SDL_SCANCODE_R, gcControllerDown)
    checkButton(SDL_SCANCODE_L, gcControllerZ)
    checkButton(SDL_SCANCODE_RETURN, gcControllerStart)

    result.stickX = if SDL_SCANCODE_LEFT in keysDown: -1f
        elif SDL_SCANCODE_RIGHT in keysDown: 1f
        else: 0f
    result.stickY = if SDL_SCANCODE_UP in keysDown: 1f
        elif SDL_SCANCODE_DOWN in keysDown: -1f
        else: 0f

    result.triggerL = if SDL_SCANCODE_1 in keysDown: 1f else: 0
    result.triggerR = if SDL_SCANCODE_2 in keysDown: 1f else: 0
