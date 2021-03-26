import
    sdl2, opengl,

    ../flipper/opengl/rasterogl

var
    window: WindowPtr
    context: GlContextPtr

    keysDown*: set[Scancode]

proc initFrontend* =
    sdl2.init(INIT_VIDEO or INIT_EVENTS)

    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE)
    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4)
    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0)
    discard sdl2.glSetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG)

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
            deinitFrontend()
            quit 0
        elif event.kind == KeyDown:
            keysDown.incl event.evKeyboard.keysym.scancode
        elif event.kind == KeyUp:
            keysDown.excl event.evKeyboard.keysym.scancode

proc presentFrame*(width, height: int, pixelData: openArray[uint32]) =
    handleEvents()

    rasterogl.presentFrame(width, height, pixelData)

    window.glSwapWindow()

proc presentBlankFrame*() =
    handleEvents()

    rasterogl.presentBlankFrame()

    window.glSwapWindow()