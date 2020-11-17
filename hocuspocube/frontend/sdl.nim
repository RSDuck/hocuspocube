import
    sdl2

var
    window: WindowPtr
    renderer: RendererPtr

    screenTexture: TexturePtr

    keysDown*: set[Scancode]

proc initFrontend* =
    sdl2.init(INIT_VIDEO or INIT_EVENTS)

    discard createWindowAndRenderer(640, 480, SDL_WINDOW_SHOWN, window, renderer)

    renderer.clear()

    screenTexture = renderer.createTexture(SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STREAMING, 640, 480)

proc deinitFrontend* =
    destroy renderer
    destroy window
    sdl2.quit()

proc handleEvents* =
    var event = sdl2.Event()
    while sdl2.pollEvent(event):
        if event.kind == QuitEvent:
            deinitFrontend()
            quit 0
        elif event.kind == KeyDown:
            echo "pressing down ", event.evKeyboard.keysym.scancode
            keysDown.incl event.evKeyboard.keysym.scancode
        elif event.kind == KeyUp:
            echo "releasing ", event.evKeyboard.keysym.scancode
            keysDown.excl event.evKeyboard.keysym.scancode

proc presentFrame*(width, height: int, pixelData: openArray[uint32]) =
    handleEvents()

    let rect: Rect = (x: cint 0, y: cint 0, w: cint width, h: cint height)
    var
        writePtr: pointer
        pitch: cint
    screenTexture.lockTexture(unsafeAddr rect, addr writePtr, addr pitch)

    for i in 0..<height:
        copyMem(cast[pointer](cast[ByteAddress](writePtr) + pitch * i), unsafeAddr pixelData[i * width], width*4)

    screenTexture.unlockTexture()

    renderer.copy(screenTexture, unsafeAddr rect, unsafeAddr rect)

    renderer.present()

proc presentBlankFrame*() =
    handleEvents()
    renderer.setDrawColor(0, 0, 0, 0)
    renderer.clear()
    renderer.present()
