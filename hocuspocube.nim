import
    streams, os,
    hocuspocube/cube,
    hocuspocube/si/gccontroller, hocuspocube/si/si,
    hocuspocube/frontend/sdl

initFrontend()

configureSiDevice 0, makeGcController()

echo "starting file ", paramStr(1)
loadDol(newFileStream(paramStr(1)))

run()