import
    streams, os,
    parsecfg,

    hocuspocube/cube,
    hocuspocube/dsp/dsp,
    hocuspocube/exi/rtcsramrom,
    hocuspocube/si/gccontroller, hocuspocube/si/si,
    hocuspocube/frontend/sdl

initFrontend()

let cfg = loadConfig("settings.ini")

loadIpl cfg.getSectionValue("General", "IPLPath")

setupDspRom cfg.getSectionValue("General", "DSPIROMPath"), cfg.getSectionValue("General", "DSPDROMPath")

configureSiDevice 0, makeGcController()

#echo "starting file ", paramStr(1)
#loadDol(newFileStream(paramStr(1)))
boot()
run()