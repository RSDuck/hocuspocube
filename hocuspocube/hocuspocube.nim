import
    streams, os,
    parsecfg,

    cube,
    dsp/dsp,
    di,
    exi/rtcsramrom,
    si/gccontroller, si/si,
    frontend/sdl

if paramCount() > 0:
    initFrontend()

    let cfg = loadConfig("settings.ini")

    loadIplSram cfg.getSectionValue("General", "IPLPath"), cfg.getSectionValue("General", "SRAMPath")

    setupDspRom cfg.getSectionValue("General", "DSPIROMPath"), cfg.getSectionValue("General", "DSPDROMPath")

    configureSiDevice 0, makeGcController()

    if paramStr(1) == "loaddol" and paramCount() >= 2:
        loadDol(newFileStream(paramStr(2)))
    elif paramStr(1) == "boot" and paramCount() >= 2:
        loadDvd(paramStr(2))
        boot()
    elif paramStr(1) == "boot":
        boot()
    else:
        raiseAssert("unrecognised command")

    run()
else:
    raiseAssert("unrecognised command")
