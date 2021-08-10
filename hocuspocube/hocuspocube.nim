import
    streams, os,
    parsecfg, strformat,

    cube,
    dsp/dsp,
    di,
    exi/rtcsramrom,
    si/gccontroller, si/si

when defined(nintendoswitch):
    import frontend/switch
else:
    import frontend/sdl

proc start*() =
    if paramCount() > 0:
        initFrontend()

        let cfg = loadConfig("settings.ini")

        loadIplSram cfg.getSectionValue("General", "IPLPath"), cfg.getSectionValue("General", "SRAMPath")

        setupDspRom cfg.getSectionValue("General", "DSPIROMPath"), cfg.getSectionValue("General", "DSPDROMPath")

        configureSiDevice 0, makeGcController(handleGcController)

        if paramStr(1) == "loaddol" and paramCount() >= 2:
            echo &"loading {paramStr(2)}"
            loadDol(newFileStream(paramStr(2)))
        elif paramStr(1) == "boot" and paramCount() >= 2:
            echo &"booting with as medium inserted {paramStr(2)}"
            loadDvd(paramStr(2))
            boot()
        elif paramStr(1) == "boot":
            boot()
        else:
            raiseAssert("unrecognised command")

        run()
        deinitFrontend()
    else:
        raiseAssert("unrecognised command")

when not defined(nintendoswitch):
    start()