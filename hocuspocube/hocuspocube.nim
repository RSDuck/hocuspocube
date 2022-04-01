import
    streams, os,
    parsecfg, strformat, options,

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
    initFrontend()

    let cfg = loadConfig("settings.ini")

    loadIplSram cfg.getSectionValue("General", "IPLPath"), cfg.getSectionValue("General", "SRAMPath")

    setupDspRom cfg.getSectionValue("General", "DSPIROMPath"), cfg.getSectionValue("General", "DSPDROMPath")

    configureSiDevice 0, makeGcController(handleGcController)

    echo paramCount()
    if paramCount() == 3 and paramStr(1) == "loaddol":
        echo &"loading {paramStr(2)}"
        loadDol(newFileStream(paramStr(2)))
    else:
        let mediumPath =
            if paramCount() == 3 and paramStr(1) == "boot":
                some(paramStr(2))
            elif (let mediumPath = cfg.getSectionValue("General", "DVDImage"); mediumPath.len > 0):
                some(mediumPath)
            else:
                none(string)

        if mediumPath.isSome:
            echo &"booting with as medium inserted {mediumPath}"
            loadDvd(mediumPath.get)

        boot()

    run()
    deinitFrontend()

when not defined(nintendoswitch):
    start()
