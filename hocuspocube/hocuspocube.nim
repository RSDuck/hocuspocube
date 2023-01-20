import
    streams, os, tables, strutils,
    parsecfg, strformat, options,

    cube,
    dsp/dsp,
    di,
    exi/exi, exi/rtcsramrom, exi/memcard,
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

    for i in 0..<2:
        let
            section = &"MemcardSlot{i}"
            typ = cfg.getSectionValue(section, "Type")
        try:
            let
                typ = parseEnum[MemCardType](typ)
                file = cfg.getSectionValue(section, "MemcardFile")

            echo "inserting memcard ", typ
            setMemcardSlot i, newMemcard(file, memcard1019)
        except ValueError:
            echo &"unknown exi peripheral {typ} in slot {i}"

    configureSiDevice 0, makeGcController(handleGcController)

    echo paramCount()
    if paramCount() == 2 and paramStr(1) == "loaddol":
        echo &"loading {paramStr(2)}"
        loadDol(newFileStream(paramStr(2)))
    else:
        let mediumPath =
            if paramCount() == 2 and paramStr(1) == "boot":
                some(paramStr(2))
            elif (let mediumPath = cfg.getSectionValue("General", "DVDImage"); mediumPath.len > 0):
                some(mediumPath)
            else:
                none(string)

        if mediumPath.isSome:
            echo &"booting with medium inserted {mediumPath.get}"
            loadDvd(mediumPath.get)

        boot()

    run()
    deinitFrontend()

when not defined(nintendoswitch):
    start()
