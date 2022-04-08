# Package

version       = "0.1.0"
author        = "RSDuck"
description   = "A stupid attempt at GameCube emulation"
license       = "GPL-3.0"
bin           = @["hocuspocube/hocuspocube"]

# Dependencies

var dependencies = @[
    "nim >= 1.1.4",
    "sdl2",
    "https://github.com/status-im/nim-stew.git#cdb1f213d073fd2ecbdaf35a866417657da9294c",
    "opengl",
    "xxhash",
    "catnip"]

when defined(windows):
    dependencies.add "winim"

requires dependencies