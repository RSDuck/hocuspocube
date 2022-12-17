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
    "opengl#f1467126bd48d47a20d85115115ec3640f414165",
    "xxhash",
    "catnip#e0401dcfa78e3e18cf65a951fbb30359b5d07f21"]

when defined(windows):
    dependencies.add "winim"

requires dependencies