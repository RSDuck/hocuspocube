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
    "https://github.com/status-im/nim-stew.git#5cf4feabea0820d7f03b146b0973a57973bcc4c1",
    "opengl",
    "xxhash"]

when defined(windows):
    dependencies.add "winim"

requires dependencies