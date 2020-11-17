# Package

version       = "0.1.0"
author        = "RSDuck"
description   = "A stupid attempt at GameCube emulation"
license       = "GPL-3.0"
bin           = @["hocuspocube"]



# Dependencies

requires @["nim >= 1.3.5", "sdl2", "https://github.com/status-im/nim-stew.git#a99dafab420bcbbffee35e9bd847a9014eafaffe"]
