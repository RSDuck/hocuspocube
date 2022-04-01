import strformat

--cpu:arm64

switch("path", "../..")

let devkitproFolder = getEnv("DEVKITPRO")
let devkitA64Folder = getEnv("DEVKITA64")

switch("passC", &"-I{devkitProFolder}/libnx/include")
switch("passL", &"-specs={devkitProFolder}/libnx/switch.specs -L{devkitproFolder}/libnx/lib -L{devkitproFolder}/portlibs/switch/lib -lEGL -lglapi -ldrm_nouveau -lnx -lstdc++ -lm")
--gc:arc

--d:nimAllocPagesViaMalloc
--debugger:native

--d:danger
--passC:"-flto"

switch("os", "nintendoswitch")

switch("gcc.path", &"{devkitA64Folder}/bin")
switch("gcc.exe", "aarch64-none-elf-gcc")
switch("gcc.linkerexe", "aarch64-none-elf-gcc")

switch("gcc.options.linker", "-g -march=armv8-a -mtune=cortex-a57 -mtp=soft -fPIE")
switch("cpp.options.linker", "-g -march=armv8-a -mtune=cortex-a57 -mtp=soft -fPIE")
switch("gcc.options.always", "-g -w -ffunction-sections -march=armv8-a -mtune=cortex-a57 -mtp=soft -fPIE -D__SWITCH__")
switch("gcc.cpp.options.always", "-fpermissive -g -w -ffunction-sections -march=armv8-a -mtune=cortex-a57 -mtp=soft -fPIE -D__SWITCH__ -fno-rtti -fno-exceptions")
