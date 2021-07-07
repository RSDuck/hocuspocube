# hocuspocube

Don't take this serious. I hope this doesn't become a Dolphin knock-off.

## Current developement state:

- A slow PPC interpreter with terrible floating point support and a somewhat faster JIT (with even more terrible floating point support)
- It boots a few commercial games
- Most DSP instructions, though the ARAM accelerator is missing (also sound output is not implemented, though it isn't really a loss since nothing runs currently at fullspeed anyway)
- A scheduler which is almost a direct copy of melonDS's
