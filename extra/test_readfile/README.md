Test to read a file from 1541 (or compatible) disk drive.
The first file ("*") will be loaded starting from $0450 (so you can see bytes being loaded on screen).

- **test_6502_kernal_readfile.asm**: Source code. Configure options before building.
- **test_6502_kernal_readfile.prg**: Kernal-independent program (shuts off ROM and then load).
- **test_6502_kernal_readfile.rom**: Same but as a KERNAL replacement ROM (8 KB). See below.

I use the **test_6502_kernal_readfile.rom** file to verify the routine to port to 6309, and especially to profile critical timings (as the 6309 has very different timings, especially in native mode).

The intent is producing an equivalent:

- **test_6309_kernal_readfile.rom**: 6309 version to test kickstarting an OS (e.g. NitrOS-9).

Note that the 6309 version is not yet working, probably due to incorrect critical timings.
