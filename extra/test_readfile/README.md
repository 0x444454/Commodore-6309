# WHAT IS THIS ?

This is a test to read a file from a 1541 (or cycle-exact compatible) disk drive.  
The first file on disk ("*") will be loaded starting from $8000 (PRG version) or $0450 (ROM version, so you can see bytes being loaded on screen).

- **test_6502_kernal_readfile.asm**: Source code. Read header comments and be sure to configure options before building with 64TASS.
- **test_6502_kernal_readfile.prg**: Kernal-independent program (i.e. shut-off all ROMs and then load "*").
- **test_6502_kernal_readfile.rom**: Same but as a KERNAL replacement ROM (8 KB). See below.

I use the **test_6502_kernal_readfile.rom** file to verify the routines to port to 6309, and especially to profile critical timings (as the 6309 has very different timings, especially in native mode).

# WHY ?

The intent is producing an equivalent:

- **test_6309_kernal_readfile.rom**: 6309 version to test kickstarting an OS (e.g. NitrOS-9).

As of now:
- The 6502 version is working, also as a Kernal replacement.
- The 6309 version is not yet working, probably due to incorrect critical timings.
