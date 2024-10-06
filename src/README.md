The ROM works with both the 6309 and 6510 as the CPU.

# BUILD THE 6502 CODE

Yes, it's a 6510 but uses 6502 instructions. Hence the file name.
Build with TASS:

```64tass.exe" -b -o "rom_6502_reset.bin" -L "rom_6502_reset.lst" -a "S:\SVNLocalPriv\Commodore-6309\rom_6502_reset.asm```

# BUILD THE 6309 CODE

Build with LWASM:

```lwasm --abs --6309 -o rom/1-k6309.rom rom.asm```

The source file includes the "rom_6502_reset.bin" file as binary.

# WRITE THE ROM

Use your EPROM or Flash programmer to create the replacement C64 kernel using the 8 KBytes file [1-k6398.rom](../release/1-k6398.rom)

# REPLACE ROM

Replace your C64 ROM with the one you just programmed.
