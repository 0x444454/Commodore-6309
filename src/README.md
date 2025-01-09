The ROM works with both the 6309 and 6510 as the CPU.

# BUILD THE 6502 CODE

Yes, it's a 6510 but uses 6502 instructions. Hence the file name.
Build with TASS:

```64tass -b -o rom_6502_reset.bin -L rom_6502_reset.lst -a rom_6502_reset.asm```

# BUILD THE 6309 CODE

Edit the code to enable the test/demo you want.  
Then build with LWASM:

```lwasm --abs --6309 -o 1-k6309.rom rom.asm```

The source file includes the "rom_6502_reset.bin" file as binary.

# WRITE THE ROM

Use your EPROM or Flash programmer to create the replacement C64 kernel using the 8 KBytes file [1-k6398.rom](../release/1-k6398.rom)

# REPLACE ROM

Replace your C64 ROM with the one you just programmed.

# 6309 TEST FUNCTIONS

Upon reset with the 6309 PCB installed, the ROM starts continuously calculating Mandelbrot sets, rendered in Color RAM (chars in Screen RAM are not affected).  
After each set is calculated, the joystick in port 2 is checked:
- FIRE: Freeze until released.
- RIGHT: Enable raster interrupt (you'll see some yellow border lines at a specific vertical position on screen).
- DOWN: Fill Screen RAM with all zeros ('@').
- UP: Enable sprites. Each time, a new sprite is enabled (up to 8). This test currently fails and it's being investigated.
- LEFT: [no function yet]

# 6510 TEST FUNCTIONS

The 6309 Kernal is also compatible with the 6510 to perform quick tests/comparisons.
Upon reset with the 6510 installed, the ROM enables all 8 sprites and prints a text message asking to install the 6309 PCB.  
The joystick in port 2 commands the following tests:
- FIRE: [no function yet].
- RIGHT: Enable raster interrupt (you'll see some yellow border lines at a specific vertical position on screen).
- DOWN: [no function yet]
- UP: [no function yet]
- LEFT: [no function yet]

# LICENSE

Creative Commons, CC BY

https://creativecommons.org/licenses/by/4.0/deed.en

Please add a link to this github project.
