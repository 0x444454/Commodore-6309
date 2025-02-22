# Commodore-6309

This experimental project shows how to transplant and run a Hitachi 6309 CPU instead of the MOS 6510 on a Commodore 64.
A 6809 should also work, but we love the faster and more advanced 6309.
Be sure to read the disclaimer below.

# DISCLAIMER

This is an experimental project and may contain flaws and errors.  
I use a disposable C64, which I can sacrifice in the name of science.  
I am perfectly aware that this is a new, unofficial mod I have just barely tested, and I know I could fry/lose my C64 in case of any issue. I just don't care.  
Don't try this on your main C64 machine.  
  
**IMPORTANT**: You can damage your C64 if improper connections are made.  
We don't even know for sure if the C64 can run a 6309 for a long time without any damage.  
Also, you will certainly lose your warranty by opening the breadbox   ;-)  

Do not proceed further unless you accept that you are trying this mod...  

## AT YOUR OWN RISK !!!

# LICENSE

Creative Commons, CC BY

https://creativecommons.org/licenses/by/4.0/deed.en

Please add a link to this github project.

# CURRENT STATUS

_"Alive! It's alive! It's alive!"_

Prototype works with preliminary test Kernal ROM, running test code like a fixed-point Mandelbrot set generator.  
IRQ is working. FIRQ is also functional, but the C64 only has one IRQ signal, so we opted to support IRQ and disabled FIRQ.  
VIC-II raster interrupts are working.  
File loading from floppy drive is working (beta). Use a real 1541 drive or a 100% compatible modern device like the Pi1541. Next step is support for OS bootstrap (e.g. NitrOS-9 loader).

There are still problems when sprites are enabled. Problem is being investigated.

# INGREDIENTS

- A Commodore 64. I have a classic breadbin with motherboard rev 250407, but this should work with other models (to be verified). Note that motherboard rev 326298 (the oldest) will __not__ work, due to physical constraints.
- The doughterboard [PCB](#pcb-and-schematics). Alternatively, you may also use a breadboard and and wire (lots of wires) according to schematics. :-)
- Hitachi 63C09E. Note the "E" at the end.
- Support Logic for clock delays and quadrature: DS1100Z-50 and DS1100Z-250 delay lines. I like using DIP8 chips in socket (for easy swap), but the PCB supports also SOP8 (surface mounted).
- Support Logic for 6510 to 6309 signals translation: GAL16V8. I use a GAL16V8D-10, but parts with different timings should work (maybe up to 25 ns ? - To be tested).
- Sockets, headers, capacitors, resistors and jumpers (see PCB project for components list). Note that all but 1 resistor in the PCB are 0 (zero) Ω, so you can simply solder jumper wires (unless you want to mod the circuit).
- Replacement 6309 Kernal ROM. I use BackBit's CornBit (2364) Flash ROM.


# PCB AND SCHEMATICS

See the hardware section: [hardware](./hardware/)

The PCB has been designed by Gary Becker (thanks !). Note that the current PCB v0.8 is __intended for debugging purposes__ and requires soldering several jumpers and probe points that won't be in the final simplified version.

![clock adjustment and quadrature](media/20250221-proto_v0.8-assembled-small.jpg)

The PCB is available as a shared project on [PCBWay](https://www.pcbway.com/project/shareproject/Commodore_6309_A_new_CPU_for_the_Commodore_64_4cdcbc60.html). Again, please note that the current version is not a final product, is intended for hardware debugging, and comes with absolutely no warranty.

The PCB is derived from the one Gary designed for the [liber809](https://github.com/boisy/liber809) project targeting Atari 8-bit computers. Kudos to Boisy Pitre for the great support !

# KERNAL ROM

The Kernal ROM for the prototype works with both 6309 and 6510 (they have different reset vectors).
At the moment, the Kernal only sets up the VIC-II and runs some test programs.
I use the _BackBit CornBit_ flash ROM to simplify development.

# PREPARATION

- Write the desired ".rom" file on a 8 KByte ROM (EPROM or Flash).
- Replace the original Kernal ROM with the 6309 ROM.
- Power on the system with the 6510. You should see the message "6510 detected. insert 6309 with adapter". This is to verify your ROM is programmed successfully. __Do NOT insert the 6309 directly in the 6510 socket__. You need the adapter circuit described in the [schematics](./hardware/).
- Check the schematics.
- Implement the translation circuit as per schematics .
- Double-check the schematics.
- Correct any errors.
- Triple-check the schematics and make sure you made no mistakes.
- Remove the 6510 and plug in its socket the 40-pin header that connects the translation circuit to the 6309.
- Power on the C64. If you don't see a screen with red border, white background, and colored characters in less than 3 seconds, then turn off your C64 immediately and troubleshoot the circuit.
- At any time, you can plug-in the 6510 again to verify you didn't fry anything.

# RESULT

At the moment, the 6309 Kernal ROM only sets up the VIC-II, CIA1, and runs a test program, like printing text, running diagnostics and a Mandelbrot generator.  
IRQ is supported (e.g. VIC-II raster interrupts).  
Better demos will come in the future.

![6309 running](media/20250220-Proto_PCB_v0.8-test.jpg)

This is the ROM execution visualized using a logic probe. Open the image in a new window to see it bigger.  
Note that the first thing the 6309 does is fetching the 16-bit reset vector at $FFFE/$FFFF.  
Then it begins executing the ROM from address $E000.  

NOTE: My old logic probe only had 16 inputs, so I had to use the upper two bits for Clock and R/W. Decoded addresses in hex at the bottom will have incorrect bit 14 and 15.

![logic probe commented](media/2024-10-05_probe_commented.jpg)

  
And this is my fixed point Mandelbrot generator. The 40x25 image is generated in about 1.5 seconds using max_iters = 16.  
Note the random missing characters pixel lines, due to timing issues that have been fixed after taking this picture.

![6309 mandel](media/20241102-6309-mandel.jpg)

# NEXT STEPS

- Prototype PCB daughterboard that plugs into the 6510 socket is almost ready (thanks to Gary Becker). This will improve timing precision and make prototyping more comfortable.
- Implement a minimalistic CPU monitor in ROM.
- Port the 586220-Diagnostics ROM to 6309, supporting harness.
- Improve the prototype and design a final version using the Expansion Port and Kernal replacement.

