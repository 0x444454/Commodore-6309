# Commodore-6309

This experimental project shows how to transplant and run a Hitachi 6309 CPU instead of the MOS 6510 on a Commodore 64.
A 6809 should also work, but we love the faster and more advanced 6309.
Be sure to read the disclaimer below.

# DISCLAIMER

This is an experimental project and may contain flaws and errors.
I use a disposable C64, which I can sacrifice in the name of science.
I am perfectly aware that this is a new, unofficial mod and I could fry/lose my C64 in case of any issue. I just don't care.
Don't try this on your main C64 machine.
  
**IMPORTANT**: You can damage your C64 if improper connections are made.  
It is not clear if the C64 can run a 6309 for a long time without damage.
You will certainly lose your warranty even by opening the box   ;-)  

Do not proceed further unless you accept that you are trying this mod...  

## AT YOUR OWN RISK !!!

# CURRENT STATUS

_"It's alive! It's alive! It's alive!"_

Prototype works with preliminary test Kernal ROM.

# INGREDIENTS

- C64. I have a classic 250407 motherboard, but this should work with all models (to be verified).
- Hitachi 6309E. Note the "E" at the end.
- Support Logic for clock quadrature: 74LS123, 4049.
- Support Logic for signal translation: 74HC00, 74LS08.
- Replacement Kernal ROM (optionally with dual-CPU support).
- Some resistors.
- Some capacitors.
- 40-pin socket to plug/solder the signal wires. You don't want to alter the onboard CPU socket, because you may want to plug the 6510 there again eventually ;-)
- Breeadboard(s) for external logic.
- Lots of wires.


# SCHEMATICS

[TODO]

Clock generator picture.
[add pic]

6309 and signal tranlation.
[add pic]

# KERNAL ROM

The Kernal ROM for the prototype works with both 6309 and 6510 (they have different reset vectors).
At the moment, the Kernal only sets up the VIC-II and prints some characters on screen.
I use the __BackBit CornBit__ flash ROM to simplify development.

# PREPARATION

- Write the "1-k6309.rom" file on a 8 KByte ROM (EPROM or Flash).
- Replace the original Kernal ROM with the 6309 ROM.
- Power on the system with the 6510. You should see the message "6510 detected. insert 6309". This is to verify your ROM is programmed successfully.
- Check the schematics.
- Implement the translation circuit as per schematics.
- Double-check the schematics.
- Correct any errors.
- Triple-check the schematics and make sure you made no mistakes.
- Remove the 6510 and plug in its socket the 40-pin socket that connects the translation circuit to the 6309.
- Power on the C64. If you don't see a screen with red border, white background, and colored characters in less than 3 seconds, then turn off your C64 immediately and troubleshoot the circuit.
- At any time, you can plug-in the 6510 again to verify you didn't fry anything.

# RESULT

At the moment, the 6309 Kernal ROM only sets up the VIC-II and prints some characters on screen.

![6309 running](media/2024-10-05_6309_running.jpg)

This is the ROM execution visualized using a logic probe.
Note that the first thing the 6309 does is fetching the 16-bit reset vector at $FFFE/$FFFF.  
Then it begins executing the ROM from address $E000.

![6309 running](media/2024-10-05_6309_probe_commented.jpg)
