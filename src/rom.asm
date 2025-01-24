; Commodore-6309 test ROM
; https://github.com/0x444454/Commodore-6309 
;
; Revision history [authors in square brackets]:
;   2024-09-24: First simple test loop. [DDT]
;   2024-11-02: Include more advanced tests and a fixed-point Mandelbrot. [DDT]
;   2025-01-03: Use joy-2 input for more flexible tests. [DDT]
;   2025-01-04: Added IRQ test (raster), and sprite test. [DDT]
;   2025-01-16: Ported 6502 code for disk file loading from C64 Kernal. [DDT]
;   2025-01-17: Debugged disk file loading routines. [DDT]
;   2025-01-12: Added utility routines. [DDT]
;   2025-01-23: Finalized disk file loading routines and tested working on Pi1541. Press Joy-2 LEFT to LOAD "*". [DDT]
        
            PRAGMA 6309
            OPT CD
        
            ORG $E000         ; Replace the C64 8KB KERNAL ($E000-$FFFF).

init:      

; Switch from 6809 emulation mode (default) to 6309 native mode.
        
            LDMD #$01  ; This also sets the FIRQ handling mode (bit 1) to 0 (i.e. save only PC and CC). Note that the current hardware prototype only uses IRQ.
 
; Setup the stack pointers
            LDS #$7FFF ; System Stack. We use this one.
            LDU #$77FF ; User Stack.

 
; Init CIAs   --------------------

            ; We could use DP for faster/shorter I/O registers setup, but we prefer code legibility.
            ; So these lines have been commented-out.
            ;SETDP $DD
            ;LDA #$DD
            ;TFR A,DP
            ;LDA #$03
            ;STA <$00 ; Set default bank 0 for VIC-II
            
            ;LDA #$FF
            ;STA $DC02 ; CIA1 port A direction: R/W
            ;STA $DC03 ; CIA1 port B direction: R/W
            ;
            ;STA $DD02 ; CIA2 port A direction: R/W
            ;STA $DD03 ; CIA2 port B direction: R/W
    
            ;LDA #$7F
            ;STA $DC0D ; CIA1: Disable all interrupts.
            ;LDA $DC0D ; CIA1: Ack pending interrupts.
            ;STA $DD0D ; CIA2: Disable all interrupts.        
            ;LDA $DD0D ; CIA2: Ack pending interrupts.
        
            LDA $DD00
            ORA #$03
            STA $DD00 ; CIA2 port A: Set VIC-II Bank #0, $0000-$3FFF, 0-16383.

; Init VIC-II -------------------
            ;LDA #$0B  ; Display off.
            LDA #$1B  ; Display on.
            STA $D011 ; CR1
    
            LDA #210 
            STA $D012 ; Interrupt rasterline.
            
            LDA #$00
            STA $D015 ; Sprite enable
    
            LDA #$C8
            STA $D016 ; CR2
    
            LDA #$14
            STA $D018 ; Mem ptrs
    
            LDA #$00  ; Disable all VIC-II interrupts.
            STA $D01A ; Interrupts enable register (normal maskable IRQ).
            
            LDA #$FF
            STA $D019 ; Acknowledge all pending VIC-II interrupts.
    
            LDA #$02  ; Red
            STA $D020 ; Border color
    
            LDA #$01  ; White
            STA $D021 ; Background color.
; -------------------------------        
        
            ; Fill screen with '.' chars.
            ;LDA #$20      ; Space
            LDA #$A0      ; INVERSE SPACE (filled block)
            ;LDA #$01       ; "A" (filled block)
            JSR fill_screen
            
            ; Make all characters black.
            LDA #$00
            JSR fill_color
            
            ; Print "6309" in the upper left corner.
            LDQ #$36333039
            STQ $0400
        
            ; Little delay before starting, or things won't work.
            ; NOTE: Took one entire debugging day to understand this :-(
            LDX #2000           ; 2 seconds.
            JSR delay_X_millis
            
            ; Initialize system.
            JSR init_system

            ; Init complete.
            ; Finally, enable IRQ.
            ANDCC #$EF          ; Enable IRQ (but not FIRQ, which is bit 6).
            
            ;LDA #'u'-96
            ;STA $0405
            ; Reset serial bus.
            ;JSR send_cmd_UNLISTEN
            ;JSR send_cmd_UNTALK

       
            ; ******** Check Joy-2 LEFT for immediate bootstrap (useful for logic analyzer debugging).
            LDA $DC00           ; Read CIA1:PRA. This is [xxxFRLDU]. All signals active low.
            BITA #$04           ; Check for LEFT (active low).
            BNE no_immediate_boostrap
            
            JSR bootstrap
            ; Wait a bit after bootstrap, so we can debug more easily.
            LDX #2000           ; 2 seconds.
            JSR delay_X_millis
;w_foreva_0: JMP w_foreva_0
            ; end immediate boostrap.
no_immediate_boostrap:
            ; ********


            LDA #$30            ; Number of test runs.
            STA $0428

main_loop:
            LDA #$00
            JSR fill_color
            
            ; Draw a Mandelbrot set in Color RAM.
            JSR Mandelbrot
            INC $0428           ; Inc number of test runs.

            ; Check joystick input on control port 2.
read_joy:        
            LDD #$2020          ; DEBUG: Clear value in upper-right corner of screen.
            STD $424
            LDA $DC00           ; Read CIA1:PRA. This is [xxxFRLDU]. All signals active low.
            JSR print_A_hex     ; DEBUG: Print A in upper-right corner of screen.
            
            ANDA #$1F           ; Get only joy-2 actions.
            BITA #$10           ; Check if FIRE pressed.
            BEQ read_joy        ; If FIRE pressed, pause.

            CMPA #$1F           ; Check for any joy input.
            BEQ end_input       ; No joy input.
            
            ; Process joy input (active low).
            BITA #$01           ; Check if UP.
            BNE no_U            ; UP not pressed.
            ; UP
            JSR Sprites_test    ; UP action: Perform sprite test (sprites will stay enabled after test).
            BRA end_input
no_U:       BITA #$02           ; Check if DOWN.
            BNE no_D            ; DOWN not pressed.
            ; DOWN
            LDA #$00
            JSR fill_screen     ; DOWN action: Fill screen RAM to all zeros ('@' char).
            BRA end_input
no_D:       BITA #$04           ; Check if LEFT.
            BNE no_L            ; LEFT not pressed.
            ; LEFT
            JSR bootstrap       ; Bootstrap OS loading "KICK" file from disk.
            BRA end_input
no_L:       BITA #$08           ; Check if RIGHT.
            BNE no_R            ; RIGHT not pressed.
            ; RIGHT
            ORCC #$10           ; Disable IRQ.
            LDA #$01            ; Enable VIC-II raster interrupt.
            STA $D01A
            ANDCC #$EF          ; Enable IRQ (but not FIRQ, which is bit 6).
            BRA end_input
no_R:   

end_input:        
            JSR wait_no_joy     ; Wait for no joy input.
            BRA main_loop       ; ...
    
            RTS                 ; Return from the subroutine (if ever).


; ==================== wait_no_joy ====================
; Wait for no joystick input on control port 2.
wait_no_joy:
            LDA $DC00
            ANDA #$1F
            CMPA #$1F
            BNE wait_no_joy
            RTS

; ==================== fill screen RAM ====================
; Fill Screen RAM to the value contained in A.
fill_screen:
            LDX #$0400    ; Start of Screen RAM
loop_fscreen:
            STA ,X+ 
            CMPX #$07E8
            BNE loop_fscreen
            RTS

; ==================== fill color RAM ====================
; Fill Screen RAM to the value contained in A.
fill_color:
            LDX #$D800    ; Start of Screen RAM
loop_fcolor:
            STA ,X+ 
            CMPX #$DBE8
            BNE loop_fcolor
            RTS

;===========================================================
; Delay for X milliseconds.
; NOTE: Clobbers X, A, E
delay_X_millis:
            JSR delay_1ms
            LEAX -1,X           ; Decrement X
            CMPX #0             ; NOTE: Should not be needed by LEAX.
            BNE delay_X_millis
            RTS

;===========================================================
; Print A as a hex number on the upper-right corner of the screen.
; NOTE: Registers are preserved.

print_A_hex:
            PSHS A        ; Save A.
            PSHS A        ; Save A.
            ; Get high nibble.
            LSRA
            LSRA
            LSRA
            LSRA
            CMPA #$0A
            BHS pA_alpha_H
            ; Not alpha, i.e. [0..9]
            ADDA #$30+9
pA_alpha_H:    
            SUBA #9
            STA $0426

            ; Get low nibble.
            PULS A        ; Restore A.
            ANDA #$0F
            CMPA #$0A
            BHS pA_alpha_L
            ; Not alpha, i.e. [0..9]
            ADDA #$30+9
pA_alpha_L:    
            SUBA #9
            STA $0427

end_pAh:    
            PULS A        ; Restore A.
            RTS


;===========================================================
; Print D as a hex number on the upper-right corner of the screen.
; NOTE: Registers are preserved.
print_D_hex:
            JSR print_A_hex
            LDA $0426
            STA $0424
            LDA $0427
            STA $0425
            TFR B,A
            JSR print_A_hex
            RTS

;===========================================================
; Debug print bits in register A at the screen address specified in X.
; NOTE: Registers are preserved.

debug_print_bits:
            PSHS A,X
            PSHSW

            LDE #8
pr_nxt_bit: ASLA            ; Carry = bit to print.
            TFR A,F         ; Save A.
            LDA #$30        ; A = '0'
            ADCA #0         ; A = '0' + carry.
            STA ,X+
            TFR F,A         ; Restore A.
            DECE
            BNE pr_nxt_bit

            PULSW
            PULS X,A
            RTS

;===========================================================
; Debug print CIA2 stuff.
; NOTE: Registers are preserved.

debug_print_CIA2:   
            PSHS A,X
            
            ; Print CIA2:PA
            LDX #$0406
            LDA $DD00
            JSR debug_print_bits
            
            ; Print CIA2:DDRA
            LDX #$0410
            LDA $DD02
            JSR debug_print_bits
            
            PULS X,A
            RTS

        
; ==================== Mandelbrot ====================
; DDT's 6309 fast fixed-point Mandelbrot routine.
; This is a fast 16bit fixed-point version derived from my 68000 version for the Amiga (1989).
; TODO: Current fixed point format is suboptimal for Mandlebrot, and should be tuned to improve zoom-in ability.
; 
; Can't store data here because we are in ROM
; These are commented-out in case one day we can build an actual program file running in RAM.
;charx:      fcb 0
;chary:      fcb 0
;iter:       fcb 0
;max_iter:   fcb 16
;
;color_ptr:  fdb $D800
;ax:         fdb -2000
;ay:         fdb 1500
;cx:         fdb 0
;cy:         fdb 0
;incx:       fdb 100 
;incy:       fdb 100 
;zx:         fdb 0
;zy:         fdb 0
;zx2:        fdb 0
;zy2:        fdb 0


;=================== VARIABLES IN RAM ===================

;---------- Mandelbrot BEGIN
;
; Bytes
var_bytes  = $8000
charx      = var_bytes
chary      = var_bytes+1
iter       = var_bytes+2
max_iter   = var_bytes+3

; Words
var_words  = var_bytes+4

color_ptr  = var_words+0
ax         = var_words+2
ay         = var_words+4
cx         = var_words+8
cy         = var_words+10
incx       = var_words+12
incy       = var_words+14
zx         = var_words+16
zy         = var_words+18
zx2        = var_words+20
zy2        = var_words+22
;---------- Mandelbrot END

 


Mandelbrot:
        LDA #0
        STA charx
        STA chary
        LDD #$D800
        STD color_ptr
        ; Max iters
        LDA #16
        STA max_iter
        ; Default coordinates (fixed_point, *1024)
        LDD #-2000
        STD ax
        LDD #1500
        STD ay
        LDD #100
        STD incx
        STD incy
        ; Start with upper left point (ax, ay).
        LDD ax
        STD cx
        LDD ay
        STD cy

; Calculate current point (cx, cy).
calc_point:
        LDA #0   ; Reset iteration counter.
        STA iter
        LDD #0
        STD zx
        STD zy

nxt_iter:
        ; z + c
        LDD zx
        ADDD cx
        STD zx
        LDD zy
        ADDD cy
        STD zy

        ; Compute zx*zx
        LDD zx
        MULD zx      ; DW = zx*zx (*1024)
        ; Perform fixed point adjustment (divide by 1024).
        ASRD         ; DW = zx*zx (*512)
        RORW
        ASRD         ; DW = zx*zx (*256)
        RORW
        STB zx2      ; Now we "divide" by 256 and convert to 16 bit by taking only BE from ABEF.
        STE zx2+1
    
        ; Compute zy*zy
        LDD zy
        MULD zy      ; DW = zy*zy (*1024)
        ; Perform fixed point adjustment (divide by 1024).
        ASRD         ; DW = zy*zy (*512)
        RORW
        ASRD         ; DW = zy*zy (*256)
        RORW
        STB zy2      ; Now we "divide" by 256 and convert to 16 bit by taking only BE from ABEF.
        STE zy2+1
    
        LDD zx2
        TFR D,W
        ADDD zy2     ; D = zx2 + zy2
        CMPD #4000
        BGE found_color ; Early exit (not black).
    
        SUBW zy2     ; W = zx2 - zy2
        LDD zx       ; D = zx
        STW zx       ; new_zx = zx2 - zy2
        
        MULD zy      ; DW = zx * zy (*1024)
        ; Perform fixed point adjustment (divide by 512). NOTE: Only 512 because we need (2*zx*zy).
        ASRD         ; DW = zx * zy (*512)
        RORW
        STB zy       ; Now we "divide" by 256 and convert to 16 bit by taking only BE from ABEF.
        STE zy+1     ; zy = 2 * zx * zy
    
        LDA iter
        INCA
        STA iter
        CMPA max_iter
        BNE nxt_iter

found_color:
        LDA iter
        STA [color_ptr]  ; Set color
        LDD color_ptr
        INCD
        STD color_ptr

; Go to nxt point.
nxt_point:
        LDD cx
        ADDD incx
        STD cx
        LDA charx
        INCA
        STA charx
        CMPA #40
        BEQ nxt_row
        JMP calc_point

nxt_row:
        ; Increment row.
        LDD cy
        SUBD incy
        STD cy
        LDD ax
        STD cx
        LDA #0
        STA charx
        LDA chary
        INCA
        STA chary
        CMPA #25
        BEQ end_mandel
        JMP calc_point

end_mandel:
        ; End of screen
        RTS ; EXIT



; Aritmetic rotate right the 32 bit value in XY
asr_XY:
        TFR X,D   ; AB is high word of product
        ASRA
        RORB
        TFR D,X
        TFR Y,D
        RORA
        RORB
        TFR D,Y
        RTS



; ==================== Sprites test ====================
; NOTE: This does not seem to be working at the moment.
; TODO: Debug.
Sprites_test:
        ; Copy sprite to VIC-II mem ($3000)
        LDX #sprite_def
        LDY #$3000
        
        ; Copy using TFM {
        ;LDW #63
        ;TFM X+,Y+
        ; }
        
        ; Copy without TFM {
        LDB #0
cpyspr: LDA ,X+
        STA ,Y+
        INCB
        CMPB #63
        BNE cpyspr
        ; }
        
        
        ; Set sprite pointers.
        ;LDQ #$C0C0C0C0
        ;STQ $7F8
        ;STQ $7FC
        
        LDB #0
        LDA #$C0
        LDY #$07F8
setsp:  STA ,Y+
        INCB
        CMPB #8
        BNE setsp
        
        
        ; Set sprite colors (2 to 9).
        ;LDQ #$02030405
        ;STQ $D027
        ;LDQ #$06070809
        ;STQ $D02B
        
        LDA #2
        LDY #$D027
setsc:  STA ,Y+
        INCA
        CMPA #10
        BNE setsc
        
        ; Set sprite positions.
        LDA #$00
        STA $D010       ; Sprites x-coords bit 8.
        LDX #$D000
        LDB #0
set_spr_pos:
        TFR B,A
        LSLA            ; Mul by 16
        LSLA
        LSLA
        LSLA
        ADDA #70
        STA ,X+         ; Sprite X coord bits [0..7].
        STA ,X+         ; Sprite Y coord.
        INCB
        CMPB #8
        BNE set_spr_pos
        
        ; Enable next sprite.
        LDA $D015
        LSLA
        ORA #$01
        STA $D015
        
        RTS

sprite_def:
        FCB %00000000,%11111111,%00000000
        FCB %00000011,%11111111,%11000000
        FCB %00000110,%00000000,%01100000
        FCB %00001100,%00000000,%00110000
        FCB %00011000,%00000000,%00011000
        FCB %00110000,%00000000,%00001100
        FCB %01100000,%00000000,%00000110
        FCB %11001110,%11101111,%01110011
        FCB %11001000,%00101001,%01010011
        FCB %11001110,%11101001,%01110011
        FCB %11001010,%00101001,%00010011
        FCB %11001010,%00101001,%00010011
        FCB %11001110,%11101111,%01110011
        FCB %11000000,%00000000,%00000011
        FCB %01100000,%00000000,%00000110
        FCB %00110000,%00000000,%00001100
        FCB %00011000,%00000000,%00011000
        FCB %00001100,%00000000,%00110000
        FCB %00000110,%00000000,%01100000
        FCB %00000011,%11111111,%11000000
        FCB %00000001,%11111111,%00000000











; ===================================================================
; ==================== BOOSTRAP ROUTINES: BEGIN ====================
; ===================================================================
; NOTE: These are ported to 6309 from the C64 KERNAL (6502).
; Kernal comments are based on Michael Steil's work: https://github.com/androdev4u/c64rom/blob/master/c64disasm
;
; Calling convention translation table for Kernal input/output registers:
;       +-------------+
;       | 6502 | 6309 |
;       +-------------+
;       |   A  |  A   |
;       |   X  |  E   |
;       |   Y  |  F   |
;       +-------------+
;       
; The 6309's 16-bit X and Y registers are better used to emulate some 6502 "fancy" addressing modes.
; The code is not optimized for 6309. I wanted to mimick 6502 code as much as possible to simplify debugging (and this helped a lot).
;
; IMPORTANT NOTES ABOUT PORTING 6502 CODE TO 6309:
; * Cycles per instruction can be very different. Timings are critical in disk routines.
; * The 6502 transfer instructions DO affect flags. The 6309 transfer instructions DO NOT affect flags.
; * The 6502 BIT instruction affects flags in a DIFFERENT way than the 6309 BIT instruction. Be careful about the subtle N-flag.
; * The 6502 SBC and CMP instruction affects the C flag in a DIFFERENT way than the 6309 SBC and CMP instructions. Use BLO/BLS and BHI/BHS.
;
bootstrap:
            ; Clear screen and set all chars black.
            LDA #' '
            JSR fill_screen
            LDA #$00
            JSR fill_color

            LDA #filename_end-filename ; Always less than 255 chars.
            LDE #filename&$FF       ; Filename addr (lo)
            LDF #filename/256       ; Filename addr (hi)
            JSR raw_SETNAM
            
            LDA #2                  ; Logical file number [1..255].
            LDE #8                  ; Device [0..31]. Use device 8 (primary disk drive).
            LDF #0                  ; Secondary address: Command 0 = relocated load.
            JSR raw_SETLFS
            
            ;LDE #$02                ; Use file #2 for input
            ;JSR KERNAL_CHKIN        ; Set input to file

            ; Print "LO" for loading message.
            LDA #'l'-96
            STA $400
            LDA #'o'-96
            STA $401           

LOAD_ADDR = $0450

            LDE #LOAD_ADDR&$FF       ; LOAD_ADDR (lo)
            LDF #LOAD_ADDR/256       ; LOAD_ADDR (hi)
            LDA #0                   ; 0 = Load, 1 = Verify
            JSR raw_LOAD
            
            ; C = Set if error (A = error code).
            BCS error_read
            JMP found_EOF
            
found_EOF:
            ; Read completed.
            ; Make border green (success).
            LDA #5
            STA $D020
            ; Print OK in the upper-left corner.
            LDA #'o'-96
            STA $400
            LDA #'k'-96
            STA $401
            ; Output number of bytes read in the upper-right corner.
            ; raw_LOAD has returned the end load address in E [LO] and F [HI].
            EXG E,F         ; Swap to get end load address in W: E [HI] and F [LO].
            SUBW #LOAD_ADDR ; Subtract start load address.
            TFR E,A         ; Print [HI].
            JSR print_A_hex
            LDD $0426       ; Move [HI] two chars to the left.
            STD $0424
            TFR F,A         ; load end addr [LO]
            JSR print_A_hex
            RTS


            ; ERROR: Read.
error_read: 
            JSR print_A_hex
            LDA #'e'-96
            STA $400
            LDA #'r'-96
            STA $401
            BRA end_error

error_illegal_device_num:
            JSR print_A_hex
            LDA #'e'-96
            STA $400
            LDA #'n'-96
            STA $401
            BRA end_error

error_missing_file_name:
            JSR print_A_hex
            LDA #'e'-96
            STA $400
            LDA #'f'-96
            STA $401
            BRA end_error

error_device_not_present:
            JSR print_A_hex
            LDA #'e'-96
            STA $400
            LDA #'p'-96
            STA $401
            BRA end_error

error_file_not_found:
            JSR print_A_hex
            LDA #'e'-96
            STA $400
            LDA #'u'-96
            STA $401
            BRA end_error

end_error:  ; Set border color to red.
            LDA #2
            STA $D020
            RTS

filename:   ;.text 'KICK'
            FCB '*'
filename_end:                  


;===========================================================
; Clear system variables area.
; Then call stock C64 kernal init functions (needed for timers and disk loading).
init_system:
            ; Debug CIA2 status (pre init).
            LDA #'r'-96
            STA $0405
            JSR debug_print_CIA2
            ; Small delay [TODO: Check if needed]
            LDX #1000           ; 1 second.
            JSR delay_X_millis
            
            ; Initialize system.
            JSR clear_sysvar_area
            JSR k_init_chipset        ; Init chipset.
            JSR k_init_serial         ; Init CIA2 for RS232 communications.

            ; Debug CIA2 status (post init).
            LDA #'k'-96
            STA $0405
            JSR debug_print_CIA2
            LDX #1000           ; 1 second.
            JSR delay_X_millis

            RTS


;===========================================================
; Clear C64 system variables currently in use, as our custom ROM will start from scratch.
clear_sysvar_area:        
            LDA #$00
            LDX #$0002              ; Skip processor port ($00 and $01), even if we do not yet emulate it.
csva_loop:  STA ,X+
            CMPX #$0400
            BNE csva_loop
            RTS


;===========================================================
; Raw SETNAM routine, same as Kernal SETNAM call.
raw_SETNAM:
                STA $B7         ; set file name length
                STE $BB         ; set file name pointer low byte
                STF $BC         ; set file name pointer high byte
                RTS
            

;===========================================================
; Raw SETLFS routine, same as Kernal SETLFS call.
raw_SETLFS:
                STA $B8         ; save the logical file
                STE $BA         ; save the device number
                STF $B9         ; save the secondary address
                RTS  
                
;===========================================================
; Raw LOAD routine, same as Kernal LOAD call.
; DEBUG: We use X as screen memory pointer to output bytes read to screen.

raw_LOAD: ; Was $F49E
    ;LDX #$0518
                STE $C3                 ; set kernal setup pointer low byte
                STF $C4                 ; set kernal setup pointer high byte
                STA $93                 ; save load/verify flag
                LDA #$00                ; clear A
                STA $90                 ; clear the serial status byte
                LDA $BA                 ; get the device number of current file
                BNE no_keyb             ; if not the keyboard (device 0) then continue
do_error_illegal_device_num:
                JMP error_illegal_device_num ; else do 'illegal device number' and return.
no_keyb:                
                CMPA #$03               ; Check if device is monitor (screen). 
                BEQ do_error_illegal_device_num ; If monitor, do 'illegal device number' and return.
                
                BLO tape_load           ; device is < 3  
                
                LDF $B7                 ; get file name length
                BNE ok_filename         ; if not null name, skip error
                JMP error_missing_file_name ; else do 'missing file name' error and return
ok_filename:
                LDE $B9                 ; save secondary address in E
                ;JSR $F5AF              ; print "Searching..."
                LDA #$60        
                STA $B9                 ; overwrite the secondary address
                JSR send_sec_addr_and_filename ; send secondary address and filename
                LDA $BA                 ; get the device number
                JSR command_serial_bus_to_talk ; command serial bus device to TALK
                LDA $B9                 ; get the secondary address
                JSR send_sec_addr_after_TALK ; send secondary address after TALK
                JSR recv_serial_byte    ; input byte from serial bus
                STA $AE                 ; save program start address low byte
                LDA $90                 ; get the serial status byte
                LSRA                    ; shift time out read ..
                LSRA                    ; .. into carry bit
                BCS file_not_found      ; if timed out go do file not found error and return
                JSR recv_serial_byte    ; input byte from serial bus
                STA $AF                 ; save program start address high byte
                TFR E,A                 ; restore secondary address (NOTE: unlike TXA, TFR does not affect N and Z flags).
                CMPA #0                 ; Simulate 6502's TXA.
                BNE no_load_location    ; load location not set in LOAD call, so continue with the load
                ; Use location in load command instead.
                LDA $C3                 ; get the load address low byte
                STA $AE                 ; save the program start address low byte
                LDA $C4                 ; get the load address high byte
                STA $AF                 ; save the program start address high byte
no_load_location:
                ;JSR $F5D2               ; display "loading" or "verifying"
try_get_serial_byte:
                LDA #$FD                ; mask xxxx xx0x, clear time out read bit
                ANDA $90                ; mask the serial status byte
                STA $90                 ; set the serial status byte
                ;JSR scan_stop_key       ; scan stop key, return Zb = 1 = [STOP]
                ;BNE get_serial_byte     ; if not [STOP] go ??
                ;JMP $F633               ; else close the serial bus device and flag stop
get_serial_byte:
                JSR recv_serial_byte    ; input byte from serial bus
                TFR A,E                 ; copy byte
                LDA $90                 ; get the serial status byte
                LSRA                    ; shift time out read ..
                LSRA                    ; .. into carry bit
                BCS try_get_serial_byte ; if timed out go try again
                TFR E,A                 ; copy received byte back
                LDF $93                 ; get load/verify flag
                ;BEQ do_load             ; if load go load
                ; [DDT] Verify code stripped.
                ; Set W with dst ptr (we can clobber E and F here).
do_load:                
                LDB $AE                 ; Arrange for big-endian fetch.
                STB $B0                 ; $B0 can be overwritten.
                LDY $AF                 ; Y = [hi] [lo]
    ;STA ,X+        
                STA ,Y                  ; Save byte to memory.
    ;TFR Y,D
    ;JSR print_D_hex

inc_save_ptr_L:
                INC $AE                 ; increment save pointer low byte
                BNE after_inc_save_ptr_H ; if no rollover skip high byte inc
                INC $AF                 ; else increment save pointer high byte
after_inc_save_ptr_H:
; [6502] BEGIN
;                BIT $90                 ; test the serial status byte
;                BVC try_get_serial_byte ; loop if not end of file (bit 6 is clear).
;                                        ; else, close file and exit
; [6502] END
                ; NOTE: The BIT instruction in 6309 does not set V. Emulate using B.
                LDB $90                 ; test the serial status byte
                ANDB #$40               ; Check bit 6
                BEQ try_get_serial_byte ; loop if not end of file (bit 6 is clear).
                                        ; else, close file and exit
 
                JSR send_cmd_UNTALK     ; command serial bus to UNTALK
                JSR close_serial_bus_device ; close serial bus device
                BCC u_return_ok         ; if success, go flag ok and exit
file_not_found:
                JMP error_file_not_found ; do file not found error and return

tape_load:                  ; Was $F533
                JMP error_file_not_found ; [DDT] TAPE LOAD UNIMPLEMENTED.
                
u_return_ok:
                ANDCC #$FE      ; Clear carry to flag ok.
                LDE $AE         ; get the LOAD end pointer low byte
                LDF $AF         ; get the LOAD end pointer high byte
u_return:
                RTS             



;--------------------------------------------------
; send secondary address and filename
send_sec_addr_and_filename: ; Was $F3D5
                LDA $B9             ; get the secondary address
                BMI exit_F3D3       ; ok exit if -ve
                LDF $B7             ; get file name length
                BEQ exit_F3D3       ; ok exit if null name

                LDA #$00            ; clear A
                STA $90             ; clear the serial status byte
                LDA $BA             ; get the device number
                JSR command_serial_bus_to_listen ; command devices on the serial bus to LISTEN
                
                LDA $B9             ; get the secondary address
                ORA #$F0            ; OR with the OPEN command
                JSR send_sec_addr_after_LISTEN ; send secondary address after LISTEN
                LDA $90             ; get the serial status byte
                BPL device_present  ; if device present skip the 'device not present' error
                PULS A              ; else dump calling address low byte
                PULS A              ; dump calling address high byte
                JMP error_device_not_present ; do 'device not present' error and return
device_present: 
                LDA $B7             ; get file name length
                BEQ do_unlisten     ; branch if null name
                ; Send filename.
; [6502] BEGIN
;                LDY #0              ; clear index
;nxt_fn_byte:
;                LDA ($BB),Y         ; get file name byte
;                JSR send_serial_byte ; output byte to serial bus
;                INY                 ; increment index
;                CPY $B7             ; compare with file name length
;                BNE nxt_fn_byte     ; loop if not all done
; [6502] END
;
; [6309] BEGIN
                LDF #0              ; file name length
                LDB $BB             ; Load Y with address at $BB [lo] $BC [hi].
                LDA $BC
                TFR D,Y             ; Y = [hi] [lo]
nxt_fn_byte:
                LDA ,Y+             ; get file name byte
                JSR send_serial_byte ; output byte to serial bus
                INCF                ; increment index [not used, but sync 6502 Y register]
                CMPF $B7            ; compare with file name length
                BNE nxt_fn_byte     ; loop if not all done
; [6309] END                

do_unlisten:
                ; command serial bus to UNLISTEN and return
                JSR send_cmd_UNLISTEN ; command serial bus to UNLISTEN
                ANDCC #$FE            ; Clear carry to flag ok.
                RTS

exit_F3D3:
                ANDCC #$FE            ; Clear carry to flag ok.
                RTS


;===========================================================
; Close the serial bus device and flag stop
close_serial_bus_device_and_flag_stop: ; Was $F633
                JSR close_serial_bus_device ; close serial bus device
                LDA #$00        
                ORCC #$01             ; Set carry to flag stop.
                RTS             

;===========================================================
; Close the serial bus device and flag stop
close_serial_bus_device: ; Was $F642
; [6502] BEGIN
;                BIT $B9         ; test the secondary address (bit 7 of $B9).
;                BMI label_F657  ; if already closed just exit.
; [6502] END
                LDB $B9                 ; Test the serial status byte.
                BMI label_F657          ; If already closed just exit.
                
                LDA $BA         ; get the device number
                JSR command_serial_bus_to_listen ; command devices on the serial bus to LISTEN
                LDA $B9         ; get the secondary address
                ANDA #$EF       ; mask the channel number
                ORA #$E0        ; OR with the CLOSE command
                JSR send_sec_addr_after_LISTEN ; send secondary address after LISTEN
                JSR send_cmd_UNLISTEN ; command serial bus to UNLISTEN
label_F657:
                ANDCC #$FE      ; Clear carry to flag ok.
                RTS  

;===========================================================
; Scan the stop key, return Zb = 1 = [STOP]
scan_stop_key: ; Was $F6ED
                LDA #$FF         ; Not pressed.
                RTS
; Original 6502 code.
;                LDA $91         ; read the stop key column
;                CMP #$7F        ; compare with [STP] down
;                BNE ssk_end     ; if not [STP] or not just [STP] exit
;                ; just [STP] was pressed
;                PHP             ; save status
;                ;JSR $FFCC       ; close input and output channels
;                STA $C6         ; save the keyboard buffer index
;                PLP             ; restore status
;ssk_end:
;                RTS
            

;===========================================================
; Command serial bus to talk.
command_serial_bus_to_talk: ; Was $ED09
                ORA #$40
                JMP send_ctrl_char

;===========================================================
; Command serial bus to listen.
command_serial_bus_to_listen: ; Was $ED0C
                ORA #$20
                JSR check_RS232_bus_idle
                JMP send_ctrl_char

;===========================================================
; Send a control character:
send_ctrl_char: ; Was $ED11
                PSHS A          ; save device address
                
                
; [6502] BEGIN
;                BIT $94         ; test the deferred character flag
;                BPL no_deferred ; if no deferred character continue
; [6502] END
                LDB $94        ; Test the deferred character flag
                BPL no_deferred ; if no deferred character continue
                
                ORCC #$01       ; Else set carry to flag EOI.
                ROR $A3        ; rotate into EOI flag byte
                JSR tx_byte     ; Tx byte on serial bus
                LSR $94        ; clear deferred character flag
                LSR $A3        ; clear EOI flag
no_deferred:
                PULS A          ;restore the device address

                ; *** defer a command
                                
                STA $95         ; save as serial defered character
                ORCC #$10       ; disable the interrupts
                JSR set_ser_data_out_high ; set the serial data out high
                CMPA #$3F       ; compare read byte with $3F
                BNE not_3F      ; branch if not $3F, this branch will always be taken as after CIA2's PCR is read it is ANDed with $DF, so the result can never be $3F ??
                JSR set_ser_clock_out_high ; set the serial clock out high
not_3F:
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ORA #$08        ; mask xxxx 1xxx, set serial ATN low
                STA $DD00       ; Save CIA2 DRA, serial port and video address. If the code drops through to here the serial clock is low and the serial data has been released so the following code will have no effect apart from delaying the first byte by 1ms.

                ; *** Set the serial clk/data, wait and Tx byte on the serial bus.
set_ser_clkdata_wait_and_tx_byte: ; Was $ED36
                ORCC #$10       ; disable the interrupts
                JSR set_ser_clock_out_low ; set the serial clock out low
                JSR set_ser_data_out_high ; set the serial data out high
                JSR delay_1ms   ; 1ms delay

                ; *** Tx byte on serial bus
tx_byte: ; Was $ED40         
                ORCC #$10       ; disable the interrupts
            ;LDB #3
            ;STB $D020 ; cyan
            ;STA $450
                JSR set_ser_data_out_high ; set the serial data out high
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCS dev_not_present ; if the serial data is high go do 'device not present'
                JSR set_ser_clock_out_high ; set the serial clock out high

; [6502] BEGIN
;                BIT $A3         ; test the EOI flag
;                BPL no_EOI      ; if not EOI go ??
;                                ; I think this is the EOI sequence so the serial clock has been released and the serial
;                                ; data is being held low by the peripheral. first up wait for the serial data to rise
; [6502] END
                LDB  $A3        ; test the EOI flag
                BPL no_EOI      ; if not EOI go ??
                                ; I think this is the EOI sequence so the serial clock has been released and the serial
                                ; data is being held low by the peripheral. first up wait for the serial data to rise

wait_data_status_hi0:
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCC wait_data_status_hi0 ; loop if the data is low
                                ; now the data is high, EOI is signalled by waiting for at least 200us without pulling
                                ; the serial clock line low again. the listener should respond by pulling the serial
                                ; data line low
wait_data_status_lo0:
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCS wait_data_status_lo0 ; loop if the data is high
                                ; the serial data has gone low ending the EOI sequence, now just wait for the serial
                                ; data line to go high again or, if this isn't an EOI sequence, just wait for the serial
                                ; data to go high the first time
no_EOI:
wait_data_status_hi1:
        INC $d020 ; DEBUG
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCC wait_data_status_hi1 ; loop if the data is low
                                ; serial data is high now pull the clock low, preferably within 60us
                JSR set_ser_clock_out_low ; set the serial clock out low
                                ; now the C64 has to send the eight bits, LSB first. first it sets the serial data line
                                ; to reflect the bit in the byte, then it sets the serial clock to high. The serial
                                ; clock is left high for 26 cycles, 23us on a PAL Vic, before it is again pulled low
                                ; and the serial data is allowed high again
                LDA #$08        ; eight bits to do
                STA $A5         ; set serial bus bit count
check_dra_change:
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                CMPA $DD00      ; compare it with itself
                BNE check_dra_change ; if changed go try again
                ASLA            ; shift the serial data into Cb
                BCC ser_bus_timeout ; if the serial data is low go do serial bus timeout
                ROR $95         ; rotate the transmit byte
                BCS set_sdo_hi  ; if the bit = 1 go set the serial data out high
                JSR set_ser_data_out_low ; else set the serial data out low
                BNE set_sco_hi  ; continue, branch always
set_sdo_hi:
                JSR set_ser_data_out_high ; set the serial data out high
set_sco_hi:
                JSR set_ser_clock_out_high ; set the serial clock out high

                NOP             ; waste ..
                NOP             ; .. a ..
                NOP             ; .. cycle ..
                NOP             ; .. or two
                ; Do it twice, as the NOP takes only 1 cycle in 6309 native mode [it's 2 cycles for a 6502].
                NOP             ; waste ..
                NOP             ; .. a ..
                NOP             ; .. cycle ..
                NOP             ; .. or two

                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ANDA #$DF       ; mask xx0x xxxx, set the serial data out high
                ORA #$10        ; mask xxx1 xxxx, set the serial clock out low
                STA $DD00       ; save CIA2 DRA, serial port and video address
                DEC $A5         ; decrement the serial bus bit count
                BNE check_dra_change ; loop if not all done
                                ; now all eight bits have been sent it's up to the peripheral to signal the byte was
                                ; received by pulling the serial data low. this should be done within one milisecond
                LDA #$04        ; wait for up to about 1ms
                STA $DC07       ; save CIA1 timer B high byte
                LDA #$19        ; load timer B, timer B single shot, start timer B
                STA $DC0F       ; save CIA1 CRB
                LDA $DC0D       ; read CIA1 ICR
wait_ser_data_lo:
                LDA $DC0D       ; read CIA1 ICR
                ANDA #$02       ; mask 0000 00x0, timer A interrupt
                BNE ser_bus_timeout ; if timer A interrupt go do serial bus timeout
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCS wait_ser_data_lo ; if the serial data is high go wait some more
                ANDCC #$EF      ; enable the interrupts
            ;LDB #0
            ;STB $D020
                RTS             ; All done.

                ; ERROR: Device not present
dev_not_present: ; Was $EDAD          
                LDA #$80        ; error $80, device not present
;.:EDAF 2C       .BYTE $2C      ; [original Kernal] makes next line BIT $03A9
                                ; timeout on serial bus
                JMP label_EDB2  ; [readable Kernal]
                ;JMP error_device_not_present ; [DDT] Just FAIL with error.
ser_bus_timeout:
            LDB #4 ; purple
            STB $D020 
                LDA #$03        ; error $03, read timeout, write timeout
label_EDB2:
                JSR or_into_serial_status_byte ; OR into the serial status byte
                ; TODO: The following could be just 2 instructions (ANDCC and BRA).
                ANDCC #$EF      ; Enable the interrupts.
                ANDCC #$FE      ; Clear carry for branch.
                BRA label_EE03  ; ATN high, delay, clock high then data high, *** branch always ***
                ; ^^^ This is a BRA.

;===========================================================
; Send secondary address after LISTEN
send_sec_addr_after_LISTEN: ; Was $EDB9
                STA $95         ; save the deferred Tx byte
                JSR set_ser_clkdata_wait_and_tx_byte ; set the serial clk/data, wait and Tx the byte
                JMP set_serial_ATN_high

;===========================================================
; Send secondary address after TALK
send_sec_addr_after_TALK: ; Was $EDC7
                STA $95         ; save the deferred Tx byte
                JSR set_ser_clkdata_wait_and_tx_byte ; set the serial clk/data, wait and Tx the byte
                ; Wait for the serial bus end after send return address from patch 6.
                ORCC #$10       ; disable the interrupts
                JSR set_ser_data_out_low ; set the serial data out low
                JSR set_serial_ATN_high ; set serial ATN high
                JSR set_ser_clock_out_high ; set the serial clock out high
ssaa_loop:
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BMI ssaa_loop   ; loop if the clock is high
                ANDCC #$EF      ; enable the interrupts
                RTS              

;===========================================================
; Set serial ATN high
set_serial_ATN_high: ; Was $EDBE
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ANDA #$F7       ; mask xxxx 0xxx, set serial ATN high
                STA $DD00       ; save CIA2 DRA, serial port and video address
                RTS    

;===========================================================
; Send the byte in A to the serial bus
send_serial_byte: ; Was $EDDD
; [6502] BEGIN
;                BIT $94         ; test the deferred character flag
;                BMI label_EDE6  ; if there is a deferred character go send it
; [6502] END
                ; NOTE: The BIT instruction in 6309 does not set N. We use LDB.
                LDB $94           ; Test the deferred character flag
                BMI send_deferred ; If there is a deferred character go send it.
                
                ; Defer this byte.
                ORCC #$01       ; Set carry.
                ROR $94         ; shift into the deferred character flag
                BNE defer_byte  ; save the byte and exit, branch always
send_deferred:
                PSHS A          ; save the byte
                JSR tx_byte     ; Tx byte on serial bus
                PULS A          ; restore the byte
defer_byte:
                STA $95         ; save the defered Tx byte
                ANDCC #$FE      ; Clear carry to flag ok.
            ;LDB #0
            ;STB $D020

                RTS


;===========================================================
; Command serial bus to UNTALK
send_cmd_UNTALK: ; Was $EDEF
                ORCC #$10       ; disable the interrupts
                JSR set_ser_clock_out_low ; set the serial clock out low
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ORA #$08        ; mask xxxx 1xxx, set the serial ATN low
                STA $DD00       ; save CIA2 DRA, serial port and video address
                LDA #$5F        ; set the UNTALK command
                JMP call_send_ctrl_char

;===========================================================
; Command serial bus to UNLISTEN
send_cmd_UNLISTEN: ; Was $EDFE
                LDA #$3F        ; set the UNLISTEN command

call_send_ctrl_char:
                JSR send_ctrl_char ; send a control character
label_EE03:
                JSR set_serial_ATN_high ; set serial ATN high
                                
                ; Delay, clock high then data high
do_delay_ch_dh:
                ; Short delay [56 cycles for 6309 in native mode]. The original C64 Kernal delays 55 cycles.
                TFR E,A         ; [4 cycles] save the device number
                LDE #$09        ; [3 cycles] Repeat 9 times.
unlisten_delay: DECE            ; [2 cycles] decrement the count
                BNE unlisten_delay ; [3 cycles]
                TFR A,E         ; [4 cycles] restore the device number

                JSR set_ser_clock_out_high ; set the serial clock out high
                JMP set_ser_data_out_high ; set the serial data out high and return

;===========================================================
; Receive a byte from the serial port and put it in A.
recv_serial_byte: ; Was $EE13
            LDB #5 ; green
            STB $D020
                ; Input a byte from the serial bus
                ORCC #$10       ; disable the interrupts
                LDA #$00        ; set 0 bits to do, will flag EOI on timeour
                STA $A5         ; save the serial bus bit count
                JSR set_ser_clock_out_high ; set the serial clock out high
label_EE1B:
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BPL label_EE1B  ; loop if the serial clock is low
label_EE20:
                LDA #$01        ; set the timeout count high byte
                STA $DC07       ; save CIA1 timer B high byte
                LDA #$19        ; load timer B, timer B single shot, start timer B
                STA $DC0F       ; save CIA1 CRB
                JSR set_ser_data_out_high ; set the serial data out high
                LDA $DC0D       ; read CIA1 ICR
label_EE30:
                LDA $DC0D       ; read CIA1 ICR
                ANDA #$02       ; mask 0000 00x0, timer A interrupt
                BNE label_EE3E  ; if timer A interrupt go ??
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BMI label_EE30  ; loop if the serial clock is low
                BPL label_EE56  ; else go set 8 bits to do, branch always
                                ; timer A timed out
label_EE3E:
                LDA $A5         ; get the serial bus bit count
                BEQ label_EE47  ; if not already EOI then go flag EOI
                LDA #$02        ; else error $02, read timeour
                JMP label_EDB2  ; set the serial status and exit
label_EE47:                
                JSR set_ser_data_out_low ; set the serial data out low
                JSR set_ser_clock_out_high ; set the serial clock out high
                LDA #$40        ; set EOI
                JSR or_into_serial_status_byte ; OR into the serial status byte
                INC $A5         ; increment the serial bus bit count, do error on the next timeout
                BNE label_EE20  ; go try again, branch always
label_EE56:
                LDA #$08        ; set 8 bits to do
                STA $A5         ; save the serial bus bit count
label_EE5A:
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                CMPA $DD00      ; compare it with itself
                BNE label_EE5A  ; if changing go try again
                ASLA            ; shift the serial data into the carry
                BPL label_EE5A  ; loop while the serial clock is low
                ROR $A4         ; shift the data bit into the receive byte
label_EE67:
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                CMPA $DD00      ; compare it with itself
                BNE label_EE67  ; if changing go try again
                ASLA            ; shift the serial data into the carry
                BMI label_EE67  ; loop while the serial clock is high
                DEC $A5         ; decrement the serial bus bit count
                BNE label_EE5A  ; loop if not all done
                JSR set_ser_data_out_low ; set the serial data out low
                
; [6502] BEGIN
;                BIT $90         ; test the serial status byte
;                BVC label_EE80  ; if EOI not set skip the bus end sequence
; [6502] END
                ; NOTE: The BIT instruction in 6309 does not set V. Emulate using B.
                LDB $90                 ; test the serial status byte
                ANDB #$40               ; Check bit 6
                BEQ label_EE80          ; if EOI not set skip the bus end sequence             
                
                JSR do_delay_ch_dh ; Enforce a delay, clock high then data high
label_EE80:
                LDA $A4         ; get the received byte
                ANDCC #$EF      ; enable the interrupts
                ANDCC #$FE      ; Clear carry to flag ok.
            ;LDB #0
            ;STB $D020
            STA $05E1                
                RTS             


;===========================================================
; Set the serial clock out high
set_ser_clock_out_high: ; Was $EE85
            ;LDB #8 ; orange
            ;STB $D020
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ANDA #$EF       ; mask xxx0 xxxx, set serial clock out high
                STA $DD00       ; save CIA2 DRA, serial port and video address
            ;LDB #0
            ;STB $D020
                RTS  

;===========================================================
; Set the serial clock out low
set_ser_clock_out_low: ; Was $EE8E
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ORA #$10        ; mask xxx1 xxxx, set serial clock out low
                STA $DD00       ; save CIA2 DRA, serial port and video address
                RTS 

;===========================================================
; Set the serial data out high
set_ser_data_out_high: ; Was $EE97
            ;LDB #4 ; magenta
            ;STB $D020
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ANDA #$DF       ; mask xx0x xxxx, set serial data out high
                STA $DD00       ; save CIA2 DRA, serial port and video address
            ;LDB #0
            ;STB $D020                
                RTS 

;===========================================================
; Set the serial data out low
set_ser_data_out_low: ; Was $EEA0
            ;LDB #10 ; light red
            ;STB $D020
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ORA #$20        ; mask xx1x xxxx, set serial data out low
                STA $DD00       ; save CIA2 DRA, serial port and video address
            ;LDB #0
            ;STB $D020                 
                RTS  

;===========================================================
; Get the serial data status in Cb
get_ser_data_status_in_C: ; Was $EEA9
            ;LDB #7 ; yellow
            ;STB $D020
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                CMPA $DD00      ; compare it with itself
                BNE get_ser_data_status_in_C ; if changing got try again
                ASLA            ;shift the serial data into Cb
            ;LDB #0
            ;STB $D020                
                RTS   

;===========================================================
; Delay 1ms (actually 935 cycles in native mode). The original C64 Kernal delays for 931 cycles.
delay_1ms: ; Was $EEB3
                TFR E,A         ; [4 cycles] save E
                LDE #$B8        ; [3 cycles] Repeat loop 184 times.
d1ms_loop:                           
                DECE            ; [2 cycles] decrement the loop count
                BNE d1ms_loop   ; [3 cycles] loop if more to do
                TFR A,E         ; [4 cycles] restore E
                RTS               [4 cycles]

;===========================================================
; Check RS232 bus idle
check_RS232_bus_idle: ; Was $F0A4
                PSHS A           ; save A
                LDA $02A1        ; get the RS-232 interrupt enable byte
                BEQ ser_interrupts_not_enabled  ; if no interrupts enabled just exit
                ; [DDT] We should never enter here in our limited use case.
label_F0AA:
                LDA $02A1        ; get the RS-232 interrupt enable byte
                ANDA #$03        ; mask 0000 00xx, the error bits
                BNE label_F0AA   ; if there are errors loop
                LDA #$10         ; disable FLAG interrupt
                STA $DD0D        ; save CIA2 ICR
                LDA #$00         ; clear A
                STA $02A1        ; clear the RS-232 interrupt enable byte
ser_interrupts_not_enabled:
                PULS A           ; restore A
                RTS 

;===========================================================
; OR into the serial status byte.
or_into_serial_status_byte: ; Was $FE1C
                ORA $90         ; OR with the serial status byte
                STA $90         ; save the serial status byte
                RTS


;===========================================================
; increment the real time clock
inc_realtime_clock: ; Was $F69B (JMP from $FFEA).
                LDE #$00
                INC $A2         ; increment the jiffy clock low byte
                BNE label_F6A7  ; if no rollover ??
                INC $A1         ; increment the jiffy clock mid byte
                BNE label_F6A7  ; branch if no rollover
                INC $A0         ; increment the jiffy clock high byte
                                ; now subtract a days worth of jiffies from current count
                                ; and remember only the Cb result
label_F6A7:
; [6502] BEGIN
;                SEC             ; set carry for subtract
;                LDA $A2         ; get the jiffy clock low byte
;                SBC #$01        ; subtract $4F1A01 low byte
;                LDA $A1         ; get the jiffy clock mid byte
;                SBC #$1A        ; subtract $4F1A01 mid byte
;                LDA $A0         ; get the jiffy clock high byte
;                SBC #$4F        ; subtract $4F1A01 high byte
;                BCC label_F6BC  ; if less than $4F1A01 jiffies skip the clock reset
;                                ; else reset clock:
; [6502] END
; [6309] BEGIN
                LDA $A2         ; get the jiffy clock low byte
                SUBA #$01       ; subtract $4F1A01 low byte
                LDA $A1         ; get the jiffy clock mid byte
                SBCA #$1A       ; subtract $4F1A01 mid byte
                LDA $A0         ; get the jiffy clock high byte
                SBCA #$4F       ; subtract $4F1A01 high byte
                BLO label_F6BC  ; if less than $4F1A01 jiffies skip the clock reset
                                ; else reset clock:

; [6309] END
                STE $A0         ; clear the jiffy clock high byte
                STE $A1         ; clear the jiffy clock mid byte
                STE $A2         ; clear the jiffy clock low byte
                                ; this is wrong, there are $4F1A00 jiffies in a day so
                                ; the reset to zero should occur when the value reaches
                                ; $4F1A00 and not $4F1A01. this would give an extra jiffy
                                ; every day and a possible TI value of 24:00:00
label_F6BC:                                
                LDA $DC01       ; read CIA1 DRB, keyboard row port
                CMPA $DC01      ; compare it with itself
                BNE label_F6BC  ; loop if changing
                TFR A,E         ; Unlike TAX, TFR does not affect N and Z flags.
                BITA #$80       ; Check if negative, then use BNE.
                BNE label_F6DA  ; This was BMI in the original 6502 code.
                LDE #$BD        ; set c6
                STE $DC00       ; save CIA1 DRA, keyboard column drive
label_F6CC:
                LDE $DC01       ; read CIA1 DRB, keyboard row port
                CMPE $DC01      ; compare it with itself
                BNE label_F6CC  ; loop if changing
                STA $DC00       ; save CIA1 DRA, keyboard column drive
                INCE             
                BNE label_F6DC       
label_F6DA:
                STA $91         ; save the stop key column
label_F6DC:
                RTS 
     
;===========================================================
; Init SID, CIA and timer IRQ.
k_init_chipset:
                JSR init_VIC_and_screen
                JSR init_SID_CIA_and_timer_IRQ
                RTS
     
;===========================================================
; Initialise VIC and screen editor
init_VIC_and_screen: ; Was $FF5B
                ;JSR $E518       ; initialise the screen and keyboard
wait_raster_0:  LDA $D012       ; read the raster compare register
                BNE wait_raster_0 ; loop if not raster line $00
                LDA $D019       ; read the vic interrupt flag register
                ANDA #$01       ; mask the raster compare flag
                STA $02A6       ; save the PAL/NTSC flag (PAL=1, NTSC=0).
                JMP set_timings              
                
;===========================================================
; Init SID, CIA and timer IRQ.
init_SID_CIA_and_timer_IRQ: ; Was $FDA3
                LDA #$7F        ; disable all interrupts
                STA $DC0D       ; save CIA1 ICR
                STA $DD0D       ; save CIA2 ICR
                STA $DC00       ; save CIA1 DRA, keyboard column drive
                LDA #$08        ; set timer single shot
                STA $DC0E       ; save CIA1 CRA
                STA $DD0E       ; save CIA2 CRA
                STA $DC0F       ; save CIA1 CRB
                STA $DD0F       ; save CIA2 CRB
                LDE #$00        ; set all inputs
                STE $DC03       ; save CIA1 DDRB, keyboard row
                STE $DD03       ; save CIA2 DDRB, RS232 port
                STE $D418       ; clear the volume and filter select register
                DECE            ; set E = $FF
                STE $DC02       ; save CIA1 DDRA, keyboard column
                LDA #$07        ; DATA out high, CLK out high, ATN out high, RE232 Tx DATA
                                ; high, video address 15 = 1, video address 14 = 1
                STA $DD00       ; save CIA2 DRA, serial port and video address
                LDA #$3F        ; set serial DATA input, serial CLK input
                STA $DD02       ; save CIA2 DDRA, serial port and video address
                
                ; [DDT] The 6309 cannot change the memory map using the 6510 port :-)
                ;LDA #$E7        ; set 1110 0111, motor off, enable I/O, enable KERNAL, enable BASIC
                ;STA $01         ; save the 6510 I/O port
                ;LDA #$2F        ; set 0010 1111, 0 = input, 1 = output
                ;STA $00         ; save the 6510 I/O port direction register
set_timings:    ; Was $FDDD            
                LDA $02A6       ; get the PAL/NTSC flag
                ;BEQ set_NTSC_timing ; if NTSC go set NTSC timing
                BRA set_NTSC_timing ; TODO: MAKE THIS A "BEQ" AND SUPPORT ALSO PAL !!!
set_PAL_timing  ; else set PAL timing
                LDA #$25        
                STA $DC04       ; save CIA1 timer A low byte
                LDA #$40        
                JMP init_chipset_done
set_NTSC_timing:       
                LDA #$95        
                STA $DC04       ; save CIA1 timer A low byte
                LDA #$42
init_chipset_done:        
                STA $DC05       ; save CIA1 timer A high byte
                JMP enable_CIA1_timer_A_IRQ

;===========================================================
; Enable CIA1 timer-A IRQ.
enable_CIA1_timer_A_IRQ: ; Was $FF6E
                LDA #$81        ; enable timer A interrupt
                STA $DC0D       ; save VIA 1 ICR
                LDA $DC0E       ; read VIA 1 CRA
                ANDA #$80       ; mask x000 0000, TOD clock
                ORA #$11        ; mask xxx1 xxx1, load timer A, start timer A
                STA $DC0E       ; save VIA 1 CRA
                JMP set_ser_clock_out_low ; set the serial clock out low and return

;===========================================================
; Initialize serial port.
k_init_serial:
                LDA #$7F        ; disable all interrupts
                STA $DD0D       ; save CIA2 ICR
                LDA #$06        ; set serial DTR output, serial RTS output
                STA $DD03       ; save CIA2 DDRB, serial port
                STA $DD01       ; save CIA2 DRB, serial port
                LDA #$04        ; mask xxxx x1xx, set serial Tx DATA high
                ORA $DD00       ; OR it with CIA2 DRA, serial port and video address
                STA $DD00       ; save CIA2 DRA, serial port and video address
                LDA #$00        ; clear Y
                STA $02A1       ; clear the serial interrupt enable byte
                RTS

;===========================================================
; Original IRQ handler ported from C64 has been incorporated into the main 6309 IRQ handler.





; ===================================================================
; ==================== BOOSTRAP ROUTINES: END   ====================
; ===================================================================
















; ==================== Here is the 6502 RESET routine used in case this ROM is used with a 6502 ====================
        ORG $F0F8

    ; 6502 machine code
code_6502:
    INCLUDEBIN "rom_6502_reset.bin"

; ==================== 6309 NMI routine ====================
        ORG $F8F0

;        ; The 6309 handler start with three apparently nonsensical instructions, which are actually 6502 instructions used in case the ROM is used with a 6502.
;                    ; CODE  | 6502 ASM
;                    ;------------------
;        INCA        ; $4C   | JMP $F601
;        NOP         ; $01   | ^
;        LDA #$00    ; $F6   | ^
;                    ; $00   | BRK (not used)
;        
nmi_6309:
        ; TODO: NMI handler
        RTI


; ==================== Here is the 6502 NMI routine used in case this ROM is used with a 6502 ====================
        ORG $F9FA
nmi_6502:        
        FCB $40      ; RTI [6502]


; ==================== 6309 "do nothing" interrupt routine ====================
        ORG $FAF9

irq_do_nothing:
        ; In native mode, CPU atomatically pushes on SS: PC, U, Y, X, DP, F, E, B, A, CC
        ; Save extra used registers on SS.

        ; [DO NOTHING]

        ; Retreive extra used registers from SS.

        RTI


; ==================== 6309 FIRQ routine ====================
; NOTE: The hardware prototype does not support FIRQ. Use IRQ instead.
        ORG $FC00

firq:
        ; We set MD.1==0, so handling FIRQ the CPU atomatically pushes on SS: PC, CC.
        ; Save extra used registers on SS.
        
;        LDA $D019
;        ;JSR print_A_hex
;        ANDA #$01            ; Check for raster interrupt.
;        BEQ no_raster
;        
;        LDA #8
;        STA $D020            ; Change border color.
;
;        LDA #$90             ; Wait a bit for color bar to be visible.
;firq_raster_delay:
;        DECA
;        BNE firq_raster_delay
;
;        LDA #9
;        STA $D020            ; Change border color.
;
;no_raster:
;
        ; Retreive extra used registers from SS.

        RTI

        
; ==================== 6309 IRQ routine ====================
        ORG $FC00

irq:
        ; In native mode, CPU atomatically pushes on SS: PC, U, Y, X, DP, F, E, B, A, CC.
        ; Save extra used registers on SS.

; --- Original C64 system IRQ handler (required for CIA timing):
irq_system:
    INC $0404
        JSR inc_realtime_clock ; increment the real time clock
        LDA $DC0D       ; read CIA1 ICR, clear the timer interrupt flag
    ;RTI    
; --- End of original C64 system IRQ handler.                

        ; Check if this is a raster interrupt.
        LDA $D019
        ;JSR print_A_hex
        ANDA #$01            
        BEQ no_raster_irq
        
        ; Handle raster interrupt.
        LDE $D020       ; Save border color.
        ; Change border color for yellow bar.
        LDA #7
        STA $D020            
        ; Wait a bit (a few cycles) for color bar to be visible.
        LDB #$10             
irq_raster_delay:
        DECB
        BNE irq_raster_delay
        ; Restore border color.
        STE $D020

no_raster_irq:
        ; Acknowledge any VIC-II interrupt.
        LDA #$FF
        STA $D019

        
        ; Retrieve extra used registers from SS.

        RTI





; ===================================================================
; ====================        CPU VECTORS        ====================
; ===================================================================


        ORG $FFF0 ; Illegal Opcode and Division by Zero Trap (exception)
        FDB $FAF9 ; [Do nothing routine]
        
        ORG $FFF2 ; Third software interrupt (SWI3 instruction)
        FDB $FAF9 ; [Do nothing routine]
        
        ORG $FFF4 ; Second software interrupt (SWI2 instruction)
        FDB $FAF9 ; [Do nothing routine]
        
        ORG $FFF6 ; Fast interrupt (FIRQ* line)
        FDB $FB00 ; [Do nothing routine]

        ORG $FFF8 ; Interrupt (IRQ* line)
        FDB $FC00
        
        ORG $FFFA ; Software interrupt (SWI instruction)
        FDB $FAF9 ;   6309: $FAF9 SWI vector [Do nothing routine]
                  ;  *6502: $F9FA NMI vector
        
        ORG $FFFC ; Non-maskable interrupt (NMI* line).
        FDB $F8F0 ;   6309: $F8F0 NMI vector
                  ;  *6502: $F0F8 RESET vector
        
        ORG $FFFE ; Processor reset (RESET* line)
        FDB $E000 ;   6309: $E000 reset vector
                  ;  *6502: $00E0 IRQ/BRK vector
                  

; ===================================================================
; =========================== END OF ROM ============================
; ===================================================================
