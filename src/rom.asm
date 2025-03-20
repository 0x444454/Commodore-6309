; Commodore-6309 Kernal ROM
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
;   2025-01-27: Added check for autostart ROM at $8000 (i.e. cartridge). [DDT]
;   2025-02-01: Support either IRQ or FIRQ (depending on prototype pin used). [DDT]
;   2025-03-07: Added keyboard input and cursor routines. Removed joystick controls. [DDT]
;   2025-03-19: Major rehauling. Kernel is now a debugging machine code monitor. Version is now 0.1. [DDT]
        
            PRAGMA 6309
            OPT CD
        
            ORG $E000         ; Replace the C64 8KB KERNAL ($E000-$FFFF).

init:      

; Switch from 6809 emulation mode (default) to 6309 native mode.
        
            LDMD #$01       ; This also sets the FIRQ handling mode (bit 1) to 0 (i.e. save only PC and CC). Note that the current hardware prototype only uses IRQ.
 
; Setup the stack pointers
            LDS #$7FFF      ; System Stack. We use this one.
            LDU #$77FF      ; User Stack.

            ; Check if we have an autostart ROM at $8000 (i.e. cartridge port).
            JSR scan_autostart_ROM
            BNE no_autostart_ROM
            JMP ($8000)     ; Austostart ROM found. Jump at start address.
no_autostart_ROM:
            
 
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
        
            ;LDA $DD00
            ;ORA #$03
            ;STA $DD00 ; CIA2 port A: Set VIC-II Bank #0, $0000-$3FFF, 0-16383.

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
    
            LDA #$14  ; Uppercase + Graphics
            ;LDA #$16  ; Lowercase + Uppercase
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
            JSR clear_screen
            
            ; Print "6309" in the upper left corner (at least we see something if rest of init fails).
            LDQ #$36333039 ; "6309"
            STQ $0400
            LDQ #$00000000 ; Make text black.
            STQ $D800
            
            ; NOTE: Current 6309 board does not support CPU port nor other memory config methods.
            ;LDA #$36                ; We only want RAM, I/O mapped, and this KERNAL. Kiss BASIC ROMs goodbye.
            ;STA $01                 ; Set processor port.
            ;LDA #$2F                ; set 0010 1111, 0 = input, 1 = output
            ;STA $00                 ; save the 6510 I/O port direction register
        
            ; Little delay before starting, or things won't work.
            ; NOTE: Took one entire debugging day to understand this :-(
            LDX #1000           ; 1 second.
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

       
;            ; ******** Check Joy-2 LEFT for immediate bootstrap (useful for logic analyzer debugging).
;            LDA $DC00           ; Read CIA1:PRA. This is [xxxFRLDU]. All signals active low.
;            BITA #$04           ; Check for LEFT (active low).
;            BNE no_immediate_boostrap
;            
;            JSR bootstrap
;            ; Wait a bit after bootstrap, so we can debug more easily.
;            LDX #2000           ; 2 seconds.
;            JSR delay_X_millis
;
;            ; end immediate boostrap.
;no_immediate_boostrap:
;            ; ********


;            LDA #$30            ; Number of test runs.
;            STA $0428
;
;main_loop:
;            LDA #$00
;            JSR fill_color
;            
;            ; Draw a Mandelbrot set in Color RAM.
;            JSR Mandelbrot
;            INC $0428           ; Inc number of test runs.
;
;            ; Check joystick input on control port 2.
;read_joy:        
;            LDD #$2020          ; DEBUG: Clear value in upper-right corner of screen.
;            STD $424
;            LDA $DC00           ; Read CIA1:PRA. This is [xxxFRLDU]. All signals active low.
;            JSR debug_A_hex     ; DEBUG: Print A in upper-right corner of screen.
;            
;            ANDA #$1F           ; Get only joy-2 actions.
;            BITA #$10           ; Check if FIRE pressed.
;            BEQ read_joy        ; If FIRE pressed, pause.
;
;            CMPA #$1F           ; Check for any joy input.
;            BEQ end_input       ; No joy input.
;            
;            ; Process joy input (active low).
;            BITA #$01           ; Check if UP.
;            BNE no_U            ; UP not pressed.
;            ; UP
;            ; Sprite test. Clear screen with spaces first.
;            LDA #$20
;            JSR fill_screen
;            JSR Sprites_test    ; UP action: Enable next sprite (sprites will stay enabled after test).
;wait_U_rel: LDA $DC00           ; Wait until UP released.
;            BITA #$01           ; Check if UP.
;            BEQ wait_U_rel      ; UP still pressed.
;            BRA main_loop
;no_U:       BITA #$02           ; Check if DOWN.
;            BNE no_D            ; DOWN not pressed.
;            ; DOWN
;            LDA #$00
;            JSR fill_screen     ; DOWN action: Fill screen RAM to all zeros ('@' char).
;            BRA end_input
;no_D:       BITA #$04           ; Check if LEFT.
;            BNE no_L            ; LEFT not pressed.
;            ; LEFT
;            JSR bootstrap       ; Bootstrap OS loading "KICK" file from disk.
;            BRA end_input
;no_L:       BITA #$08           ; Check if RIGHT.
;            BNE no_R            ; RIGHT not pressed.
;            ; RIGHT
;            ORCC #$10           ; Disable IRQ.
;            LDA #$01            ; Enable VIC-II raster interrupt.
;            STA $D01A
;            ANDCC #$EF          ; Enable IRQ (but not FIRQ, which is bit 6).
;            BRA end_input
;no_R:   
;
;end_input:        
;            JSR wait_no_joy     ; Wait for no joy input.
;            BRA main_loop       ; ...
;    
;            RTS                 ; Return from the subroutine (if ever).

            ; Setup 5 seconds boot countdown.
            ; Print intro msg.
            LDX #str_bootdelay
            STX CUR_STR
            JSR print_str
            LDA #2              ; Red
            LDX #$D800
red_boot:   STA ,X+
            CMPX #$D828
            BNE red_boot
            LDA #0              ; Autoboot countdown active.
            STA BOOT_PHASE
            STA AUTOBOOT_CD_LO
            LDA #$05
            STA AUTOBOOT_CD_HI

            ; Enter DEBUG Monitor.
            JSR enter_monitor
            
            SWI     ; Break here, just in case for some reason we return from DEBUG Monitor.


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
; Handle errors.
; TODO: Use X instead of ERR_STR
; Use entry point related to the error.
; TODO: Use X instead of ERR_STR
;
; Inputs:
;   A: Error code [if needed].
; Clobbered: [none]
            
error_open:
                PSHS A
                LDA #str_error_fopen&$FF
                STA ERR_STR_LO
                LDA #str_error_fopen/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error
            
error_read:
                PSHS A
                LDA #str_error_fread&$FF
                STA ERR_STR_LO
                LDA #str_error_fread/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error

error_illegal_device_num:
                PSHS A
                LDA #str_error_ill_dev&$FF
                STA ERR_STR_LO
                LDA #str_error_ill_dev/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error

error_missing_file_name:
                PSHS A
                LDA #str_error_miss_fn&$FF
                STA ERR_STR_LO
                LDA #str_error_miss_fn/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error

error_device_not_present:
                PSHS A
                LDA #str_error_dev_np&$FF
                STA ERR_STR_LO
                LDA #str_error_dev_np/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error

error_file_not_found:
                PSHS A
                LDA #str_error_f_nfound&$FF
                STA ERR_STR_LO
                LDA #str_error_f_nfound/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error

error_unknown:  ; Unknown error (generic).
                PSHS A
                LDA #str_error_unknown&$FF
                STA ERR_STR_LO
                LDA #str_error_unknown/$100
                STA ERR_STR_HI
                PULS A
                JSR print_error
                JMP end_error

end_error:      ; Common error handling.
                ; Set carry to indicate error.
                ORCC #$01
                RTS

str_bootdelay:      .str 'AUTOBOOT COUNTDOWN:[ ] ANY KEY TO CANCEL'
                    .byte $0D,0

str_error:          .strz 'ERROR'
str_error_fopen:    .strz 'FILE OPEN'
str_error_fread:    .strz 'FILE READ'
str_error_ill_dev:  .strz 'ILLEGAL DEVICE NUM'
str_error_miss_fn:  .strz 'MISSING FILENAME'
str_error_dev_np:   .strz 'DEVICE NOT PRESENT'
str_error_f_nfound: .strz 'FILE NOT FOUND'
str_error_unknown:  .strz '[UNKNOWN]'

;===========================================================
; Print error message at cursor position.
; TODO: Use X instead of ERR_STR.
;
; Inputs:
;   A: Error code.
;   ERR_STR_LO: Addr of string to print [LO].
;   ERR_STR_HI: Addr of string to print [HI].
;
; Clobbered: [none]
print_error:
                PSHS A,B
                PSHS A          ; Save error code.
                LDA #':'
                JSR print_char_adv
                LDD #str_error
                STD CUR_STR
                JSR print_str
                LDA #'='
                JSR print_char_adv
                PULS A          ; Retrieve error code.
                JSR print_A_hex
                LDA #':'
                JSR print_char_adv
                LDD ERR_STR
                STD CUR_STR
                JSR print_str
                LDA #0 ; CR
                JSR handle_CR_or_Down
                PULS A,B
                RTS

;===========================================================
; Clear screen (and some more for cleaner scroll-up :-) with all black spaces.
; Clobbered: [none]
clear_screen:
                PSHS A,X
                LDA #' '                ; Space.
                JSR fill_screen
                LDA #$00                ; Black.
                JSR fill_color
                PULS A,X
                RTS

;===========================================================
; Print a zero-terminated PETSCII string at cursor position.
;
; Inputs:
;   CUR_STR_HI: Addr of string to print [HI].
;   CUR_STR_LO: Addr of string to print [LO].
;
; Clobbered: [none]
print_str:
                PSHS A,X
                PSHSW
                LDX CUR_STR
print_str_loop:
                LDA ,X+
                BEQ print_str_done
                CMPA #$0D           ; If CR, handle it.
                BNE pr_str_no_CR
                LDA #0 ; CR
                JSR handle_CR_or_Down
                BRA print_str_loop
pr_str_no_CR:   JSR petscii_to_scr  ; Convert PETSCII to screen code
                JSR print_char_adv
                BRA print_str_loop
print_str_done:
                PULSW
                PULS A,X
                RTS

;===========================================================
; Output A as a binary string at the cursor position.
; Clobbered: [none]
output_A_bin:   PSHS A,B

                LDB #8
oAb_loop:       ASLA
                PSHS A
                LDA #$18
                ROLA
                JSR print_char_adv
                PULS A
                DECB
                BNE oAb_loop
                
                PULS A,B
                RTS

;===========================================================
; Output A as a hex string at the cursor position.
; Clobbered: [none]
print_A_hex:    PSHS A,X
                JSR conv_A_hex_str
                LDX #TMP_STR
                STX CUR_STR
                JSR print_str
                PULS A,X
                RTS

;===========================================================
; Convert A to a zero-terminated PETSCII string stored in TMP_STR[0] (HI_nibble), TMP_STR[1] (LO_nibble), TMP_STR[2] (0).
; Clobbered: [none]
conv_A_hex_str:
                PSHS A          ; Save A.
                ; First, convert to char codes.
                JSR conv_A_hex_chars
                
                ; Then convert to PETSCII.
                LDA TMP_STR
                CMPA #'0'
                BHS cAhs_H      ; >=
                ADDA #$40
                STA TMP_STR
cAhs_H:         
                LDA TMP_STR+1
                CMPA #'0'
                BHS cAhs_L      ; >=
                ADDA #$40
                STA TMP_STR+1
cAhs_L:         
                LDA #0
                STA TMP_STR+2   ; Zero-terminate.
                PULS A          ; Restore A.
                RTS


;===========================================================
; Convert A to two hex char codes stored in TMP_STR[0] and TMP_STR[1].
; Clobbered: [none]
conv_A_hex_chars:
                PSHS A          ; Save A.
                PSHS A          ; Save A (2nd time).

                LSRA
                LSRA
                LSRA
                LSRA
                CMPA #$0A
                BHS pA_alpha_0  ; >=
                ; Not alpha, i.e. [0..9]
                ADDA #$30+9
pA_alpha_0:     SUBA #9
                STA TMP_STR     ; Store HI nibble.
nxt_nibble:
                PULS A          ; Restore A.
                ANDA #$0F
                CMPA #$0A
                BHS pA_alpha_1  ; >=
                ; Not alpha, i.e. [0..9]
                ADDA #$30+9
pA_alpha_1:     SUBA #9
                STA TMP_STR+1   ; Store LO nibble.
end_pAh:
                PULS A          ; Restore A.
                RTS



;===========================================================
; Print A as a hex number on the upper-right corner of the screen.
;
; Clobbered: [none]
debug_A_hex:
                PSHS A          ; Save A.
                JSR conv_A_hex_chars
                LDA TMP_STR
                STA $426
                LDA TMP_STR+1
                STA $427
                PULS A          ; Restore A.
                RTS


;===========================================================
; Print D as a hex number on the upper-right corner of the screen.
;
; Clobbered: [none]
debug_D_hex:
                PSHS D          ; Save D.
                JSR conv_A_hex_chars
                LDA TMP_STR
                STA $424
                LDA TMP_STR+1
                STA $425
                TFR B,A
                JSR conv_A_hex_chars
                LDA TMP_STR
                STA $426
                LDA TMP_STR+1
                STA $427                
                PULS D          ; Restore D.
                RTS

;===========================================================
; Debug print bits in register A at the screen address specified in X.
; NOTE: Registers are preserved.

debug_print_bits:
                PSHS A,X
                PSHSW

                LDE #8
pr_nxt_bit:     ASLA            ; Carry = bit to print.
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
;
;debug_print_CIA2:   
;            PSHS A,X
;            
;            ; Print CIA2:PA
;            LDX #$0406
;            LDA $DD00
;            JSR debug_print_bits
;            
;            ; Print CIA2:DDRA
;            LDX #$0410
;            LDA $DD02
;            JSR debug_print_bits
;            
;            PULS X,A
;            RTS

        
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
        ; Fill screen with chars (we render only in Color RAM).
        ;LDA #$20      ; Space
        LDA #$A0      ; INVERSE SPACE (filled block)
        ;LDA #$01       ; "A" (filled block)
        JSR fill_screen
        
        ; Make all characters black.
        LDA #$00
        JSR fill_color

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
test_Sprites:
        ; Copy sprite to VIC-II mem ($3000)
        LDX #sprite_def
        LDY #$3000
        
        ; Copy using TFM {
        ;LDW #63
        ;TFM X+,Y+
        ; }
        
        ; Copy without TFM {
        LDB #63
cpyspr: LDA ,X+
        STA ,Y+
        DECB
        BNE cpyspr
        ; }
        
        
        ; Set sprite pointers.
        LDQ #$C0C0C0C0
        STQ $7F8
        STQ $7FC
       
        
        ; Set sprite colors (2 to 9).
        LDQ #$02030405
        STQ $D027
        LDQ #$06070809
        STQ $D02B
        
     
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



; ==================== Handle autostart ROM ====================
; Scan for autostart ROM at $8000.
; This searches for the 5-bytes signature at $8004 and returns Z=1 if ROM found.
; Output:
;   Zero Flag: Set if found (use BEQ as condition after invocation).
;
scan_autostart_ROM:
            LDB #5              ; Five characters to test
            LDX #$8004
            LDY #signature_AS_ROM
loop_ASchk: LDA ,X+
            CMPA ,Y+
            BNE end_ASchk       ; Exit if no match.
            DECB
            BNE loop_ASchk           ; Loop if not all done.
            ; If we are here, then Z=1 (ROM found).
end_ASchk:  RTS             

; Autostart ROM signature (to check).
signature_AS_ROM: .str "DDT25" ; NOTE: Different than Commodore's autostart cartridge signature.


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


;bootstrap:
;            ; Clear screen and set all chars black.
;            LDA #' '
;            JSR fill_screen
;            LDA #$00
;            JSR fill_color
;
;            LDA #filename_end-filename ; Always less than 255 chars.
;            LDE #filename&$FF       ; Filename addr (lo)
;            LDF #filename/256       ; Filename addr (hi)
;            JSR raw_SETNAM
;            
;            LDA #2                  ; Logical file number [1..255].
;            LDE #8                  ; Device [0..31]. Use device 8 (primary disk drive).
;            LDF #0                  ; Secondary address: Command 0 = relocated load.
;            JSR raw_SETLFS
;            
;            ;LDE #$02                ; Use file #2 for input
;            ;JSR KERNAL_CHKIN        ; Set input to file
;
;            ; Print "LO" for loading message.
;            LDA #'l'-96
;            STA $400
;            LDA #'o'-96
;            STA $401           
;
;LOAD_ADDR = $0450
;
;            LDE #LOAD_ADDR&$FF       ; LOAD_ADDR (lo)
;            LDF #LOAD_ADDR/256       ; LOAD_ADDR (hi)
;            LDA #0                   ; 0 = Load, 1 = Verify
;            JSR raw_LOAD
;            
;            ; C = Set if error (A = error code).
;            BCS error_read
;            JMP found_EOF
;            
;found_EOF:
;            ; Read completed.
;            ; Make border green (success).
;            LDA #5
;            STA $D020
;            ; Print OK in the upper-left corner.
;            LDA #'o'-96
;            STA $400
;            LDA #'k'-96
;            STA $401
;            ; Output number of bytes read in the upper-right corner.
;            ; raw_LOAD has returned the end load address in E [LO] and F [HI].
;            EXG E,F         ; Swap to get end load address in W: E [HI] and F [LO].
;            SUBW #LOAD_ADDR ; Subtract start load address.
;            TFR E,A         ; Print [HI].
;            JSR debug_A_hex
;            LDD $0426       ; Move [HI] two chars to the left.
;            STD $0424
;            TFR F,A         ; load end addr [LO]
;            JSR debug_A_hex
;            RTS
;
;
;            ; ERROR: Read.
;error_read: 
;            JSR debug_A_hex
;            LDA #'e'-96
;            STA $400
;            LDA #'r'-96
;            STA $401
;            BRA end_error
;
;error_illegal_device_num:
;            JSR debug_A_hex
;            LDA #'e'-96
;            STA $400
;            LDA #'n'-96
;            STA $401
;            BRA end_error
;
;error_missing_file_name:
;            JSR debug_A_hex
;            LDA #'e'-96
;            STA $400
;            LDA #'f'-96
;            STA $401
;            BRA end_error
;
;error_device_not_present:
;            JSR debug_A_hex
;            LDA #'e'-96
;            STA $400
;            LDA #'p'-96
;            STA $401
;            BRA end_error
;
;error_file_not_found:
;            JSR debug_A_hex
;            LDA #'e'-96
;            STA $400
;            LDA #'u'-96
;            STA $401
;            BRA end_error
;
;end_error:  ; Set border color to red.
;            LDA #2
;            STA $D020
;            RTS
;
;filename:   ;.text 'KICK'
;            FCB '*'
;filename_end:                  


;===========================================================
; Clear system variables area.
; Then call stock C64 kernal init functions (needed for timers and disk loading).
init_system:
        ;    ; Debug CIA2 status (pre init).
        ;    LDA #'r'-96
        ;    STA $0405
        ;    JSR debug_print_CIA2
        ;    ; Small delay [TODO: Check if needed]
        ;    LDX #1000           ; 1 second.
        ;    JSR delay_X_millis
            
                ; Initialize system.
                JSR clear_sysvar_area
                JSR k_init_chipset        ; Init chipset.
                JSR k_init_serial         ; Init CIA2 for RS232 communications.

        ;    ; Debug CIA2 status (post init).
        ;    LDA #'k'-96
        ;    STA $0405
        ;    JSR debug_print_CIA2
        ;    LDX #1000           ; 1 second.
        ;    JSR delay_X_millis

                RTS


;===========================================================
; Clear C64 system variables currently in use, as our custom ROM will start from scratch.
clear_sysvar_area:        
                LDA #$00
                LDX #$0002              ; Skip processor port ($00 and $01), even if we do not yet emulate it.
csva_loop:      STA ,X+
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
    ;JSR debug_D_hex

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
                RTS             


;===========================================================
; Set the serial clock out high
set_ser_clock_out_high: ; Was $EE85
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ANDA #$EF       ; mask xxx0 xxxx, set serial clock out high
                STA $DD00       ; save CIA2 DRA, serial port and video address
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
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ANDA #$DF       ; mask xx0x xxxx, set serial data out high
                STA $DD00       ; save CIA2 DRA, serial port and video address
                RTS 

;===========================================================
; Set the serial data out low
set_ser_data_out_low: ; Was $EEA0
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ORA #$20        ; mask xx1x xxxx, set serial data out low
                STA $DD00       ; save CIA2 DRA, serial port and video address
                RTS  

;===========================================================
; Get the serial data status in Cb
get_ser_data_status_in_C: ; Was $EEA9
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                CMPA $DD00      ; compare it with itself
                BNE get_ser_data_status_in_C ; if changing got try again
                ASLA            ;shift the serial data into Cb
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
; Clobbered: A, B
inc_realtime_clock: ; Was $F69B (JMP from $FFEA).
                CLRB
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
                STB $A0         ; clear the jiffy clock high byte
                STB $A1         ; clear the jiffy clock mid byte
                STB $A2         ; clear the jiffy clock low byte
                                ; this is wrong, there are $4F1A00 jiffies in a day so
                                ; the reset to zero should occur when the value reaches
                                ; $4F1A00 and not $4F1A01. this would give an extra jiffy
                                ; every day and a possible TI value of 24:00:00
label_F6BC:                                
                LDA $DC01       ; read CIA1 DRB, keyboard row port
                CMPA $DC01      ; compare it with itself
                BNE label_F6BC  ; loop if changing
                ;TFR A,B         ; Unlike TAX, TFR does not affect N and Z flags.
                BITA #$80       ; Check if negative, then use BNE.
                BNE label_F6DA  ; This was BMI in the original 6502 code.
                LDB #$BD        ; set c6
                STB $DC00       ; save CIA1 DRA, keyboard column drive
label_F6CC:
                LDB $DC01       ; read CIA1 DRB, keyboard row port
                CMPB $DC01      ; compare it with itself
                BNE label_F6CC  ; loop if changing
                STA $DC00       ; save CIA1 DRA, keyboard column drive
                INCB             
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
; Initialize VIC and screen editor
init_VIC_and_screen: ; Was $FF5B in C64 Kernal
                ; Init keyboard.
                JSR init_keyb
 
                ; Detect NTSC/PAL.
                ; NOTE: Original unreliable C64 Kernal code has been removed.
                ;       Replaced with J0x variant (https://csdb.dk/forums/?roomid=10&topicid=3352).
wait_raster_1:  LDA $D012
wait_raster_2:  CMPA $D012
                BEQ wait_raster_2
                BMI wait_raster_1
                CMPA #$20
                ; Carry: 1=NTSC, 0=PAL (NOTE: CMP carry is the opposite as 6502).
                LDA #0
                ROLA                ; A = Carry.
                EORA #1             ; A = ~Carry
                STA VIDEO_STD       ; Save the PAL/NTSC flag (NTSC=0, PAL=1).
            
                ;jsr debug_A_hex
                JMP set_timings              
                
;===========================================================
; Initialize keyboard.
init_keyb:      ;Was $E518 in C64 Kernal
                ;JSR $E5A0           ; initialise the vic chip
                LDA #$00            ; clear A
                ;STA $0291           ; clear the shift mode switch
                STA CRS_PHASE       ; clear the cursor blink phase
                STA CRS_ADDR_LO
                LDA #$04
                STA CRS_ADDR_HI
                ;LDA #$48            ; get the keyboard decode logic pointer low byte
                ;STA $028F           ; save the keyboard decode logic pointer low byte
                ;LDA #$EB            ; get the keyboard decode logic pointer high byte
                ;STA $0290           ; save the keyboard decode logic pointer high byte
                LDA #$0A            ; set the maximum size of the keyboard buffer
                STA KB_BUF_MAX_LEN  ; save the maximum size of the keyboard buffer
                STA KB_START_RPT_CD ; save the repeat delay counter
                ;LDA #$0E            ; set light blue
                LDA #$00            ; Black
                STA TXT_COLOR       ; Init text color.
                LDA #$04            ; speed 4
                STA KB_NEXT_RPT_CD  ; save the repeat speed counter
                LDA #$0C            ; set the cursor flash timing
                STA CRS_PHASE_CD    ; save the cursor timing countdown
                LDA #0
                STA CRS_DISABLE     ; Enable cursor. $00 = flash cursor.
                LDA #$80            ; repeat all keys.
                STA KB_RPT_FLAGS
; [...]
                ; Home the cursor (0,0).
                LDX #0
                LDY #0
                LDA #$00
                JSR move_cursor

                RTS             
                
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
                LDA VIDEO_STD       ; get the PAL/NTSC flag
                BEQ set_NTSC_timing ; if NTSC go set NTSC timing
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
; NOTE: Original IRQ handler ported from C64 has been incorporated into the main 6309 IRQ handler.

; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; ^^^^^^^^^^^^^^^^^^^^  BOOSTRAP ROUTINES: END  ^^^^^^^^^^^^^^^^^^^^
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^








;===========================================================
;              KEYBOARD AND USER-INPUT ROUTINES
;===========================================================

; Memory map. As similar as possible to C64, so porting C64 Kernal functions is easier.
;
; *** HOWEVER *** The 6309 is big-endian, so the order of HI and LO in ptrs is swapped.

TMP_BYTE0       = $07   ; Temporary byte storage for some routines.
TMP_BYTE1       = $08   ; Temporary byte storage for some routines.

TMP_STR         = $19   ; Temporary area for progessing strings (9 bytes).

TMP_BYTE2       = $22   ; Temporary byte storage for some routines.
TMP_BYTE3       = $23   ; Temporary byte storage for some routines.
TMP_BYTE4       = $24   ; Temporary byte storage for some routines.
TMP_BYTE5       = $25   ; Temporary byte storage for some routines.

CUR_STR_HI      = $35   ; Ptr to the current string variable [HI].
CUR_STR_LO      = $36   ; Ptr to the current string variable [LO].
CUR_STR         = CUR_STR_HI

STOP_KEY        = $91   ; Stop key indicator. $7F = Stop key is pressed. $FF = Stop key is not pressed.

CMD             = $9F   ; Current monitor command (code).
PARAM0          = $A6   ; Current monitor command param.
PARAM1          = $A7   ; Current monitor command param.
PARAM2          = $A8   ; Current monitor command param.
PARAM3          = $A9   ; Current monitor command param.
PARAM4          = $AA   ; Current monitor command param.
PARAM5          = $AB   ; Current monitor command param.

CUR_ADDR_HI     = $B0   ; Current monitor address [HI].
CUR_ADDR_LO     = $B1   ; Current monitor address [HI].
CUR_ADDR        = CUR_ADDR_HI

ERR_STR_HI      = $B4   ; Ptr to lat error string [HI].
ERR_STR_LO      = $B5   ; Ptr to lat error string [HI].
ERR_STR         = ERR_STR_HI

FILENAME_LEN    = $B7   ; Length of current filename.

KB_MTX_CODE_PREV= $C5   ; Matrix code of key currently being pressed. Previous value.
KB_BUF_LEN      = $C6   ; Keyboard buffer current length.
KB_MTX_CODE     = $CB   ; Matrix code of key currently being pressed. $00-$3F:Keyboard matrix code. $40:No key is currently pressed.
CRS_DISABLE     = $CC   ; Cursor disable. $00:Cursor is on, otherwise off.
CRS_U_CHAR      = $CE   ; Screen code of character under cursor.
CRS_PHASE       = $CF   ; Cursor phase. $00=Original char. $01=Reverse char.
CRS_PHASE_CD    = $CD   ; Countdown for changing cursor phase (change phase at 0).
CRS_X           = $D3   ; Cursor column (X)
CRS_Y           = $D6   ; Cursor row (Y)

KB_BUF          = $0277 ; Keyboard buffer, 10 bytes $0277-$0280.

TXT_COLOR       = $0286 ; Text color (when overwriting).
CRS_U_COLOR     = $0287 ; Text color of char under cursor.
KB_BUF_MAX_LEN  = $0289 ; Keyboard buffer max length (set to 0 to disable buffer).
KB_RPT_FLAGS    = $028A ; Keyboard repeat flags. [7,6]: 1x = All keys; 01 = None; 00 = Only cursor dirs, Ins/Del and Space.
KB_NEXT_RPT_CD  = $028B ; Keyboard next-repeat delay countdown (repeat at 0).
KB_START_RPT_CD = $028C ; Keyboard start-repeat delay countdown (repeat at 0).
KB_MODS         = $028D ; Keyboard modifiers flags. [0]:Shift (also shift-lock). [1]:Commodore. [2]:Ctrl.
KB_MODS_PREV    = $028E ; Keyboard modifiers flags. Previous value.

VIDEO_STD       = $2A6  ; $00=NTSC; $01=PAL


; SAVED registers (6309)
SAVED_PC        = $2A7  ; WORD: $2A7 [LO] and $2A8 [HI]
SAVED_DP        = $2A9  ; BYTE
SAVED_A         = $2AA  ; BYTE
SAVED_B         = $2AB  ; BYTE
SAVED_E         = $2AC  ; BYTE
SAVED_F         = $2AD  ; BYTE
SAVED_V         = $2AE  ; WORD: $2AE [HI] and $2AF [LO]
SAVED_X         = $2B0  ; WORD: $2B0 [HI] and $2B1 [LO]
SAVED_Y         = $2B2  ; WORD: $2B2 [HI] and $2B3 [LO]
SAVED_U         = $2B4  ; WORD: $2B4 [HI] and $2B5 [LO]
SAVED_S         = $2B6  ; WORD: $2B6 [HI] and $2B7 [LO]
SAVED_CC        = $2B8  ; BYTE $2B8
SAVED_MD        = $2B9  ; BYTE $2B9

; Unused till $2FF

TMP_BUF         = $033C ; Start of buffer for temporary processing ($033C-$03FB), 192 bytes.

AUTOBOOT_CD_HI  = $03FC ; Autoboot countdown [HI]
AUTOBOOT_CD_LO  = $03FD ; Autoboot countdown [HI].
AUTOBOOT_CD = AUTOBOOT_CD_HI
BOOT_PHASE      = $03FE ; Boot phase: 0=countdown; 1=loading; 2=load_ok; 3=load_error; 4=disabled

; Custom impl (must not conflict with Kernal vars).
CRS_ADDR_HI     = $FB   ; Cursor ptr to Screen RAM $0400-$7E8 [HI byte].
CRS_ADDR_LO     = $FC   ; Cursor ptr to Screen RAM $0400-$7E8 [HI byte].
CRS_ADDR        = CRS_ADDR_HI
TMP_HI          = $FD   ; Temporary ptr [HI byte].
TMP_LO          = $FE   ; Temporary ptr [HI byte].
TMP             = TMP_HI




;===========================================================
; Scan keyboard. This is adapted from the C64 Kernal.
;
scan_keyboard:  ; Was $EA87
                LDA #$00            ; clear A
                STA KB_MODS         ; clear the keyboard shift/control/c= flag
                LDF #$40            ; set no key
                STF KB_MTX_CODE     ; save which key
                STA $DC00           ; clear CIA1 DRA, keyboard column drive
                LDE $DC01           ; read CIA1 DRB, keyboard row port
                CMPE #$FF           ; compare with all bits set
                ;BEQ check_repeat   ; if no key pressed clear current key and exit
                BNE key_pressed
                JMP check_repeat    ; if no key pressed clear current key and exit
key_pressed:
                TFR A,F             ; clear the key count
                LDX #keytab_standard ; X = decode table ptr
                LDA #$FE            ; set column 0 low
                STA $DC00           ; save CIA1 DRA, keyboard column drive
k_nxt_col:      LDE #$08            ; set the row count
                PSHS A              ; save the column
chg_retry:      LDA $DC01           ; read CIA1 DRB, keyboard row port
                CMPA $DC01          ; compare it with itself
                BNE chg_retry       ; loop if changing
k_nxt_row:      LSRA                ; shift row right to Carry
                BCS k_skp_row       ; if no key closed on this row go do next row
                PSHS A              ; save row
                LDA F,X             ; get character from decode_table[F]
                CMPA #$05           ; compare with $05, there is no $05 key but the control
                                    ; keys are all less than $05
                BHS save_k          ; if not shift/control/c=/stop go save key count
                                    ; else was shift/control/c=/stop key
                CMPA #$03           ; compare with $03, stop
                BEQ save_k          ; if stop go save key count and continue character is $01 - shift, $02 - c= or $04 - control
                ORA KB_MODS         ; OR it with the keyboard shift/control/c= flag
                STA KB_MODS         ; save the keyboard shift/control/c= flag
                BPL no_save_k       ; skip save key, branch always
save_k:         STF KB_MTX_CODE     ; save key count
no_save_k:      PULS A              ; restore row
k_skp_row:      INCF                ; increment key count
                CMPF #$41           ; compare with max+1
                BHS k_scn_done      ; exit loop if >= max+1
                                    ; else still in matrix
                DECE                ; decrement row count
                BNE k_nxt_row       ; loop if more rows to do
                ORCC #$01           ; set carry for keyboard column shift
                PULS A              ; restore the column
                ROLA                ; shift left the keyboard column
                STA $DC00           ; Set CIA1 DRA, keyboard column drive
                BNE k_nxt_col       ; loop for next column, branch always

k_scn_done:     PULS A              ; dump the saved column
                ;JMP eval_shift_ctrl_cbm_keys

; *** evaluate the SHIFT/CTRL/C= keys
eval_shift_ctrl_cbm_keys            ; Was $EB48
                LDA KB_MODS         ; get the keyboard shift/control/c= flag
                CMPA #$03           ; Check for [SHIFT] + [C=]
                BNE no_SHFT_CBM
                CMPA KB_MODS_PREV   ; Compare with last
                LBEQ scan_kb_done   ; exit if still the same
                ;BNE diff_kflag
                ;JMP scan_kb_done    ; exit if still the same
;diff_kflag:
                ;LDA $0291           ; get the shift mode switch $00 = enabled, $80 = locked
                ;BMI kb_decode      ; if locked continue keyboard decode
                ; toggle text mode
                LDA $D018           ; get the start of character memory address
                EORA #$02           ; toggle address b1
                STA $D018           ; save the start of character memory address
                BRA kb_decode       ; continue the keyboard decode
                                    ; select keyboard table
no_SHFT_CBM:    ASLA                ; << 1
                CMPA #$08           ; compare with [CTRL]
                BLO no_CTRL         ; if [CTRL] is not pressed skip the index change
                LDA #$06            ; else [CTRL] was pressed so make the index = $06
no_CTRL:        TFR A,E             ; copy the index to E
                LDX #ptrs_keytabs   ; X = ptr to decode tables
                LDX E,X             ; X = ptr to decode table[E]

kb_decode:
                LDF KB_MTX_CODE     ; get saved key count
                LDA F,X             ; get character from decode_table[F]
                TFR A,E             ; Save character to E
                CMPF KB_MTX_CODE_PREV ; compare key count with last key count
                BEQ key_held        ; If this key == current key then key is being held, go test repeat.
                LDF #$10            ; set the repeat delay count
                STF KB_START_RPT_CD ; save the repeat delay count
                BNE save_key        ; go save key to buffer and exit, branch always

key_held:       ANDA #$7F           ; clear bit 7
                LDB KB_RPT_FLAGS    ; Test key repeat flags.
                BMI check_repeat    ; If bit 7 set, then all keys repeat.
                ANDB #$40           ; Check for "repeat none".
                BNE scan_kb_done    ; If no keys repeat, all done here...
                CMPA #$7F           ; compare with end marker
                BEQ check_repeat    ; if $00/end marker go save key to buffer and exit
                CMPA #$14           ; compare with [INSERT]/[DELETE]
                BEQ check_repeat    ; if [INSERT]/[DELETE] go test for repeat
                CMPA #$20           ; compare with [SPACE]
                BEQ check_repeat    ; if [SPACE] go test for repeat
                CMPA #$1D           ; compare with [CURSOR RIGHT]
                BEQ check_repeat    ; if [CURSOR RIGHT] go test for repeat
                CMPA #$11           ; compare with [CURSOR DOWN]
                BNE scan_kb_done    ; if not [CURSOR DOWN] just exit

                ; This was one of the cursor movement keys, insert/delete key or the space bar, so always do repeat tests
check_repeat:   LDF KB_START_RPT_CD ; get the repeat delay counter
                BEQ rep_del_exp     ; If expired, go handle repeat.
                DEC KB_START_RPT_CD ;   else decrement repeat delay counter.
                BNE scan_kb_done    ; If delay not expired, nothing else to do.

rep_del_exp     ; Repeat delay counter has expired.
                DEC KB_NEXT_RPT_CD  ; decrement the repeat speed counter
                BNE scan_kb_done    ; branch if repeat speed count not expired
                LDF #$04            ; set for 4/60ths of a second
                STF KB_NEXT_RPT_CD  ; Set the repeat speed counter
                LDF KB_BUF_LEN      ; get the keyboard buffer index
                DECF                ; decrement it
                BPL scan_kb_done    ; if the buffer isn't empty just exit, else repeat the key immediately possibly save the key to the keyboard buffer.
                                    ; If there was no key pressed or the key was not found during the scan (possibly due to key bounce) then X will be $FF here.

save_key:       LDF KB_MTX_CODE     ; get the key count
                STF KB_MTX_CODE_PREV ; save it as the current key count
                LDF KB_MODS         ; get the keyboard shift/control/c= flag
                STF KB_MODS_PREV    ; save it as last keyboard shift pattern
                CMPE #$FF           ; compare the character with the table end marker or no key
                BEQ scan_kb_done    ; if it was the table end marker or no key just exit
                TFR E,A             ; Restore saved character to A.
                LDE KB_BUF_LEN      ; get the keyboard buffer index
                CMPE KB_BUF_MAX_LEN ; compare it with the keyboard buffer size
                BHS scan_kb_done    ; if the buffer is full just exit
                LDX #KB_BUF
                STA E,X             ; save the character to the keyboard buffer

                INCE                ; increment the index
                STE KB_BUF_LEN      ; save the keyboard buffer index
scan_kb_done:   LDA #$7F            ; enable column 7 for the stop key
                STA $DC00           ; save CIA1 DRA, keyboard column drive
                RTS


;*** standard keyboard table
keytab_standard:    ; Was $EB81
    .byte $14,$0D,$1D,$88,$85,$86,$87,$11
    .byte $33,$57,$41,$34,$5A,$53,$45,$01
    .byte $35,$52,$44,$36,$43,$46,$54,$58
    .byte $37,$59,$47,$38,$42,$48,$55,$56
    .byte $39,$49,$4A,$30,$4D,$4B,$4F,$4E
    .byte $2B,$50,$4C,$2D,$2E,$3A,$40,$2C
    .byte $5C,$2A,$3B,$13,$01,$3D,$5E,$2F
    .byte $31,$5F,$04,$32,$20,$02,$51,$03
    .byte $FF

;*** shifted keyboard table
keytab_shifted:    ; Was $EBC2
    .byte $94,$8D,$9D,$8C,$89,$8A,$8B,$91
    .byte $23,$D7,$C1,$24,$DA,$D3,$C5,$01
    .byte $25,$D2,$C4,$26,$C3,$C6,$D4,$D8
    .byte $27,$D9,$C7,$28,$C2,$C8,$D5,$D6
    .byte $29,$C9,$CA,$30,$CD,$CB,$CF,$CE
    .byte $DB,$D0,$CC,$DD,$3E,$5B,$BA,$3C
    .byte $A9,$C0,$5D,$93,$01,$3D,$DE,$3F
    .byte $21,$5F,$04,$22,$A0,$02,$D1,$83
    .byte $FF

;*** CBM key keyboard table
keytab_cbm:    ; Was $EC03
    .byte $94,$8D,$9D,$8C,$89,$8A,$8B,$91
    .byte $96,$B3,$B0,$97,$AD,$AE,$B1,$01
    .byte $98,$B2,$AC,$99,$BC,$BB,$A3,$BD
    .byte $9A,$B7,$A5,$9B,$BF,$B4,$B8,$BE
    .byte $29,$A2,$B5,$30,$A7,$A1,$B9,$AA
    .byte $A6,$AF,$B6,$DC,$3E,$5B,$A4,$3C
    .byte $A8,$DF,$5D,$93,$01,$3D,$DE,$3F
    .byte $81,$5F,$04,$95,$A0,$02,$AB,$83
    .byte $FF

;*** control keyboard table
keytab_ctrl:    ; Was $EC78
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
    .byte $1C,$17,$01,$9F,$1A,$13,$05,$FF
    .byte $9C,$12,$04,$1E,$03,$06,$14,$18
    .byte $1F,$19,$07,$9E,$02,$08,$15,$16
    .byte $12,$09,$0A,$92,$0D,$0B,$0F,$0E
    .byte $FF,$10,$0C,$FF,$FF,$1B,$00,$FF
    .byte $1C,$FF,$1D,$FF,$FF,$1F,$1E,$FF
    .byte $90,$06,$FF,$05,$FF,$FF,$11,$FF
    .byte $FF

;*** table pointers
ptrs_keytabs:    ; Was $EB79
    .word keytab_standard           ; Table: standard
    .word keytab_shifted            ; Table: shift
    .word keytab_cbm                ; Table: commodore
    .word keytab_ctrl               ; Table: control


;===========================================================
; Input from the keyboard buffer
; Output:
;   Carry: 0 if present, 1 if buffer empty (never happens).
;   A: PETSCII value of char.
input_from_kb_buf:                  ; Was $E5B4
                PSHS X
                PSHSW
                LDX #KB_BUF         ; X = Ptr to keyboard input buffer.
                LDF ,X              ; get the current character from the buffer
                LDE #0              ; Counter.
update_kb_fifo: LDA 1,X             ; get the next character,X from the buffer
                STA ,X+             ; save it as the current character. Inc buf ptr.
                INCE                ; increment the index
                CMPE KB_BUF_LEN     ; compare it with the keyboard buffer index
                BNE update_kb_fifo  ; loop if more to do
                DEC KB_BUF_LEN      ; decrement keyboard buffer index
                TFR F,A             ; copy the input char to A
                ANDCC #$EF          ; enable the interrupts
                ANDCC #$FE          ; flag got byte (clear carry).
                PULSW
                PULS X
                RTS


;===========================================================
; Wait for a key from the keyboard
; Inputs:
;   [none]
; Outputs:
;   A: PETSCII code, or $FF on error.
; Clobbered: A,X,Y
;
wait_key:                           ; Was $E5CD
                LDA KB_BUF_LEN      ; get the keyboard buffer index
                BEQ wait_key        ; loop if the buffer is empty

                ORCC #$10           ; Disable IRQ.

                ; Check if boot countdown (phase 0).
                LDA BOOT_PHASE
                BNE no_boot_cd
                ; Cancel boot countdown.
                LDA #4              ; Boot canceled.
                STA BOOT_PHASE
                JSR update_boot_msg
no_boot_cd:
                ; Disable cursor while processing key.
                JSR cursor_disable

                JSR input_from_kb_buf ; input from the keyboard buffer
        
        ;JSR debug_A_hex ; DEBUG: Show key code on upper-right corner.

                ; We got our key. Check for special keys first.

                CMPA #$83          ; compare with [SHIFT][RUN].
                BNE no_SHFT_RUN    ; if not [SHIFT][RUN] skip the buffer fill
                LDA #1             ; Loading
                STA BOOT_PHASE
                JSR inject_boot_cmd
                BRA wait_key


no_SHFT_RUN:
                ; Check CR
                CMPA #$0D
                BNE no_CR

                JSR parse_line      ; Parse command in current line.
                LDA #0 ; CR
                JSR handle_CR_or_Down ; [CR] pressed
                LBRA wait_key_done
no_CR:

                ; Check CLS
                CMPA #$93
                BNE no_CLS
                JSR clear_screen
                CLRA                ; move_cursor flags: Restore char under cursor.
                CLRE
                CLRF
                JSR move_cursor
                LBRA wait_key_done
no_CLS:

                ; Check Home
                CMPA #$13
                BNE no_Home
                LDA #$01            ; move_cursor flags: Restore char under cursor.
                CLRE
                CLRF
                JSR move_cursor
                LBRA wait_key_done
no_Home:

                ; Check Down
                CMPA #$11
                BNE no_Down
                LDA #1              ; Handle Down.
                JSR handle_CR_or_Down ; [Down] pressed.
                LBRA wait_key_done
no_Down:

                ; Check Up
                CMPA #$91
                BNE no_Up
                LDE CRS_X
                LDF CRS_Y
                BNE up_nofr
                LBRA wait_key_done
up_nofr:        DECF
                LDA #$01            ; move_cursor flags: Restore char under cursor.
                JSR move_cursor
                LBRA wait_key_done
no_Up:

                ; Check Left or Backspace.
                CMPA #$9D
                BEQ is_Left
                CMPA #$14
                BNE no_Left_or_BS
is_Left:        LDF CRS_Y
                LDE CRS_X
                BEQ crsL_wrap       ; If X==0, try wrapping to the right of the upper row.
                DECE
                BRA crsL_done
crsL_wrap:      CMPF #0             ; See if we have an upper row.
                BNE crsL_cont
                LBRA wait_key_done
crsL_cont:      DECF                ; Jump to the upper row.
                LDE #39             ; Rightmost pos.
crsL_done:      PSHS A
                LDA #$01            ; move_cursor flags: Restore char under cursor.
                JSR move_cursor
                PULS A
                CMPA #$14
                BNE no_BS
                ; BS: Move line from cursor to EOL one char to the left.
                JSR find_cur_line_start_addr
                LDF CRS_X
                INCF
                LDX TMP
mov_line_left:  LDA F,X
                DECF
                STA F,X
                INCF
                INCF
                CMPF #40
                BLO mov_line_left   ; <
                LDF #39
                LDA #' '
                STA F,X
                JSR crs_save_under
no_BS:          BRA wait_key_done
no_Left_or_BS:

                ; Check Insert
                CMPA #$94
                BNE no_Ins
                ; Ins: Move line from cursor to EOL one char to the right.
                JSR find_cur_line_start_addr
                LDF #38
                LDX TMP
mov_line_right: LDA F,X
                INCF
                STA F,X
                DECF
                DECF
                CMPF CRS_X
                BHS mov_line_right  ; >=
                LDA #' '
                LDF #0
                LDX CRS_ADDR
                STA F,X
                JSR crs_save_under
                BRA wait_key_done
no_Ins:

                ; Check Right
                CMPA #$1D
                BNE no_Right
                LDF CRS_Y
                LDE CRS_X
                CMPE #39
                BEQ crsR_wrap       ; If X==39, try wrapping to the left of the lower row.
                INCE
                BRA crsR_done
crsR_wrap:      CMPF #24            ; See if we have a lower row.
                BEQ wait_key_done
                INCF                ; Jump to the lower row.
                LDE #0              ; Leftmost pos.
crsR_done:      LDA #$01            ; move_cursor flags: Restore char under cursor.
                JSR move_cursor
                BRA wait_key_done
no_Right:

                ; If not a special character, print it.

                JSR petscii_to_scr  ; Convert PETSCII to screen code
                JSR print_char


                ; Check if we can advance cursor.
                LDE CRS_X
                CMPE #39
                BEQ wait_key_done   ; At EOL, can't advance cursor.
                ; Advance cursor.
                ; Inc X
                INCE
                LDF CRS_Y
                LDA #$00            ; Don't restore char under cursor.
                JSR move_cursor

wait_key_done:
                ; Enable cursor.
                JSR cursor_enable

                ANDCC #$EF          ; Enable IRQ
                RTS

str_booting_msg:    .str 'BOOTING...'
                    .byte $0D,0
str_autoload_cmd:   .str 'L"*"8000'
                    .byte $0D,0


;===========================================================
; Disable cursor.
; Clobbered: [none]
cursor_disable:
                PSHS A
                LDA #1
                STA CRS_DISABLE
                JSR crs_restore_under
                PULS A
                RTS

;===========================================================
; Enable cursor.
; Clobbered: [none]
cursor_enable:
                CLR CRS_DISABLE
                RTS


;===========================================================
; Update autboot message.
; Clobbered: [none]
update_boot_msg:
                PSHS A,X
                PSHSW

                LDA #' '
                LDX #$0400
erase_boot_msg: STA ,X+
                CMPX #$0428
                BNE erase_boot_msg

                ; Check if booting.
                LDA BOOT_PHASE
                CMPA #1
                BNE cbcd_done
                ; Booting.
                JSR clear_screen
                CLRA                ; move_cursor flags: Restore char under cursor.
                CLRE
                CLRF
                JSR move_cursor
                ; Print booting message.
                LDX #str_booting_msg
                STX CUR_STR
                JSR print_str

cbcd_done:
                PULSW
                PULS A,X
                RTS

;===========================================================
; Inject a boot cmd in the keyboard buffer.
; Clobbered: [none]
inject_boot_cmd:
                PSHS A,B,X,Y
                ; Put 'L"*"8000\n' into the keyboard buffer.
                LDX #str_autoload_cmd
                LDY #KB_BUF
                CLRB                ; B = new keyboard buffer length.
label_E5F3:     LDA ,X+             ; Get byte from the auto load/run table.
                BEQ e_autoload
                STA ,Y+             ; Save it to the keyboard buffer.
                INCB                ; Inc len.
                BRA label_E5F3
e_autoload:     STB KB_BUF_LEN      ; Set new keyboard buffer length.
                PULS A,B,X,Y
                RTS


;===========================================================
; CR or Down pressed.
; Inputs: A: 0 for CR, 1 for Down.
; Outputs: [none]
; Clobbered: [none]
;
handle_CR_or_Down:
                PSHS A
                PSHSW

                CLRE
                CMPA #0
                BEQ is_CR
                LDE CRS_X
is_CR:          LDF CRS_Y
                ; If we are at the bottom line, then scroll up.
                CMPF #24
                BLO no_scroll_up    ; <
                ; Scroll up and CR.
                ; Restore char under cursor, and assume the new line is all SPACEs.
                JSR crs_restore_under
                LDA #' '            ; SPACE
                STA CRS_U_CHAR
                JSR scroll_up
                JMP done_CR
no_scroll_up:   ; Don't scroll. Just cursor down one line.
                INCF
done_CR:
                LDA #$01  ; move_cursor flags: Restore char under cursor.
                JSR move_cursor

                PULSW
                PULS A
                RTS


;===========================================================
; Handle the cursor (this is invoked by IRQ).
; Inputs: [none]
; Outputs: [none]
; Clobbered: [none]
;
handle_cursor:
                PSHS A,X
                PSHSW
                
                LDA CRS_DISABLE     ; get the cursor enable, $00 = flash cursor
                BNE done_h_cursor   ; if flash not enabled skip the flash...

                LDF #0

                LDA CRS_PHASE_CD    ; If this is zero, we just moved the cursor.
                BEQ no_char_change

                DEC CRS_PHASE_CD    ; decrement the cursor timing countdown
                BNE done_h_cursor   ; if not counted out skip the flash

                ; Phase change.

no_char_change:
                ; Change phase.
                LDA #20             ; Reset the flash countdown.
                STA CRS_PHASE_CD
                LDA CRS_PHASE
                EORA #$01
                STA CRS_PHASE
                LDA #$80            ; Reverse char flag.
                LDX CRS_ADDR
                EORA ,X
                STA ,X
done_h_cursor:
                PULSW
                PULS A,X
                RTS


;===========================================================
; Move the cursor at the specified position.
; Inputs:
;   E: X coord.
;   F: Y coord.
;   A: Flags. [0]=Set to restore char previously under cursor.
; Clobbered: A
move_cursor:
                PSHSW

                ; If phase 1, de-reverse char.
                ANDA #$01            ; Check flag: Restore char.
                BEQ set_crs_pos

do_restore_under:
                ; Restore char under cursor.
                JSR crs_restore_under

set_crs_pos:
                STE CRS_X           ; Set the cursor column.
                STF CRS_Y           ; Set the cursor row.
                ; Find abs addr: $0400 + X + Y*40  ==  $0400 + X + Y*32 + Y*8.
                STE CRS_ADDR_LO     ; offset = X
                ; Multiply Y 40: (X + Y*40) == (X + Y*32 + Y*8).
                ; TODO: Use 6309 MUL instruction.
                TFR F,A             ; A = F
                ASLA                ; A = F*2
                ASLA                ; A = F*4
                ASLA                ; A = F*8 (fits in 8 bits).
                TFR A,F             ; Save F*8 in F-reg.
                ADDA CRS_ADDR_LO    ; A = E + F*8 (fits in 8 bits).
                STA CRS_ADDR_LO
                CLRA                ; A = 0
                STA CRS_ADDR_HI
                TFR F,A             ; A = F*8
                ASLA                ; A = Y*16 (low 8 bits). Carry = bit 9.
                ROL CRS_ADDR_HI     ; CRS_ADDR_HI = Y*16 (bit 9).
                ASLA                ; A = Y*32 (low 8 bits). Carry = bit 9. CRS_ADDR_HI[0] = bit 10.
                ROL CRS_ADDR_HI     ; CRS_ADDR_HI[1,0] = Y*32 (bit 10 and 9).
                ; A still has the low 8 bits of Y*32.
                ; Add TMP[HI_LO] to cursor offset.
                ADDA CRS_ADDR_LO
                STA CRS_ADDR_LO
                LDA CRS_ADDR_HI
                ADCA #$04           ; Offset to Screen RAM ($0400).
                STA CRS_ADDR_HI     ; offset = $0400 + (X + Y*40)
                ; Save char under cursor.
                JSR crs_save_under
                ; Prepare transition to phase 1, so cursor is visible.
                CLRA                ; Will become 1 after transition.
                STA CRS_PHASE
                ;CLRA                ; Reset phase at next handle_cursor invocation.
                STA CRS_PHASE_CD
                
                PULSW
                RTS

;===========================================================
; Save char code and color of char under cursor.
; Clobbered: [none]
crs_save_under:
                PSHS A,X
                
                LDX CRS_ADDR
                LDA ,X                  ; Get the char under the cursor.
                STA CRS_U_CHAR          ; Save it.
                ; Switch to Color RAM.
                LDA CRS_ADDR_HI
                EORA #$DC
                STA CRS_ADDR_HI
                
                LDX CRS_ADDR
                LDA ,X                  ; Get the color under the cursor.
                STA CRS_U_COLOR         ; Save it.
                ; Switch back to Screen RAM.
                LDA CRS_ADDR_HI
                EORA #$DC
                STA CRS_ADDR_HI
                
                PULS A,X
                RTS

;===========================================================
; Restore char code and color of char under cursor.
; Clobbered: A
crs_restore_under:
                PSHS A,X

                LDX CRS_ADDR                
                LDA CRS_U_CHAR          ; Get char under the cursor.
                STA ,X                  ; Restore it.
                ; Switch to Color RAM.
                LDA CRS_ADDR_HI
                EORA #$DC
                STA CRS_ADDR_HI
                
                LDX CRS_ADDR    
                LDA CRS_U_COLOR         ; Get the color under the cursor.
                STA ,X                  ; Restore it.
                ; Switch back to Screen RAM.
                LDA CRS_ADDR_HI
                EORA #$DC
                STA CRS_ADDR_HI
                
                PULS A,X
                RTS


;===========================================================
; Print char (screen code) at cursor position.
; NOTE: Cursor is not automatically advanced. Use print_char_adv to also advance cursor.
; Inputs:
;   A: Screen code to output on screen at cursor position.
; Outputs: [none]
; Clobbered: [none]
;
print_char:
                PSHS A,X

                LDX CRS_ADDR
                STA ,X              ; Write char on screen.
                LDA CRS_ADDR_HI
                EORA #$DC           ; Switch to Color matrix.
                STA CRS_ADDR_HI

                LDX CRS_ADDR
                LDA TXT_COLOR       ; Black.
                STA ,X              ; Set color.
                LDA CRS_ADDR_HI
                EORA #$DC           ; Switch to Screen matrix.
                STA CRS_ADDR_HI     ; Back to Screen matrix.

                PULS A,X
                RTS


;===========================================================
; Print char (screen code) at cursor position and advance cursor.
; Inputs:
;   A: Screen code to output on screen at cursor position.
; Outputs: [none]
; Clobbered: [none]
;
print_char_adv:
                ; Output the char.
                JSR print_char

                ; Advance cursor.
                PSHS A
                PSHSW

                LDF CRS_Y
                LDE CRS_X
                CMPE #39
                BEQ pr_crsR_wrap    ; If X==39, try wrapping to the left of the lower row.
                INCE
                BRA pr_crsR_done
pr_crsR_wrap:   CMPF #24            ; If at the bottom of screen, scroll up.
                BNE pr_no_scroll
                JSR scroll_up
                BRA pr_new_line
pr_no_scroll:
                INCF                ; Jump to the lower row.
pr_new_line     CLRE                ; Leftmost pos.
pr_crsR_done:   CLRA                ; move_cursor flags: Do not restore char under cursor.
                JSR move_cursor

                PULSW
                PULS A
                RTS

;===========================================================
; Print PETSCII char at cursor position and advance cursor.
; Inputs:
;   A: PETSCII code to output on screen at cursor position.
; Outputs: [none]
; Clobbered: [none]
;
print_petscii_adv:
                JSR petscii_to_scr  ; Convert PETSCII to screen code.
                JSR print_char_adv  ; Print the screen code.
                RTS

;===========================================================
; Convert a PETSCII character into a nibble.
; Inputs:
;   A: The PETSCII character ['0'..'9'] or ['a'..'f'].
; Outputs:
;   A: The nibble [$00..$0F], or $FF if invalid PETSCII char.
; Clobbered: [none]
petscii_to_nibble:
                CMPA #'0'
                BLO ptn_invalid     ; <
                CMPA #'F'+1
                BHS ptn_invalid     ; >=
                CMPA #'A'
                BLO ptn_num         ; <
                SUBA #'A'-10
                RTS
ptn_num:        CMPA #'9'+1
                BHS ptn_invalid     ; >=
                SUBA #'0'
                RTS
ptn_invalid:    ; Invalid char.
                LDA #$FF
                RTS


;===========================================================
; Convert PETSCII to screen code.
; Inputs:
;   A: PETSCII code to convert.
; Outputs:
;   A: Screen code, or $FF if conversion failed.
; Clobbered: [none]
;
petscii_to_scr:
                CMPA #$20           ; compare with [SPACE]
                BLO unprintable     ; branch if < [SPACE], not a printable character
                CMPA #$60
                BLO pet_lt_60       ; branch if $20 to $5F
                ; Char is >= $60.
                ANDA #$DF
                RTS
pet_lt_60:      ; Char is < $60
                ANDA #$3F
                RTS
unprintable:    LDA #$FF
                RTS


;===========================================================
; Convert screen code to PETSCII.
; Inputs:
;   A: Screen code do convert.
; Outputs:
;   A: PETSCII code, or $FF if conversion failed.
; Clobbered: [none]
;
scr_to_petscii:
                CMPA #$20
                BHS from_20     ; >=
                ADDA #64        ; $00-$1F:  +64
                RTS
from_20:        CMPA #$40
                BHS from_40     ; >=
                ; Identical.    ; $20-$3F: Pass through.
                RTS
from_40:        CMPA #$60
                BHS from_60     ; >=
                ADDA #128       ; $40-$5F: +128
                RTS
from_60:        CMPA #$80
                BHS from_80     ; >=
                ADDA #64        ; $60-$7F:  +64
                RTS
from_80:        CMPA #$C0
                BHS from_C0     ; >=
                SUBA #128       ; $80-$BF: -128
                RTS
from_C0:        SUBA #64        ; $C0-$FF:  -64
                RTS


;===========================================================
; Find the current cursor line start address, and put it in TMP_HI, and TMP_LO (Big-Endian).
; Clobbered: A
find_cur_line_start_addr:
                LDA CRS_ADDR_LO
                SUBA CRS_X
                STA TMP_LO
                LDA CRS_ADDR_HI
                SBCA #0
                STA TMP_HI
                RTS

;===========================================================
; Simplified screen scroll up routine (screen matrix and attrs).
; Clobbered: [none]
scroll_up:
                PSHS A,X,Y

                LDX #$0400
                LDY #$D800
scr_up_loop:    ; Screen
                LDA 40,X
                STA ,X+
                ; Colors
                LDA 40,Y
                STA ,Y+
                CMPX #$07C0
                BNE scr_up_loop

                ; We reached the last line. Now clear it.
clr_last_line:  LDA #' '        ; Space
                STA ,X+
                LDA #0          ; Black
                STA ,Y+
                CMPX #$07E8
                BNE clr_last_line

                PULS A,X,Y
                RTS



;===========================================================
;                COMMAND LINE INTERFACE
;===========================================================
; Commands:
;   . <addr> <opcode> [operands]                          : Assemble a line of Assembly code
;   A <addr> <opcode> [operands]                          : Assemble a line of Assembly code
;   C <addr_start> <addr_end> <addr_start_other>          : Compare mem ranges.
;   D [addr]                                              : Disassemble
;   F <addr_start> <addr_end> <byte>                      : Fill mem range.
;   G [addr]                                              : JMP to address.
;   H <addr_start> <addr_end> <byte>[,<byte>] ...         : Hunt (search) mem range.
;   I                                                     : System info.
;   J [addr]                                              : JSR to address.
;   L "<filename>"[,<device>,] [addr_start]               : Load file.
;   M [<addr_start> [<addr_end>]]                         : Display mem range (hex).
;   N [<addr_start> [<addr_end>]]                         : Display mem range (bin).
;   R                                                     : Show registers.
;   R <reg> <value>                                       : Edit register.
;   S "<filename>"[,<device>,] <addr_start> <addr_end>    : Save file.
;   T <addr_start> <addr_end> <addr_dest>                 : Transfer (copy) mem range.
;   TEST <num>                                            : Launch a pre-defined test program.
;   X                                                     : Exit.
;   > <addr> <byte>[,<byte>] ...                          : Modify mem.
;   ; [PC [DP [A [B [E [F [X [Y [V]]]]]]]]]               : Modify registers.
;   , [U [S [CC [MD]]]]                                   : Modify registers.
;   :                                                     ; EOL (Ignore all characters after this on this line).
;   !                                                     ; Help command. When not a command: EOL (ignore all characters after this on this line).


;-----------------------------------------------------------

; Entry point from interrupt.
enter_monitor_from_interrupt:
                ; Stacking order (low mem to high mem):
                ; CC, A, B, E, F, DP, X[H], X[L], Y[H], Y[L], U/S[H], U/S[L], PC[H], PC[L], ...
                ; ^^ Stack ptr after stacking.                                              ^^ Stack ptr before stacking.
            INC $D020 ; Visual cue.
                STA SAVED_A
                STX SAVED_X
                LDA ,S              ; Get stacked CC.
                STA SAVED_CC
                LDX 12,S            ; Get stacked PC.
                STX SAVED_PC
                BRA em_after_A_X_CC_PC

; Entry point using JSR. We only have PC on the stack.
enter_monitor:
                ; Stacking order (low mem to high mem):
                ; PC[H], PC[L], ...
                ; ^^ Stack ptr after stacking.                                              ^^ Stack ptr before stacking.
                STA SAVED_A
                STX SAVED_X
                TFR CC,A            ; Get CC.
                STA SAVED_CC
                LDX 0,S             ; Get stacked PC.
                STX SAVED_PC                

                ; Save registers.
em_after_A_X_CC_PC: 
                STB SAVED_B
                STE SAVED_E
                STF SAVED_F
                STY SAVED_Y
                
                STS SAVED_S
                STU SAVED_U
                
                TFR V,X
                STX SAVED_V

                TFR DP,A
                STA SAVED_DP

                ; Find value of MD.
                LDA #$01            ; Assume "FM" (bit 1) is 0, and "NM" (bit 0) is 1. TODO: Use code to get actual values.
                BITMD #$80          ; Test (and clear) MD bit "/0".
                BEQ no_D0
                ORA #$80
no_D0:          BITMD #$40          ; Test (and clear) MD bit "IL".
                BEQ no_IL
                ORA #$40
no_IL:          STA SAVED_MD

                ; Set current monitor address to PC.
                LDX SAVED_PC
                STX CUR_ADDR
                
                ; Print intro msg.
                LDX #str_mon_intro
                STX CUR_STR
                JSR print_str

em_no_BRK:      ; Print registers.
                JSR print_registers
                LDA #0 ; CR
                JSR handle_CR_or_Down

                JSR cursor_enable
                ANDCC #$EF          ; Enable IRQ

monitor_loop:
                JSR wait_key
                JMP monitor_loop

;-----------------------------------------------------------
exit_monitor:
                ORCC #$10           ; Disable IRQ.

                ; Restore registers (possibly modified by user).
                LDX SAVED_V
                TFR X,V

                LDU SAVED_U
                LDS SAVED_S
                LDY SAVED_Y
                LDX SAVED_Y
                LDF SAVED_F
                LDE SAVED_E
                LDB SAVED_B

                LDA SAVED_DP
                TFR A,DP
                
                ; TODO: Restore MD ?
                
                ; Now set CC and A.
                LDA SAVED_CC
                PSHS A
                LDA SAVED_A
                PULS CC             ; This will also set IRQ flag as saved.
                ; TODO: Set PC into stack frame and unwind.
                JMP [SAVED_PC]
                RTS

;-----------------------------------------------------------
; Parse command line at cursor.
; This is typically invoked when the user presses CR.
; Outputs: [none]
; Clobbered: [none]
parse_line:
                PSHS A,X
                PSHSW

                ; Find the ptr to the beginning of cursor line, and put it in TMP.
                JSR find_cur_line_start_addr
        ;LDA #':'
        ;JSR print_petscii_adv
        ;LDA #'X'
        ;JSR print_petscii_adv
        ;LDA CRS_X
        ;JSR print_A_hex        
        ;LDA #'Y'
        ;JSR print_petscii_adv
        ;LDA CRS_Y
        ;JSR print_A_hex
        ;LDA #'H'
        ;JSR print_petscii_adv
        ;LDA CRS_ADDR_HI
        ;JSR print_A_hex        
        ;LDA #'L'
        ;JSR print_petscii_adv
        ;LDA CRS_ADDR_LO
        ;JSR print_A_hex
        ;LDA #'T'
        ;JSR print_petscii_adv
        ;LDA TMP_HI
        ;JSR print_A_hex        
        ;LDA #'S'
        ;JSR print_petscii_adv
        ;LDA TMP_LO
        ;JSR print_A_hex
        ;JMP done_parsing_line
            
                ; Now TMP contains the line start. NOTE: All chars are converted to PETSCII for parsing.
                LDF #0              ; F = Parsed char in line.

                ; Search for command (skip spaces and stop at ':' and '?').
                LDA #0
                STA CMD             ; 0 = No command.
                JSR parser_find_token ; Search for command (skip spaces and stop at EOL or EOL chars ':' and '?').
                CMPA #$FF           ; EOL (command not found).
                LBEQ done_parsing_line

                ; Command found. All commands are one byte (letter).
                STA CMD

                ; Point to next char after command first char.
                INCF

                ; Parse command.
                ; Some commands will read params.
                CMPA #'!'
                BNE no_cmd_HELP
                JSR cmd_HELP
                JMP done_token
no_cmd_HELP:
                CMPA #'>'
                BNE no_cmd_EditM
                JSR cmd_EditM
                JMP done_token
no_cmd_EditM:
                CMPA #';'
                BNE no_cmd_EditR
                JSR cmd_EditR
                JMP done_token
no_cmd_EditR:
                CMPA #'G'
                BNE no_cmd_G
                JSR cmd_GJ
                JMP done_token
no_cmd_G:
                CMPA #'I'
                BNE no_cmd_I
                JSR cmd_I
                JMP done_token
no_cmd_I:
                CMPA #'J'
                BNE no_cmd_J
                JSR cmd_GJ
                JMP done_token
no_cmd_J:
                CMPA #'L'
                BNE no_cmd_L
                JSR cmd_L
                JMP done_token
no_cmd_L:
                CMPA #'M'
                BNE no_cmd_M
                JSR cmd_M
                JMP done_token
no_cmd_M:
                CMPA #'N'
                BNE no_cmd_N
                JSR cmd_N
                JMP done_token
no_cmd_N:
                CMPA #'R'
                BNE no_cmd_R
                JSR cmd_R
                JMP done_token
no_cmd_R:
                CMPA #'T'
                BNE no_cmd_T
                JSR cmd_T
                JMP done_token
no_cmd_T:
                ; Print a '?' next to the unrecognized command.
                LDX TMP
                LDA #'?'
                STA F,X

done_token:     ; Token done.
                ; Char under cursor might have changed after command processing.
                JSR crs_save_under

done_parsing_line:

                PULSW
                PULS A,X
                RTS


;-----------------------------------------------------------
; Point to the first char of the found token and put its PETSCII value in A.
; Inputs:
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
; Outputs:
;   A: First PETSCII char of next token, or $FF if EOL (end of line).
;   F: Index of first token char, or index of EOL.
; Clobbered: [none]
parser_find_token:
                PSHS X

                LDX TMP             ; X = Ptr to line start.
pft_loop:       CMPF #40            ; Check for physical end of line.
                BHS pft_not_found   ; >=
                LDA F,X
                ANDA #$7F           ; Ignore reverse flag.
                CMPA #' '           ; No need to convert SPACE, ':', '?' to PETSCII.
                BEQ pft_next_chr
                CMPA #':'           ; EOL
                BEQ pft_not_found
                CMPA #'?'           ; EOL
                BEQ pft_not_found
                ; Found
                JSR scr_to_petscii  ; Convert screen code to PETSCII.
                BRA pft_done

pft_next_chr:   INCF
                BRA pft_loop

pft_not_found:  LDA #$FF

pft_done:       PULS X
                RTS


;-----------------------------------------------------------
; Verify that we have a separator (white space or EOL) at the current parse position.
; If separator is not found, a '?' is printed after it.
; NOTE: If not EOL, current parse position is advanced by one.
; Inputs:
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
; Outputs:
;   A: $00 = separator not found; $01 = SPACE found; $FF = end of line.
;   F: Index of next char after param.
; Clobbered: [none]
parser_check_separator:
                PSHS X
                LDX TMP
                CMPF #40
                BHS pcs_EOL         ; >=
                LDA F,X
                ANDA #$7F           ; Ignore reverse flag.
                CMPA #' '           ; No need to convert SPACE, ':', '?' to PETSCII.
                BEQ pcs_found
                CMPA #':'           ; EOL
                BEQ pcs_EOL
                CMPA #'?'           ; EOL
                BEQ pcs_EOL
                ; ERROR: No separator found. Print '?' after it.
                INCF
                LDA #'?'
                STA F,X
                JSR crs_save_under  ; In case we put the '?' under the cursor, save it.
                ; Separator not found.
                LDA #0
                BRA pcs_done
                
pcs_found:      ; Found SPACE.
                INCF
                LDA #1
                BRA pcs_done

pcs_EOL:        ; Found EOL
                LDA #$FF

pcs_done:       PULS X
                RTS


;-----------------------------------------------------------
; Parse a string param.
; Parsed string will be copied as PETSCII in TMP_BUF (max 192 bytes, including zero-termination).
; Inputs:
;   A: Mode. 0=space delimited; 1=quotes delimited
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
; Outputs:
;   A: 0 if parse error; 0 < A < 191 on success, string len, $FF if not found.
;   F: Index of next char after param.
; Clobbered: [none]
parser_get_param_str:
                STA TMP_BYTE0       ; Store mode.
                JSR parser_find_token
                CMPA #$FF
                BNE pgps_found
                ; Not found.
                RTS

pgps_found:     PSHS B,X,Y
                LDX TMP
                LDY #TMP_BUF
                TST TMP_BYTE0       ; Check mode.
                BEQ pgps_no_quotes  ; No need to check/skip quotes.
                ; Verify this is a quote char.
                CMPA #'"'
                BEQ pgps_open_q
                ; ERROR: Must start with quotes.
                INCF
                LDA #'?'
                STA F,X
                LDA #0
                BRA pgps_done

pgps_open_q:    INCF                 ; Skip (strip) open quote.

pgps_no_quotes:
                LDB #0              ; Current string length.
pgps_nxt_char:  CMPF #40            ; Check EOL.
                BHS pgps_EOS        ; >=

                ; Check for EOS depending on mode.
                TST TMP_BYTE0       ; Check mode.
                BNE pgps_q_eos
                ; Check for EOS (separator).
                LDA F,X
                CMPA #' '           ; Check EOS.
                BEQ pgps_EOS
                CMPA #':'           ; Check EOS.
                BEQ pgps_EOS
                CMPA #'?'           ; Check EOS.
                BEQ pgps_EOS
                BNE pgps_copy       ; BRA

pgps_q_eos:     ; Check for EOS (quotes).
                LDA F,X
                CMPA #'"'           ; Check EOS.
                BNE pgps_copy
                INCF                ; Skip (strip) close quote.
                BNE pgps_EOS        ; BRA

pgps_copy:      JSR scr_to_petscii
                STA B,Y
                INCF
                INCB
                CMPB #191           ; Check for max buffer length.
                BLS pgps_nxt_char   ; <

pgps_EOS:       ; EOS. Zero-terminate string.
                CLR B,Y
                TFR B,A             ; A = length.

pgps_done:      PULS B,X,Y            
                RTS


;-----------------------------------------------------------
; Parse a byte param into the specified param number.
; Inputs:
;   E: Start param number.
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
; Outputs:
;   A: $00 parse error; $01 on success (1 nibble found); $02 on success (2 nibbles found), $FF if not found.
;   E: On success, point to next param.
;   F: Index of next char after param.
;   PARAM[E]: Byte value.
parser_get_param_byte:
                PSHS X,Y
                LDX #PARAM0
                ; Clear param.
                CLR E,X
                ; First nibble.
                JSR parser_find_token
                CMPA #$FF            ; EOL (not found).
                BEQ pgpb_done

pgpb_found:
                LDY TMP
                CMPA #'%'
                BNE pgpb_hex

                ; Byte is in BIN format.
                LDA #8
                STA TMP_BYTE1       ; TMP_BYTE1 = Bit counter.
                INCF                ; Point to next char.
pgpb_read_bit:  CMPF #40
                BEQ pgpb_set_b      ; EOL.
                LDA F,Y             ; Read char.
                CMPA #':'
                BEQ pgpb_set_b      ; EOL
                CMPA #'?'
                BEQ pgpb_set_b      ; EOL
                CMPA #' '
                BEQ pgpb_set_b      ; No more binary digits in this byte.
                LSRA                ; C = bit value. A /= 2.
                ROL E,X             ; Insert read bit (carry) into the PARAM.
                CMPA #'0'/2         ; Check for valid binary digit.
                BEQ pgpb_rb_ok
                ; Read bit error.
                JMP pgpb_error      ; BRA (error).
pgpb_rb_ok:     ; Valid digit.
                INCF                ; Point to next digit.
                DEC TMP_BYTE1       ; Dec bit counter.
                BNE pgpb_read_bit
                BEQ pgpb_set_b
pgpb_set_b_iny: INCF
pgpb_set_b:
                LDA #2              ; Binary mode always sets 2 nibbles.
                BRA pgpb_done


pgpb_hex:       ; Byte is in HEX format.
                ; Found first nibble.
                JSR petscii_to_nibble
                CMPA #$FF           ; A=0a, or $FF on error.
                BEQ pgpb_error
                STA E,X             ; PARAM=0a
                ; Second nibble ?
                INCF
                LDA F,Y
                ANDA #$7F
                CMPA #' '
                BEQ pgpb_no_nib2
                JSR parser_find_token
                CMPA #$FF           ; EOL (not found).
                BNE nibble_two
                ; Success (1 nibble found).
pgpb_no_nib2:   INCE                ; Point to next param.
                LDA #1
                BRA pgpb_done

nibble_two:     ; Token found. Get value.
                JSR petscii_to_nibble
                CMPA #$FF           ; A=0b, or $FF on error.
                BEQ pgpb_error
                ASL E,X
                ASL E,X
                ASL E,X
                ASL E,X             ; PARAM=a0
                ORA E,X             ; A=ab
                STA E,X             ; PARAM=ab
                INCF

pgpb_store_param:
                ; Success (2 nibbles found).
                INCE                ; Point to next param.
                LDA #2
                BRA pgpb_done

pgpb_error:     INCF                ; Move after parser error.
                LDA #'?'
                STA F,Y
                LDA #0              ; Parse error.

pgpb_done
                PULS X,Y
                RTS


;-----------------------------------------------------------
; Parse a word param into the specified param number [LO] and the next one [HI]. Little-endian.
; NOTE: Although the 6309 is a Big-endian processor, this method stores a word in Little-endian format.
;
; Inputs:
;   E: Start param number.
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
; Outputs:
;   PARAM[E]  = WORD[LO]
;   PARAM[E+1]= WORD[HI]
;   A: $00 on error, $01 on success, $FF if not found.
;   E: On success, point to next param (i.e. X=X+2).
;   F: Index of next char after param.
parser_get_param_word:
                PSHS B,X,Y
                LDX #PARAM0
                LDY TMP
                CLRD
                STD E,X             ; Clear [LO] and [HI].
                JSR parser_get_param_byte ; If found, E=E+1.
                CMPA #0
                BEQ pfpw_done       ; ERROR parsing first byte.
                CMPA #$FF           ; Found first byte ?
                BEQ pfpw_done       ; No param found.
                
                ; At least one byte found. E=E+1.
                CMPA #1
                BEQ pfpw_success_1  ; If only one nibble found, then second byte not present, exit making E+2.
                ; Two nibbles found in first byte, read more.
                LDA F,Y             ; Read char.
                CMPA #' '
                BEQ pfpw_success_1  ; Second byte not found, exit making E+2.
                JSR parser_get_param_byte ; If found, E=E+1.
                CMPA #$FF
                BEQ pfpw_success_1  ; Second byte not found, exit making E+2.
                CMPA #0
                BEQ pfpw_done       ; ERROR parsing second byte.
                ; Second byte was present. X has been incremented twice, so point back to first byte.
                DECE
                DECE
                CMPA #2
                BEQ pfpw_swap_bytes ; 2 nibbles found in second byte. No adj needed, directly swap bytes.
                ; If we are here, then only 1 nibble found in the second byte.
                ; Adjust nibbles: ab 0c -> 0a bc, then swap bytes.
                INCE                ; Point to second byte.
                LDA E,X             ; A = 0c
                STA TMP_BYTE1       ; TMP_BYTE1 = 0c
                DECE                ; Point back to first byte.                
                LDA E,X             ; A = ab
                LSRA
                ROR TMP_BYTE0
                LSRA
                ROR TMP_BYTE0
                LSRA
                ROR TMP_BYTE0
                LSRA                ; A = 0a
                ROR TMP_BYTE0       ; TMP_BYTE0 = b?
                STA E,X             ; Param[E] = 0a
                LDA TMP_BYTE0       ; A = b?
                ANDA #$F0           ; A = b0
                ORA TMP_BYTE1       ; A = bc
                INCE
                STA E,X             ; Param[E+1] = bc
                DECE
pfpw_swap_bytes:
                LDA E,X             ; A=Param[E]
                INCE
                LDB E,X             ; B=Param[E+1]
                STA E,X
                DECE
                STB E,X             ; Swapped.
                INCE                ; Point to next param.
pfpw_success_1: INCE
pfpw_success:   LDA #1              ; Success.

pfpw_done:      PULS B,X,Y
                RTS
 
;-----------------------------------------------------------
; Parse a word param into the specified param number [LO] and the next one [HI]. Little-endian.
; NOTE: Although the 6309 is a Big-endian processor, this method stores a word in Little-endian format.
; If not found, use the current monitor address.
; On error, print message. On success, advance the param index (X) to the next param.
;
; Inputs:
;   E: Start param number.
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
; Outputs:
;   A: $00 on error, $01 on success, $FF if not found.
;   E: On success, point to next param (i.e. E=E+2).
;   F: Index of next char after param.
parse_addr_or_get_cur:
                JSR parser_get_param_word
                JSR check_param_error
                CMPA #0
                BNE paorgc_no_error
                ; ERROR.
                RTS
paorgc_no_error:
                ; No error.
                CMPA #$FF
                BNE paorgc_done     ; Found (X=X+2).
                ; Param not found, use current PC.
                PSHS X
                LDA CUR_ADDR_LO
                LDX #PARAM0
                STA E,X
                LDA CUR_ADDR_HI
                LDX #PARAM1
                STA E,X
                PULS X
                LDA #$FF
                ; Point to next param. Make X=X+2.
                INCE
                INCE
paorgc_done:    RTS 


;-----------------------------------------------------------
; Parse: [addr_start] [addr_end]
; NOTE: Although the 6309 is a Big-endian processor, this method stores a word in Little-endian format.
;
; Inputs:
;   A: If addr_end not present, addr_end = addr_start+A.
;   E: Start param number.
;   F: Current offset in parsed line.
;   TMP_HI, TMP_LO: Parsed line start.
;
; Output:
;   A: 0 on error, 1 on success.
;   E: On success, point to next param.
;   PARAM[E+0]: addr_start [LO], or CUR_ADDR[LO] if not present.
;   PARAM[E+1]: addr_start [HI], or CUR_ADDR[HI] if not present.
;   PARAM[E+2]: addr_end [LO], or addr_start + 64 [LO] if not present.
;   PARAM[E+3]: addr_end [HI], or addr_start + 64 [HI] if not present.
;
parser_get_start_end_addresses:
                PSHS X
                STA TMP_BYTE0   ; Save range.
                ; Parse start addr into param0 [LO] and param1 [HI]
                JSR parse_addr_or_get_cur
                CMPA #0
                BNE pea_noerr_0
                JMP pea_end
pea_noerr_0:    ; Check for param separator.
                JSR parser_check_separator
                CMPA #0
                BEQ pea_end
pea_noerr_1:    ; Parse end addr into param2 [LO] and param3 [HI]
                ; X+2
                JSR parser_get_param_word
                JSR check_param_error
                CMPA #0
                BEQ pea_end
pea_noerr_2:    ; No error.
                CMPA #$FF
                BEQ pea_no_addr_end
                ; Found. X+4
                ; Add 1 to end_addr.
                LDX #PARAM0-2
                INC E,X            ; X = PARAM1-2
                BNE pea_no_carry
                INC E,X
pea_no_carry:   JMP pea_end_present

pea_no_addr_end:
                ; End addr is not present. Set end_addr = start_addr + range.
                ; E+2
                LDX #PARAM0-2
                LDA E,X
                ADDA TMP_BYTE0      ; Add range.
                LDX #PARAM0
                STA E,X
                LDX #PARAM1-2
                LDA E,X
                ADCA #0
                LDX #PARAM1
                STA E,X

pea_end_present:
                LDA #1              ; OK.
pea_end:
                PULS X
                RTS


;-----------------------------------------------------------
; Check for param error after a parse_param_* call, and print msg if error.
; Inputs:
;   A: parser_find_param* return code.
; Clobbered: [none]
;
check_param_error:
                CMPA #0
                BEQ param_error
                RTS

param_error:    PSHS A
                LDA #0 ; CR
                JSR handle_CR_or_Down
                ; Print error msg.
                LDA #str_par_error&$FF
                STA CUR_STR_LO
                LDA #str_par_error/$100
                STA CUR_STR_HI
                JSR print_str
                PULS A
                RTS


;-----------------------------------------------------------
; COMMAND: ! (help)
cmd_HELP:
                ; Print help msg.
                LDA #str_mon_help&$FF
                STA CUR_STR_LO
                LDA #str_mon_help/$100
                STA CUR_STR_HI
                JSR print_str
                RTS


;-----------------------------------------------------------
; COMMAND: > (edit mem bytes starting from given address)
cmd_EditM:
                PSHS B,X
                ; Parse start addr into param0 [LO] and param1 [HI]
                LDE #0
                JSR parser_get_param_word
                CMPA #0
                BNE em_found_addr
                BRA cmd_EditM_err   ; Error parsing address.
                
em_found_addr:  ; Save start addr in TMP ptr.
                LDA PARAM0
                STA TMP_BYTE2
                LDA PARAM1
                STA TMP_BYTE3
                
                LDA #0
                STA TMP_BYTE4       ; Number of parsed bytes.
em_fetch_byte:  ; Check for param separator.
                JSR parser_check_separator
                CMPA #0
                BEQ cmd_EditM_err   ; Error parsing separator.
                CMPA #$FF
                BEQ cmd_EditM_ok    ; EOL.

em_hex:         ; Read next byte to set.
                LDE #2              ; Fetch byte in param2.
                JSR parser_get_param_byte
                CMPA #0
                BEQ cmd_EditM_err   ; Error parsing byte.
                CMPA #$FF
                BEQ cmd_EditM_ok    ; EOL.

em_set_byte:    LDA PARAM1          ; D=PARAM1,PARAM0=[HI],[LO]
                LDB PARAM0
                TFR D,X
                LDA PARAM2
                STA ,X
                INC TMP_BYTE4       ; Increment number of parsed bytes.
                INC PARAM0
                BNE em_no_ovfl
                INC PARAM1
em_no_ovfl:     BRA em_fetch_byte

cmd_EditM_ok:   ; Refresh edited line showing 8 or 1 bytes (autodetected).
                LDE #0
                LDF CRS_Y
                JSR move_cursor
                LDA TMP_BYTE2
                STA PARAM0          ; addr_start [LO]
                LDA TMP_BYTE3
                STA PARAM1          ; addr_start [HI]
                LDA TMP_BYTE4
                CMPA #2
                BLS cmd_EditM_refN  ; <
                ; More than 1 parsed. Assume 8-byte edit ("M" command).
                LDA PARAM0
                ADDA #8             ; Line is 8 bytes ("M" command).
                STA PARAM2          ; addr_end [LO]
                LDA PARAM1
                ADCA #0
                STA PARAM3          ; addr_end [HI]
                JSR m_print_mem
                JMP cmd_EditM_end
cmd_EditM_refN:
                ; 1 or less parsed. Assume single byte edit ("N" command).
                LDA PARAM0
                ADDA #1             ; Line is 1 byte ("N" command).
                STA PARAM2          ; addr_end [LO]
                LDA PARAM1
                ADCA #0
                STA PARAM3          ; addr_end [HI]
                JSR n_print_mem
                JMP cmd_EditM_end
cmd_EditM_err:
cmd_EditM_end:
                PULS B,X
                RTS

;-----------------------------------------------------------
; COMMAND: ; (edit registers, 6502 version)
cmd_EditR:      
                ; PC
                LDE #0
                JSR parser_get_param_word
                CMPA #0
                BNE er_fetched_PC
                JMP cmd_EditR_error ; Error parsing word.
er_fetched_PC:  LDA PARAM0          ; [LO]
                STA SAVED_PC+1
                LDA PARAM1          ; [HI]
                STA SAVED_PC+0

                JSR parser_check_separator
                CMPA #0
                BEQ cmd_EditR_error
                
                ; TODO: Implement.
cmd_EditR_error:
                RTS   


;-----------------------------------------------------------
; COMMAND: G (JMP addr), or J (JSR addr)
cmd_GJ:
                LDE #0              ; Parse addr into param0 [LO] and param1 [HI]
                JSR parse_addr_or_get_cur
                CMPA #0
                BEQ cmd_GJ_END      ; Parse error.
                CMPA #$FF
                BNE cmd_GJ_found_param

                ; Param not found, use current PC.
                LDA SAVED_PC+0      ; [HI]
                LDX PARAM1
                STA E,X
                LDA SAVED_PC+1
                LDX PARAM0          ; [LO]
                STA E,X

cmd_GJ_found_param:
                LDA #0 ; CR
                JSR handle_CR_or_Down
                ; Print address before execution.
                LDA #str_cmd_G_ok&$FF
                STA CUR_STR_LO
                LDA #str_cmd_G_ok/$100
                STA CUR_STR_HI
                LDA CMD
                CMPA #'G'
                BEQ cmd_GJ_p_addr
                LDA #str_cmd_J_ok&$FF
                STA CUR_STR_LO
                LDA #str_cmd_J_ok/$100
                STA CUR_STR_HI
cmd_GJ_p_addr:  JSR print_str
                LDA PARAM1
                JSR print_A_hex
                LDA PARAM0
                JSR print_A_hex
                LDA #0 ; CR
                JSR handle_CR_or_Down
                ; Execute JMP or JSR.
                LDA PARAM0          ; [LO]
                STA SAVED_PC+1
                LDA PARAM1          ; [HI]
                STA SAVED_PC+0
                LDA CMD
                CMPA #'J'
                BEQ cmd_J_exec
                JMP exit_monitor    ; Exec 'G'
cmd_J_exec:     JSR exit_monitor    ; Exec 'J'

cmd_GJ_END:     RTS

str_cmd_G_ok: .strz "JMP $"
str_cmd_J_ok: .strz "JSR $"


;-----------------------------------------------------------
; COMMAND: I (show registers)
cmd_I:          
                PSHS X
                LDA #0 ; CR
                JSR handle_CR_or_Down

                LDX #str_si_sys
                STX CUR_STR
                JSR print_str

                LDX #str_si_video
                STX CUR_STR
                JSR print_str

                LDA VIDEO_STD
                BNE i_is_PAL
                LDX #str_si_NTSC
                STX CUR_STR
                BRA i_prt_video         ; BRA
i_is_PAL:       LDX #str_si_PAL
                STX CUR_STR
i_prt_video:    JSR print_str

                ; TODO: More info here [...]

                LDA #0 ; CR
                JSR handle_CR_or_Down

                PULS X
                RTS

str_si_sys:     .str "SYSTEM: COMMODORE 6309"
                .byte $0D,0
str_si_video:   .strz "VIDEO:  "
str_si_NTSC:    .strz "NTSC"
str_si_PAL:     .strz "PAL"


;-----------------------------------------------------------
; COMMAND: L (LOAD)
cmd_L:          ; Parse start addr into param0 [LO] and param1 [HI]
                LDA #1
                JSR parser_get_param_str
                CMPA #0
                BNE l_noerr_0
                ; Error.
                JSR check_param_error
                JMP cmd_L_end
l_noerr_0:
                ; Save filename length.
                STA FILENAME_LEN
                ; Parse start addr into param0 [LO] and param1 [HI]
                LDE #0
                JSR parse_addr_or_get_cur
                CMPA #0
                BNE l_noerr_1
                JMP cmd_L_end
l_noerr_1:
                ; Load file.

                LDA FILENAME_LEN
                LDE #TMP_BUF&$FF
                LDF #TMP_BUF/$100
                JSR raw_SETNAM

                LDA #2                  ; Logical file number [1..255].
                LDE #8                  ; Device [0..31]. Use device 8 (primary disk drive).
                LDF #0                  ; Secondary address: Command 0 = relocated load.
                JSR raw_SETLFS

                LDE PARAM0              ; Load address [LO].
                LDF PARAM1              ; Load address [HI].
                LDA #0                  ; 0 = Load, 1 = Verify
                ; Do raw LOAD.
                JSR raw_LOAD

                ; E/F = ptr last byte loaded
                ; C = Set if error (A = error code).
                STE PARAM2              ; Save ptr to last byte loaded.
                STF PARAM3
                STA PARAM4              ; PARAM4: Error code (if error).
                LDA #0
                ADCA #0                 ; A=1 on error (Carry set).
                STA PARAM5              ; PARAM5: 0=Load OK, 1=Load ERROR.
                
                ; Check error.
                LDA PARAM5              ; PARAM5: 0=Load OK, 1=Load ERROR.
                BNE cmd_L_load_ERR

cmd_L_load_OK:
                ; LOAD OK
                ; Compute load length in PARAM2 and PARAM3.
                LDA PARAM2
                SUBA PARAM0
                STA PARAM2
                LDA PARAM3
                SBCA PARAM1
                STA PARAM3

                ; Set current address to load address (whatever the result).
                LDA PARAM0
                STA CUR_ADDR_LO
                LDA PARAM1
                STA CUR_ADDR_HI

                LDA #0 ; CR
                JSR handle_CR_or_Down

                LDX #str_loaded
                STX CUR_STR
                JSR print_str
                LDA PARAM3
                JSR print_A_hex
                LDA PARAM2
                JSR print_A_hex
                LDX #str_bytes_at
                STX CUR_STR
                JSR print_str
                LDA PARAM1
                JSR print_A_hex
                LDA PARAM0
                JSR print_A_hex
                LDA #0 ; CR.
                JSR handle_CR_or_Down

                LDA #5                  ; Green border.
                STA $D020

                LDA BOOT_PHASE
                CMPA #1                  ; Check if we were booting.
                BNE l_load_OK_no_boot
                LDA #2
                STA BOOT_PHASE          ; Boot load OK.
                ; Jump to load address.
                LDX #str_jmp_at
                STX CUR_STR
                JSR print_str
                LDA PARAM1
                JSR print_A_hex
                LDA PARAM0
                JSR print_A_hex
                LDA #0 ; CR.
                JSR handle_CR_or_Down
                ; PARAM0,PARAM1 is Little-Endian, so we can't directly indirect-jump.
                LDA PARAM1              ; [HI]
                LDB PARAM0              ; [LO]
                TFR D,X                 ; X = jump address.
                JMP ,X                  ; Jump to boot code.
                RTS ; Just in case.

cmd_L_load_ERR:
                ; LOAD ERROR
                LDA #2                  ; Red border.
                STA $D020
                ; Print error msg.
                LDX #str_load_error
                STX CUR_STR
                JSR print_str
                LDA PARAM4              ; Error code.
                JSR print_A_hex
                LDA #0 ; CR
                JSR handle_CR_or_Down
                ; Check if we were booting.
                LDA BOOT_PHASE
                CMPA #1
                BNE l_load_OK_no_boot
                LDA #3
                STA BOOT_PHASE          ; Boot load ERROR.
                ; End.
                BRA cmd_L_end

l_load_OK_no_boot:

cmd_L_end:
                RTS


;-----------------------------------------------------------
; COMMAND: M (Display mem hex)
cmd_M:
                LDA #64                 ; Bytes to display if addr_end not present.
                LDE #0
                JSR parser_get_start_end_addresses
                CMPA #0                 ; Check for error.
                BEQ cmd_M_end

                LDA #0 ; CR
                JSR handle_CR_or_Down

m_print_mem:    LDA PARAM0              ; [LO]
                STA CUR_ADDR_LO
                LDA PARAM0+1            ; [HI]
                STA CUR_ADDR_HI

m_nxt_row:      ; Save cursor address at beginning of line.
                LDA CRS_ADDR_LO
                STA TMP_BYTE1
                LDA CRS_ADDR_HI
                STA TMP_BYTE0
                LDA #'>'
                JSR print_char_adv
                ; Convert addr[HI] to hex str.
                LDA CUR_ADDR_HI
                JSR print_A_hex
                ; Convert addr[LO] to hex str.
                LDA CUR_ADDR_LO
                JSR print_A_hex
                LDE #0                  ; Cur row byte index.
m_nxt_byte:     LDA #' '
                JSR print_char_adv
                LDF #0
                LDX CUR_ADDR
                LDA ,X
                JSR print_A_hex
                PSHS A
                TFR E,A
                ADDA #31
                TFR A,F
                PULS A
                LDX TMP_BYTE0
                STA F,X
                TFR E,A
                CMPA #0
                BNE m_no_col
                LDA #':'
                DECF
                STA F,X
m_no_col:       INC CUR_ADDR_LO
                BNE m_no_carry
                INC CUR_ADDR_HI
m_no_carry:     ; Check for end_addr.
                LDA CUR_ADDR_HI
                CMPA PARAM2+1           ; end_addr[HI]
                BNE m_not_last
                LDA CUR_ADDR_LO
                CMPA PARAM2+0           ; end_addr[LO]
                BEQ cmd_M_end
m_not_last:     INCE
                CMPE #8
                BNE m_nxt_byte
                ; Next row.
                LDA #0 ; CR
                JSR handle_CR_or_Down
                LDE #0
                BEQ m_nxt_row           ; BRA

cmd_M_end:      ;LDA #0 ; CR
                ;JSR handle_CR_or_Down
                RTS


;-----------------------------------------------------------
; COMMAND: N (Display mem bin)
cmd_N:
                LDA #8                  ; Bytes to display if addr_end not present.
                LDE #0
                JSR parser_get_start_end_addresses
                CMPA #0                 ; Check for error.
                BEQ cmd_N_end

                LDA #0 ; CR
                JSR handle_CR_or_Down

n_print_mem:    LDA PARAM0
                STA CUR_ADDR_LO
                LDA PARAM0+1
                STA CUR_ADDR_HI

n_nxt_row:      LDA #'>'
                JSR print_char_adv
                ; Convert addr[HI] to hex str.
                LDA CUR_ADDR_HI
                JSR print_A_hex
                ; Convert addr[LO] to hex str.
                LDA CUR_ADDR_LO
                JSR print_A_hex
                LDE #0              ; Cur row byte index.
                LDA #' '
                JSR print_char_adv
                LDA #'%'
                JSR print_char_adv
                LDF #0
                LDX CUR_ADDR
                LDA ,X
                JSR output_A_bin
                LDA #' '
                JSR print_char_adv
                LDA #':'
                JSR print_char_adv
                LDA ,X
                JSR print_A_hex
                INC CUR_ADDR_LO
                BNE n_no_carry
                INC CUR_ADDR_HI
n_no_carry:     ; Check for end_addr.
                LDA CUR_ADDR_HI
                CMPA PARAM2+1
                BNE n_not_last
                LDA CUR_ADDR_LO
                CMPA PARAM2+0
                BEQ cmd_N_end
n_not_last:     ; Next row.
                LDA #0 ; CR
                JSR handle_CR_or_Down
                LDE #0
                BEQ n_nxt_row       ; BRA

cmd_N_end:      ;LDA #0 ; CR
                ;JSR handle_CR_or_Down
                RTS


;-----------------------------------------------------------
; COMMAND: R (Show registers)
cmd_R:          LDA #0 ; CR
                JSR handle_CR_or_Down
                JSR print_registers
                RTS


;-----------------------------------------------------------
; COMMAND: T (Display mem hex)
cmd_T:
                PSHS A,X
                PSHSW

                ; Parse test number in PARAM0.
                LDE #0
                JSR parser_get_param_byte
                CMPA #0
                BNE t_noerr_0
                ; Error.
                BRA cmd_T_end
t_noerr_0:      CMPA #$FF
                BNE t_do_test
                ; No parameter. Print list of tests.
                LDA #0 ; CR
                JSR handle_CR_or_Down
                LDA #str_tests_list&$FF
                STA CUR_STR_LO
                LDA #str_tests_list/$100
                STA CUR_STR_HI
                JSR print_str
                BRA cmd_T_end
t_do_test:      LDA PARAM0
                CMPA #0
                BNE t_not_0
                ; Test 0: RAM test
                ;JSR test_RAM
                BRA cmd_T_end
t_not_0:        CMPA #1
                BNE t_not_1
                ; Test 1: Sprites test.
                JSR test_Sprites
                BRA cmd_T_end
t_not_1:        CMPA #2
                BNE t_not_2
                ; Test 2: Enable VIC-II raster IRQ.
                LDA #$01
                STA $D01A
                BRA cmd_T_end
t_not_2:        CMPA #3
                BNE t_not_3
                ; Test 3: Mandelbrot.
                JSR Mandelbrot
                BRA cmd_T_end
t_not_3:

cmd_T_end:
                PULSW
                PULS A,X
                RTS

;-----------------------------------------------------------
str_mon_intro:  .str "DDT'S 6309 DEBUG KERNAL V0.1"
                .byte $0D
                .str "! FOR HELP"
                .byte $0D,0

                      ;|------------ Screen width ------------|
                      ;0         1         2         3
                      ;0123456789012345678901234567890123456789
str_mon_help:   .byte $0D
                .str "G [ADDR]                    :JMP TO ADDR"
                .str "I                           :SYSTEM INFO"
                .str "J [ADDR]                    :JSR TO ADDR"
                .str 'L ["FILENAME"] [ADDR]      :LOAD AT ADDR'
                .str "M [<START> [<END>]]   :DISPLAY MEM (HEX)"
                .str "N [<START> [<END>]]   :DISPLAY MEM (BIN)"
                .str "R                        :SHOW REGISTERS"
                .str "T [NUM]                   :LAUNCH A TEST"
                .str "; [REGVALUES]           : EDIT REGISTERS"
                .str "> ADDR [BYTE] ...             : EDIT MEM"
                .byte 0

str_tests_list: ;.str "t 0: ram test"
                ;.byte $0D
                .str "T 1: SPRITES"
                .byte $0D
                .str "T 2: RASTER IRQ"
                .byte $0D
                .str "t 3: MANDEL"
                .byte $0D
                .byte 0

str_break:      .str "BREAK"
                .byte $0D,0   
str_cmd_error:  .str "COMMAND ERROR"
                .byte $0D,0 
str_par_error:  .str "PARAM ERROR"
                .byte $0D,0 
str_load_error: .strz "LOAD ERROR:"
str_loaded:     .strz "LOADED $"
str_bytes_at:   .strz " BYTES AT $"
str_jmp_at:     .strz "JMP $"


;===========================================================
; Print registers
print_registers:
                PSHS A,X
                
                ; Print 6309 regs names.
                LDX #reg_names_6309_0
                STX CUR_STR
                JSR print_str
                LDX #reg_names_6309_1
                STX CUR_STR
                JSR print_str
                LDX #reg_names_6309_2
                STX CUR_STR
                JSR print_str
                ; Print semicolon.
                LDA #';'
                JSR print_char_adv
                LDA #' '
                JSR print_char_adv
                ; Convert PC[HI] to hex str.
                LDA SAVED_PC+0
                JSR print_A_hex
                ; Convert PC[LO] to hex str.
                LDA SAVED_PC+1
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert DP to hex str.
                LDA SAVED_DP
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert A to hex str.
                LDA SAVED_A
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert B to hex str.
                LDA SAVED_B
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert E to hex str.
                LDA SAVED_E
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert F to hex str.
                LDA SAVED_F
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert X[HI] to hex str.
                LDA SAVED_X+0
                JSR print_A_hex
                ; Convert X[LO] to hex str.
                LDA SAVED_X+1
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert Y[HI] to hex str.
                LDA SAVED_Y+0
                JSR print_A_hex
                ; Convert Y[LO] to hex str.
                LDA SAVED_Y+1
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert V[HI] to hex str.
                LDA SAVED_V+0
                JSR print_A_hex
                ; Convert V[LO] to hex str.
                LDA SAVED_V+1
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Print CR
                LDA #0 ; Handle CR.
                JSR handle_CR_or_Down
                ; Print regs names (3).
                LDX #reg_names_6309_3
                STX CUR_STR
                JSR print_str
                ; Print comma.
                LDA #','
                JSR print_char_adv
                LDA #' '
                JSR print_char_adv
                ; Convert U[HI] to hex str.
                LDA SAVED_U+0
                JSR print_A_hex
                ; Convert U[LO] to hex str.
                LDA SAVED_U+1
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert S[HI] to hex str.
                LDA SAVED_S+0
                JSR print_A_hex
                ; Convert S[LO] to hex str.
                LDA SAVED_S+1
                JSR print_A_hex
                LDA #' '
                JSR print_char_adv
                ; Convert CC to hex str.
                LDA SAVED_CC
                JSR print_A_hex
                LDA #':'
                JSR print_char_adv
                ; Convert CC to bin.
                LDA SAVED_CC
                JSR output_A_bin
                LDA #' '
                JSR print_char_adv
                ; Convert MD to hex str.
                LDA SAVED_MD
                JSR print_A_hex
                LDA #':'
                JSR print_char_adv
                ; Convert MD to bin.
                LDA SAVED_MD
                JSR output_A_bin
                
                PULS A,X
                RTS

                         ;|------------ Screen width ------------|
                         ;0         1         2         3
                         ;0123456789012345678901234567890123456789
reg_names_6309_0:   .str "          ----------Q"
                    .byte $0D,$0
reg_names_6309_1:   .str "          ----D ----W"
                    .byte $0D,$0
reg_names_6309_2:   .str "  --PC DP -A -B -E -F ---X ---Y ---V"
                    .byte $0D,$0
reg_names_6309_3:   .str "  ---U ---S CC:EFHINZVC MD:DI----FN"
                    .byte $0D,$0



; ==================================================================
; ==================== START OF VECTOR HANDLERS ====================
; ==================================================================



; ==================== Here is the 6502 NMI routine used in case this ROM is used with a 6502 ====================
;                ORG $xxxx
;nmi_6502:        
;                FCB $40      ; RTI [6502]


; ==================== Here is the 6502 RESET routine used in case this ROM is used with a 6502 ====================
                IFGE *-$FBFD
                    ERROR 6502 reset handler overlapped
                ENDC
        
                ORG $FBFD

                ; 6502 machine code
code_6502:
                INCLUDEBIN "rom_6502_reset.bin"

; ==================== 6309 "illegal instruction" interrupt routine ====================
                IFGE *-$FD80
                    ERROR 6309 ILL handler overlapped
                ENDC
                
                ORG $FD80
                ; In native mode, CPU atomatically pushes on SS: PC, U, Y, X, DP, F, E, B, A, CC
                
                ; Print "ILL" in the upper-left corner of the screen, then the PC of the next instruction.
                LDA #'i'-96
                STA $0400
                LDA #'l'-96
                STA $0401
                STA $0402
                ; Get stacked address of next instruciton past the illegal one.
                LDA 12,S                ; PC[HI]
                JSR conv_A_hex_chars    ; Result in TMP_STR[0] and TMP_STR[1]
                LDA TMP_STR+0
                STA $0403
                LDA TMP_STR+1
                STA $0404
                LDA 13,S                ; PC[LO]
                JSR conv_A_hex_chars    ; Result in TMP_STR[0] and TMP_STR[1]
                LDA TMP_STR+0
                STA $0405
                LDA TMP_STR+1
                STA $0406
                
                ; Retrieve extra used registers from SS.
                RTI

; ==================== 6309 NMI routine ====================
                IFGE *-$FDFB
                    ERROR 6309 NMI handler overlapped
                ENDC

                ORG $FDFB

;        ; The 6309 handler start with three apparently nonsensical instructions, which are actually 6502 instructions used in case the ROM is used with a 6502.
;                    ; CODE  | 6502 ASM
;                    ;------------------
;        INCA        ; $4C   | JMP $F601
;        NOP         ; $01   | ^
;        LDA #$00    ; $F6   | ^
;                    ; $00   | BRK (not used)
;        
nmi_6309:
                ; Jump to monitor entry point.
                JMP enter_monitor_from_interrupt
                RTI ; Shouldn't be necessary, as the monitor handles everything.


; ==================== 6309 "enter monitor" interrupt routine ====================
                IFGE *-$FE00
                    ERROR 6309 enter_monitor handler overlapped
                ENDC
                
                ORG $FE00

irq_enter_monitor:
                ; In native mode, CPU atomatically pushes on SS: PC, U, Y, X, DP, F, E, B, A, CC
        
                ; Jump to monitor entry point.
                JMP enter_monitor_from_interrupt
                ; NOTE: Stack unwinding is taken care by the monitor.
        
                ; Retrieve extra used registers from SS.
        
                RTI ; Shouldn't be necessary, as the monitor handles everything.


; ==================== 6309 FIRQ routine ====================
; NOTE: The v0.8 hardware prototype uses IRQ, not FIRQ.
                IFGE *-$FE80
                    ERROR 6309 FIRQ handler overlapped
                ENDC

                ORG $FE80
        
firq:       
                ; We set MD.1==0, so handling FIRQ the CPU atomatically pushes on SS: PC, CC.
                ; Save extra used registers on SS.
                PSHS A,B
                
                INC $0405 ; See if FIRQs are running.
                
                ; Retrieve extra used registers from SS.
                PULS A,B
                RTI

        
; ==================== 6309 IRQ routine ====================
                IFGE *-$FF00
                    ERROR 6309 IRQ handler overlapped
                ENDC

                ORG $FF00

irq:
                ; In native mode, CPU atomatically pushes on SS: PC, U, Y, X, DP, F, E, B, A, CC.

        ;INC $0423 ; Debug (visual feedback about IRQ freq).

                ; Check if raster interrupt enabled.
                LDA $D01A
                BITA #$01
                BEQ no_raster_irq                
                ; Check if this is a raster interrupt by checking the raster comparator.
                ; NOTE: Raster comparator test can pass even if raster interrupt is disabled.
                LDA $D019
                ;JSR debug_A_hex
                BITA #$01
                BEQ no_raster_irq

; ------------- Handle raster interrupt

                LDB $D020       ; Save border color.
                ; Change border color for yellow bar.
                LDA #7
                STA $D020
                ; Wait a bit (a few cycles) for color bar to be visible.
                LDE #$10
irq_raster_delay:
                DECE
                BNE irq_raster_delay
                ; Restore border color.
                STB $D020

                ; Acknowledge any VIC-II interrupt.
                LDA #$FF
                STA $D019

                JMP end_IRQ_handler

; ------------- Handle timer interrupt.
no_raster_irq:
                ; Handle system IRQ like the original C64:
irq_system:     ; Was $EA31
                JSR inc_realtime_clock ; increment the real time clock (required for disk ops).
                JSR handle_cursor
                JSR scan_keyboard ; scan the keyboard

                ; Check if boot countdown is in progress (phase 0).
                LDA BOOT_PHASE
                BNE no_autoboot
                ; Boot countdown in progress.
                LDA AUTOBOOT_CD_HI
                ADDA #'0'
                STA $0414
                LDA AUTOBOOT_CD_LO
                SUBA #4
                STA AUTOBOOT_CD_LO
                BNE no_autoboot
                DEC AUTOBOOT_CD_HI
                BNE no_autoboot
                ; Boot countdown elapsed. Boot now.
                LDA #1             ; Loading
                STA BOOT_PHASE
                JSR update_boot_msg
                ; Inject autoboot command in keyboard buffer.
                JSR inject_boot_cmd
no_autoboot:

                LDA $DC0D       ; Read CIA1 ICR to clear the timer interrupt flag.

end_IRQ_handler:
                RTI





; ===================================================================
; ====================        CPU VECTORS        ====================
; ===================================================================
        IFGE *-$FFF0
            ERROR 6309 Vector table overlapped
        ENDC

        ORG $FFF0 ; Illegal Opcode and Division by Zero Trap (exception)
        FDB $FD80 ; [Illegal instruction]
        
        ORG $FFF2 ; Third software interrupt (SWI3 instruction)
        FDB $FE00 ; [Enter monitor]
        
        ORG $FFF4 ; Second software interrupt (SWI2 instruction)
        FDB $FE00 ; [Enter monitor]
        
        ORG $FFF6 ; Fast interrupt (FIRQ* line)
        FDB $FE80 ; 

        ORG $FFF8 ; Interrupt (IRQ* line)
        FDB $FF00
        
        ORG $FFFA ; Software interrupt (SWI instruction)
        FDB $FE00 ;   6309: $FE00 SWI vector [Enter monitor]
                  ;  *6502: $00FE NMI vector [not-implemented]
        
        ORG $FFFC ; Non-maskable interrupt (NMI* line).
        FDB $FDFB ;   6309: $FDFB NMI vector
                  ;  *6502: $FBFD RESET vector
        
        ORG $FFFE ; Processor reset (RESET* line)
        FDB $E000 ;   6309: $E000 reset vector
                  ;  *6502: $00E0 IRQ/BRK vector
                  

; ===================================================================
; =========================== END OF ROM ============================
; ===================================================================
