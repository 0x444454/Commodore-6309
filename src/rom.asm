; Commodore-6309 test ROM
; https://github.com/0x444454/Commodore-6309 
;
; Revision history [authors in square brackets]:
;   2024-09-24: First simple test loop. [DDT]
;   2024-11-02: Include more advanced tests and a fixed-point Mandelbrot. [DDT]
;   2025-01-03: Use joy-2 input for more flexible tests.
;   2025-01-04: Added IRQ test (raster), and sprite test.
        
        PRAGMA 6309
        OPT CD
        
        ORG $E000         ; Replace the C64 8KB KERNAL ($E000-$FFFF).

        ;LDA #$01  ; White
        ;STA $D020 ; Border color

init:      

; Switch from 6809 emulation mode (default) to 6309 native mode.
        
        LDMD #$01  ; This also sets the FIRQ handling mode (bit 1) but we don't use IRQ (yet).
 
; Setup the stack pointers
        LDS #$7000
        LDU #$7800

 
; Init CIAs   --------------------

        ; We could use DP for faster/shorter I/O registers setup, but we prefer code legibility.
        ; So these lines have been commented-out.
        ;SETDP $DD
        ;LDA #$DD
        ;TFR A,DP
        ;LDA #$03
        ;STA <$00 ; Set default bank 0 for VIC-II
        
        LDA #$FF
        STA $DC02 ; CIA1 port A direction: R/W
        STA $DC03 ; CIA1 port B direction: R/W
        STA $DD02 ; CIA2 port A direction: R/W
        STA $DD03 ; CIA2 port B direction: R/W

        LDA #$7F
        STA $DC0D ; CIA1: Disable all interrupts.
        LDA $DC0D ; CIA1: Ack pending interrupts.
        STA $DD0D ; CIA2: Disable all interrupts.        
        LDA $DD0D ; CIA2: Ack pending interrupts.
        
        LDA #$03
        STA $DD00 ; CIA2 port A: Set VIC-II Bank #0, $0000-$3FFF, 0-16383.



; Init VIC-II --------------------
        LDA #$1B
        STA $D011 ; CR1

        LDA #210 
        STA $D012 ; Interrupt rasterline.

        
        LDA #$00
        STA $D015 ; Sprite enable

        LDA #$C8
        STA $D016 ; CR2

        LDA #$14
        STA $D018 ; Mem ptrs

        ;LDA #$00
        ;STA $D019 ; Interrupts register

        LDA #$00  ; Disable all VIC-II interrupts.
        STA $D01A ; Interrupts enable register (normal maskable IRQ).

        LDA #$02  ; Red
        STA $D020 ; Border color

        LDA #$01  ; White
        STA $D021 ; Background color 0
        
        ;JMP init
      
        ; Fill screen with 'A' chars.
        ;LDA #$20      ; Space
        ;LDA #$A0      ; INVERSE SPACE (filled block)
        LDA #$01       ; "A" (filled block)
        JSR fill_screen
        
        ; Print "6309" in the upper left corner.
        ;LDQ #$36333039
        ;STQ $0400
        LDA #$36
        STA $0400
        LDA #$33
        STA $0401
        LDA #$30
        STA $0402
        LDA #$39
        STA $0403
      
        LDA #$30            ; Number of test runs.
        STA $0428


main_loop:

        ; Preset Color RAM (all black).
        LDA #$00
        LDX #$D800    ; Start of Color RAM
loop_clcram:
        STA ,X+ 
        CMPX #$DC00
        BNE loop_clcram

        ; Draw a Mandelbrot set in Color RAM.
        JSR Mandelbrot
        INC $0428           ; Inc number of test runs.
        ; Copy number of test runs indicator to next char on the right (VIC-II bad-line troubleshooting).
        LDA $0428
        STA $0429

        ; Check joystick input on control port 2.
read_joy:        
        LDA $DC00           ; Read CIA1:PRA. This is [xxxFRLDU]. All signals active low.
        JSR print_A_hex
        ANDA #$1F           ; Get only joy-2 actions.
        BITA #$10           ; Check if FIRE pressed.
        BEQ read_joy        ; If FIRE pressed, pause.
    ;BRA read_joy    

        CMPA #$1F           ; Check for any joy input.
        BEQ end_input       ; No joy input.
        
        ; Process joy input (active low).
        BITA #$01           ; Check if UP.
        BNE no_U            ; UP not pressed.
        ; UP
        JSR Sprites_test    ; UP action: Perform sprite test (sprites will stay enabled after test).
        BRA end_input
no_U:   BITA #$02           ; Check if DOWN.
        BNE no_D            ; DOWN not pressed.
        ; DOWN
        LDA #$00
        JSR fill_screen     ; DOWN action: Fill screen RAM to all zeros ('@' char).
        BRA end_input
no_D:   BITA #$04           ; Check if LEFT.
        BNE no_L            ; LEFT not pressed.
        ; LEFT
        BRA end_input
no_L:   BITA #$08           ; Check if RIGHT.
        BNE no_R            ; RIGHT not pressed.
        ; RIGHT
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

; ==================== clear_screen ====================
; Clear Screen RAM to the value contained in A.
fill_screen:
        LDX #$0400    ; Start of Screen RAM
loop_clsram:
        STA ,X+ 
        CMPX #$07E8
        BNE loop_clsram
        RTS

;===========================================================
; Print A as a hex number on the upper-left corner of the screen.
; NOTE: Registers are preserved.

print_A_hex:
        PSHU A        ; Save A.
        PSHU A        ; Save A.
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
        PULU A        ; Restore A.
        ANDA #$0F
        CMPA #$0A
        BHS pA_alpha_L
        ; Not alpha, i.e. [0..9]
        ADDA #$30+9
pA_alpha_L:    
        SUBA #9
        STA $0427

end_pAh:    
        PULU A        ; Restore A.
        RTS



        
; ==================== Mandelbrot ====================
; DDT's 6309 fast fixed-point Mandelbrot routine.
; This is a fast 16bit fixed-point version derived from my 68000 version for the Amiga (1989).
; TODO: Fixed point format is suboptimal for Mandlebrot, and should be tuned to improve zoom-in ability.
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


;----------------- VARIABLES IN RAM

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
        ORG $FC00

firq:
        ; In native mode, CPU atomatically pushes on SS: PC, CC
        ; Save extra used registers on SS.
        PSHS A
        
        LDA #8
        STA $D020            ; Change border color.

        LDA #$90             ; Wait a bit for color bar to be visible.
firq_raster_delay:
        DECA
        BNE firq_raster_delay

        LDA #9
        STA $D020            ; Change border color.

        ; Retreive extra used registers from SS.
        PULS A
        RTI

        
; ==================== 6309 IRQ routine ====================
        ORG $FC00

irq:
        ; In native mode, CPU atomatically pushes on SS: PC, U, Y, X, DP, F, E, B, A, CC
        ; Save extra used registers on SS.
        
        LDA #7
        STA $D020            ; Change border color.

        LDA #$20             ; Wait a bit for color bar to be visible.
irq_raster_delay:
        DECA
        BNE irq_raster_delay

        LDA #0
        STA $D020            ; Change border color.

        LDA #$01
        ORA $D019       ; Acknowledge raster interrupt.
        STA $D019

        ; Retreive extra used registers from SS.

        RTI

; ==================== CPU VECTORS ====================

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
                  
