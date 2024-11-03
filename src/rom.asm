; Commodore-6309 test ROM
; https://github.com/0x444454/Commodore-6309 
;
; Revision history [authors in square brackets]:
;   2024-09-24: First simple test loop. [DDT]
;   2024-11-02: Include more advanced tests and a fixed-point Mandelbrot. [DDT]
        
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

 
; Init CIA2   --------------------

        ;SETDP $DD
        ;LDA #$DD
        ;TFR A,DP
        ;LDA #$03
        ;STA <$00 ; Set default bank 0 for VIC-II
        
        LDA #$03
        STA $DD00

; Init VIC-II --------------------
        LDA #$1B
        STA $D011 ; CR1
        
        LDA #$00
        STA $D015 ; Sprite enable

        LDA #$C8
        STA $D016 ; CR2

        LDA #$14
        STA $D018 ; Mem ptrs

        ;LDA #$00
        ;STA $D019 ; Interrupts register

        LDA #$00  ; Disable all.
        STA $D01A ; Interrupts enabled (normal maskable IRQ).

        LDA #$02  ; Red
        STA $D020 ; Border color

        LDA #$01  ; White
        STA $D021 ; Background color 0
        
        ;JMP init
      
        ; Clear screen.
        ;LDA #$20      ; Space
        ;LDA #$A0      ; INVERSE SPACE (filled block)
        LDA #$01       ; "A" (filled block)
clear_screen:
        LDX #$0400
loop_cls:
        STA ,X+        ; Screen RAM
        CMPX #$7E8
        BNE loop_cls
        ; Uncomment the following 2 lines to visually keep testing for DRAM write issues.
        ;INCA
        ;BRA clear_screen ; Forever
        
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
      
        ; Preset Color RAM (all black).
        LDA #$00
        LDX #$D800    ; Start of color RAM
loop_clcram:
        STA ,X+ 
        CMPX #$DC00
        BNE loop_clcram

        ; Run sprite test.
        ;JSR Sprites_test

        LDA #$30            ; Number of test runs.
        STA $0428

main_loop:
        ; Draw a Mandelbrot set in Color RAM.
        JSR Mandelbrot
        INC $0428           ; Inc number of test runs.
        JMP main_loop       ; ...

        RTS                 ; Return from the subroutine (if ever).

; ==================== Mandelbrot ====================
; DDT's 6309 fast Mandelbrot routine.
; This is a fast 16bit fixed-point version derived from my 68000 version for the Amiga (1989).

; Can't store data here because we are in ROM
; These are commented-out in case one day we can build a program file.
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

; Bytes
var_bytes  = $8000
charx      = var_bytes
chary      = var_bytes+1
iter       = var_bytes+2
max_iter   = var_bytes+3

; Words
var_words  = var_bytes+4
color_ptr  = var_words+6
ax         = var_words+8
ay         = var_words+10
cx         = var_words+12
cy         = var_words+14 
incx       = var_words+16
incy       = var_words+18
zx         = var_words+20
zy         = var_words+22
zx2        = var_words+24
zy2        = var_words+26


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
        LDB #63
cpyspr: LDA ,X+
        STA ,Y+
        DECB
        BPL cpyspr
        ; }
        
        
        ; Set sprite pointers.
        LDQ #$C0C0C0C0
        STQ $7F8
        STQ $7FC
        
        ; Set sprite pointers.
        LDQ #$02030405
        STQ $D027
        LDQ #$06070809
        STQ $D02B
        
        ; Set sprite positions.
        LDA #$00
        STA $D010
        LDX #$D000
set_spr_pos:
        TFR X,A
        LSLA ; Mul by 8
        LSLA
        LSLA
        ADDA #70
        STA ,X+
        STA ,X+
        CMPX #$d016
        BNE set_spr_pos
        
        ; Enable all sprites.
        LDA #$FF
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

; ==================== 6309 NMI routine, which doubles as 6502 RESET routine ====================
        ORG $F8F0
;        ; The 6309 handler start with three apparently nonsensical instructions, which are actually 6502 instructions used in case the ROM is used with a 6502.
;                    ; CODE  | 6502 ASM
;                    ;------------------
;        INCA        ; $4C   | JMP $F601
;        NOP         ; $01   | ^
;        LDA #$00    ; $F6   | ^
;                    ; $00   | BRK (not used)
;        
 
        ; TODO: NMI handler
        
        RTI


        
; ==================== CPU VECTORS ====================

        ORG $FFF0 ; Illegal Opcode and Division by Zero Trap (exception)
        
        ORG $FFF2 ; Third software interrupt (SWI3 instruction)
        
        ORG $FFF4 ; Second software interrupt (SWI2 instruction)
        
        ORG $FFF6 ; Fast interrupt (FIRQ* line)
        
        ORG $FFF8 ; Interrupt (IRQ* line)
        
        ORG $FFFA ; Software interrupt (SWI instruction)
;        FDB $F8F0 ;   6309: $F8F0 IRQ vector
;                  ;   6502: $F0F8 NMI vector
        
        ORG $FFFC ; Non-maskable interrupt (NMI* line).
        FDB $F8F0 ;   6309: $F8F0 NMI vector
                  ;   6502: $F0F8 RESET vector
        
        ORG $FFFE ; Processor reset (RESET* line)
        FDB $E000 ;   6309: $E000 reset vector
                  ;   6502: $00E0 IRQ/BRK vector
                  
