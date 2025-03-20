; 6510 code for the Commodore-6309 ROM.
; https://github.com/0x444454/Commodore-6309
;
; Use 64TASS Assembler.
l
; Revision history [authors in square brackets]:
;   2024-09-24: First simple test loop. [DDT]
;   2025-01-03: Use joy-2 input for more flexible tests.
;   2025-03-19: Changed entry point in 6309 Kernal. Stripped some unneeded stuff.

BUILD_ROM = 1

.if BUILD_ROM
        * = $FBFD   ; Use to build ROM for 6309 Kernal in 6510 mode.
.else        
        * = $C000   ; Use for testing this code as a PRG.
.endif        
        
        SEI         ; Disable interrupts.
        
        LDA #$7F
        STA $DC0D ; CIA1: Disable all interrupts.
        LDA $DC0D ; CIA1: Ack pending interrupts.
        STA $DD0D ; CIA2: Disable all interrupts.        
        LDA $DD0D ; CIA2: Ack pending interrupts.
        
        ; Init CIA2
        LDA #$03
        STA $DD00 ; Set default bank 0 for VIC-II
        
        ; Init VIC-II
        AND $D011
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
    
        LDA #$00  ; Disable all.
        STA $D01A ; Interrupts enabled (normal maskable IRQ).
    
        LDA #$02  ; Red
        STA $D020 ; Border color
    
        LDA #$01  ; White
        STA $D021 ; Background color 0
        
        ; Clear screen (and some more :-) with all spaces.
        LDX #$00
        LDA #$20      ; Space
loop_cls:
        STA $0400, X
        STA $0500, X
        STA $0600, X
        STA $0700, X
        INX
        BNE loop_cls ; Print till zero term or max 256 chars.
      
        ; Just print a message.
        
        LDX #$00
loop_prt:
        LDA msg_6502,X
        BEQ done_prt
        STA $0400, X
        LDA #$00      ; Black.
        STA $D800, X  ; Set color RAM.
        INX
        BNE loop_prt  ; Print till zero term or max 256 chars.
done_prt:

        ; Sprite test
        JSR Sprites_test
            
foreva:
;
;        ; Check joystick input on control port 2.
;read_joy:        
;        LDA $DC00           ; Read CIA1:PRA. This is [xxxFRLDU]. All signals active low.
;        AND #$1F            ; Get only joy-2 actions.
;        TAX
;        AND #$10            ; Check if FIRE pressed.
;        BEQ read_joy        ; If FIRE pressed, pause.
;        
;        TXA                 ; Retreive read value.
;        CMP #$1F            ; Check for any joy input.
;        BEQ end_input       ; No joy input.
;        
;        ; Process joy input (active low).
;        TXA
;        AND #$01            ; Check if UP.
;        BNE no_U            ; UP not pressed.
;        ; UP
;        JMP end_input
;no_U:   TXA
;        AND #$02            ; Check if DOWN.
;        BNE no_D            ; DOWN not pressed.
;        ; DOWN
;        JMP end_input
;no_D:   TXA
;        AND #$04            ; Check if LEFT.
;        BNE no_L            ; LEFT not pressed.
;        ; LEFT
;        JMP end_input
;no_L:   TXA
;        AND #$08            ; Check if RIGHT.
;        BNE no_R            ; RIGHT not pressed.
;        ; RIGHT
;.if BUILD_ROM
;        ; Copy code to $00E0 (IRQ vector destination in ROM).
;        LDX #irq_6502_end - irq_6502
;cpy_irq_handler:
;        LDA irq_6502,X
;        STA $00E0,X
;        DEX
;        BNE cpy_irq_handler
;.else
;        ; Intercept OS IRQ vector.
;        LDA #<irq_6502      ; set interrupt vectors, pointing to interrupt service routine below
;        STA $0314
;        LDA #>irq_6502
;        STA $0315
;.endif
;        LDA #$01            ; Enable VIC-II raster interrupt.
;        STA $D01A
;        CLI                 ; Enable IRQ.
;        JMP end_input
;no_R:   
;
end_input:        
        JSR wait_no_joy     ; Wait for no joy input.
        JMP foreva

; ==================== wait_no_joy ====================
; Wait for no joystick input on control port 2.
wait_no_joy:
        LDA $DC00
        AND #$1F
        CMP #$1F
        BNE wait_no_joy
        RTS


;===========================================================
; Print A as a hex number on the upper-right corner of the screen.
; NOTE: Registers are preserved.

print_A_hex:
        PHA         ; Save A.
        PHA         ; Save A.
        
        LSR
        LSR
        LSR
        LSR
        CMP #$0A
        BCS pA_alpha_0
        ; Not alpha, i.e. [0..9]
        ADC #$30 + 9
pA_alpha_0:    
        SEC
        SBC #9
        STA $426    ; Print high nibble.

nxt_nibble:
        PLA         ; Restore A.
        AND #$0F
        CMP #$0A
        BCS pA_alpha_1
        ; Not alpha, i.e. [0..9]
        ADC #$30 + 9
pA_alpha_1:    
        SEC
        SBC #9
        STA $427    ; Print lo nibble.

end_pAh:    
        PLA         ; Restore A.
        RTS


; ==================== IRQ routine ====================
; NOTE: The 6502 does not save A,X,Y when calling the IRQ service routine.

irq_6502:
        ; Save used registers.
        PHA
        TXA
        PHA

        INC $D020
        LDA $D019
        ;JSR print_A_hex
        AND #$01            ; Check for raster interrupt.
        BEQ no_raster
        
        ; Handle raster interrupts.

        LDA #$7
        STA $D020

        LDX #$20
irq_delay:
        DEX
        BNE irq_delay
        

        LDA #$0
        STA $D020

        LDA #$01            ; Acknowledge raster interrupt.
        ;ORA $D019
        STA $D019

no_raster:
        ; Handle other interrupts.
        
        
        ; Retreive used registers.
        PLA
        TAX
        PLA

.if BUILD_ROM        
        RTI
.else        
        JMP $ea31
.endif
irq_6502_end:


msg_6502:
.enc "screen"   ;use screen encoding
        .text "6510 detected. insert 6309 with adapter.", $00
.enc "none"     ;normal encoding

; ==================== Sprites test ====================
Sprites_test:
        ; Copy sprite to VIC-II mem ($3000)
        LDX #62
cpy_spr_data:
        LDA sprite_def, X
        STA $3000, X
        DEX
        BPL cpy_spr_data
        
        ; Set sprite pointers colors.
        LDX #7
set_spr_ptr:
        LDA #$C0        ; Set sprite ptr to $C0 (sprite data starts at $3000)
        STA $7F8, X
        TXA             ; Set sprite color.
        ADC #$2         ;   Avoid white.
        STA $D027, X
        DEX
        BPL set_spr_ptr
        
        ; Set sprite positions.
        LDA #$00
        STA $D010       ; Sprites x-coords bit 8.
        LDX #14
set_spr_pos:
        TXA
        ASL ; Mul by 8
        ASL
        ASL
        ADC #70
        STA $D000, X
        STA $D001, X
        DEX
        DEX
        BPL set_spr_pos
        
        ; Enable all sprites.
        LDA #$FF
        STA $D015
        
        RTS

sprite_def:
        .byte %00000000,%11111111,%00000000
        .byte %00000011,%11111111,%11000000
        .byte %00000110,%00000000,%01100000
        .byte %00001100,%00000000,%00110000
        .byte %00011000,%00000000,%00011000
        .byte %00110000,%00000000,%00001100
        .byte %01100000,%00000000,%00000110
        .byte %11001110,%11100100,%11110011
        .byte %11001000,%10001100,%10010011
        .byte %11001110,%11100100,%10010011
        .byte %11001010,%00100100,%10010011
        .byte %11001010,%00100100,%10010011
        .byte %11001110,%11101110,%11110011
        .byte %11000000,%00000000,%00000011
        .byte %01100000,%00000000,%00000110
        .byte %00110000,%00000000,%00001100
        .byte %00011000,%00000000,%00011000
        .byte %00001100,%00000000,%00110000
        .byte %00000110,%00000000,%01100000
        .byte %00000011,%11111111,%11000000
        .byte %00000001,%11111111,%00000000

; ==================== Mandelbrot test ====================
; TODO: ...
