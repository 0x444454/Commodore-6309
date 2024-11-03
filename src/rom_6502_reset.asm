; 6510 code for the Commodore-6309 ROM.
; https://github.com/0x444454/Commodore-6309

        * = $F0F8   ; Use to build ROM for 6309 Kernal in 6510 mode.
        ;* = $C000   ; Use for testing this code as a PRG.
        
        ; Init CIA2
          LDA #$03
          STA $DD00 ; Set default bank 0 for VIC-II
        
        ; Init VIC-II
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
      
        ; Clear screen (and some more :-) with all spaces.
          LDX #$00
          LDA #$20      ; Space
loop_cls: STA $0400, X
          STA $0500, X
          STA $0600, X
          STA $0700, X
          INX
          BNE loop_cls ; Print till zero term or max 256 chars.
      
        ; Just print a message.
        
          LDX #$00
loop_prt: LDA msg_6502,X
          BEQ done_prt
          STA $0400, X
          LDA #$00      ; Black.
          STA $D800, X  ; Set color RAM.
          INX
          BNE loop_prt  ; Print till zero term or max 256 chars.
done_prt:

          ; Sprite test
          JSR Sprites_test
        
foreva:   JMP foreva


msg_6502:
.enc "screen"   ;use screen encoding
        .text "6510 detected. insert 6309 with adatper.", $00
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
        STA $D010
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
