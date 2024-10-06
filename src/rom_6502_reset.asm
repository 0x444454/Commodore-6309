        * = $F0F8
        
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
          BEQ foreva
          STA $0400, X
          LDA #$00      ; Black.
          STA $D800, X  ; Set color RAM.
          INX
          BNE loop_prt  ; Print till zero term or max 256 chars.

        ; Clear screen
        
foreva:   JMP foreva

msg_6502:
.enc "screen"   ;use screen encoding
        .text "6510 detected. insert 6309.", $00
.enc "none"     ;normal encoding
