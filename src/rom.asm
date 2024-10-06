; Commodore-6309 test ROM
; 
; Revision history [authors in square brackets]:
;   2024-09-24: First simple test loop. [DDT]
        
        PRAGMA 6309
        
        ORG   $E000         ; Replace the C64 8KB KERNAL ($E000-$FFFF).
        
        ;LDA #$01  ; White
        ;STA $D020 ; Border color

 init:      
; Init CIA2   --------------------

        SETDP $DD
        LDA #$DD
        FDB $1F8B  ; TFR A, DP

        LDA #$03
        STA <$00 ; Set default bank 0 for VIC-II

; Init VIC-II --------------------
        SETDP $D0
        LDA #$D0
        FDB $1F8B  ; TFR A, DP

        LDA #$1B
        STA <$11 ; CR1
        
        LDA #$00
        STA <$15 ; Sprite enable

        LDA #$C8
        STA <$16 ; CR2

        LDA #$14
        STA <$18 ; Mem ptrs

        ;LDA #$00
        ;STA $D019 ; Interrupts register

        LDA #$00  ; Disable all.
        STA <$1A ; Interrupts enabled (normal maskable IRQ).

        LDA #$02  ; Red
        STA <$20 ; Border color

        LDA #$01  ; White
        STA <$21 ; Background color 0
        
        ;JMP init
      
        ; Clear screen (and some more :-) with all spaces.
;        LDX #$00
;        LDA #$20      ; Space
loop_cls:
;        STA $0400, X
;        STA $0500, X
;        STA $0600, X
;        STA $0700, X
;        INX
;        BNE loop_cls ; Print till zero term or max 256 chars.
      
        ; Just print all screen codes.
        
        LDX   #$400         ; Point to screen memory.
        LDA   #$00          ; Start with the value 0
Loop:   STA   ,X+           ; Store the value of A in the memory location pointed to by X
        INCA                ; Increment A to get the next value (0 to 255)
        CMPA  #$FF          ; Check if A has reached 256 (which would wrap to 0)
        BNE   Loop          ; If A is not 256, repeat the loop

Foreva: JMP Foreva

        RTS                 ; Return from the subroutine        


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