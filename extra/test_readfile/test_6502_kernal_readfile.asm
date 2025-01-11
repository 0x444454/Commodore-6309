; Test KERNAL binary file load, with and without Kernal mapped in mem.
; This is the Kernal code that needs to be ported to 6309 for disk-based kickstarting.
; Project page:
; https://github.com/0x444454/Commodore-6309 
;
; Kernal comments are based on Michael Steil's work: https://github.com/androdev4u/c64rom/blob/master/c64disasm
;
; How to use:
; - Define USE_KERNAL as required.
; - Use 64TASS Assembler.
; - Load program and run.
; - Program will load the first file ("*") found on disk starting from address $8000.
; - Wait until load completed or error (nothing will happen on screen).
; - After loading complete/error: See result code in upper-right corner of screen; loaded size in upper-left corner of screen.
;
; Revision history [authors in square brackets]:
;   2025-01-11: First version. [DDT]

USE_KERNAL = 0 ; Set to 1 to use KERNAL routines. Set to 0 to test our extracted code in RAM.

.if USE_KERNAL
KERNAL_SETLFS   = $FFBA
KERNAL_SETNAM   = $FFBD
KERNAL_OPEN     = $FFC0
KERNAL_CLRCHN   = $FFCC
KERNAL_CHKIN    = $FFC6
KERNAL_CLOSE    = $FFC3
KERNAL_CHRIN    = $FFCF
KERNAL_READST   = $FFB7
KERNAL_LOAD     = $FFD5
.endif

  * = $0801
  .word end_BASIC
  .word 10
  .byte $9e ; SYS
  .text "2061", $00
end_BASIC:
  .word 0


main:  
            LDA #filename_end - filename
            LDX #<filename
            LDY #>filename
.if USE_KERNAL            
            JSR KERNAL_SETNAM
.else            
            JSR raw_SETNAM
.endif
            
            LDA #2                ; Logical file number [1..255].
            LDX #8                ; Device [0..31]. Use device 8 (primary disk drive).
            LDY #0                ; Secondary address: Command 0 = relocated load.
.if USE_KERNAL            
            JSR KERNAL_SETLFS
.else
            JSR raw_SETLFS
.endif
            
            ;LDX #$02                ; Use file #2 for input
            ;JSR KERNAL_CHKIN        ; Set input to file

LOAD_ADDR = $8000
            ; Load file one byte at a time.
;            JSR KERNAL_OPEN         ; Open file.
;            BCS error_open          ; Error if carry set.
;read_byte:  JSR KERNAL_READST       ; Read status byte.
;            BEQ got_byte
;            CMP #$40
;            BEQ found_EOF           ; Either EOF or read error.
;            BNE error_read
;got_byte:   JSR KERNAL_CHRIN        ; Read next byte.
;            JSR print_A_hex
;            JMP read_byte

.if USE_KERNAL    
            ; Load file with KERNAL ROM.
            LDX #<LOAD_ADDR
            LDY #>LOAD_ADDR
            LDA #0                   ; 0 = Load, 1 = Verify
            JSR KERNAL_LOAD
.else
            ; Load file without KERNAL ROM (i.e. using only our extracted routines in RAM).
            ; For proper no-kernal load test, we disable all ROMs and only leave RAM + I/O space.
            SEI                     ; Disable interrupts for this critical section.
            LDA $01                 ; Get processor port value.
            AND #$F8                ; Don't mess with tape stuff (we don't support tape).
            ORA #$05                ; We only want RAM and I/O mapped. Kiss ROMs goodbye.
            STA $01                 ; Set processor port.
            ; LOAD seems to need IRQ for timings, so we setup the 6502 IRQ vector in RAM @ $FFFE (remember: ROM is gone).
            LDA #<irq_handler
            STA $FFFE
            LDA #>irq_handler
            STA $FFFF
            CLI                     ; Re-enable interrupts after this critical section.
            
            ; Do raw LOAD.
            LDX #<LOAD_ADDR
            LDY #>LOAD_ADDR
            LDA #0                   ; 0 = Load, 1 = Verify
            JSR raw_LOAD
.endif
            
            ; X/Y = ptr last byte loaded
            ; C = Set if error (A = error code).
            BCS error_read
            JMP found_EOF
            

found_EOF:
            ; Read completed.
            ; Print OK in the upper-left corner.
            .enc "screen"
            LDA #'o'
            STA $400
            LDA #'k'
            STA $401
            .enc "none"
            ; Output number of bytes read in the upper-right corner.
            TXA
            SEC
            SBC #<LOAD_ADDR
            TYA
            SBC #>LOAD_ADDR
            JSR print_A_hex
            LDA $0426
            STA $0424
            LDA $0427
            STA $0425
            TXA
            SEC
            SBC #<LOAD_ADDR
            JSR print_A_hex
            ; Stall here forever.
forever:    JMP forever
            RTS


            ; ERROR: Open.
error_open: 
            JSR print_A_hex
            .enc "screen"
            LDA #'e'
            STA $400
            LDA #'r'
            STA $401
            .enc "none"
            RTS

            ; ERROR: Read.
error_read: 
            JSR print_A_hex
            .enc "screen"
            LDA #'e'
            STA $400
            LDA #'r'
            STA $401
            .enc "none"
            RTS

error_illegal_device_num:
            JSR print_A_hex
            .enc "screen"
            LDA #'e'
            STA $400
            LDA #'n'
            STA $401
            .enc "none"
            RTS

error_missing_file_name:
            JSR print_A_hex
            .enc "screen"
            LDA #'e'
            STA $400
            LDA #'f'
            STA $401
            .enc "none"
            RTS

error_device_not_present:
            JSR print_A_hex
            .enc "screen"
            LDA #'e'
            STA $400
            LDA #'p'
            STA $401
            .enc "none"
            RTS

error_file_not_found:
            JSR print_A_hex
            .enc "screen"
            LDA #'e'
            STA $400
            LDA #'u'
            STA $401
            .enc "none"
            RTS

filename:   ;.text 'KICK'
            .text '*'
filename_end:            


;===========================================================
; Raw SETNAM routine, same as Kernal SETNAM call.
raw_SETNAM:
                STA $B7         ; set file name length
                STX $BB         ; set file name pointer low byte
                STY $BC         ; set file name pointer high byte
                RTS
            

;===========================================================
; Raw SETLFS routine, same as Kernal SETLFS call.
raw_SETLFS:
                STA $B8         ; save the logical file
                STX $BA         ; save the device number
                STY $B9         ; save the secondary address
                RTS  
                
                
;===========================================================
; Raw LOAD routine, same as Kernal LOAD call.

raw_LOAD: ; Was $F49E
                STX $C3                 ; set kernal setup pointer low byte
                STY $C4                 ; set kernal setup pointer high byte
                ;JMP ($0330)            ; do LOAD vector, usually points to $F4A5
; Was $F4A5
                STA $93                 ; save load/verify flag
                LDA #$00                ; clear A
                STA $90                 ; clear the serial status byte
                LDA $BA                 ; get the device number of current file
                BNE no_keyb             ; if not the keyboard (device 0) then continue
do_error_illegal_device_num:
                JMP error_illegal_device_num ; else do 'illegal device number' and return.
no_keyb:                
                CMP #$03                ; Check if device is monitor (screen). 
                BEQ do_error_illegal_device_num ; If monitor, do 'illegal device number' and return.
                
                BCC tape_load           ; device is < 3  
                
                LDY $B7                 ; get file name length
                BNE ok_filename         ; if not null name, skip error
                JMP error_missing_file_name ; else do 'missing file name' error and return
ok_filename:
                LDX $B9                 ; get the secondary address
                ;JSR $F5AF              ; print "Searching..."
                LDA #$60        
                STA $B9                 ; save the secondary address
                JSR send_sec_addr_and_filename ; send secondary address and filename
                LDA $BA                 ; get the device number
                JSR command_serial_bus_to_talk ; command serial bus device to TALK
                LDA $B9                 ; get the secondary address
                JSR send_sec_addr_after_TALK ; send secondary address after TALK
                JSR recv_serial_byte    ; input byte from serial bus
                STA $AE                 ; save program start address low byte
                LDA $90                 ; get the serial status byte
                LSR                     ; shift time out read ..
                LSR                     ; .. into carry bit
                BCS file_not_found ; if timed out go do file not found error and return
                JSR recv_serial_byte    ; input byte from serial bus
                STA $AF                 ; save program start address high byte
                TXA                     ; copy secondary address
                BNE no_load_location    ; load location not set in LOAD call, so continue with the load
                LDA $C3                 ; get the load address low byte
                STA $AE                 ; save the program start address low byte
                LDA $C4                 ; get the load address high byte
                STA $AF                 ; save the program start address high byte
no_load_location:
                ;JSR $F5D2               ; display "loading" or "verifying"
try_get_serial_byte:
                LDA #$FD                ; mask xxxx xx0x, clear time out read bit
                AND $90                 ; mask the serial status byte
                STA $90                 ; set the serial status byte
                ;JSR scan_stop_key       ; scan stop key, return Zb = 1 = [STOP]
                ;BNE get_serial_byte     ; if not [STOP] go ??
                ;JMP $F633               ; else close the serial bus device and flag stop
get_serial_byte:
                JSR recv_serial_byte    ; input byte from serial bus
                TAX                     ; copy byte
                LDA $90                 ; get the serial status byte
                LSR                     ; shift time out read ..
                LSR                     ; .. into carry bit
                BCS try_get_serial_byte ; if timed out go try again
                TXA                     ; copy received byte back
                LDY $93                 ; get load/verify flag
                BEQ do_load             ; if load go load
                                        ; else is verify
                LDY #$00                ; clear index
                CMP ($AE),Y             ; compare byte with previously loaded byte
                BEQ inc_save_ptr_L      ; if match go ??
                LDA #$10                ; flag read error
                JSR or_into_serial_status_byte ; OR into the serial status byte
                JMP inc_save_ptr_L
do_load:
                STA ($AE),Y             ; save byte to memory
inc_save_ptr_L:
                INC $AE                 ; increment save pointer low byte
                BNE after_inc_save_ptr_H ; if no rollover skip high byte inc
                INC $AF                 ; else increment save pointer high byte
after_inc_save_ptr_H:
                BIT $90                 ; test the serial status byte
                BVC try_get_serial_byte ; loop if not end of file
                                        ; close file and exit
                JSR send_cmd_UNTALK     ; command serial bus to UNTALK
                JSR close_serial_bus_device ; close serial bus device
                BCC u_return_ok         ; if success, go flag ok and exit
file_not_found:
                JMP error_file_not_found ; do file not found error and return

tape_load:                  ; Was $F533 (*** ??)
                 JMP error_file_not_found ; TAPE LOAD UNIMPLEMENTED.
;                LSR             
;                BCS u_ok0       
;                JMP $F713       ; else do 'illegal device number' and return
;u_ok0:
;                JSR $F7D0       ; get tape buffer start pointer in XY
;                BCS u_ok1       ; if ??
;                JMP $F713       ; else do 'illegal device number' and return
;u_ok1:
;                JSR $F817       ; wait for PLAY
;                BCS u_return    ; exit if STOP was pressed
;                JSR $F5AF       ; print "Searching..."
;get_fn_len:
;                LDA $B7         ; get file name length
;                BEQ u_find_tape_hdr ; No file name.
;                JSR $F7EA       ; find specific tape header
;                BCC u_ok2       ; if no error continue
;                BEQ u_return    ; exit if ??
;                BCS file_not_found
;u_find_tape_hdr:
;                JSR $F72C       ; find tape header, exit with header in buffer
;                BEQ u_return    ; exit if ??
;                BCS file_not_found
;u_ok2:
;                LDA $90         ; get the datassette/serial status byte
;                AND #$10        ; mask 000x 0000, read error
;                SEC             ; flag fail
;                BNE u_return    ; if read error just exit
;                CPX #$01        
;                BEQ u_ok3       
;                CPX #$03        
;                BNE get_fn_len
;u_label_0:       
;                LDY #$01        
;                LDA ($B2),Y     ; Fetch from tape buffer.
;                STA $C3         
;                INY             
;                LDA ($B2),Y     
;                STA $C4         
;                BCS u_label_1
;u_ok3:   
;                LDA $B9         ; get the secondary address
;                BNE u_label_0  
;u_label_1:     
;                LDY #$03        
;                LDA ($B2),Y     
;                LDY #$01        
;                SBC ($B2),Y     
;                TAX             
;                LDY #$04        
;                LDA ($B2),Y     
;                LDY #$02        
;                SBC ($B2),Y     
;                TAY             
;                CLC             
;                TXA             
;                ADC $C3         
;                STA $AE         
;                TYA             
;                ADC $C4         
;                STA $AF         
;                LDA $C3         
;                STA $C1         ;set I/O start addresses low byte
;                LDA $C4         
;                STA $C2         ; set I/O start addresses high byte
;                ;JSR $F5D2       ; display "LOADING" or "VERIFYING"
;                JSR $F84A       ; do the tape read
;                .byte $24       ; makes next line BIT $18, keep the error flag in Cb
;                
u_return_ok:
                CLC             ; flag ok
                LDX $AE         ; get the LOAD end pointer low byte
                LDY $AF         ; get the LOAD end pointer high byte
u_return:
                RTS             



;--------------------------------------------------
; send secondary address and filename
send_sec_addr_and_filename: ; Was $F3D5
                LDA $B9             ; get the secondary address
                BMI exit_F3D3       ; ok exit if -ve
                LDY $B7             ; get file name length
                BEQ exit_F3D3       ; ok exit if null
                LDA #$00            ; clear A
                STA $90             ; clear the serial status byte
                LDA $BA             ; get the device number
                JSR command_serial_bus_to_listen ; command devices on the serial bus to LISTEN
                LDA $B9             ; get the secondary address
                ORA #$F0            ; OR with the OPEN command
                JSR send_sec_addr_after_LISTEN ; send secondary address after LISTEN
                LDA $90             ; get the serial status byte
                BPL device_present  ; if device present skip the 'device not present' error
                PLA                 ; else dump calling address low byte
                PLA                 ; dump calling address high byte
                JMP error_device_not_present ; do 'device not present' error and return
device_present: 
                LDA $B7             ; get file name length
                BEQ do_unlisten     ; branch if null name
                LDY #$00            ; clear index
nxt_fn_byte:
                LDA ($BB),Y         ; get file name byte
                JSR send_serial_byte ; output byte to serial bus
                INY                 ; increment index
                CPY $B7             ; compare with file name length
                BNE nxt_fn_byte     ; loop if not all done
do_unlisten:
                ; command serial bus to UNLISTEN and return
                JSR send_cmd_UNLISTEN ; command serial bus to UNLISTEN
                CLC                 ; flag ok
                RTS

exit_F3D3:
                CLC                 ; Flag ok.
                RTS


;===========================================================
; Close the serial bus device and flag stop
close_serial_bus_device_and_flag_stop: ; Was $F633
                JSR close_serial_bus_device ; close serial bus device
                LDA #$00        
                SEC             ; flag stop
                RTS             

;===========================================================
; Close the serial bus device and flag stop
close_serial_bus_device: ; Was $F642
                BIT $B9         ; test the secondary address
                BMI label_F657  ; if already closed just exit
                LDA $BA         ; get the device number
                JSR command_serial_bus_to_listen ; command devices on the serial bus to LISTEN
                LDA $B9         ; get the secondary address
                AND #$EF        ; mask the channel number
                ORA #$E0        ; OR with the CLOSE command
                JSR send_sec_addr_after_LISTEN ; send secondary address after LISTEN
                JSR send_cmd_UNLISTEN ; command serial bus to UNLISTEN
label_F657:
                CLC             ; flag ok
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
                PHA             ; save device address
                BIT $94         ; test deferred character flag
                BPL no_deferred ; if no deferred character continue
                SEC             ; else flag EOI
                ROR $A3         ; rotate into EOI flag byte
                JSR tx_byte     ; Tx byte on serial bus
                LSR $94         ; clear deferred character flag
                LSR $A3         ; clear EOI flag
no_deferred:
                PLA             ;restore the device address

                ; *** defer a command
                                
                STA $95         ; save as serial defered character
                SEI             ; disable the interrupts
                JSR set_ser_data_out_high ; set the serial data out high
                CMP #$3F        ; compare read byte with $3F
                BNE not_3F      ; branch if not $3F, this branch will always be taken as after CIA2's PCR is read it is ANDed with $DF, so the result can never be $3F ??
                JSR set_ser_clock_out_high ; set the serial clock out high
not_3F:
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                ORA #$08        ; mask xxxx 1xxx, set serial ATN low
                STA $DD00       ; Save CIA2 DRA, serial port and video address. If the code drops through to here the serial clock is low and the serial data has been released so the following code will have no effect apart from delaying the first byte by 1ms.

                ; *** Set the serial clk/data, wait and Tx byte on the serial bus.
set_ser_clkdata_wait_and_tx_byte: ; Was $ED36
                SEI             ; disable the interrupts
                JSR set_ser_clock_out_low ; set the serial clock out low
                JSR set_ser_data_out_high ; set the serial data out high
                JSR delay_1ms   ; 1ms delay

                ; *** Tx byte on serial bus
tx_byte: ; Was $ED40         
                SEI             ; disable the interrupts
                JSR set_ser_data_out_high ; set the serial data out high
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCS dev_not_present ; if the serial data is high go do 'device not present'
                JSR set_ser_clock_out_high ; set the serial clock out high
                BIT $A3         ; test the EOI flag
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
                CMP $DD00       ; compare it with itself
                BNE check_dra_change ; if changed go try again
                ASL             ; shift the serial data into Cb
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
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                AND #$DF        ; mask xx0x xxxx, set the serial data out high
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
                AND #$02        ; mask 0000 00x0, timer A interrupt
                BNE ser_bus_timeout ; if timer A interrupt go do serial bus timeout
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BCS wait_ser_data_lo ; if the serial data is high go wait some more
                CLI             ; enable the interrupts
                RTS             ; All done.

                ; ERROR: Device not present
dev_not_present: ; Was $EDAD          
                LDA #$80        ; error $80, device not present
;.:EDAF 2C       .BYTE $2C      ; [original Kernal] makes next line BIT $03A9
                JMP label_EDB2  ; [readable Kernal]
ser_bus_timeout:
                LDA #$03        ; error $03, read timeout, write timeout
label_EDB2:
                JSR or_into_serial_status_byte ; OR into the serial status byte
                CLI             ; enable the interrupts
                CLC             ; clear for branch
                BCC label_EE03  ; ATN high, delay, clock high then data high, *** branch always ***
                ;JMP send_sec_addr_after_LISTEN ; TODO: Is this needed.

;===========================================================
; Send secondary address after LISTEN
send_sec_addr_after_LISTEN: ; Was $EDB9
                STA $95         ; save the defered Tx byte
                JSR set_ser_clkdata_wait_and_tx_byte ; set the serial clk/data, wait and Tx the byte
                JMP set_serial_ATN_high

;===========================================================
; Send secondary address after TALK
send_sec_addr_after_TALK: ; Was $EDC7
                STA $95         ; save the defered Tx byte
                JSR set_ser_clkdata_wait_and_tx_byte ; set the serial clk/data, wait and Tx the byte
                ; Wait for the serial bus end after send return address from patch 6.
                SEI             ; disable the interrupts
                JSR set_ser_data_out_low ; set the serial data out low
                JSR set_serial_ATN_high ; set serial ATN high
                JSR set_ser_clock_out_high ; set the serial clock out high
ssaa_loop:
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BMI ssaa_loop   ; loop if the clock is high
                CLI             ; enable the interrupts
                RTS              


;===========================================================
; Set serial ATN high
set_serial_ATN_high: ; Was $EDBE
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                AND #$F7        ; mask xxxx 0xxx, set serial ATN high
                STA $DD00       ; save CIA2 DRA, serial port and video address
                RTS    

;===========================================================
; Send a byte to the serial bus
send_serial_byte: ; Was $EDDD
                BIT $94         ; test the deferred character flag
                BMI label_EDE6  ; if there is a deferred character go send it
                SEC             ; set carry
                ROR $94         ; shift into the deferred character flag
                BNE label_EDEB  ; save the byte and exit, branch always
label_EDE6:
                PHA             ; save the byte
                JSR tx_byte     ; Tx byte on serial bus
                PLA             ; restore the byte
label_EDEB:
                STA $95         ; save the defered Tx byte
                CLC             ; flag ok
                RTS


;===========================================================
; Command serial bus to UNTALK
send_cmd_UNTALK: ; Was $EDEF
                SEI             ; disable the interrupts
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
                                
                ; 1ms delay, clock high then data high
do_1ms_delay_ch_dh:
                TXA             ; save the device number
                LDX #$0A        ; short delay
label_EE09:
                DEX             ; decrement the count
                BNE label_EE09  ; loop if not all done
                TAX             ; restore the device number
                JSR set_ser_clock_out_high ; set the serial clock out high
                JMP set_ser_data_out_high ; set the serial data out high and return

;===========================================================
; Receive a byte from the serial port.
recv_serial_byte: ; Was $EE13
                ; Input a byte from the serial bus
                SEI             ; disable the interrupts
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
                AND #$02        ; mask 0000 00x0, timer A interrupt
                BNE label_EE3E  ; if timer A interrupt go ??
                JSR get_ser_data_status_in_C ; get the serial data status in Cb
                BMI label_EE30  ; loop if the serial clock is low
                BPL label_EE56  ; else go set 8 bits to do, branch always
                                ; timer A timed out
label_EE3E:
                LDA $A5         ; get the serial bus bit count
                BEQ label_EE47  ; if not already EOI then go flag EOI
                LDA #$02        ; else error $02, read timeour
                JMP $EDB2       ; set the serial status and exit
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
                CMP $DD00       ; compare it with itself
                BNE label_EE5A  ; if changing go try again
                ASL             ; shift the serial data into the carry
                BPL label_EE5A  ; loop while the serial clock is low
                ROR $A4         ; shift the data bit into the receive byte
label_EE67:
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                CMP $DD00       ; compare it with itself
                BNE label_EE67  ; if changing go try again
                ASL             ; shift the serial data into the carry
                BMI label_EE67  ; loop while the serial clock is high
                DEC $A5         ; decrement the serial bus bit count
                BNE label_EE5A  ; loop if not all done
                JSR set_ser_data_out_low ; set the serial data out low
                BIT $90         ; test the serial status byte
                BVC label_EE80  ; if EOI not set skip the bus end sequence
                JSR do_1ms_delay_ch_dh ; 1ms delay, clock high then data high
label_EE80:
                LDA $A4         ; get the receive byte
                CLI             ; enable the interrupts
                CLC             ; flag ok
                RTS             


;===========================================================
; Set the serial clock out high
set_ser_clock_out_high: ; Was $EE85
                LDA $DD00       ; read CIA2 DRA, serial port and video address
                AND #$EF        ; mask xxx0 xxxx, set serial clock out high
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
                AND #$DF        ; mask xx0x xxxx, set serial data out high
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
                CMP $DD00       ; compare it with itself
                BNE get_ser_data_status_in_C ; if changing got try again
                ASL             ;shift the serial data into Cb
                RTS   

;===========================================================
; Delay 1ms
delay_1ms: ; Was $EEB3
                TXA             ; save X
                LDX #$B8        ; set the loop count
d1ms_loop:
                DEX             ; decrement the loop count
                BNE d1ms_loop   ; loop if more to do
                TAX             ; restore X
                RTS  

;===========================================================
; Check RS232 bus idle
check_RS232_bus_idle: ; Was $F0A4
                PHA              ; save A
                LDA $02A1        ; get the RS-232 interrupt enable byte
                BEQ ser_interrupts_not_enabled  ; if no interrupts enabled just exit
label_F0AA:
                LDA $02A1        ; get the RS-232 interrupt enable byte
                AND #$03         ; mask 0000 00xx, the error bits
                BNE label_F0AA   ; if there are errors loop
                LDA #$10         ; disable FLAG interrupt
                STA $DD0D        ; save VIA 2 ICR
                LDA #$00         ; clear A
                STA $02A1        ; clear the RS-232 interrupt enable byte
ser_interrupts_not_enabled:
                PLA              ; restore A
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
                LDX #$00        ; clear X
                INC $A2         ; increment the jiffy clock low byte
                BNE label_F6A7  ; if no rollover ??
                INC $A1         ; increment the jiffy clock mid byte
                BNE label_F6A7  ; branch if no rollover
                INC $A0         ; increment the jiffy clock high byte
                                ; now subtract a days worth of jiffies from current count
                                ; and remember only the Cb result
label_F6A7:
                SEC             ; set carry for subtract
                LDA $A2         ; get the jiffy clock low byte
                SBC #$01        ; subtract $4F1A01 low byte
                LDA $A1         ; get the jiffy clock mid byte
                SBC #$1A        ; subtract $4F1A01 mid byte
                LDA $A0         ; get the jiffy clock high byte
                SBC #$4F        ; subtract $4F1A01 high byte
                BCC label_F6BC  ; if less than $4F1A01 jiffies skip the clock reset
                                ; else ..
                STX $A0         ; clear the jiffy clock high byte
                STX $A1         ; clear the jiffy clock mid byte
                STX $A2         ; clear the jiffy clock low byte
                                ; this is wrong, there are $4F1A00 jiffies in a day so
                                ; the reset to zero should occur when the value reaches
                                ; $4F1A00 and not $4F1A01. this would give an extra jiffy
                                ; every day and a possible TI value of 24:00:00
label_F6BC:                                
                LDA $DC01       ; read CIA1 DRB, keyboard row port
                CMP $DC01       ; compare it with itself
                BNE label_F6BC  ; loop if changing
                TAX             
                BMI label_F6DA       
                LDX #$BD        ; set c6
                STX $DC00       ; save CIA1 DRA, keyboard column drive
label_F6CC:
                LDX $DC01       ; read CIA1 DRB, keyboard row port
                CPX $DC01       ; compare it with itself
                BNE label_F6CC  ; loop if changing
                STA $DC00       ; save CIA1 DRA, keyboard column drive
                INX             
                BNE label_F6DC       
label_F6DA:
                STA $91         ; save the stop key column
label_F6DC:
                RTS 

;===========================================================
; IRQ handler
irq_handler: ; Was $FF48
                ; Save registers.
                PHA             ; save A
                TXA             ; copy X
                PHA             ; save X
                TYA             ; copy Y
                PHA             ; save Y

                TSX             ; copy stack pointer
                LDA $0104,X     ; get stacked status register
                AND #$10        ; mask BRK flag
                BEQ not_BRK     ; branch if not BRK
                ; Handle BRK
handle_BRK:
                ; Was $FE66
stall_on_BRK:   JMP stall_on_BRK ; TODO: [delete this line] This is for debugging purposes only.                
                ; TODO: Properly handle BRK.
                JMP end_IRQ_handler
                
not_BRK:
                ; Handle system IRQ.
irq_system:      ; Was $EA31
                JSR inc_realtime_clock ; increment the real time clock
                
                ; We dont handle cursor (yet), so the following code has been commented-out.
                
;                LDA $CC         ; get the cursor enable, $00 = flash cursor
;                BNE label_no_cursors_flash ; if flash not enabled skip the flash
;                DEC $CD         ; decrement the cursor timing countdown
;                BNE label_no_cursors_flash ; if not counted out skip the flash
;                LDA #$14        ; set the flash count
;                STA $CD         ; save the cursor timing countdown
;                LDY $D3         ; get the cursor column
;                LSR $CF         ; shift b0 cursor blink phase into carry
;                LDX $0287       ; get the colour under the cursor
;                LDA ($D1),Y     ; get the character from current screen line
;                BCS label_EA5C  ; branch if cursor phase b0 was 1
;                INC $CF         ; set the cursor blink phase to 1
;                STA $CE         ; save the character under the cursor
;                JSR $EA24       ; calculate the pointer to colour RAM
;                LDA ($F3),Y     ; get the colour RAM byte
;                STA $0287       ; save the colour under the cursor
;                LDX $0286       ; get the current colour code
;                LDA $CE         ; get the character under the cursor
;label_EA5C:
;                EOR #$80        ; toggle b7 of character under cursor
;                JSR $EA1C       ; save the character and colour to the screen @ the cursor
label_no_cursors_flash:
                ; Handle tape motor.
;                LDA $01         ; read the 6510 I/O port
;                AND #$10        ; mask 000x 0000, the cassette switch sense
;                BEQ label_EA71  ; if the cassette sense is low skip the motor stop
;                                ; the cassette sense was high, the switch was open, so turn
;                                ; off the motor and clear the interlock
;                LDY #$00        ; clear Y
;                STY $C0         ; clear the tape motor interlock
;                LDA $01         ; read the 6510 I/O port
;                ORA #$20        ; mask xxxx xx1x, turn off the motor
;                BNE label_EA79  ; go save the port value, branch always
;                                ; the cassette sense was low so turn the motor on, perhaps
;label_EA71:                                
;                LDA $C0         ; get the tape motor interlock
;                BNE label_EA7B  ; if the cassette interlock <> 0 don't turn on motor
;                LDA $01         ; read the 6510 I/O port
;                AND #$1F        ; mask xxxx xx0x, turn on the motor
;label_EA79:
;                STA $01         ; save the 6510 I/O port
label_EA7B:
;                JSR $EA87       ; scan the keyboard
                LDA $DC0D       ; read CIA1 ICR, clear the timer interrupt flag
                
end_IRQ_handler:                
                ; Restore registers.
                PLA             ; pull Y
                TAY             ; restore Y
                PLA             ; pull X
                TAX             ; restore X
                PLA             ; restore A
                RTI             ; Return from IRQ.


         
;===========================================================
; Print A as a hex number on the upper-right corner of the screen.
; NOTE: Registers are preserved.

print_A_hex:
        PHA        ; Save A.
        PHA        ; Save A.
        
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
        STA nibble_char_h

nxt_nibble:
        PLA        ; Restore A.
        AND #$0F
        CMP #$0A
        BCS pA_alpha_1
        ; Not alpha, i.e. [0..9]
        ADC #$30 + 9
pA_alpha_1:    
        SEC
        SBC #9
        STA nibble_char_l
        
        ; Output.
        LDA nibble_char_h
        STA $426
        LDA nibble_char_l
        STA $427

end_pAh:    
        PLA        ; Restore A.
        RTS

nibble_char_h: .byte 0
nibble_char_l: .byte 0            

