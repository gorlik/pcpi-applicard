;; **************************************************************************
;; PCPI boot rom V9
;; **************************************************************************

reloc_addr:	equ 8000h
rom_size:	equ 800h

;; I/O ports
SEND:	equ 0h
RECV:	equ 20h
DRDY:	equ 40h
ROMEN:	equ 60h
CTC0:	equ 80h
CTC1:	equ 81h
RAMBNK:	equ 0C0h

;; other constants
sp_high:	equ 0fff9h
sp_low:	        equ 07ff9h
sign_loc:	equ 0fffeh

;; variables used by diags
tmp_size:	equ 0fffbh
tmp_addr:	equ 0fffdh
tmp_sum:	equ 0ffffh

		org 	0
                jr      call_main

;; **************************************************************************
vers_data:
                db	5Ah ; Z
                db	38h ; 8
                db	30h ; 0
                db	9               ; ROM Version
                dw	2F0Fh           ; serial number LSBs
                dw	0h              ; serial number MSBs

;; **************************************************************************
	;; this will alter the init flow by setting c to 1 or 2
	;; can't figure out where this is called from
                ld      hl, 1000h
                ld      c, 1
                jr      skip_13
                ld	c, 2
skip_13:
                push    hl
                pop     ix
                ld      de, init_alt_c
                add     ix, de
        	jp      (ix)

;; **************************************************************************
zero:           db	0
copyright_msg:  db 	"** Copyright (c) 1982 Personnel Computer Products, Inc.  **"
                db 	2
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0
                db	0

;; **************************************************************************
;; the NMI code must be at 0x66
		org 	66h
NMI:
	;; push 2 and jump to init_card
                ld      sp, sp_high
                ld      a, 2
                push    af
                ld      hl, init_card
                push    hl
                ei
        	retn

;; **************************************************************************
call_main:                              ; called by the reset vector at 0x0000
                ld      a, 1
                jr      main_entry

;; **************************************************************************
author_msg:     db 	">> Winthrop L. Saville III and Raymond Klein - authors <<"

;; **************************************************************************
main_entry:                             ; celled from call_main
                ld      b, 1Fh
                ld      c, 0DFh

bank_loop:
                out     (c), b
                dec     c
                djnz    bank_loop       ; cycle trhough all RAM banks
                out     (c), b          ; set RAM bank 0

check_init_signature:
                ld      sp, sp_high
                push    af              ; push 1 on the stack
                ld      a, 7Ah ; 'z'
                out     (SEND), a          ; send 0x7A to apple
                ld      hl, (sign_loc)
                ld      de, 55ABh
                add     hl, de
                ld      a, h
                or      l
                jp      nz, init_card
                pop     af
        	jp      main_loop

;; **************************************************************************
init_card:                                 ; called from main_entry
                ld      a, 31h ; '1'
                out     (SEND), a          ; send 0x31 to apple
                ld      a, 32h ; '2'
        	out     (SEND), a          ; send 0x32 to apple

	;;  compute rom checksum
                ld      hl, 0		   ; start address to checksum
        	ld      c, 3
init_alt_c:	
                ld      de, rom_size-1  ; checksum is at last location of ROM, skip it in actual calculation
                ld      b, 0

checksum_loop:
                ld      a, b
                add     a, (hl)
                ld      b, a
                inc     hl
                dec     de
                ld      a, d
                or      e
                jr      nz, checksum_loop
                ld      (hl), b         ; checksum is in b and rom is enabled - why write?
                ld      a, (hl)         ; get checksum from 0x07ff
                cp      b               ; compare values
                push    af              ; push comparison result on the strack
                ld      a, c
                cp      3
                jr      z, labl_FC      ; take the jump in normal init (c==3) 
                pop     hl              ; 
                cp      2		; c!=3 if called from the code around labl_13
                ret     z 		; return to caller if c==2
                rst     38h             ; in this case (c==1) the ROM has been relocated to 0x1000. No idea what is at 0x38 

labl_FC:
                pop     af              ; get checksum compare result from stack
                jr      z, checksum_ok
                ld      a, (zero)
                or      a
                jp      z, init_fail

checksum_ok:
                pop     af
                cp      1	     	; a == 1 if called from normal init (reset)
                jp      z, main_loop    ; jmp if called from normal init
                ld      a, 33h ; '3'    ; fall through if called from NMI
                out     (SEND), a       ; send 0x33 to apple
                ld      hl, 0		; copy rom to 0x8000 
                ld      de, reloc_addr
                ld      bc, rom_size
                ldir
                jp      reloc_addr+next1 ;	811Eh ; jump to next inst in RAM
next1:	        ld      a, 0
        	out     (ROMEN), a        ; disable ROM
	; write 0x43, 0x44, 0x45... pattern to memory
                ld      c, 0		  
                ld      hl, 0             ; and compute the checksum in c
                ld      de, reloc_addr
                ld      b, 43h ; 'C'
labl_12C:
                ld      a, c
                add     a, b
                ld      c, a
                ld      (hl), b
                inc     b
                inc     hl
                dec     de
                ld      a, e
                or      d
                jr      nz, labl_12C     ; write & checksum loop
                ld      hl, reloc_addr	 ; add rom copy checksum to c
                ld      de, rom_size

labl_13D:
                ld      a, c
                add     a, (hl)
                ld      c, a
                inc     hl
                dec     de
                ld      a, e
                or      d
        	jr      nz, labl_13D     ; loop rom copy checksum
	; fill the rest of the memory with 0x43, 0x44, ... and keep adding the checksum
                ld      b, 43h ; 'C'
labl_148:
                ld      a, c
                add     a, b
                ld      c, a
                ld      (hl), b
                inc     b
                inc     hl
                ld      a, h
                or      l
                jr      nz, labl_148     ; loop checksum to top of memory
	; delay 0x8000 
        	ld      hl, reloc_addr
labl_155:
                dec     hl
                ld      a, h
                or      l
                jr      nz, labl_155     ; delay loop
	; checksum the entire memory
        	ld      hl, 0
                ld      b, 0

labl_15F:
                ld      a, b
                add     a, (hl)
                ld      b, a
                inc     hl
                ld      a, h
                or      l
                jr      nz, labl_15F     ; loop checksum
                ld      a, b
                cp      c	         ; compare against expected checksum in c
                jr      nz, init_fail    ; jump to init_fail if checksum does not match
                ld      a, 34h ; '4'
                out     (SEND), a          ; send 0x34 to apple
                jp      reloc_addr+next2	; 8172h           ; jump to next instr at 0x8000
next2:	        ld      a, 0
                out     (ROMEN), a        ; disable ROM
                ld      hl, 0
                call    test_mem+reloc_addr ; 	81B9h  test_mem
                ld      hl, reloc_addr	    ; copy 0x8000 to 0x0
                ld      de, 0
                ld      bc, rom_size
                ldir                       ; do the copy
	        ;; compute checksum of new copy at 0x0
                ld      hl, 0
                ld      b, 0
                ld      de, rom_size-1

labl_18F:
                ld      a, b
                add     a, (hl)
                ld      b, a
                inc     hl
                dec     de
                ld      a, d
                or      e
                jr      nz, labl_18F     ; checksum loop
                ld      a, b
                cp      (hl)	         ; compare checksum with last byte of ROM
                jr      nz, init_fail
                jp      next4            ; switch execution to copy at 0x0
next4:
                ld      sp, sp_low
                ld      hl, reloc_addr
                call    test_mem
                ld      sp, sp_high
                jp      detect_ctc


;; **************************************************************************

init_fail:
                di
                ld      hl, 0FFFFh

forever:
                in      a, (0E0h)
                ld      a, (hl)
                nop
                ld      (hl), a
                jr      forever

;; **************************************************************************

test_mem:
                ld      de, 8000h ; memory size to check (32k)
check_loc:                                ; CODE XREF: test_mem+28↓j
                ld      a, 55h ; 'U'
                ld      (hl), a
                cp      (hl)
                jr      nz, init_fail
                ld      a, 0AAh
                ld      (hl), a
                cp      (hl)
                jr      nz, init_fail
                ld      a, 0FFh
                ld      (hl), a
                cp      (hl)
                jr      nz, init_fail
                ld      a, 0
                ld      (hl), a
                cp      (hl)
                jr      nz, init_fail
                ld      a, 1
	;; walking bit test
wbit_loop:
                ld      (hl), a
                cp      (hl)
                jr      nz, init_fail
                rlca
                jr      nc, wbit_loop
                inc     hl	; check for done
                dec     de
                ld      a, d
                or      e
                jr      nz, check_loc
                ret

;; **************************************************************************

detect_ctc:
                ld      a, 35h ; '5'
                out     (SEND), a          ; send 0x35 to apple
                ld      a, 0
                out     (CTC0), a        ; set CTC ch0 interrupt vector to 0
                ld      a, 27h ; '''
                out     (CTC0), a        ; CTC ch0 IRQ off, prescaler 256, falling edge, auto mode, Time reg follows, sw reset
                ld      a, 0FFh
                out     (CTC0), a        ; set CTC conter to 255
                ld      b, 0

ctc0_retry_fd:
                in      a, (CTC0)        ; read CTC ch0
                cp      0FDh
                jp      z, ctc0_wait_fc
                nop
                nop
                nop
                inc     b
                jp      nz, ctc0_retry_fd
                jp      ctc_detect_end

ctc0_wait_fc:
                ld      b, 0

ctc0_retry_fc:
                in      a, (CTC0)        ; read CTC ch0
                cp      0FDh
                jp      z, ctc0_still_fd
                cp      0FCh
                jp      z, ctc_detected
                jp      ctc_detect_end

ctc0_still_fd:
                inc     b
                jp      nz, ctc0_retry_fc
                jp      ctc_detect_end

ctc_detected:
                ld      a, 36h ; '6'
                out     (SEND), a          ; send 0x36 to apple
                im      2               ; initialize interrupts
                ld      a, 90h
                ld      i, a            ; set IRQ vector at 0x9000
                ld      hl, 9000h

irq_loop:
                ld      (hl), 0AEh      ; set all IRQ vectors to point to init_fail
                inc     l
                ld      (hl), 1
                inc     l
                jp      nz, irq_loop
                ld      hl, CTC_IRQ
                ld      (9002h), hl     ; set IRQ vector 1 to point to CTC_IRQ
                ld      de, 0
                ld      a, 87h
                out     (CTC1), a        ; CTC ch1 IRQ on, prescaler 16, falling edge, auto mode, Time reg follows, sw reset
                ld      a, 80h
                out     (CTC1), a        ; CTC ch1 set time to 128
                ei
                ld      hl, 514h

ctc_delay:
                dec     hl
                ld      a, h
                or      l
                jp      nz, ctc_delay
                di
                ld      a, 7
                out     (CTC1), a        ; CTC ch1 IRQ off, prescaler 16, falling edge, auto mode, Time reg follows, sw reset
                ld      a, 0
                out     (CTC1), a        ; CTC ch1 set timer to 256
                ld      hl, 0Ah
                sbc     hl, de
                jp      p, init_fail

ctc_detect_end:
                ld      a, 1
                out     (ROMEN), a        ; enable ROM
                jp      main_loop


;; **************************************************************************

CTC_IRQ:
                inc     de
                ei
        	reti


;; **************************************************************************

main_loop:
                ld      hl, 0AA55h      ; set init signature
                ld      (sign_loc), hl
                ld      a, 5Ah ; 'Z'
                out     (SEND), a          ; send 0x5a to apple

wait_cmd_loop:
                ld      de, 0A0h

labl_279:
        	ld      hl, 0

labl_27C:
                in      a, (40h)
                rla
                jr      c, parse_cmd
                dec     hl
                ld      a, h
                or      l
                jr      nz, labl_27C
                dec     de
                ld      a, d
                or      e
                jr      nz, labl_279
                jp      run_diag

parse_cmd:
                in      a, (20h)
                cp      6
                jr      nc, cmd_gt_6
                ld      l, a
                ld      h, 0
                add     hl, hl
                ld      de, jump_table
                add     hl, de
                call    jmp_hl_indirect
                jr      wait_cmd_loop


;; **************************************************************************

jmp_hl_indirect:
                ld      a, (hl)
                inc     hl
                ld      h, (hl)
                ld      l, a
                jp      (hl)


;; **************************************************************************

cmd_gt_6:
                cp      0F0h
                jr      nz, wait_cmd_loop
                call    test_mem_at_addr
                jp      wait_cmd_loop


;; **************************************************************************

jump_table:     dw send_version_info
                dw read_data_at_addr
                dw write_data_at_addr
                dw exec_at_addr
                dw write_port
                dw read_port


;; **************************************************************************

send_version_info:
                ld      hl, vers_data
                ld      b, 08h

send_ver_loop:
                ld      c, (hl)
                call    send_byte_c
                inc     hl
                djnz    send_ver_loop
                ret


;; **************************************************************************

read_data_at_addr:
                call    receive_word_de
                ex      de, hl
                call    receive_word_de

read_data_loop:
                ld      a, d
                or      e
                ret     z
                ld      c, (hl)
                call    send_byte_c
                inc     hl
                dec     de
                jr      read_data_loop


;; **************************************************************************

write_data_at_addr:
                call    receive_word_de
                ex      de, hl
                call    receive_word_de

write_data_loop:
                ld      a, d
                or      e
                ret     z
                call    receive_byte_a
                ld      (hl), a
                inc     hl
                dec     de
                jr      write_data_loop


;; **************************************************************************

exec_at_addr:
                call    receive_word_de
                ex      de, hl
                jp      (hl)


;; **************************************************************************

write_port:
                call    receive_byte_a
                ld      c, a
                call    receive_byte_a
                out     (c), a
                ret


;; **************************************************************************

read_port:
                call    receive_byte_a
                ld      c, a
                in      c, (c)
                call    send_byte_c
                ret


;; **************************************************************************

test_mem_at_addr:
                call    receive_word_de
                ld      (tmp_addr), de    ; mem address
                call    receive_word_de
                ld      (tmp_size), de    ; mem size
                ld      a, e
                or      d
                jp      z, test_mem_ret_ok
                ld      hl, (tmp_addr)
                ld      bc, (tmp_size)
                ld      a, 55h ; 'U'
                call    memset  ; write 0x55
                ld      hl, (tmp_addr)
                ld      bc, (tmp_size)
                ld      a, 55h ; 'U'
                call    memcheck
                ret     c
                ld      hl, (tmp_addr)
                ld      bc, (tmp_size)
                ld      a, 0AAh
                call    memset  ; write 0xaa
                ld      hl, (tmp_addr)
                ld      bc, (tmp_size)
                ld      a, 0AAh
                call    memcheck
                ret     c
                ld      hl, (tmp_addr)
                ld      bc, (tmp_size)

aad_write_loop:
                ld      (hl), l         ; write lsb of addr to memory
                inc     hl
                dec     bc
                ld      a, b
                or      c
                jr      nz, aad_write_loop
                ld      hl, (tmp_addr)
                ld      bc, (tmp_size)

aad_read_loop:
                ld      a, (hl)
                cp      l
                jr      nz, aad_error
                inc     hl
                dec     bc
                ld      a, b
                or      c
                jr      nz, aad_read_loop
                jr      test_mem_ret_ok


;; **************************************************************************

aad_error:
                ld      d, a
                ld      e, l
                call    send_mem_error_info
                inc     hl
                jr      nc, aad_read_loop
                ret     c

test_mem_ret_ok:
                ld      c, 0
                call    send_byte_c
                ret


;; **************************************************************************

memset:                         ; called from test_mem_at_addr
	;; HL = addr
	;; A = value
	;; BC = lenght
                ld      e, a
                ld      a, b
                or      c
                ret     z
                ld      (hl), e
                dec     bc
                ld      a, b
                or      c
                ret     z
                ld      d, h
                ld      e, l
                inc     de
                ldir
                ret


;; **************************************************************************

memcheck:                       ; called from test_mem_at_addr
	;; HL = addr
	;; A = value
	;; BC = lenght
	;; return: OK = carry clear, ERROR = carry set 
                ld      e, a
                ld      a, b
                or      c
                ret     z

memcheck_loop:
                ld      a, (hl)
                cp      e
                jr      nz, memcheck_error
                inc     hl
                dec     bc
                ld      a, b
                or      c
                jr      nz, memcheck_loop
                ret

memcheck_error:
                ld      d, a
                call    send_mem_error_info
                inc     hl
                jr      nc, memcheck_loop
                ret


;; **************************************************************************

send_mem_error_info:
                push    hl
                push    de
                push    bc
                ld      c, 1
                ld      a, (hl)         ; read fail location again
                cp      e               ; check against correct value
                jr      nz, skip1
                ld      c, 2

skip1:
                call    send_byte_c     ; send 1 if multiple errors, send 2 for single error
                ex      de, hl
                call    send_word_de    ; send error address
                ld      c, l
                call    send_byte_c     ; send expected read
                ld      c, h
                call    send_byte_c     ; send bad read
                call    receive_byte_a
                cp      0
                scf
                jr      nz, skip2       ; set carry (continue testing) if reply from apple !=0
                ccf

skip2:
                pop     bc
                pop     de
                pop     hl
                ret


;; **************************************************************************

receive_word_de:
                call    receive_byte_a
                ld      e, a
                call    receive_byte_a
                ld      d, a
                ret


;; **************************************************************************

send_word_de:
                push    bc
                ld      c, e
                call    send_byte_c
                ld      c, d
                call    send_byte_c
                pop     bc
                ret


;; **************************************************************************

receive_byte_a:
                in      a, (40h)
                rla
                jp      nc, receive_byte_a
                in      a, (20h)
                ret


;; **************************************************************************

send_byte_c:
                in      a, (40h)
                rra
                jp      c, send_byte_c
                ld      a, c
                out     (SEND), a
                ret


;; **************************************************************************

run_diag:
                ld      a, 42h ; 'B'
                out     (SEND), a          ; send 0x42 to apple
                ld      sp, 0EFFFh
                call    checksum_code
                ld      (tmp_sum), a     ; store 1-byte checksum to 0xffff
                ld      hl, 0           ; copy rom to 0x8000
                ld      de, reloc_addr
                ld      bc, code_size
                ldir
                jp      reloc_addr+next3 ;	840Ah           ; jump to next instr at 0x8000
next3:
                ld      a, 0
                out     (ROMEN), a        ; disable ROM
                ld      hl, reloc_addr  ; copy 0x8000 back to 0x0
                ld      de, 0
                ld      bc, code_size
                ldir
                jp      run_diag_2      ; go back to execute at 0x0
run_diag_2:
                call    checksum_code
                ld      hl, tmp_sum
                cp      (hl)	; compare the checksum with the one stored at 0xffff
                call    nz, diag_fail
                ld      (byte_521), a
                ld      sp, 541h
                ld      hl, byte_520
                ld      (hl), 55h ; 'U'
                dec     hl
                ld      (hl), 1
                ex      af, af'	; zero a'
                xor     a
                ex      af, af'

labl_437:
                call    labl_503
                call    labl_48E
                call    checksum_code
                ld      hl, byte_521
                cp      (hl)
                call    nz, diag_fail
                ld      hl, byte_520
                ld      b, (hl)
                ld      hl, 600h
                ld      de, 0FA00h

labl_451:
                ld      a, (hl)
                cp      b
                call    nz, diag_fail
                inc     hl
                dec     de
                ld      a, d
                or      e
                jr      nz, labl_451
                ld      hl, byte_51F
                inc     (hl)
                ld      a, (hl)
                and     2
                jr      nz, labl_437
                ld      hl, byte_520
                ld      a, (hl)
                cpl
                ld      (hl), a
                jr      labl_437


;; **************************************************************************

diag_fail:
                ld      hl, 0FFFFh

forever2:
                in      a, (0E0h)
                ld      a, (hl)
                nop
                ld      (hl), a
                jr      forever2


;; **************************************************************************

checksum_code:
                ld      hl, 0
                ld      de, code_size
                ld      b, 0

code_sum_loop:
                ld      a, (hl)
                ld      (hl), a
                add     a, b
                ld      b, a
                inc     hl
                dec     de
                ld      a, d
                or      e
                jr      nz, code_sum_loop
                ld      a, b
                or      a
                ret     nz
                cpl
                ret


;; **************************************************************************

labl_48E:
                ld      hl, 600h
                ld      de, 0FA00h
                ld      a, (byte_520)
                ld      b, a

loop_498:                                ;
                ld      (hl), b
                inc     hl
                dec     de
                ld      a, d
                or      e
                jp      nz, loop_498
                ld      hl, 600h
                ld      c, h
                ld      a, 0FAh
                push    af

labl_4A7:
                in      a, (0E0h)
                ld      l, 0
                ld      h, c
                ld      a, (byte_51F)
                ld      b, a

loop_4B0:                                ;
                ld      (hl), b
                inc     b
                inc     l
                jp      nz, loop_4B0
                call    labl_503
                call    delay_0x0b00
                call    labl_503
                call    delay_0x0b00
                ld      hl, 600h
                ld      de, 0FA00h

labl_4C8:
                ld      a, h
                cp      c
                jr      z, labl_4D6
                ld      a, (byte_520)
                cp      (hl)
                call    nz, diag_fail
                jp      labl_4DC

labl_4D6:
                ld      a, (hl)
                cp      b
                call    nz, diag_fail
                inc     b

labl_4DC:
                inc     hl
                dec     de
                ld      a, d
                or      e
                jp      nz, labl_4C8
                call    labl_503
                ld      l, 0
                ld      h, c
                ld      a, (byte_520)

labl_4EC:
                ld      (hl), a
                inc     l
                jp      nz, labl_4EC
                inc     c
                pop     af
                dec     a
                push    af
                jr      nz, labl_4A7
                pop     af
                ret


;; **************************************************************************

delay_0x0b00:
                ld      hl, 0B00h
delay_loop:
                dec     hl
                ld      a, h
                or      l
                jp      nz, delay_loop
                ret


;; **************************************************************************

labl_503:
                push    af
                push    bc
                ex      af, af'
                ld      c, a
                inc     a
                ex      af, af'
                ld      b, 80h

labl_50B:
                ld      a, c
                out     (SEND), a
                ld      a, 1
                out     (ROMEN), a        ; enable ROM
                ld      a, 0
                out     (ROMEN), a        ; disable ROM
                in      a, (DRDY)
                in      a, (RECV)
                djnz    labl_50B
                pop     bc
                pop     af
                ret


;; **************************************************************************

code_size:
byte_51F:       db	0
byte_520:       db	0
byte_521:       db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
        	db	0
		db	0
	
