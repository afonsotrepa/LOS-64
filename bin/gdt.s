[Bits 32]
; Declare constants for the multiboot header.
MBALIGN  equ  1<<0		; align loaded modules on page boundaries
MEMINFO  equ  1<<1				; provide memory map
FLAGS	 equ  MBALIGN | MEMINFO ; this is the Multiboot 'flag' field
MAGIC	 equ  0x1BADB002	; 'magic number' lets bootloader find the header
CHECKSUM equ -(MAGIC + FLAGS)	; checksum of above, to prove we are multiboot
 
; Declare a multiboot header that marks the program as a kernel. These are magic
; values that are documented in the multiboot standard. The bootloader will
; search for this signature in the first 8 KiB of the kernel file, aligned at a
; 32-bit boundary. The signature is in its own section so the header can be
; forced to be within the first 8 KiB of the kernel file.
section .multiboot
align 4
	dd MAGIC
	dd FLAGS
	dd CHECKSUM

; This is the GDT table pre-filled with the entries required to make the entire address space accessible 
;	from user and kernel mode for both data and code.
gdt_contents:
  db 0, 0, 0, 0, 0, 0, 0, 0	   ; Offset: 0	- Null selector - required 
  db 255, 255, 0, 0, 0, 0x9A, 0xCF, 0  ; Offset: 8 - KM Code selector - covers the entire 4GiB address range
  db 255, 255, 0, 0, 0, 0x92, 0xCF, 0  ; Offset: 16 - KM Data selector - covers the entire 4GiB address range


; Size - Change if adding/removing rows from GDT contents
; Size = Total bytes in GDT - 1
gdt_pointer db 23, 0, 0, 0, 0, 0

GDT64:                           ; Global Descriptor Table (64-bit).
	.Null: equ $ - GDT64         ; The null descriptor.
	dw 0                         ; Limit (low).
	dw 0                         ; Base (low).
	db 0                         ; Base (middle)
	db 0                         ; Access.
	db 0                         ; Granularity.
	db 0                         ; Base (high).
	.Code: equ $ - GDT64         ; The code descriptor.
	dw 0                         ; Limit (low).
	dw 0                         ; Base (low).
	db 0                         ; Base (middle)
	db 10011010b                 ; Access (exec/read).
	db 00100000b                 ; Granularity.
	db 0                         ; Base (high).
	.Data: equ $ - GDT64         ; The data descriptor.
	dw 0                         ; Limit (low).
	dw 0                         ; Base (low).
	db 0                         ; Base (middle)
	db 10010010b                 ; Access (read/write).
	db 00000000b                 ; Granularity.
	db 0                         ; Base (high).
	.Pointer:                    ; The GDT-pointer.
	dw $ - GDT64 - 1             ; Limit.
	dq GDT64                     ; Base.

global load_gdt
load_gdt:
	cli
	; Fill in GDT Pointer structure
	mov dword [gdt_pointer + 2], gdt_contents
	; Tell CPU about GDT
	mov dword eax, gdt_pointer
	lgdt [eax]
	jmp load_segments


load_segments:

	mov dword eax, 0x10  ; Load data segment selector for KM Data Segment descriptor
	mov word ds, eax	 ; Load data segment selector registers
	mov word es, eax
	mov word fs, eax
	mov word gs, eax
	mov word ss, eax
	; Force reload of code segment
	jmp 8:pic_remap	; Far jump to next line of code which causes CS reload.

pic_remap:
	; Remap IRQs 0-7   to ISRs 32-39
	; and	IRQs 8-15  to ISRs 40-47

	;cli ; Disable maskable interrupts (unnecessary since it's already called in load_gdt
		
	; Interrupt Vectors 0x20 for IRQ 0 to 7 
	;and 0x28 for IRQ 8 to 15
	mov al, 0x11	; INIT command (ICW1 + ICW4)
	out 0x20, al	; Send INIT to PIC1
	out 0xA0, al	; Send INIT to PIC2

	mov al, 0x20	; PIC1 interrupts start at 0x20
	out 0x21, al	; Send the value to PIC1 DATA
	mov al, 0x28	; PIC2 interrupts start at 0x28
	out 0xA1, al	; Send the value to PIC2 DATA

	mov al, 0x04	; MASTER identifier
	out 0x21, al	; set PIC1 as MASTER
	mov al, 0x02	; SLAVE identifier
	out 0xA1, al	; set PIC2 as SLAVE

	mov al, 0x01	; This is the x86 mode code for both 8259 PIC chips
	out 0x21, al	; Set PIC1 mode
	out 0xA1, al	; Set PIC2 mode
	
	mov ax, 0xFFFF	; Set interrupt mask to disable all interrupts
	out 0x21, al	; Set mask of PIC1_DATA
	xchg al, ah	; Switch low and high byte of the mask so we can send the high byte
			;	In this particular case, 0xFF is switched with 0xFF so it has no consequence
	out 0xA1, al	; Set mask of PIC2_DATA

	;sti		; Enable maskable interrupts
	nop		; Required - STI takes effect after the next instruction runs

	jmp 8:enter_long_mode

enter_long_mode:
	;;;;;;;;
	mov eax, cr0	; Set the A-register to control register 0.
	xor eax, 0 << 31; Clear the PG-bit, which is bit 31.
	mov cr0, eax	; Set control register 0 to the A-register.

	mov edi, 0x1000 ; Set the destination index to 0x1000.
	mov cr3, edi    ; Set control register 3 to the destination index.
	xor eax, eax    ; Nullify the A-register.
	mov ecx, 4096   ; Set the C-register to 4096.
	rep stosd       ; Clear the memory.
	mov edi, cr3    ; Set the destination index to control register 3.

	mov DWORD [edi], 0x2003
	add edi, 0x1000
	mov DWORD [edi], 0x3003
	add edi, 0x1000
	mov DWORD [edi], 0x4003
	add edi, 0x1000

	mov ebx, 0x00000003 ; Set the B-register to 0x00000003.
	mov ecx, 512 ; Set the C-register to 512.

.SetEntry:
	mov DWORD [edi], ebx
	add ebx, 0x1000 ; Add 0x1000 to the B-register.
	add edi, 8 	; Add eight to the destination index.
	loop .SetEntry  ; Set the next entry.

	mov eax, cr4	; Set the A-register to control register 4.
	or eax, 1 << 5	; Set the PAE-bit, which is the 6th bit (bit 5).
	mov cr4, eax  	; Set control register 4 to the A-register.

	mov ecx, 0xC0000080 ; Set the C-register to EFER MSR.
	rdmsr		; Read from the model-specific register.
	or eax, 1 << 8	; Set the LM-bit which is the 9th bit (bit 8).
	wrmsr           ; Write to the model-specific register.

    	mov eax, cr0	; Set the A-register to control register 0.
    	or eax, 1 << 31	; Set the PG-bit, which is the 32nd bit (bit 31).
	;; crashes here:
    	mov cr0, eax   	; Set control register 0 to the A-register.

	lgdt [GDT64.Pointer] ; Load the 64-bit global descriptor table.
	extern _start
	jmp GDT64.Code:_start ; Set the code segment and enter 64-bit long mode.
