[Bits 64]
%include "macros.s"
global enable_irq
enable_irq:
	ENTER
	
	;Load existing mask
	in al, 0xA1
	xchg al, ah
	in al, 0x21
	
	;Clear the relevant bit
	mov rbx, 1
	shl bx, cl
	not bx
	and ax, cx
	
	;Set the new mask
	out 0x21, al
	xchg al, ah
	in al, 0xA1

	LEAVE
	ret


global disable_irq
disable_irq:
	ENTER
	
	;Load existing mask
	in al, 0xA1
	xchg al, ah
	in al, 0x21
	
	;Set the relevant bit
	mov rbx, 1
	shl bx, cl
	or ax, cx
	
	;Set the new mask
	out 0x21, al
	xchg al, ah
	in al, 0xA1

	LEAVE
	ret

global load_idt
load_idt:
	;Tell CPU about IDT
	lidt [rdi]
	ret

;; ISR's:
global empty_isr
empty_isr:
	PUSHALL
	extern empty_handler
	call empty_handler
	POPALL
	iretq

global empty_isr1
empty_isr1:
	PUSHALL
	extern empty_handler1
	call empty_handler1
	POPALL
	iretq

global empty_isr2
empty_isr2:
	PUSHALL
	extern empty_handler2
	call empty_handler2
	POPALL
	iretq

global pit_isr
pit_isr:
	PUSHALL
	extern pit_handler
	call pit_handler
	POPALL
	iretq

global kbd_isr
kbd_isr:
	push rax
	push rdx
	;;inb(0x60) (read from master pic)
	mov dx, 0x60
	xor rax, rax
	in al, dx
	;;if (scan_code == 0xE0 || scan_code == 0xE1)
	lea edx, [rax-0xE0]
	cmp dx, 1
	jl .end
	shl rax, 8
	;;read (from master pic) again
	mov dx, 0x60
	in al, dx
.end:	extern scancode_in ;defined in kbd.h
	mov [scancode_in], rax
	;;outb(0x20, 0x20) (eoi to master pic)
	mov dx, 0x20
	mov al, 0x20
	out dx, al

	pop rdx
	pop rax
	iretq
