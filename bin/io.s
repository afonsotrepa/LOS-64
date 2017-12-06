[Bits 64]
%include "macros.s"

global outb
outb:
	push rdx

	mov dx, di ;port is 2 bytes
	mov al, sil ;output is one byte
	out dx, al

	pop rdx
	ret

global inb
inb:
	push rdx

	mov dx, di ; move the port number into dx
	xor rax, rax ; clear rax
	in al, dx ; using al instead of ax or rax cuz I want to read a byte

	pop rdx
	ret
