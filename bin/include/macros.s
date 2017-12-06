%macro ENTER 0
	push rbp
	mov rbp, rsp
	push rbx
	push r12
	push r13
	push r14
	push r15
%endmacro
%macro LEAVE 0
	pop r15
	pop r14
	pop r13
	pop r12
	pop rbx
	mov rsp, rbp
	pop rbp
%endmacro

%macro PUSHALL 0
	push rax
	push rcx
	push rdx
	push rbx
	push rsp
	push rbp
	push rsi
	push rdi
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
%endmacro
%macro POPALL 0
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop rdi
	pop rsi
	pop rbp
	pop rsp
	pop rbx
	pop rdx
	pop rcx
	pop rax
%endmacro

%macro SAVE 0
	push rdi
	push rsi
	push rdx
	push rcx
	push r8
	push r9
%endmacro
%macro LOAD 0
	pop r9
	pop r8
	pop rcx
	pop rdx
	pop rsi
	pop rdi
%endmacro
