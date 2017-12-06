[Bits 64]
%include "macros.s"
section .data
repl_start: db "REPL Initialize",0x0a, 0
section .text
global repl_initialize
repl_initialize:
extern alist_ptr
mov qword [alist_ptr], 47
ENTER
mov rdi, 0x1000
extern malloc
call malloc
extern heap_ptr
mov [heap_ptr], rax
mov r15, 47
mov rdi, repl_start
extern print
call print
push RDI
mov RDI, 12
push RSI
mov RSI, 19983
extern MAKE_STRING
call MAKE_STRING
pop RSI
pop RDI
push rax
xor rax, 011b
add rax, 8
inc rax
mov byte [rax], 73
inc rax
mov byte [rax], 76
inc rax
pop rax
push RDI
mov RDI, rax
extern MAKE_SYMBOL
call MAKE_SYMBOL
pop RDI
push RDI
mov RDI, rax
push RDI
mov RDI, 0
push RSI
mov RSI, 4
extern EQ
call EQ
pop RSI
pop RDI
push RSI
mov RSI, rax
extern SET
call SET
pop RSI
pop RDI
push RDI
mov RDI, 4
push RSI
mov RSI, 29711
extern MAKE_STRING
call MAKE_STRING
pop RSI
pop RDI
push RDI
mov RDI, rax
extern MAKE_SYMBOL
call MAKE_SYMBOL
pop RDI
push RDI
mov RDI, rax
push RDI
mov RDI, 4
push RSI
mov RSI, 4
extern EQ
call EQ
pop RSI
pop RDI
push RSI
mov RSI, rax
extern SET
call SET
pop RSI
pop RDI
;;Compiled code starts here
push RDI
mov RDI, 16
push RSI
mov RSI, 21519
extern MAKE_STRING
call MAKE_STRING
pop RSI
pop RDI
push rax
xor rax, 011b
add rax, 8
inc rax
mov byte [rax], 69
inc rax
mov byte [rax], 83
inc rax
mov byte [rax], 84
inc rax
pop rax
push RDI
mov RDI, rax
push RSI
mov RSI, 47
extern MAKE_SYMBOL
call MAKE_SYMBOL
pop RSI
pop RDI
push RDI
mov RDI, rax
jmp G536.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G536:
ENTER
push rdi
push rsi
mov rax, 1284
jmp ..@G537
..@G537:
pop rsi
pop rdi
LEAVE
ret
.end: mov rax, G536
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
pop rsi
pop rdi
;;end of list
mov [G536-8], rax
pop rax
push RSI
mov RSI, rax
extern SET
call SET
pop RSI
pop RDI
mov rax, 47
push rax
push RDI
mov RDI, 16
push RSI
mov RSI, 21519
extern MAKE_STRING
call MAKE_STRING
pop RSI
pop RDI
push rax
xor rax, 011b
add rax, 8
inc rax
mov byte [rax], 69
inc rax
mov byte [rax], 83
inc rax
mov byte [rax], 84
inc rax
pop rax
push RDI
mov RDI, rax
push RSI
mov RSI, 47
extern MAKE_SYMBOL
call MAKE_SYMBOL
pop RSI
pop RDI
push RDI
mov RDI, rax
extern SYMBOL_VALUE
call SYMBOL_VALUE
pop RDI
mov rdi, rax
pop rsi
extern FUNCALL
call FUNCALL
;;Compiled code ends here
LEAVE
ret
