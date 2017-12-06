%include "macros.s"
section .bss
align 8
;;An alist holding the dynamic variables
global alist_ptr
alist_ptr: resb 8
;;The heap to hold the lisp objects (ex: cons and strings)
;align 8
;global heap_min
;heap_min:
;;resb 0x10000
;resq 0x1000 ;reserve 0x1000 qwords (0x8000 bytes)
;global heap_max
;heap_max:

section .data
;;A pointer to a free location on the lisp heap
;global heap_ptr
;heap_ptr: dq heap_min

ADD_error: db "Number passed to ADD ins't of type fixnum", 0
SUB_error: db "Number passed to SUB ins't of type fixnum", 0
MUL_error: db "Number passed to MUL ins't of type fixnum", 0
EXPT_error: db "Baser or power passed to EXPT ins't of type fixnum", 0
DIV_error: db "Number or divisor passed to DIV ins't of type fixnum", 0
REM_error: db "Number or divisor passed to REM ins't of type fixnum", 0
LOG_error: db "Number or base passed to LOG ins't of type fixnum", 0
CAR_error: db "Cons passed to CAR ins't of type cons", 0
CDR_error: db "Cons passed to CDR ins't of type cons", 0
WRITE_CHAR_error: db "Char passed to WRITE_CHAR ins't of type char", 0
MAKE_STRING_length_error: db "Length passed to MAKE_STRING ins't of type fixnum", 0
MAKE_STRING_char_error: db "Char passed to MAKE_STRING ins't of type char", 0
WRITE_STRING_error: db "String passed to WRITE_STRING ins't of type string", 0
SET_CHAR_string_error: db "String passed to SET_CHAR ins't of type string", 0
SET_CHAR_index_error: db "Index passed to SET_CHAR ins't of type fixnum", 0
SET_CHAR_char_error: db "Char passed to SET_CHAR ins't of type char", 0
SET_CHAR_length1_error: db "Index passed to SET_CHAR is too big", 0
SET_CHAR_length2_error: db "Index passed to SET_CHAR is negative", 0
STRING_EQ_error: db "String passed to STRING_EQ ins't of type string", 0
MAKE_SYMBOL_error: db "Name passed to MAKE_SYMBOL ins't of type string", 0
SET_error: db "Symbol passed to SET ins't of type symbol", 0
SYMBOL_VALUE_error: db "Symbol passed to SYMBOL_VALUE ins't of type symbol", 0
SYMBOL_NAME_error: db "Symbol passed to SYMBOL_NAME ins't of type symbol", 0
SYMBOL_LEXICAL_VALUE_error: db \
	"Symbol passed to SYMBOL_LEXICAL_VALUE ins't of type symbol", 0
APPLY_function_error: db "Function passed to APPLY ins't of type function", 0
APPLY_args_error: db  "Args passed to APPLY ins't of type cons", 0
APPLY_args_short_error: db "Args passed to APPLY is too short", 0
APPLY_args_long_error: db "Args passed to APPLY is too long", 0
SETL_error: db "Symbol passed to SETL ins't of type symbol", 0
CHAR_string_error: db "String passed to CHAR ins't of type string", 0
CHAR_index_error: db "Index passed to CHAR ins't of type fixnum", 0
CHAR_length1_error: db "Index passed to CHAR is too big", 0
CHAR_length2_error: db "Index passed to CHAR is negative", 0
STRING_LENGTH_error: db "String passed to STRING_LENGTH isn't of type string", 0
SET_CAR_error: db "Cons passed to SET_CAR isn't of type cons", 0
SET_CDR_error: db "Cons passed to SET_CDR isn't of type cons", 0
GREATER_error: db "Number passed to GREATER isn't of type fixnum", 0
LESSER_error: db "Number passed to LESSER isn't of type fixnum", 0
lisp_alloc_overflow: db 10, "###Lisp heap overflow###", 10, 0

section .text
global ERROR ;(error_message)
ERROR:	push rdi
	mov rdi, 10
	extern put_char
	call put_char ;print a newline before the error message
	pop rdi
	call WRITE_STRING ;print the error message
	cli
	hlt

;;asm_error is meant to be called from assembly or C
global asm_error ;(error_message)
asm_error:
	push rdi
	mov rdi, 10
	call put_char ;print a newline before the error message
	pop rdi
	extern print
	call print ;print the error message
	cli
	hlt

global ATOM ;(object)
ATOM:	and rdi, 111b
	cmp rdi, 001b
	je .eq
	mov rax, 1<<2
	ret
.eq: 	mov rax, 47
	ret

global SYMBOLP ;(object)
SYMBOLP:
	and rdi, 111b
	cmp rdi, 101b
	jne .diff
	mov rax, 1<<2
	ret
.diff: 	mov rax, 47
	ret

global NUMBERP ;(object)
NUMBERP:
	and rdi, 11b
	cmp rdi, 00b
	jne .diff
	mov rax, 1<<2
	ret
.diff: 	mov rax, 47
	ret

global STRINGP ;(object)
STRINGP:
	and rdi, 111b
	cmp rdi, 011b
	jne .diff
	mov rax, 1<<2
	ret
.diff: 	mov rax, 47
	ret

global CHARP ;(object)
CHARP:	and rdi, 11111111b
	cmp rdi, 00001111b
	jne .diff
	mov rax, 1<<2
	ret
.diff: 	mov rax, 47
	ret

global FUNCTIONP ;(object)
FUNCTIONP:
	and rdi, 111b
	cmp rdi, 010b
	jne .diff
	mov rax, 1<<2
	ret
.diff: 	mov rax, 47
	ret

global ADD ;(fixnum1 fixnum2)
ADD:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	mov rax, rdi
	add rax, rsi
	ret

.error: mov rdi, ADD_error
	call asm_error

global SUB ;(fixnum1 fixnum2)
SUB:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	mov rax, rdi
	sub rax, rsi
	ret

.error: mov rdi, SUB_error
	call asm_error

global MUL ;(fixnum1 fixnum2)
MUL:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	shr rdi, 2
	shr rsi, 2
	imul rdi, rsi
	mov rax, rdi
	shl rax, 2
	ret

.error: mov rdi, MUL_error
	call asm_error

global EXPT ;(base power)
EXPT:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	shr rdi, 2
	shr rsi, 2

	extern expt ;defined in lib.c
	call expt
	shl rax, 2
	ret

.error: mov rdi, EXPT_error
	call asm_error

global DIV ;(number divisor)
DIV:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	shr rdi, 2
	shr rsi, 2
	mov rax, rdi
	cqo ;sign extend rax to rdx:rax
	idiv rsi
	shl rax, 2
	ret

.error: mov rdi, DIV_error
	call asm_error

global REM ;(number divisor)
REM:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	shr rdi, 2
	shr rsi, 2
	mov rax, rdi
	cqo ;sign extend rax to rdx:rax
	idiv rsi
	mov rax, rdx
	shl rax, 2
	ret

.error: mov rdi, REM_error
	call asm_error

global LOG ;(number base)
LOG:	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	jne .error

	shr rdi, 2
	shr rsi, 2
	extern log ;defined in lib.c
	call log
	shl rax, 2
	ret
	

.error: mov rdi, LOG_error
	call asm_error
global GREATER ;(n1 n2)
GREATER:
	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	je .cont
	mov rdi, GREATER_error
	call asm_error

.cont: 	cmp rdi, rsi
	jg .greater
	mov rax, 47
	ret
.greater:
	mov rax, 1<<2 ;T
	ret

global LESSER ;(n1 n2)
LESSER:
	mov rax, rdi
	or rax, rsi ;we can check both of them at the same time by oring them
	and rax, 11b
	cmp rax, 00b
	je .cont
	mov rdi, LESSER_error
	call asm_error

.cont:	cmp rdi, rsi
	jl .lesser
	mov rax, 47
	ret
.lesser:
	mov rax, 1<<2 ;T
	ret


;;size is the number of bytes needed (without a fixnum tag)
;;lisp_alloc is only meant to be called from asm or C
;;A faster implementation might use a register to stop cache r/w ([heap_ptr])
;global lisp_alloc ;(size)
;lisp_alloc:
;;;rdi: size; rsi: offset
;	mov rsi, 0 ;offset == 0
;	;;check if [heap_ptr]+rsi is anywhere in alist or r15 (root-set)
;.check:	push rdi
;	push rsi
;	mov rdi, [heap_ptr]
;	add rdi, rsi
;	;;test for overflow before calling gc_in_root
;	cmp rdi, heap_max
;	jge .of
;	mov rsi, r15 ;lexical enviroment
;	call gc_in_root
;	pop rsi
;	pop rdi
;	cmp rax, 47
;	jne .found
;	;;test in the alist if it's not in r15
;	push rdi
;	push rsi
;	mov rdi, [heap_ptr]
;	add rdi, rsi
;	mov rsi, [alist_ptr] ;alist (dynamic variables)
;	call gc_in_root
;	pop rsi
;	pop rdi
;	cmp rax, 47
;	jne .found
;	;;check if offset is greater or equal to size
;	cmp rsi, rdi
;	jge .end
;	;;if not, check the next qword
;	add rsi, 8
;	jmp .check
;
;.of:	mov qword [heap_ptr], heap_min ;reset the ptr
;	mov rsi, 0 ;reset the offset
;	;;debug print to warn about an overflow (not a crash)
;	push rdi
;	push rsi
;	mov rdi, lisp_alloc_overflow
;	call print
;	pop rsi
;	pop rdi
;	;;
;	jmp .check
;
;	;;jump here if [heap_ptr]+rsi is found in alist or r15
;.found:	add [heap_ptr], rsi ;move the ptr to the last tested location
;	mov rsi, 0 ;reset the offset
;	;;debug (seems like the problem is in this block of code (.found))
;	push rdi
;	push rsi
;	push rax
;	mov rdi, 'F'
;	call put_char
;	pop rax
;	pop rsi
;	pop rdi
;	;;
;	;;test if string or cons
;	mov rdx, rax
;	and rdx, 111b
;	cmp rdx, 001b ;cons tag
;	jne .str
;	add qword [heap_ptr], 16 ;16 == 2 qwords (cons)
;	jmp .check
;.str:	mov rax, [rax-011b] ;011b == string tag
;	shr rax, 2 ;remove the fixnum tag from the length
;	add rax, 8 ;add 8 to take into account the qword holding the length
;	;;debug
;	push rdi
;	push rsi
;	push rax
;	mov rdi, 'S'
;	call put_char
;	pop rax
;	pop rsi
;	pop rdi
;	;;
;	;;align rax by 3 bits (align 8)
;	mov rdx, rax ;rdx is caller saved, so there's no need to push it
;	and rdx, 111b
;	cmp rdx, 0 ;check if aligned
;	je .jmp ;if aligned: check the next area
;	or rax, 111b
;	add rax, 1b
;	;;check the next area
;.jmp:	add [heap_ptr], rax
;	jmp .check
;
;.end:	mov rax, [heap_ptr] ;ret the address of the first allocated byte/qword
;	add [heap_ptr], rsi ;inc the ptr by the allocated memory (last offset)
;	ret
	

global CONS ;(car cdr)
CONS:	;malloc_aligned crashes after being called too many times
	push rdi
	push rsi
	mov rdi, 16 ;2x qword
	extern lisp_alloc ;defined in mem.c
	call lisp_alloc
	pop rsi
	pop rdi

.end:	mov [rax], rdi ;set the car
	mov [rax+8], rsi ;set the cdr
	add  rax, 001b ;cons tag
	ret

global CAR ;(cons)
CAR:	mov rax, rdi
	and rax, 111b
	cmp rax, 001b
	jne .error

	mov rax, [rdi-1]
	ret

.error: mov rdi, CAR_error
	call asm_error

global CDR ;(cons)
CDR:	mov rax, rdi
	and rax, 111b
	cmp rax, 001b
	jne .error

	mov rax, [rdi+7]
	ret

.error: mov rdi, CDR_error
	call asm_error

;;EQ doesn't have any type checking, but it's behaviour is only defined for
;;immediates
global EQ ;(immediate1 immediate2)
EQ:	cmp rdi, rsi
	jne .diff
	mov rax, 1<<2 ;return fixnum 1
	ret
.diff:	mov rax, 00101111b ;return NIL
	ret

global WRITE_CHAR ;(char)
WRITE_CHAR:
	push rax
	mov rax, rdi
	and rax, 11111111b
	cmp rax, 00001111b
	jne .error

	shr rdi, 8
	extern put_char
	call put_char
	pop rax
	ret

.error:	mov rdi, WRITE_CHAR_error
	call asm_error

global MAKE_STRING ;(length char)
MAKE_STRING:
	mov rax, rdi
	and rax, 11b
	cmp rax, 00b
	je .char_check
	mov rdi, MAKE_STRING_length_error
	call asm_error
.char_check:
	mov rax, rsi
	and rax, 11111111b
	cmp rax, 00001111b
	je .cont
	mov rdi, MAKE_STRING_char_error
	call asm_error

.cont:	push rdi
	push rsi
	shr rdi, 2 ;remove the fixnum tag
	add rdi, 8 ;reserve an extra qword for the length of the string
	call lisp_alloc
	pop rsi
	pop rdi


	push r15 ;r15 is callee-saved
	mov r15, rax ;r15 now holds the pointer
	mov [r15], rdi ;put the size of the string at the start

	;;memset the string
	shr rsi, 8 ;remove the char tag
	mov rax, rsi ;char
	shr rdi, 2 ;remove the fixnum tag
	mov rcx, rdi ;length
	mov rdi, r15 ;ptr
	add rdi, 8 ;move rdi so it doesn't point to the size
	cld ;set the direction flag to increment rcx
	rep stosb ;copy bytes, not qwords!

	mov rax, r15
	or rax, 011b ;put the string tag
	pop r15
	ret

global WRITE_STRING ;(string)
WRITE_STRING:
	mov rax, rdi
	and rax, 111b
	cmp rax, 011b
	je .cont
	mov rdi, WRITE_STRING_error
	call asm_error

.cont:	push rdi ;save rdi to return it later

	xor rdi, 011b ;remove the string tag
	mov rcx, [rdi] ;rcx = length of string
	shr rcx, 2 ;remove the fixnum tag
	cmp rcx, 0
	je .end
	add rdi, 8;increment rdi so it points to the first char 
	mov rsi, rdi ;we will use rsi as a pointer to the string

.loop:	push rsi
	push rcx
	mov  dil, [rsi] ;dil == lowest byte of rdi
	call put_char
	pop rcx
	pop rsi
	inc rsi
	loop .loop

.end: 	pop rax ;pop rdi from the stack and put it on rax to return it
	ret

global SET_CHAR ;(string index char)
SET_CHAR:
	mov rax, rdi
	and rax, 111b
	cmp rax, 011b
	je .index_check
	mov rdi, SET_CHAR_string_error
	call asm_error
.index_check:
	mov rax, rsi
	and rax, 11b
	cmp rax, 00b
	je .char_check
	mov rdi, SET_CHAR_index_error
	call asm_error
.char_check:
	mov rax, rdx
	and rax, 11111111b
	cmp rax, 00001111b
	je .check_length1
	mov rdi, SET_CHAR_char_error
	call asm_error
.check_length1:
	cmp rsi, qword [rdi-011b]
	jl .check_length2
	mov rdi, SET_CHAR_length1_error
	call asm_error
.check_length2:
	cmp rsi, 0
	jnl .cont
	mov rdi, SET_CHAR_length2_error
	call asm_error

.cont:	mov rax, rdi
	xor rax, 011b ;remove the string tag
	shr rsi, 2 ;remove the fixnum tag
	shr rdx, 8 ;remove the char tag

	add rax, 8 ;8 is the length of the length (64 bits integer)
	add rax, rsi ;add the index
	mov [rax], dl ;move the char into place

	mov rax, rdi ;return the string
	ret

global STRING_EQ ;(string1 string2)
STRING_EQ:
	mov rax, rdi
	and rax, 111b
	cmp rax, 011b
	je .string2_check
	mov rdi, STRING_EQ_error
	call asm_error
.string2_check:
	mov rax, rsi
	and rax, 111b
	cmp rax, 011b
	je .cont
	mov rdi, STRING_EQ_error
	call asm_error
	
.cont:	push rbx

	sub rdi, 011b ;remove the tag
	sub rsi, 011b ;remove the tag
	mov rax, [rdi]
	mov rbx, [rsi]
	cmp rax, rbx ;compare the lengths of the strings
	jne .diff

	mov rcx, [rdi] ;rcx = length of the strings
	shr rcx, 2 ;remove the fixnum tag
	cmp rcx, 0
	je .eq
	add rdi, 8
	add rsi, 8

.loop:	mov al, [rdi]
	mov bl, [rsi]
	cmp al, bl
	jne .diff
	inc rdi
	inc rsi
	loop .loop

	;;return fixnum 1 (true)
.eq:	mov rax, 1<<2
	jmp .end

.diff:	mov rax, 101111b
.end:	pop rbx
	ret

global MAKE_SYMBOL ;(name value)
MAKE_SYMBOL:
	mov rax, rdi
	and rax, 111b
	cmp rax, 011b
	je .cont
	mov rdi, MAKE_SYMBOL_error
	call asm_error

.cont:	push rbx ;rbx is callee-saved
	mov rbx, [alist_ptr] ;rbx will hold the cons to test
	;;check that the symbol doesn't already exist
.check:	cmp rbx, 47 ;if NIL
	je .add
	push rdi ;save rdi
	push rsi ;save rsi
	mov rdi, rbx
	;;caar
	call CAR
	mov rdi, rax
	call CAR
	mov rsi, rax
	mov rdi, [rsp+8] ;retrieve the value of rdi saved on the stack (name)
	call STRING_EQ ;compare the strings
	pop rsi
	pop rdi

	;;if NIL (different)
	cmp rax, 47
	je .diff

	;;if they're equal:
	mov rdi, rbx
	call CAR
	jmp .end

	;;check the cdr (the recursive part of assoc)
.diff:	push rdi ;save rdi
	push rsi ;save rsi
	mov rdi, rbx
	call CDR
	pop rsi
	pop rdi
	mov rbx, rax
	jmp .check

	;;make a cons = (name . value)
.add:	call CONS
	push rax ;save rax to return it later
	;;add the symbol to the alist of variables
	mov rdi, rax
	mov rsi, [alist_ptr]
	call CONS
	mov [alist_ptr], rax ;update the alist_ptr
	pop rax

.end:	or rax, 101b ;put the symbol tag
	pop rbx
	ret

global SET ;(symbol value)
SET:	mov rax, rdi
	and rax, 111b
	cmp rax, 101b
	je .cont
	mov rdi, SET_error
	call asm_error

.cont:	mov [rdi+3], rsi
	mov rax, rsi ;return the value
	ret

global SYMBOL_VALUE ;(symbol)
SYMBOL_VALUE:
	mov rax, rdi
	and rax, 111b
	cmp rax, 101b
	je .cont
	mov rdi, SYMBOL_VALUE_error
	call asm_error

.cont	mov rax, [rdi+3]
	ret

global SYMBOL_NAME ;(symbol)
SYMBOL_NAME:
	mov rax, rdi
	and rax, 111b
	cmp rax, 101b
	je .cont
	mov rdi, SYMBOL_NAME_error
	call asm_error

.cont:	mov rax,  [rdi-5]
	ret

global SYMBOL_LEXICAL_VALUE ;(symbol)
SYMBOL_LEXICAL_VALUE:
	mov rax, rdi
	and rax, 111b
	cmp rax, 101b
	je .cont
	mov rdi, SYMBOL_LEXICAL_VALUE_error
	call asm_error

.cont:	push r15 ;r15 is callee-saved
.check:	cmp r15, 47 ;if no lexical enviroment or end of enviroment
	je .dynamic
	push rdi
	mov rdi, r15
	call CAR
	mov rdi, rax
	call CAR
	pop rdi
	cmp rdi, rax ;compare the symbols
	jne .diff

.equal:	mov rdi, r15
	call CAR
	mov rdi, rax
	call CDR
	jmp .end

.diff:	push rdi
	mov rdi, r15
	call CDR
	mov r15, rax
	pop rdi
	jmp .check

.dynamic:
	call SYMBOL_VALUE

.end:	pop r15
	ret

;;SETL is the same as SET, but also works for lexical variables
global SETL ;(symbol value)
SETL:	mov rax, rdi
	and rax, 111b
	cmp rax, 101b
	je .cont
	mov rdi, SETL_error
	call asm_error

.cont:	push r15 ;r15 is callee-saved
	push rsi
.check:	cmp r15, 47 ;if no lexical enviroment or end of enviroment
	je .dynamic
	push rdi
	mov rdi, r15
	call CAR
	mov rdi, rax
	call CAR
	pop rdi
	cmp rdi, rax ;compare the symbols
	jne .diff

.equal:	mov rdi, r15
	call CAR
	pop rsi ;new value
	;;this may not work if CONS/CDR changes
	mov [rax+7], rsi ;replace the old value with the new one
	mov rax, rsi ;return the new value
	jmp .end

.diff:	push rdi
	mov rdi, r15
	call CDR
	mov r15, rax
	pop rdi
	jmp .check

.dynamic:
	pop rsi
	call SET

.end:	pop r15
	ret


global APPLY ;(function args)
APPLY:
	mov rax, rdi
	and rax, 111b
	cmp rax, 010b
	je .check_args
	mov rdi, APPLY_function_error
	call asm_error
.check_args:
	mov rax, rsi
	cmp rax, 00101111b ;NIL
	je .cont1
	and rax, 111b
	cmp rax, 001b
	je .cont1
	mov rdi, APPLY_args_error
	call asm_error

.cont1:	push r15 ;r15 (the lexical enviroment) is callee-saved
	push rbx
	mov rbx, [rdi-10] ;rbx = list of args expected by the function

.pass:	cmp rsi, 47 ;if no list or end of list
	je .call
	cmp rbx, 47
	jne .cont2
	mov rdi, APPLY_args_long_error
	call asm_error
.cont2:	push rdi
	push rsi
	;;get the passed argument
	mov rdi, rsi
	call CAR
	;;get the symbol for the argument
	push rax
	mov rdi, rbx
	call CAR
	;;add it to the lexical enviroment
	mov rdi, rax
	pop rsi
	call CONS
	mov rdi, rax
	mov rsi, r15
	call CONS
	mov r15, rax
	;;update rbx (list of expected arguments)
	mov rdi, rbx
	call CDR
	mov rbx, rax
	;;update rsi (list of passed values/args)
	pop rdi
	call CDR
	mov rsi, rax
	;;check the next arg
	pop rdi
	jmp .pass

.call:	cmp rbx, 47
	je .cont3
	mov rdi, APPLY_args_short_error
	call asm_error
.cont3:	sub rdi, 010b ;remove the compiled function tag
	call rdi ;call the function
	pop rbx
	pop r15
	ret

;;defined in kbd.c
;global READ_CHAR ;()
;READ_CHAR:
;.eq:	sti
	;hlt
	;cli ;disable interrupts to prevent byte [char_in] from changing
	;extern char_in
	;cmp byte [char_in], 0
	;je .eq
	;xor rax, rax
	;mov al, byte [char_in]
	;mov byte [char_in], 0 ;mark it as read
	;;;put the tag
	;shl rax, 8
	;or rax, 00001111b
	;sti
	;ret

global CHAR ;(string index)
CHAR:	mov rax, rdi
	and rax, 111b
	cmp rax, 011b
	je .check_index
	mov rdi, CHAR_string_error
	call asm_error
.check_index:
	mov rax, rsi
	and rax, 11b
	cmp rax, 00b
	je .check_length1
	mov rdi, CHAR_index_error
	call asm_error
.check_length1:
	cmp rsi, qword [rdi-011b]
	jl .check_length2
	mov rdi,CHAR_length1_error
	call asm_error
.check_length2:
	cmp rsi, 0
	jnl .cont
	mov rdi, CHAR_length2_error
	call asm_error
	
.cont:	shr rsi, 2 ;remove the fixnum tag
	add rdi, rsi
	xor rax, rax ;clear rax
	mov al, [rdi+5] ;8 (length of qword) - 011b (string tag) == 5
	;;add the tag
	shl rax, 8
	or rax, 00001111b
	ret

global STRING_LENGTH ;(string)
STRING_LENGTH:
	mov rax, rdi
	and rax, 111b
	cmp rax, 011b
	je .cont
	mov rdi, STRING_LENGTH_error
	call asm_error

.cont:	mov rax, [rdi-011b]
	ret

global SET_CAR ;(cons value)
SET_CAR:
	mov rax, rdi
	and rax, 111b
	cmp rax, 001b
	je .cont
	mov rdi, SET_CAR_error
	call asm_error

.cont:	mov [rdi-001b], rsi
	mov rax, rdi ;return the cons
	ret

global SET_CDR ;(cons value)
SET_CDR:
	mov rax, rdi
	and rax, 111b
	cmp rax, 001b
	je .cont
	mov rdi, SET_CDR_error
	call asm_error

.cont:	mov [rdi+(8-001b)], rsi
	mov rax, rdi ;return the cons
	ret


;;Helper function for the garbage collector that tests if a cons is present in a tree
;;No type-checking because it should only called by the garbage collector, and to speed up the garbage collection process
;global gc_in_root ;(ptr root-set)
;gc_in_root:
;	;;if root is not a cons or string, return NIL
;	mov rax, rsi
;	and rax, 111b
;	cmp rax, 001b
;	je .start
;	cmp rax, 011b
;	je .start
;	mov rax, 0 ;47
;	ret
;
;.start:	mov rax, rsi
;	or rax, 111b
;	xor rax, 111b
;	cmp rax, rdi
;	je .eq
;	;;check if it's a cons
;	mov rax, rsi
;	and rax, 111b
;	cmp rax, 001b
;	je .test0
;	mov rax, 0 ;47
;	ret
;
;.test0:	push rdi
;	push rsi
;	;;test the CAR first
;	mov rsi, [rsi-1]
;	call gc_in_root
;	pop rsi
;	pop rdi
;	cmp rax, 0 ;47
;	je .test1
;	ret
;	;;test the CDR if the test for CAR returns NIL (47)
;.test1:	mov rsi, [rsi+7]
;	call gc_in_root
;	ret
; 
;.eq:	mov rax, rsi
;.end:	ret

;;This is a good example of how arguments are passed in x86-64 (first 6 arguments go in registers, rest go on the stack, the last one get's pushed first (just like on x86))
global ADD8
ADD8:	push rbp
	mov rbp, rsp

	mov rax, rdi
	add rax, rsi
	add rax, rdx
	add rax, rcx
	add rax, r8
	add rax, r9
	add rax, [rbp+16]
	add rax, [rbp+24]

	pop rbp
	ret
