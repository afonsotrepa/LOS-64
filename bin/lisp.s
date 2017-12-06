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
mov r15, 47
mov rdi, repl_start
extern print
call print
;;Compiled code starts here
jmp ..@G391
align 8
..@G390:
dq 12
db 78
db 73
db 76
..@G391:
mov rax, ..@G390
or rax, 3
mov RDI, rax
extern MAKE_SYMBOL
call MAKE_SYMBOL
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern SET
call SET
jmp ..@G393
align 8
..@G392:
dq 4
db 116
..@G393:
mov rax, ..@G392
or rax, 3
mov RDI, rax
extern MAKE_SYMBOL
call MAKE_SYMBOL
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern SET
call SET
SAVE
jmp ..@G395
align 8
..@G394:
dq 16
db 67
db 65
db 68
db 82
..@G395:
mov rax, ..@G394
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G396.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G396:
ENTER
SAVE
SAVE
SAVE
jmp ..@G399
align 8
..@G398:
dq 4
db 88
..@G399:
mov rax, ..@G398
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G397:
LOAD
LEAVE
ret
.end: mov rax, G396
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G401
align 8
..@G400:
dq 4
db 88
..@G401:
mov rax, ..@G400
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G396-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G403
align 8
..@G402:
dq 16
db 67
db 68
db 65
db 82
..@G403:
mov rax, ..@G402
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G404.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G404:
ENTER
SAVE
SAVE
SAVE
jmp ..@G407
align 8
..@G406:
dq 4
db 88
..@G407:
mov rax, ..@G406
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
..@G405:
LOAD
LEAVE
ret
.end: mov rax, G404
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G409
align 8
..@G408:
dq 4
db 88
..@G409:
mov rax, ..@G408
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G404-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G411
align 8
..@G410:
dq 16
db 67
db 65
db 65
db 82
..@G411:
mov rax, ..@G410
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G412.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G412:
ENTER
SAVE
SAVE
SAVE
jmp ..@G415
align 8
..@G414:
dq 4
db 88
..@G415:
mov rax, ..@G414
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CAR
call CAR
..@G413:
LOAD
LEAVE
ret
.end: mov rax, G412
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G417
align 8
..@G416:
dq 4
db 88
..@G417:
mov rax, ..@G416
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G412-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G419
align 8
..@G418:
dq 16
db 67
db 68
db 68
db 82
..@G419:
mov rax, ..@G418
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G420.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G420:
ENTER
SAVE
SAVE
SAVE
jmp ..@G423
align 8
..@G422:
dq 4
db 88
..@G423:
mov rax, ..@G422
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
..@G421:
LOAD
LEAVE
ret
.end: mov rax, G420
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G425
align 8
..@G424:
dq 4
db 88
..@G425:
mov rax, ..@G424
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G420-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G427
align 8
..@G426:
dq 20
db 67
db 68
db 68
db 65
db 82
..@G427:
mov rax, ..@G426
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G428.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G428:
ENTER
SAVE
SAVE
SAVE
jmp ..@G431
align 8
..@G430:
dq 4
db 88
..@G431:
mov rax, ..@G430
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
..@G429:
LOAD
LEAVE
ret
.end: mov rax, G428
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G433
align 8
..@G432:
dq 4
db 88
..@G433:
mov rax, ..@G432
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G428-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G435
align 8
..@G434:
dq 20
db 67
db 65
db 68
db 68
db 82
..@G435:
mov rax, ..@G434
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G436.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G436:
ENTER
SAVE
SAVE
SAVE
jmp ..@G439
align 8
..@G438:
dq 4
db 88
..@G439:
mov rax, ..@G438
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G437:
LOAD
LEAVE
ret
.end: mov rax, G436
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G441
align 8
..@G440:
dq 4
db 88
..@G441:
mov rax, ..@G440
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G436-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G443
align 8
..@G442:
dq 20
db 67
db 68
db 68
db 68
db 82
..@G443:
mov rax, ..@G442
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G444.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G444:
ENTER
SAVE
SAVE
SAVE
jmp ..@G447
align 8
..@G446:
dq 4
db 88
..@G447:
mov rax, ..@G446
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
..@G445:
LOAD
LEAVE
ret
.end: mov rax, G444
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G449
align 8
..@G448:
dq 4
db 88
..@G449:
mov rax, ..@G448
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G444-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G451
align 8
..@G450:
dq 20
db 67
db 68
db 65
db 68
db 82
..@G451:
mov rax, ..@G450
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G452.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G452:
ENTER
SAVE
SAVE
SAVE
jmp ..@G455
align 8
..@G454:
dq 4
db 88
..@G455:
mov rax, ..@G454
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
..@G453:
LOAD
LEAVE
ret
.end: mov rax, G452
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G457
align 8
..@G456:
dq 4
db 88
..@G457:
mov rax, ..@G456
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G452-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G459
align 8
..@G458:
dq 20
db 67
db 65
db 68
db 65
db 82
..@G459:
mov rax, ..@G458
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G460.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G460:
ENTER
SAVE
SAVE
SAVE
jmp ..@G463
align 8
..@G462:
dq 4
db 88
..@G463:
mov rax, ..@G462
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G461:
LOAD
LEAVE
ret
.end: mov rax, G460
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G465
align 8
..@G464:
dq 4
db 88
..@G465:
mov rax, ..@G464
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G460-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G467
align 8
..@G466:
dq 20
db 67
db 65
db 65
db 68
db 82
..@G467:
mov rax, ..@G466
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G468.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G468:
ENTER
SAVE
SAVE
SAVE
jmp ..@G471
align 8
..@G470:
dq 4
db 88
..@G471:
mov rax, ..@G470
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CAR
call CAR
..@G469:
LOAD
LEAVE
ret
.end: mov rax, G468
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G473
align 8
..@G472:
dq 4
db 88
..@G473:
mov rax, ..@G472
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G468-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G475
align 8
..@G474:
dq 20
db 67
db 65
db 65
db 65
db 82
..@G475:
mov rax, ..@G474
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G476.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G476:
ENTER
SAVE
SAVE
SAVE
jmp ..@G479
align 8
..@G478:
dq 4
db 88
..@G479:
mov rax, ..@G478
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CAR
call CAR
..@G477:
LOAD
LEAVE
ret
.end: mov rax, G476
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G481
align 8
..@G480:
dq 4
db 88
..@G481:
mov rax, ..@G480
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G476-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G483
align 8
..@G482:
dq 24
db 67
db 68
db 68
db 68
db 68
db 82
..@G483:
mov rax, ..@G482
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G484.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G484:
ENTER
SAVE
SAVE
SAVE
jmp ..@G487
align 8
..@G486:
dq 4
db 88
..@G487:
mov rax, ..@G486
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
..@G485:
LOAD
LEAVE
ret
.end: mov rax, G484
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G489
align 8
..@G488:
dq 4
db 88
..@G489:
mov rax, ..@G488
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G484-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G491
align 8
..@G490:
dq 24
db 67
db 65
db 68
db 68
db 68
db 82
..@G491:
mov rax, ..@G490
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G492.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G492:
ENTER
SAVE
SAVE
SAVE
jmp ..@G495
align 8
..@G494:
dq 4
db 88
..@G495:
mov rax, ..@G494
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G493:
LOAD
LEAVE
ret
.end: mov rax, G492
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G497
align 8
..@G496:
dq 4
db 88
..@G497:
mov rax, ..@G496
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G492-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G499
align 8
..@G498:
dq 24
db 67
db 65
db 68
db 65
db 68
db 82
..@G499:
mov rax, ..@G498
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G500.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G500:
ENTER
SAVE
SAVE
SAVE
jmp ..@G503
align 8
..@G502:
dq 4
db 88
..@G503:
mov rax, ..@G502
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G501:
LOAD
LEAVE
ret
.end: mov rax, G500
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G505
align 8
..@G504:
dq 4
db 88
..@G505:
mov rax, ..@G504
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G500-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G507
align 8
..@G506:
dq 24
db 67
db 65
db 65
db 68
db 65
db 82
..@G507:
mov rax, ..@G506
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G508.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G508:
ENTER
SAVE
SAVE
SAVE
jmp ..@G511
align 8
..@G510:
dq 4
db 88
..@G511:
mov rax, ..@G510
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CAR
call CAR
..@G509:
LOAD
LEAVE
ret
.end: mov rax, G508
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G513
align 8
..@G512:
dq 4
db 88
..@G513:
mov rax, ..@G512
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G508-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G515
align 8
..@G514:
dq 24
db 67
db 65
db 68
db 68
db 65
db 82
..@G515:
mov rax, ..@G514
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G516.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G516:
ENTER
SAVE
SAVE
SAVE
jmp ..@G519
align 8
..@G518:
dq 4
db 88
..@G519:
mov rax, ..@G518
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G517:
LOAD
LEAVE
ret
.end: mov rax, G516
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G521
align 8
..@G520:
dq 4
db 88
..@G521:
mov rax, ..@G520
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G516-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G523
align 8
..@G522:
dq 28
db 67
db 65
db 68
db 68
db 68
db 65
db 82
..@G523:
mov rax, ..@G522
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G524.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G524:
ENTER
SAVE
SAVE
SAVE
jmp ..@G527
align 8
..@G526:
dq 4
db 88
..@G527:
mov rax, ..@G526
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G525:
LOAD
LEAVE
ret
.end: mov rax, G524
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G529
align 8
..@G528:
dq 4
db 88
..@G529:
mov rax, ..@G528
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G524-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G531
align 8
..@G530:
dq 28
db 67
db 65
db 68
db 65
db 68
db 65
db 82
..@G531:
mov rax, ..@G530
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G532.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G532:
ENTER
SAVE
SAVE
SAVE
jmp ..@G535
align 8
..@G534:
dq 4
db 88
..@G535:
mov rax, ..@G534
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G533:
LOAD
LEAVE
ret
.end: mov rax, G532
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G537
align 8
..@G536:
dq 4
db 88
..@G537:
mov rax, ..@G536
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G532-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G539
align 8
..@G538:
dq 32
db 67
db 65
db 68
db 68
db 68
db 68
db 65
db 82
..@G539:
mov rax, ..@G538
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G540.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G540:
ENTER
SAVE
SAVE
SAVE
jmp ..@G543
align 8
..@G542:
dq 4
db 88
..@G543:
mov rax, ..@G542
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
..@G541:
LOAD
LEAVE
ret
.end: mov rax, G540
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G545
align 8
..@G544:
dq 4
db 88
..@G545:
mov rax, ..@G544
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G540-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G547
align 8
..@G546:
dq 16
db 78
db 85
db 76
db 76
..@G547:
mov rax, ..@G546
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G548.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G548:
ENTER
SAVE
SAVE
SAVE
jmp ..@G551
align 8
..@G550:
dq 4
db 88
..@G551:
mov rax, ..@G550
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern EQ
call EQ
..@G549:
LOAD
LEAVE
ret
.end: mov rax, G548
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G553
align 8
..@G552:
dq 4
db 88
..@G553:
mov rax, ..@G552
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G548-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G555
align 8
..@G554:
dq 12
db 65
db 78
db 68
..@G555:
mov rax, ..@G554
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G556.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G556:
ENTER
SAVE
SAVE
SAVE
jmp ..@G561
align 8
..@G560:
dq 4
db 88
..@G561:
mov rax, ..@G560
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G559
SAVE
SAVE
jmp ..@G565
align 8
..@G564:
dq 4
db 89
..@G565:
mov rax, ..@G564
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G563
mov rax, 4
jmp ..@G562
..@G563:
mov rax, 4
cmp rax, 47
je ..@G566
mov rax, 47
jmp ..@G562
..@G566:
..@G562:
jmp ..@G558
..@G559:
mov rax, 4
cmp rax, 47
je ..@G567
mov rax, 47
jmp ..@G558
..@G567:
..@G558:
..@G557:
LOAD
LEAVE
ret
.end: mov rax, G556
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G569
align 8
..@G568:
dq 4
db 89
..@G569:
mov rax, ..@G568
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G571
align 8
..@G570:
dq 4
db 88
..@G571:
mov rax, ..@G570
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G556-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G573
align 8
..@G572:
dq 12
db 78
db 79
db 84
..@G573:
mov rax, ..@G572
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G574.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G574:
ENTER
SAVE
SAVE
SAVE
jmp ..@G579
align 8
..@G578:
dq 4
db 88
..@G579:
mov rax, ..@G578
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G577
mov rax, 47
jmp ..@G576
..@G577:
mov rax, 4
cmp rax, 47
je ..@G580
mov rax, 4
jmp ..@G576
..@G580:
..@G576:
..@G575:
LOAD
LEAVE
ret
.end: mov rax, G574
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G582
align 8
..@G581:
dq 4
db 88
..@G582:
mov rax, ..@G581
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G574-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G584
align 8
..@G583:
dq 24
db 65
db 80
db 80
db 69
db 78
db 68
..@G584:
mov rax, ..@G583
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G585.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G585:
ENTER
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G590
align 8
..@G589:
dq 4
db 88
..@G590:
mov rax, ..@G589
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G592
align 8
..@G591:
dq 16
db 78
db 85
db 76
db 76
..@G592:
mov rax, ..@G591
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G588
SAVE
SAVE
jmp ..@G594
align 8
..@G593:
dq 4
db 89
..@G594:
mov rax, ..@G593
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G587
..@G588:
mov rax, 4
cmp rax, 47
je ..@G595
SAVE
SAVE
jmp ..@G597
align 8
..@G596:
dq 4
db 88
..@G597:
mov rax, ..@G596
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G599
align 8
..@G598:
dq 4
db 89
..@G599:
mov rax, ..@G598
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G601
align 8
..@G600:
dq 4
db 88
..@G601:
mov rax, ..@G600
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G603
align 8
..@G602:
dq 24
db 65
db 80
db 80
db 69
db 78
db 68
..@G603:
mov rax, ..@G602
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G587
..@G595:
..@G587:
..@G586:
LOAD
LEAVE
ret
.end: mov rax, G585
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G605
align 8
..@G604:
dq 4
db 89
..@G605:
mov rax, ..@G604
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G607
align 8
..@G606:
dq 4
db 88
..@G607:
mov rax, ..@G606
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G585-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G609
align 8
..@G608:
dq 16
db 76
db 73
db 83
db 84
..@G609:
mov rax, ..@G608
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G610.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G610:
ENTER
SAVE
SAVE
SAVE
jmp ..@G613
align 8
..@G612:
dq 4
db 88
..@G613:
mov rax, ..@G612
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G615
align 8
..@G614:
dq 4
db 89
..@G615:
mov rax, ..@G614
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov rax, 47
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern CONS
call CONS
..@G611:
LOAD
LEAVE
ret
.end: mov rax, G610
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G617
align 8
..@G616:
dq 4
db 89
..@G617:
mov rax, ..@G616
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G619
align 8
..@G618:
dq 4
db 88
..@G619:
mov rax, ..@G618
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G610-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G621
align 8
..@G620:
dq 16
db 80
db 65
db 73
db 82
..@G621:
mov rax, ..@G620
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G622.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G622:
ENTER
SAVE
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G627
align 8
..@G626:
dq 4
db 89
..@G627:
mov rax, ..@G626
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G629
align 8
..@G628:
dq 16
db 78
db 85
db 76
db 76
..@G629:
mov rax, ..@G628
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G631
align 8
..@G630:
dq 4
db 88
..@G631:
mov rax, ..@G630
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G633
align 8
..@G632:
dq 16
db 78
db 85
db 76
db 76
..@G633:
mov rax, ..@G632
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G635
align 8
..@G634:
dq 12
db 65
db 78
db 68
..@G635:
mov rax, ..@G634
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G625
mov rax, 47
jmp ..@G624
..@G625:
mov rax, 47
push rax
SAVE
SAVE
SAVE
jmp ..@G641
align 8
..@G640:
dq 4
db 89
..@G641:
mov rax, ..@G640
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern ATOM
call ATOM
LOAD
cmp rax, 47
je ..@G637
..@G638:
push RDI
mov rax, 47
jmp ..@G639
..@G637:
push RDI
mov rax, 4
..@G639:
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
SAVE
jmp ..@G646
align 8
..@G645:
dq 4
db 88
..@G646:
mov rax, ..@G645
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern ATOM
call ATOM
LOAD
cmp rax, 47
je ..@G642
..@G643:
push RDI
mov rax, 47
jmp ..@G644
..@G642:
push RDI
mov rax, 4
..@G644:
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G648
align 8
..@G647:
dq 12
db 65
db 78
db 68
..@G648:
mov rax, ..@G647
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G636
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G650
align 8
..@G649:
dq 4
db 89
..@G650:
mov rax, ..@G649
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G652
align 8
..@G651:
dq 4
db 88
..@G652:
mov rax, ..@G651
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G654
align 8
..@G653:
dq 16
db 76
db 73
db 83
db 84
..@G654:
mov rax, ..@G653
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G656
align 8
..@G655:
dq 4
db 89
..@G656:
mov rax, ..@G655
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G658
align 8
..@G657:
dq 4
db 88
..@G658:
mov rax, ..@G657
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G660
align 8
..@G659:
dq 16
db 80
db 65
db 73
db 82
..@G660:
mov rax, ..@G659
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G624
..@G636:
..@G624:
..@G623:
LOAD
LEAVE
ret
.end: mov rax, G622
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G662
align 8
..@G661:
dq 4
db 89
..@G662:
mov rax, ..@G661
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G664
align 8
..@G663:
dq 4
db 88
..@G664:
mov rax, ..@G663
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G622-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G666
align 8
..@G665:
dq 20
db 65
db 83
db 83
db 79
db 67
..@G666:
mov rax, ..@G665
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G667.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G667:
ENTER
SAVE
SAVE
SAVE
jmp ..@G672
align 8
..@G671:
dq 4
db 89
..@G672:
mov rax, ..@G671
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G674
align 8
..@G673:
dq 4
db 88
..@G674:
mov rax, ..@G673
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G670
SAVE
SAVE
jmp ..@G676
align 8
..@G675:
dq 4
db 89
..@G676:
mov rax, ..@G675
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern CDR
call CDR
mov RDI, rax
extern CAR
call CAR
jmp ..@G669
..@G670:
mov rax, 4
cmp rax, 47
je ..@G677
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G679
align 8
..@G678:
dq 4
db 89
..@G679:
mov rax, ..@G678
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G681
align 8
..@G680:
dq 4
db 88
..@G681:
mov rax, ..@G680
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G683
align 8
..@G682:
dq 20
db 65
db 83
db 83
db 79
db 67
..@G683:
mov rax, ..@G682
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G669
..@G677:
..@G669:
..@G668:
LOAD
LEAVE
ret
.end: mov rax, G667
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G685
align 8
..@G684:
dq 4
db 89
..@G685:
mov rax, ..@G684
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G687
align 8
..@G686:
dq 4
db 88
..@G687:
mov rax, ..@G686
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G667-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G689
align 8
..@G688:
dq 44
db 76
db 73
db 83
db 84
db 45
db 76
db 69
db 78
db 71
db 84
db 72
..@G689:
mov rax, ..@G688
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G690.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G690:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 0
push rax
SAVE
jmp ..@G693
align 8
..@G692:
dq 4
db 78
..@G693:
mov rax, ..@G692
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G694:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G699
align 8
..@G698:
dq 12
db 76
db 83
db 84
..@G699:
mov rax, ..@G698
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G701
align 8
..@G700:
dq 16
db 78
db 85
db 76
db 76
..@G701:
mov rax, ..@G700
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G695
..@G696:
SAVE
SAVE
jmp ..@G703
align 8
..@G702:
dq 4
db 78
..@G703:
mov rax, ..@G702
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G691
jmp ..@G697
..@G695:
SAVE
SAVE
jmp ..@G705
align 8
..@G704:
dq 4
db 78
..@G705:
mov rax, ..@G704
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G707
align 8
..@G706:
dq 4
db 78
..@G707:
mov rax, ..@G706
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G709
align 8
..@G708:
dq 12
db 76
db 83
db 84
..@G709:
mov rax, ..@G708
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G711
align 8
..@G710:
dq 12
db 76
db 83
db 84
..@G711:
mov rax, ..@G710
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G694
..@G697:
;;end of tagbody
pop r15
..@G691:
LOAD
LEAVE
ret
.end: mov rax, G690
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G715
align 8
..@G714:
dq 12
db 76
db 83
db 84
..@G715:
mov rax, ..@G714
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G690-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G717
align 8
..@G716:
dq 48
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
..@G717:
mov rax, ..@G716
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G718.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G718:
ENTER
SAVE
mov rax, 47
push rax
mov rax, 47
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G721
align 8
..@G720:
dq 12
db 76
db 83
db 84
..@G721:
mov rax, ..@G720
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G723
align 8
..@G722:
dq 64
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
db 45
db 65
db 85
db 88
..@G723:
mov rax, ..@G722
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
..@G719:
LOAD
LEAVE
ret
.end: mov rax, G718
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G725
align 8
..@G724:
dq 12
db 76
db 83
db 84
..@G725:
mov rax, ..@G724
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G718-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G727
align 8
..@G726:
dq 64
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
db 45
db 65
db 85
db 88
..@G727:
mov rax, ..@G726
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G728.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G728:
ENTER
SAVE
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G734
align 8
..@G733:
dq 12
db 76
db 83
db 84
..@G734:
mov rax, ..@G733
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G736
align 8
..@G735:
dq 16
db 78
db 85
db 76
db 76
..@G736:
mov rax, ..@G735
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G730
..@G731:
SAVE
SAVE
jmp ..@G738
align 8
..@G737:
dq 12
db 65
db 85
db 88
..@G738:
mov rax, ..@G737
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G732
..@G730:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G740
align 8
..@G739:
dq 12
db 76
db 83
db 84
..@G740:
mov rax, ..@G739
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G742
align 8
..@G741:
dq 12
db 65
db 85
db 88
..@G742:
mov rax, ..@G741
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G744
align 8
..@G743:
dq 12
db 76
db 83
db 84
..@G744:
mov rax, ..@G743
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G746
align 8
..@G745:
dq 64
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
db 45
db 65
db 85
db 88
..@G746:
mov rax, ..@G745
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
..@G732:
..@G729:
LOAD
LEAVE
ret
.end: mov rax, G728
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G748
align 8
..@G747:
dq 12
db 65
db 85
db 88
..@G748:
mov rax, ..@G747
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G750
align 8
..@G749:
dq 12
db 76
db 83
db 84
..@G750:
mov rax, ..@G749
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G728-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G752
align 8
..@G751:
dq 52
db 76
db 73
db 83
db 84
db 45
db 80
db 79
db 83
db 73
db 84
db 73
db 79
db 78
..@G752:
mov rax, ..@G751
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G753.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G753:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 0
push rax
SAVE
jmp ..@G756
align 8
..@G755:
dq 4
db 73
..@G756:
mov rax, ..@G755
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G761
align 8
..@G760:
dq 16
db 73
db 84
db 69
db 77
..@G761:
mov rax, ..@G760
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G763
align 8
..@G762:
dq 12
db 76
db 83
db 84
..@G763:
mov rax, ..@G762
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
push rax
SAVE
jmp ..@G765
align 8
..@G764:
dq 16
db 71
db 55
db 53
db 55
..@G765:
mov rax, ..@G764
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G766:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G771
align 8
..@G770:
dq 16
db 71
db 55
db 53
db 55
..@G771:
mov rax, ..@G770
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G773
align 8
..@G772:
dq 16
db 78
db 85
db 76
db 76
..@G773:
mov rax, ..@G772
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G767
..@G768:
mov rax, 47
jmp ..@G759
jmp ..@G769
..@G767:
push RDI
mov rax, 47
..@G769:
SAVE
jmp ..@G775
align 8
..@G774:
dq 16
db 73
db 84
db 69
db 77
..@G775:
mov rax, ..@G774
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G777
align 8
..@G776:
dq 16
db 71
db 55
db 53
db 55
..@G777:
mov rax, ..@G776
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
SAVE
jmp ..@G782
align 8
..@G781:
dq 16
db 73
db 84
db 69
db 77
..@G782:
mov rax, ..@G781
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G784
align 8
..@G783:
dq 12
db 79
db 66
db 74
..@G784:
mov rax, ..@G783
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G778
..@G779:
SAVE
SAVE
jmp ..@G786
align 8
..@G785:
dq 4
db 73
..@G786:
mov rax, ..@G785
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G754
jmp ..@G780
..@G778:
SAVE
SAVE
jmp ..@G788
align 8
..@G787:
dq 4
db 73
..@G788:
mov rax, ..@G787
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G790
align 8
..@G789:
dq 4
db 73
..@G790:
mov rax, ..@G789
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
..@G780:
SAVE
jmp ..@G792
align 8
..@G791:
dq 16
db 71
db 55
db 53
db 55
..@G792:
mov rax, ..@G791
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G794
align 8
..@G793:
dq 16
db 71
db 55
db 53
db 55
..@G794:
mov rax, ..@G793
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G766
;;end of tagbody
pop r15
..@G759:
mov rax, 47
pop r15
..@G754:
LOAD
LEAVE
ret
.end: mov rax, G753
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G802
align 8
..@G801:
dq 12
db 76
db 83
db 84
..@G802:
mov rax, ..@G801
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G804
align 8
..@G803:
dq 12
db 79
db 66
db 74
..@G804:
mov rax, ..@G803
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G753-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G806
align 8
..@G805:
dq 36
db 82
db 69
db 65
db 68
db 45
db 76
db 73
db 78
db 69
..@G806:
mov rax, ..@G805
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G807.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G807:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 0
push rax
SAVE
jmp ..@G810
align 8
..@G809:
dq 12
db 76
db 69
db 78
..@G810:
mov rax, ..@G809
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
jmp ..@G812
align 8
..@G811:
dq 0
..@G812:
mov rax, ..@G811
or rax, 3
push rax
SAVE
jmp ..@G814
align 8
..@G813:
dq 12
db 83
db 84
db 82
..@G814:
mov rax, ..@G813
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G816
align 8
..@G815:
dq 16
db 76
db 73
db 78
db 69
..@G816:
mov rax, ..@G815
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G818:
SAVE
jmp ..@G820
align 8
..@G819:
dq 16
db 76
db 73
db 78
db 69
..@G820:
mov rax, ..@G819
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
extern READ_CHAR
call READ_CHAR
mov RDI, rax
extern WRITE_CHAR
call WRITE_CHAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G822
align 8
..@G821:
dq 16
db 76
db 73
db 78
db 69
..@G822:
mov rax, ..@G821
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
SAVE
jmp ..@G827
align 8
..@G826:
dq 16
db 76
db 73
db 78
db 69
..@G827:
mov rax, ..@G826
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
mov RSI, 2063
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G823
..@G824:
SAVE
SAVE
jmp ..@G829
align 8
..@G828:
dq 16
db 76
db 73
db 78
db 69
..@G829:
mov rax, ..@G828
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G831
align 8
..@G830:
dq 16
db 76
db 73
db 78
db 69
..@G831:
mov rax, ..@G830
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G833
align 8
..@G832:
dq 16
db 67
db 68
db 68
db 82
..@G833:
mov rax, ..@G832
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G825
..@G823:
SAVE
SAVE
SAVE
jmp ..@G838
align 8
..@G837:
dq 16
db 76
db 73
db 78
db 69
..@G838:
mov rax, ..@G837
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
mov RSI, 2575
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G834
..@G835:
;;start of tagbody
push rdi
push rsi
SAVE
jmp ..@G841
align 8
..@G840:
dq 16
db 76
db 73
db 78
db 69
..@G841:
mov rax, ..@G840
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G843
align 8
..@G842:
dq 16
db 76
db 73
db 78
db 69
..@G843:
mov rax, ..@G842
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G848
align 8
..@G847:
dq 16
db 76
db 73
db 78
db 69
..@G848:
mov rax, ..@G847
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G850
align 8
..@G849:
dq 16
db 78
db 85
db 76
db 76
..@G850:
mov rax, ..@G849
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G844
..@G845:
SAVE
SAVE
jmp ..@G852
align 8
..@G851:
dq 12
db 83
db 84
db 82
..@G852:
mov rax, ..@G851
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G808
jmp ..@G846
..@G844:
push RDI
mov rax, 47
..@G846:
SAVE
jmp ..@G854
align 8
..@G853:
dq 12
db 76
db 69
db 78
..@G854:
mov rax, ..@G853
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G856
align 8
..@G855:
dq 16
db 76
db 73
db 78
db 69
..@G856:
mov rax, ..@G855
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G858
align 8
..@G857:
dq 44
db 76
db 73
db 83
db 84
db 45
db 76
db 69
db 78
db 71
db 84
db 72
..@G858:
mov rax, ..@G857
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G860
align 8
..@G859:
dq 12
db 83
db 84
db 82
..@G860:
mov rax, ..@G859
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G862
align 8
..@G861:
dq 12
db 76
db 69
db 78
..@G862:
mov rax, ..@G861
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G864
align 8
..@G863:
dq 16
db 76
db 73
db 78
db 69
..@G864:
mov rax, ..@G863
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern MAKE_STRING
call MAKE_STRING
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G866
align 8
..@G865:
dq 12
db 76
db 69
db 78
..@G866:
mov rax, ..@G865
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G868
align 8
..@G867:
dq 12
db 76
db 69
db 78
..@G868:
mov rax, ..@G867
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern SUB
call SUB
mov RSI, rax
pop RDI
extern SETL
call SETL
..@G839:
SAVE
SAVE
jmp ..@G870
align 8
..@G869:
dq 12
db 83
db 84
db 82
..@G870:
mov rax, ..@G869
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G872
align 8
..@G871:
dq 12
db 76
db 69
db 78
..@G872:
mov rax, ..@G871
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
push RSI
SAVE
SAVE
jmp ..@G874
align 8
..@G873:
dq 16
db 76
db 73
db 78
db 69
..@G874:
mov rax, ..@G873
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDX, rax
pop RSI
pop RDI
extern SET_CHAR
call SET_CHAR
SAVE
jmp ..@G876
align 8
..@G875:
dq 12
db 76
db 69
db 78
..@G876:
mov rax, ..@G875
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G878
align 8
..@G877:
dq 12
db 76
db 69
db 78
..@G878:
mov rax, ..@G877
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern SUB
call SUB
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G880
align 8
..@G879:
dq 16
db 76
db 73
db 78
db 69
..@G880:
mov rax, ..@G879
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G882
align 8
..@G881:
dq 16
db 76
db 73
db 78
db 69
..@G882:
mov rax, ..@G881
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G887
align 8
..@G886:
dq 16
db 76
db 73
db 78
db 69
..@G887:
mov rax, ..@G886
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G889
align 8
..@G888:
dq 16
db 78
db 85
db 76
db 76
..@G889:
mov rax, ..@G888
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G883
..@G884:
SAVE
SAVE
jmp ..@G891
align 8
..@G890:
dq 12
db 83
db 84
db 82
..@G891:
mov rax, ..@G890
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G808
jmp ..@G885
..@G883:
jmp ..@G839
..@G885:
pop rsi
pop rdi
;;end of tagbody
jmp ..@G836
..@G834:
push RDI
mov rax, 47
..@G836:
..@G825:
jmp ..@G818
;;end of tagbody
..@G817:
pop r15
..@G808:
LOAD
LEAVE
ret
.end: mov rax, G807
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
pop rsi
pop rdi
;;end of list
mov [G807-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G907
align 8
..@G906:
dq 88
db 82
db 69
db 65
db 68
db 45
db 84
db 79
db 75
db 69
db 78
db 45
db 70
db 82
db 79
db 77
db 45
db 83
db 84
db 82
db 73
db 78
db 71
..@G907:
mov rax, ..@G906
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G908.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G908:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 0
push rax
SAVE
jmp ..@G911
align 8
..@G910:
dq 16
db 67
db 78
db 84
db 82
..@G911:
mov rax, ..@G910
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G913
align 8
..@G912:
dq 12
db 83
db 84
db 82
..@G913:
mov rax, ..@G912
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern STRING_LENGTH
call STRING_LENGTH
push rax
SAVE
jmp ..@G915
align 8
..@G914:
dq 12
db 76
db 69
db 78
..@G915:
mov rax, ..@G914
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 12303
push rax
SAVE
jmp ..@G917
align 8
..@G916:
dq 12
db 67
db 72
db 82
..@G917:
mov rax, ..@G916
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
jmp ..@G919
align 8
..@G918:
dq 0
..@G919:
mov rax, ..@G918
or rax, 3
push rax
SAVE
jmp ..@G921
align 8
..@G920:
dq 20
db 84
db 79
db 75
db 69
db 78
..@G921:
mov rax, ..@G920
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G923
align 8
..@G922:
dq 12
db 76
db 83
db 84
..@G923:
mov rax, ..@G922
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G924:
SAVE
SAVE
SAVE
jmp ..@G931
align 8
..@G930:
dq 4
db 73
..@G931:
mov rax, ..@G930
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G933
align 8
..@G932:
dq 12
db 76
db 69
db 78
..@G933:
mov rax, ..@G932
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G927
..@G928:
jmp ..@G925
jmp ..@G929
..@G927:
push RDI
mov rax, 47
..@G929:
SAVE
jmp ..@G935
align 8
..@G934:
dq 12
db 67
db 72
db 82
..@G935:
mov rax, ..@G934
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G937
align 8
..@G936:
dq 12
db 83
db 84
db 82
..@G937:
mov rax, ..@G936
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G939
align 8
..@G938:
dq 4
db 73
..@G939:
mov rax, ..@G938
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CHAR
call CHAR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
SAVE
jmp ..@G944
align 8
..@G943:
dq 12
db 67
db 72
db 82
..@G944:
mov rax, ..@G943
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 8207
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G940
..@G941:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G949
align 8
..@G948:
dq 12
db 76
db 83
db 84
..@G949:
mov rax, ..@G948
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G951
align 8
..@G950:
dq 16
db 78
db 85
db 76
db 76
..@G951:
mov rax, ..@G950
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G945
..@G946:
SAVE
SAVE
jmp ..@G953
align 8
..@G952:
dq 4
db 73
..@G953:
mov rax, ..@G952
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G955
align 8
..@G954:
dq 4
db 73
..@G955:
mov rax, ..@G954
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G924
jmp ..@G947
..@G945:
jmp ..@G925
..@G947:
jmp ..@G942
..@G940:
push RDI
mov rax, 47
..@G942:
SAVE
SAVE
SAVE
jmp ..@G969
align 8
..@G968:
dq 12
db 67
db 72
db 82
..@G969:
mov rax, ..@G968
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 10255
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G965
..@G966:
nop
jmp ..@G967
..@G965:
SAVE
SAVE
SAVE
jmp ..@G974
align 8
..@G973:
dq 12
db 67
db 72
db 82
..@G974:
mov rax, ..@G973
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 10511
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G970
..@G971:
nop
jmp ..@G972
..@G970:
push RDI
mov rax, 47
..@G972:
..@G967:
cmp rax, 47
je ..@G962
..@G963:
nop
jmp ..@G964
..@G962:
SAVE
SAVE
SAVE
jmp ..@G979
align 8
..@G978:
dq 12
db 67
db 72
db 82
..@G979:
mov rax, ..@G978
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 9999
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G975
..@G976:
nop
jmp ..@G977
..@G975:
push RDI
mov rax, 47
..@G977:
..@G964:
cmp rax, 47
je ..@G959
..@G960:
nop
jmp ..@G961
..@G959:
SAVE
SAVE
SAVE
jmp ..@G984
align 8
..@G983:
dq 12
db 67
db 72
db 82
..@G984:
mov rax, ..@G983
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 24591
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G980
..@G981:
nop
jmp ..@G982
..@G980:
push RDI
mov rax, 47
..@G982:
..@G961:
cmp rax, 47
je ..@G956
..@G957:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G989
align 8
..@G988:
dq 12
db 76
db 83
db 84
..@G989:
mov rax, ..@G988
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G991
align 8
..@G990:
dq 16
db 78
db 85
db 76
db 76
..@G991:
mov rax, ..@G990
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G985
..@G986:
mov RDI, 4
push RDI
SAVE
SAVE
jmp ..@G993
align 8
..@G992:
dq 12
db 67
db 72
db 82
..@G993:
mov rax, ..@G992
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern MAKE_STRING
call MAKE_STRING
mov RDI, rax
push RDI
mov RDI, 4
push RDI
SAVE
SAVE
jmp ..@G995
align 8
..@G994:
dq 4
db 73
..@G995:
mov rax, ..@G994
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G909
jmp ..@G987
..@G985:
jmp ..@G925
..@G987:
jmp ..@G958
..@G956:
push RDI
mov rax, 47
..@G958:
SAVE
SAVE
SAVE
jmp ..@G1000
align 8
..@G999:
dq 12
db 67
db 72
db 82
..@G1000:
mov rax, ..@G999
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 11279
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G996
..@G997:
SAVE
jmp ..@G1002
align 8
..@G1001:
dq 312
db 67
db 111
db 109
db 109
db 97
db 32
db 111
db 117
db 116
db 115
db 105
db 100
db 101
db 32
db 111
db 102
db 32
db 98
db 97
db 99
db 107
db 113
db 117
db 111
db 116
db 101
db 100
db 32
db 101
db 120
db 112
db 114
db 101
db 115
db 115
db 105
db 111
db 110
db 32
db 105
db 110
db 32
db 115
db 116
db 114
db 32
db 112
db 97
db 115
db 115
db 101
db 100
db 32
db 116
db 111
db 32
db 114
db 101
db 97
db 100
db 45
db 116
db 111
db 107
db 101
db 110
db 45
db 102
db 114
db 111
db 109
db 45
db 115
db 116
db 114
db 105
db 110
db 103
..@G1002:
mov rax, ..@G1001
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
LOAD
jmp ..@G998
..@G996:
push RDI
mov rax, 47
..@G998:
SAVE
SAVE
SAVE
jmp ..@G1007
align 8
..@G1006:
dq 12
db 67
db 72
db 82
..@G1007:
mov rax, ..@G1006
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 8719
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G1003
..@G1004:
SAVE
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1012
align 8
..@G1011:
dq 12
db 76
db 83
db 84
..@G1012:
mov rax, ..@G1011
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1014
align 8
..@G1013:
dq 16
db 78
db 85
db 76
db 76
..@G1014:
mov rax, ..@G1013
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1016
align 8
..@G1015:
dq 12
db 78
db 79
db 84
..@G1016:
mov rax, ..@G1015
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1008
..@G1009:
jmp ..@G925
jmp ..@G1010
..@G1008:
push RDI
mov rax, 47
..@G1010:
;;start of tagbody
push rdi
push rsi
SAVE
jmp ..@G1019
align 8
..@G1018:
dq 12
db 76
db 83
db 84
..@G1019:
mov rax, ..@G1018
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1021
align 8
..@G1020:
dq 12
db 67
db 72
db 82
..@G1021:
mov rax, ..@G1020
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1023
align 8
..@G1022:
dq 12
db 76
db 83
db 84
..@G1023:
mov rax, ..@G1022
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1025
align 8
..@G1024:
dq 4
db 73
..@G1025:
mov rax, ..@G1024
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1027
align 8
..@G1026:
dq 4
db 73
..@G1027:
mov rax, ..@G1026
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
..@G1017:
SAVE
jmp ..@G1029
align 8
..@G1028:
dq 12
db 67
db 72
db 82
..@G1029:
mov rax, ..@G1028
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1031
align 8
..@G1030:
dq 12
db 83
db 84
db 82
..@G1031:
mov rax, ..@G1030
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1033
align 8
..@G1032:
dq 4
db 73
..@G1033:
mov rax, ..@G1032
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CHAR
call CHAR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1038
align 8
..@G1037:
dq 4
db 73
..@G1038:
mov rax, ..@G1037
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1040
align 8
..@G1039:
dq 12
db 76
db 69
db 78
..@G1040:
mov rax, ..@G1039
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1042
align 8
..@G1041:
dq 12
db 67
db 72
db 82
..@G1042:
mov rax, ..@G1041
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 8719
pop RDI
extern EQ
call EQ
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1044
align 8
..@G1043:
dq 12
db 78
db 79
db 84
..@G1044:
mov rax, ..@G1043
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1046
align 8
..@G1045:
dq 12
db 65
db 78
db 68
..@G1046:
mov rax, ..@G1045
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1034
..@G1035:
SAVE
jmp ..@G1048
align 8
..@G1047:
dq 196
db 77
db 105
db 115
db 115
db 105
db 110
db 103
db 32
db 34
db 32
db 105
db 110
db 32
db 115
db 116
db 114
db 32
db 112
db 97
db 115
db 115
db 101
db 100
db 32
db 116
db 111
db 32
db 114
db 101
db 97
db 100
db 45
db 116
db 111
db 107
db 101
db 110
db 45
db 102
db 114
db 111
db 109
db 45
db 115
db 116
db 114
db 105
db 110
db 103
..@G1048:
mov rax, ..@G1047
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
LOAD
jmp ..@G1036
..@G1034:
push RDI
mov rax, 47
..@G1036:
SAVE
SAVE
SAVE
jmp ..@G1053
align 8
..@G1052:
dq 12
db 67
db 72
db 82
..@G1053:
mov rax, ..@G1052
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 8719
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G1049
..@G1050:
SAVE
SAVE
jmp ..@G1055
align 8
..@G1054:
dq 4
db 73
..@G1055:
mov rax, ..@G1054
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1057
align 8
..@G1056:
dq 4
db 73
..@G1057:
mov rax, ..@G1056
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1059
align 8
..@G1058:
dq 12
db 76
db 83
db 84
..@G1059:
mov rax, ..@G1058
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1061
align 8
..@G1060:
dq 12
db 67
db 72
db 82
..@G1061:
mov rax, ..@G1060
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1063
align 8
..@G1062:
dq 12
db 76
db 83
db 84
..@G1063:
mov rax, ..@G1062
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G925
jmp ..@G1051
..@G1049:
push RDI
mov rax, 47
..@G1051:
SAVE
SAVE
SAVE
jmp ..@G1068
align 8
..@G1067:
dq 12
db 67
db 72
db 82
..@G1068:
mov rax, ..@G1067
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 23567
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G1064
..@G1065:
SAVE
SAVE
SAVE
jmp ..@G1073
align 8
..@G1072:
dq 12
db 76
db 69
db 78
..@G1073:
mov rax, ..@G1072
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1075
align 8
..@G1074:
dq 4
db 73
..@G1075:
mov rax, ..@G1074
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G1069
..@G1070:
SAVE
jmp ..@G1077
align 8
..@G1076:
dq 196
db 77
db 105
db 115
db 115
db 105
db 110
db 103
db 32
db 34
db 32
db 105
db 110
db 32
db 115
db 116
db 114
db 32
db 112
db 97
db 115
db 115
db 101
db 100
db 32
db 116
db 111
db 32
db 114
db 101
db 97
db 100
db 45
db 116
db 111
db 107
db 101
db 110
db 45
db 102
db 114
db 111
db 109
db 45
db 115
db 116
db 114
db 105
db 110
db 103
..@G1077:
mov rax, ..@G1076
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
LOAD
jmp ..@G1071
..@G1069:
SAVE
SAVE
jmp ..@G1079
align 8
..@G1078:
dq 4
db 73
..@G1079:
mov rax, ..@G1078
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1081
align 8
..@G1080:
dq 4
db 73
..@G1081:
mov rax, ..@G1080
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1083
align 8
..@G1082:
dq 12
db 76
db 83
db 84
..@G1083:
mov rax, ..@G1082
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1085
align 8
..@G1084:
dq 12
db 67
db 72
db 82
..@G1085:
mov rax, ..@G1084
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1087
align 8
..@G1086:
dq 12
db 76
db 83
db 84
..@G1087:
mov rax, ..@G1086
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1089
align 8
..@G1088:
dq 4
db 73
..@G1089:
mov rax, ..@G1088
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1091
align 8
..@G1090:
dq 4
db 73
..@G1091:
mov rax, ..@G1090
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1093
align 8
..@G1092:
dq 12
db 76
db 83
db 84
..@G1093:
mov rax, ..@G1092
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1095
align 8
..@G1094:
dq 12
db 67
db 72
db 82
..@G1095:
mov rax, ..@G1094
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1097
align 8
..@G1096:
dq 12
db 76
db 83
db 84
..@G1097:
mov rax, ..@G1096
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1017
..@G1071:
jmp ..@G1066
..@G1064:
push RDI
mov rax, 47
..@G1066:
SAVE
jmp ..@G1099
align 8
..@G1098:
dq 4
db 73
..@G1099:
mov rax, ..@G1098
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1101
align 8
..@G1100:
dq 4
db 73
..@G1101:
mov rax, ..@G1100
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1103
align 8
..@G1102:
dq 12
db 76
db 83
db 84
..@G1103:
mov rax, ..@G1102
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1105
align 8
..@G1104:
dq 12
db 67
db 72
db 82
..@G1105:
mov rax, ..@G1104
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1107
align 8
..@G1106:
dq 12
db 76
db 83
db 84
..@G1107:
mov rax, ..@G1106
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G1017
pop rsi
pop rdi
;;end of tagbody
jmp ..@G1005
..@G1003:
push RDI
mov rax, 47
..@G1005:
SAVE
jmp ..@G1119
align 8
..@G1118:
dq 4
db 73
..@G1119:
mov rax, ..@G1118
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1121
align 8
..@G1120:
dq 4
db 73
..@G1121:
mov rax, ..@G1120
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1123
align 8
..@G1122:
dq 12
db 76
db 83
db 84
..@G1123:
mov rax, ..@G1122
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1125
align 8
..@G1124:
dq 12
db 67
db 72
db 82
..@G1125:
mov rax, ..@G1124
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1127
align 8
..@G1126:
dq 12
db 76
db 83
db 84
..@G1127:
mov rax, ..@G1126
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G924
..@G925:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1132
align 8
..@G1131:
dq 12
db 76
db 83
db 84
..@G1132:
mov rax, ..@G1131
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1134
align 8
..@G1133:
dq 16
db 78
db 85
db 76
db 76
..@G1134:
mov rax, ..@G1133
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1128
..@G1129:
jmp ..@G1136
align 8
..@G1135:
dq 0
..@G1136:
mov rax, ..@G1135
or rax, 3
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1138
align 8
..@G1137:
dq 4
db 73
..@G1138:
mov rax, ..@G1137
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G909
jmp ..@G1130
..@G1128:
push RDI
mov rax, 47
..@G1130:
SAVE
jmp ..@G1140
align 8
..@G1139:
dq 20
db 84
db 79
db 75
db 69
db 78
..@G1140:
mov rax, ..@G1139
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1142
align 8
..@G1141:
dq 12
db 76
db 83
db 84
..@G1142:
mov rax, ..@G1141
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1144
align 8
..@G1143:
dq 44
db 76
db 73
db 83
db 84
db 45
db 76
db 69
db 78
db 71
db 84
db 72
..@G1144:
mov rax, ..@G1143
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1146
align 8
..@G1145:
dq 12
db 76
db 83
db 84
..@G1146:
mov rax, ..@G1145
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern MAKE_STRING
call MAKE_STRING
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1148
align 8
..@G1147:
dq 16
db 67
db 78
db 84
db 82
..@G1148:
mov rax, ..@G1147
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1150
align 8
..@G1149:
dq 12
db 76
db 83
db 84
..@G1150:
mov rax, ..@G1149
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1152
align 8
..@G1151:
dq 44
db 76
db 73
db 83
db 84
db 45
db 76
db 69
db 78
db 71
db 84
db 72
..@G1152:
mov rax, ..@G1151
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern SUB
call SUB
mov RSI, rax
pop RDI
extern SETL
call SETL
..@G926:
SAVE
jmp ..@G1154
align 8
..@G1153:
dq 12
db 76
db 83
db 84
..@G1154:
mov rax, ..@G1153
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1156
align 8
..@G1155:
dq 12
db 76
db 83
db 84
..@G1156:
mov rax, ..@G1155
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1161
align 8
..@G1160:
dq 12
db 76
db 83
db 84
..@G1161:
mov rax, ..@G1160
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1163
align 8
..@G1162:
dq 16
db 78
db 85
db 76
db 76
..@G1163:
mov rax, ..@G1162
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1157
..@G1158:
SAVE
SAVE
jmp ..@G1165
align 8
..@G1164:
dq 20
db 84
db 79
db 75
db 69
db 78
..@G1165:
mov rax, ..@G1164
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1167
align 8
..@G1166:
dq 4
db 73
..@G1167:
mov rax, ..@G1166
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G909
jmp ..@G1159
..@G1157:
push RDI
mov rax, 47
..@G1159:
SAVE
jmp ..@G1169
align 8
..@G1168:
dq 16
db 67
db 78
db 84
db 82
..@G1169:
mov rax, ..@G1168
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1171
align 8
..@G1170:
dq 16
db 67
db 78
db 84
db 82
..@G1171:
mov rax, ..@G1170
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern SUB
call SUB
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
jmp ..@G1173
align 8
..@G1172:
dq 20
db 84
db 79
db 75
db 69
db 78
..@G1173:
mov rax, ..@G1172
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1175
align 8
..@G1174:
dq 16
db 67
db 78
db 84
db 82
..@G1175:
mov rax, ..@G1174
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
push RSI
SAVE
SAVE
jmp ..@G1177
align 8
..@G1176:
dq 12
db 76
db 83
db 84
..@G1177:
mov rax, ..@G1176
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDX, rax
pop RSI
pop RDI
extern SET_CHAR
call SET_CHAR
jmp ..@G926
;;end of tagbody
pop r15
..@G909:
LOAD
LEAVE
ret
.end: mov rax, G908
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1199
align 8
..@G1198:
dq 4
db 73
..@G1199:
mov rax, ..@G1198
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G1201
align 8
..@G1200:
dq 12
db 83
db 84
db 82
..@G1201:
mov rax, ..@G1200
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G908-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1203
align 8
..@G1202:
dq 48
db 80
db 65
db 82
db 83
db 69
db 45
db 78
db 85
db 77
db 66
db 69
db 82
..@G1203:
mov rax, ..@G1202
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1204.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1204:
ENTER
SAVE
push r15
mov rax, r15
push rax
SAVE
SAVE
jmp ..@G1207
align 8
..@G1206:
dq 12
db 83
db 84
db 82
..@G1207:
mov rax, ..@G1206
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern STRING_LENGTH
call STRING_LENGTH
push rax
SAVE
jmp ..@G1209
align 8
..@G1208:
dq 12
db 76
db 69
db 78
..@G1209:
mov rax, ..@G1208
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 0
push rax
SAVE
jmp ..@G1211
align 8
..@G1210:
dq 4
db 78
..@G1211:
mov rax, ..@G1210
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 12303
push rax
SAVE
jmp ..@G1213
align 8
..@G1212:
dq 12
db 67
db 72
db 82
..@G1213:
mov rax, ..@G1212
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 0
push rax
SAVE
jmp ..@G1215
align 8
..@G1214:
dq 4
db 73
..@G1215:
mov rax, ..@G1214
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G1216:
SAVE
jmp ..@G1218
align 8
..@G1217:
dq 12
db 67
db 72
db 82
..@G1218:
mov rax, ..@G1217
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1220
align 8
..@G1219:
dq 12
db 83
db 84
db 82
..@G1220:
mov rax, ..@G1219
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1222
align 8
..@G1221:
dq 4
db 73
..@G1222:
mov rax, ..@G1221
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CHAR
call CHAR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1224
align 8
..@G1223:
dq 4
db 78
..@G1224:
mov rax, ..@G1223
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1226
align 8
..@G1225:
dq 12
db 67
db 72
db 82
..@G1226:
mov rax, ..@G1225
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
shr rax, 6
mov RDI, rax
push RDI
mov RSI, 192
pop RDI
extern SUB
call SUB
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1228
align 8
..@G1227:
dq 4
db 78
..@G1228:
mov rax, ..@G1227
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 40
pop RDI
extern MUL
call MUL
mov RSI, rax
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1230
align 8
..@G1229:
dq 4
db 73
..@G1230:
mov rax, ..@G1229
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1232
align 8
..@G1231:
dq 4
db 73
..@G1232:
mov rax, ..@G1231
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
SAVE
jmp ..@G1237
align 8
..@G1236:
dq 12
db 76
db 69
db 78
..@G1237:
mov rax, ..@G1236
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1239
align 8
..@G1238:
dq 4
db 73
..@G1239:
mov rax, ..@G1238
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G1233
..@G1234:
SAVE
SAVE
jmp ..@G1241
align 8
..@G1240:
dq 4
db 78
..@G1241:
mov rax, ..@G1240
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G1205
jmp ..@G1235
..@G1233:
jmp ..@G1216
..@G1235:
;;end of tagbody
pop r15
..@G1205:
LOAD
LEAVE
ret
.end: mov rax, G1204
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1248
align 8
..@G1247:
dq 12
db 83
db 84
db 82
..@G1248:
mov rax, ..@G1247
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1204-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1250
align 8
..@G1249:
dq 48
db 76
db 79
db 87
db 69
db 82
db 45
db 67
db 65
db 83
db 69
db 45
db 80
..@G1250:
mov rax, ..@G1249
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1251.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1251:
ENTER
SAVE
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1257
align 8
..@G1256:
dq 16
db 67
db 72
db 65
db 82
..@G1257:
mov rax, ..@G1256
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
shr rax, 6
mov RDI, rax
push RDI
mov RSI, 492
pop RDI
extern LESSER
call LESSER
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1259
align 8
..@G1258:
dq 16
db 67
db 72
db 65
db 82
..@G1259:
mov rax, ..@G1258
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
shr rax, 6
mov RDI, rax
push RDI
mov RSI, 384
pop RDI
extern GREATER
call GREATER
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1261
align 8
..@G1260:
dq 12
db 65
db 78
db 68
..@G1261:
mov rax, ..@G1260
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1253
..@G1254:
push RDI
mov rax, 4
jmp ..@G1255
..@G1253:
push RDI
mov rax, 47
..@G1255:
..@G1252:
LOAD
LEAVE
ret
.end: mov rax, G1251
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1263
align 8
..@G1262:
dq 16
db 67
db 72
db 65
db 82
..@G1263:
mov rax, ..@G1262
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1251-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1265
align 8
..@G1264:
dq 96
db 67
db 72
db 65
db 82
db 45
db 76
db 79
db 87
db 69
db 82
db 45
db 84
db 79
db 45
db 85
db 80
db 80
db 69
db 82
db 45
db 67
db 65
db 83
db 69
..@G1265:
mov rax, ..@G1264
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1266.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1266:
ENTER
SAVE
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1272
align 8
..@G1271:
dq 12
db 67
db 72
db 82
..@G1272:
mov rax, ..@G1271
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1274
align 8
..@G1273:
dq 48
db 76
db 79
db 87
db 69
db 82
db 45
db 67
db 65
db 83
db 69
db 45
db 80
..@G1274:
mov rax, ..@G1273
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1268
..@G1269:
SAVE
SAVE
jmp ..@G1276
align 8
..@G1275:
dq 12
db 67
db 72
db 82
..@G1276:
mov rax, ..@G1275
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
sub rax, 0x20<<8
jmp ..@G1270
..@G1268:
SAVE
SAVE
jmp ..@G1278
align 8
..@G1277:
dq 12
db 67
db 72
db 82
..@G1278:
mov rax, ..@G1277
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
..@G1270:
..@G1267:
LOAD
LEAVE
ret
.end: mov rax, G1266
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1280
align 8
..@G1279:
dq 12
db 67
db 72
db 82
..@G1280:
mov rax, ..@G1279
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1266-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1282
align 8
..@G1281:
dq 104
db 83
db 84
db 82
db 73
db 78
db 71
db 45
db 76
db 79
db 87
db 69
db 82
db 45
db 84
db 79
db 45
db 85
db 80
db 80
db 69
db 82
db 45
db 67
db 65
db 83
db 69
..@G1282:
mov rax, ..@G1281
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1283.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1283:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 0
push rax
SAVE
jmp ..@G1286
align 8
..@G1285:
dq 4
db 73
..@G1286:
mov rax, ..@G1285
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1288
align 8
..@G1287:
dq 12
db 83
db 84
db 82
..@G1288:
mov rax, ..@G1287
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern STRING_LENGTH
call STRING_LENGTH
push rax
SAVE
jmp ..@G1290
align 8
..@G1289:
dq 12
db 76
db 69
db 78
..@G1290:
mov rax, ..@G1289
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G1291:
SAVE
SAVE
jmp ..@G1293
align 8
..@G1292:
dq 12
db 83
db 84
db 82
..@G1293:
mov rax, ..@G1292
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1295
align 8
..@G1294:
dq 4
db 73
..@G1295:
mov rax, ..@G1294
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
push RSI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1297
align 8
..@G1296:
dq 12
db 83
db 84
db 82
..@G1297:
mov rax, ..@G1296
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1299
align 8
..@G1298:
dq 4
db 73
..@G1299:
mov rax, ..@G1298
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CHAR
call CHAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1301
align 8
..@G1300:
dq 96
db 67
db 72
db 65
db 82
db 45
db 76
db 79
db 87
db 69
db 82
db 45
db 84
db 79
db 45
db 85
db 80
db 80
db 69
db 82
db 45
db 67
db 65
db 83
db 69
..@G1301:
mov rax, ..@G1300
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDX, rax
pop RSI
pop RDI
extern SET_CHAR
call SET_CHAR
SAVE
jmp ..@G1303
align 8
..@G1302:
dq 4
db 73
..@G1303:
mov rax, ..@G1302
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov RDI, 4
push RDI
SAVE
SAVE
jmp ..@G1305
align 8
..@G1304:
dq 4
db 73
..@G1305:
mov rax, ..@G1304
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1310
align 8
..@G1309:
dq 4
db 73
..@G1310:
mov rax, ..@G1309
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1312
align 8
..@G1311:
dq 12
db 76
db 69
db 78
..@G1312:
mov rax, ..@G1311
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1314
align 8
..@G1313:
dq 12
db 78
db 79
db 84
..@G1314:
mov rax, ..@G1313
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1306
..@G1307:
jmp ..@G1291
jmp ..@G1308
..@G1306:
SAVE
SAVE
jmp ..@G1316
align 8
..@G1315:
dq 12
db 83
db 84
db 82
..@G1316:
mov rax, ..@G1315
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G1284
..@G1308:
;;end of tagbody
pop r15
..@G1284:
LOAD
LEAVE
ret
.end: mov rax, G1283
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1322
align 8
..@G1321:
dq 12
db 83
db 84
db 82
..@G1322:
mov rax, ..@G1321
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1283-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1324
align 8
..@G1323:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 87
db 79
db 82
db 68
..@G1324:
mov rax, ..@G1323
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1325.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1325:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1328
align 8
..@G1327:
dq 24
db 83
db 89
db 77
db 66
db 79
db 76
..@G1328:
mov rax, ..@G1327
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 0
push rax
SAVE
jmp ..@G1330
align 8
..@G1329:
dq 4
db 78
..@G1330:
mov rax, ..@G1329
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1332
align 8
..@G1331:
dq 16
db 87
db 79
db 82
db 68
..@G1332:
mov rax, ..@G1331
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 0
pop RDI
extern CHAR
call CHAR
push rax
SAVE
jmp ..@G1334
align 8
..@G1333:
dq 12
db 67
db 72
db 82
..@G1334:
mov rax, ..@G1333
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 0
push rax
SAVE
jmp ..@G1336
align 8
..@G1335:
dq 4
db 73
..@G1336:
mov rax, ..@G1335
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1338
align 8
..@G1337:
dq 16
db 87
db 79
db 82
db 68
..@G1338:
mov rax, ..@G1337
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern STRING_LENGTH
call STRING_LENGTH
push rax
SAVE
jmp ..@G1340
align 8
..@G1339:
dq 32
db 87
db 79
db 82
db 68
db 45
db 76
db 69
db 78
..@G1340:
mov rax, ..@G1339
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G1341:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1348
align 8
..@G1347:
dq 12
db 67
db 72
db 82
..@G1348:
mov rax, ..@G1347
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
shr rax, 6
mov RDI, rax
push RDI
mov RSI, 232
pop RDI
extern LESSER
call LESSER
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1350
align 8
..@G1349:
dq 12
db 67
db 72
db 82
..@G1350:
mov rax, ..@G1349
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
shr rax, 6
mov RDI, rax
push RDI
mov RSI, 164
pop RDI
extern GREATER
call GREATER
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1352
align 8
..@G1351:
dq 12
db 65
db 78
db 68
..@G1352:
mov rax, ..@G1351
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1344
..@G1345:
SAVE
SAVE
jmp ..@G1354
align 8
..@G1353:
dq 4
db 73
..@G1354:
mov rax, ..@G1353
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1356
align 8
..@G1355:
dq 4
db 73
..@G1356:
mov rax, ..@G1355
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
SAVE
jmp ..@G1361
align 8
..@G1360:
dq 32
db 87
db 79
db 82
db 68
db 45
db 76
db 69
db 78
..@G1361:
mov rax, ..@G1360
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1363
align 8
..@G1362:
dq 4
db 73
..@G1363:
mov rax, ..@G1362
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G1357
..@G1358:
jmp ..@G1342
jmp ..@G1359
..@G1357:
push RDI
mov rax, 47
..@G1359:
SAVE
SAVE
jmp ..@G1365
align 8
..@G1364:
dq 12
db 67
db 72
db 82
..@G1365:
mov rax, ..@G1364
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1367
align 8
..@G1366:
dq 16
db 87
db 79
db 82
db 68
..@G1367:
mov rax, ..@G1366
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1369
align 8
..@G1368:
dq 4
db 73
..@G1369:
mov rax, ..@G1368
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CHAR
call CHAR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1341
jmp ..@G1346
..@G1344:
jmp ..@G1343
..@G1346:
..@G1342:
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1371
align 8
..@G1370:
dq 16
db 87
db 79
db 82
db 68
..@G1371:
mov rax, ..@G1370
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1373
align 8
..@G1372:
dq 48
db 80
db 65
db 82
db 83
db 69
db 45
db 78
db 85
db 77
db 66
db 69
db 82
..@G1373:
mov rax, ..@G1372
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G1326
..@G1343:
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1375
align 8
..@G1374:
dq 16
db 87
db 79
db 82
db 68
..@G1375:
mov rax, ..@G1374
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1377
align 8
..@G1376:
dq 104
db 83
db 84
db 82
db 73
db 78
db 71
db 45
db 76
db 79
db 87
db 69
db 82
db 45
db 84
db 79
db 45
db 85
db 80
db 80
db 69
db 82
db 45
db 67
db 65
db 83
db 69
..@G1377:
mov rax, ..@G1376
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
;;end of tagbody
pop r15
..@G1326:
LOAD
LEAVE
ret
.end: mov rax, G1325
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1385
align 8
..@G1384:
dq 16
db 87
db 79
db 82
db 68
..@G1385:
mov rax, ..@G1384
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1325-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1387
align 8
..@G1386:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 76
db 73
db 83
db 84
..@G1387:
mov rax, ..@G1386
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1388.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1388:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1391
align 8
..@G1390:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1391:
mov rax, ..@G1390
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1393
align 8
..@G1392:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1393:
mov rax, ..@G1392
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
SAVE
jmp ..@G1397
align 8
..@G1396:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1397:
mov rax, ..@G1396
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1399
align 8
..@G1398:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1399:
mov rax, ..@G1398
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
..@G1394:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1404
align 8
..@G1403:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1404:
mov rax, ..@G1403
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1406
align 8
..@G1405:
dq 16
db 78
db 85
db 76
db 76
..@G1406:
mov rax, ..@G1405
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1400
..@G1401:
SAVE
jmp ..@G1408
align 8
..@G1407:
dq 96
db 77
db 105
db 115
db 115
db 105
db 110
db 103
db 32
db 41
db 32
db 105
db 110
db 32
db 112
db 97
db 114
db 115
db 101
db 100
db 32
db 108
db 105
db 115
db 116
..@G1408:
mov rax, ..@G1407
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
LOAD
jmp ..@G1402
..@G1400:
push RDI
mov rax, 47
..@G1402:
SAVE
SAVE
SAVE
jmp ..@G1413
align 8
..@G1412:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1413:
mov rax, ..@G1412
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1415
align 8
..@G1414:
dq 4
db 41
..@G1415:
mov rax, ..@G1414
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1409
..@G1410:
jmp ..@G1395
jmp ..@G1411
..@G1409:
push RDI
mov rax, 47
..@G1411:
SAVE
SAVE
SAVE
jmp ..@G1420
align 8
..@G1419:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1420:
mov rax, ..@G1419
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1422
align 8
..@G1421:
dq 4
db 40
..@G1422:
mov rax, ..@G1421
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1416
..@G1417:
SAVE
SAVE
jmp ..@G1424
align 8
..@G1423:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1424:
mov rax, ..@G1423
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1426
align 8
..@G1425:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1426:
mov rax, ..@G1425
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1428
align 8
..@G1427:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 76
db 73
db 83
db 84
..@G1428:
mov rax, ..@G1427
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1430
align 8
..@G1429:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1430:
mov rax, ..@G1429
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1432
align 8
..@G1431:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1432:
mov rax, ..@G1431
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1434
align 8
..@G1433:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1434:
mov rax, ..@G1433
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1436
align 8
..@G1435:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1436:
mov rax, ..@G1435
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1438
align 8
..@G1437:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1438:
mov rax, ..@G1437
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1394
jmp ..@G1418
..@G1416:
push RDI
mov rax, 47
..@G1418:
SAVE
jmp ..@G1440
align 8
..@G1439:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1440:
mov rax, ..@G1439
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1442
align 8
..@G1441:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1442:
mov rax, ..@G1441
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1444
align 8
..@G1443:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 87
db 79
db 82
db 68
..@G1444:
mov rax, ..@G1443
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1446
align 8
..@G1445:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1446:
mov rax, ..@G1445
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1448
align 8
..@G1447:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1448:
mov rax, ..@G1447
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1450
align 8
..@G1449:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1450:
mov rax, ..@G1449
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G1394
..@G1395:
SAVE
jmp ..@G1452
align 8
..@G1451:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1452:
mov rax, ..@G1451
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1454
align 8
..@G1453:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1454:
mov rax, ..@G1453
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1456
align 8
..@G1455:
dq 48
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
..@G1456:
mov rax, ..@G1455
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1458
align 8
..@G1457:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1458:
mov rax, ..@G1457
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1460
align 8
..@G1459:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1460:
mov rax, ..@G1459
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
jmp ..@G1462
align 8
..@G1461:
dq 32
db 84
db 77
db 80
db 45
db 76
db 73
db 83
db 84
..@G1462:
mov rax, ..@G1461
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1464
align 8
..@G1463:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1464:
mov rax, ..@G1463
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G1389
;;end of tagbody
pop r15
..@G1389:
LOAD
LEAVE
ret
.end: mov rax, G1388
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1478
align 8
..@G1477:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1478:
mov rax, ..@G1477
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1388-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1480
align 8
..@G1479:
dq 44
db 80
db 65
db 82
db 83
db 69
db 45
db 81
db 85
db 79
db 84
db 69
..@G1480:
mov rax, ..@G1479
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1481.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1481:
ENTER
SAVE
SAVE
jmp ..@G1484
align 8
..@G1483:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1484:
mov rax, ..@G1483
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1486
align 8
..@G1485:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1486:
mov rax, ..@G1485
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1488
align 8
..@G1487:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1488:
mov rax, ..@G1487
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1490
align 8
..@G1489:
dq 16
db 69
db 88
db 80
db 82
..@G1490:
mov rax, ..@G1489
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
SAVE
SAVE
SAVE
jmp ..@G1496
align 8
..@G1495:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1496:
mov rax, ..@G1495
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1498
align 8
..@G1497:
dq 4
db 40
..@G1498:
mov rax, ..@G1497
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1492
..@G1493:
SAVE
SAVE
jmp ..@G1500
align 8
..@G1499:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1500:
mov rax, ..@G1499
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1502
align 8
..@G1501:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1502:
mov rax, ..@G1501
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1504
align 8
..@G1503:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 76
db 73
db 83
db 84
..@G1504:
mov rax, ..@G1503
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1506
align 8
..@G1505:
dq 16
db 69
db 88
db 80
db 82
..@G1506:
mov rax, ..@G1505
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1508
align 8
..@G1507:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1508:
mov rax, ..@G1507
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1510
align 8
..@G1509:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1510:
mov rax, ..@G1509
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1512
align 8
..@G1511:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1512:
mov rax, ..@G1511
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1491
jmp ..@G1494
..@G1492:
push RDI
mov rax, 47
..@G1494:
SAVE
SAVE
SAVE
jmp ..@G1517
align 8
..@G1516:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1517:
mov rax, ..@G1516
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1519
align 8
..@G1518:
dq 4
db 39
..@G1519:
mov rax, ..@G1518
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1513
..@G1514:
SAVE
SAVE
jmp ..@G1521
align 8
..@G1520:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1521:
mov rax, ..@G1520
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1523
align 8
..@G1522:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1523:
mov rax, ..@G1522
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1525
align 8
..@G1524:
dq 44
db 80
db 65
db 82
db 83
db 69
db 45
db 81
db 85
db 79
db 84
db 69
..@G1525:
mov rax, ..@G1524
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1527
align 8
..@G1526:
dq 16
db 69
db 88
db 80
db 82
..@G1527:
mov rax, ..@G1526
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1529
align 8
..@G1528:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1529:
mov rax, ..@G1528
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1531
align 8
..@G1530:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1531:
mov rax, ..@G1530
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1533
align 8
..@G1532:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1533:
mov rax, ..@G1532
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1491
jmp ..@G1515
..@G1513:
push RDI
mov rax, 47
..@G1515:
SAVE
SAVE
SAVE
jmp ..@G1538
align 8
..@G1537:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1538:
mov rax, ..@G1537
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1540
align 8
..@G1539:
dq 4
db 96
..@G1540:
mov rax, ..@G1539
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1534
..@G1535:
SAVE
jmp ..@G1542
align 8
..@G1541:
dq 220
db 81
db 117
db 97
db 115
db 105
db 113
db 117
db 111
db 116
db 101
db 115
db 32
db 104
db 97
db 118
db 101
db 110
db 39
db 116
db 32
db 98
db 101
db 101
db 110
db 32
db 105
db 109
db 112
db 108
db 101
db 109
db 101
db 110
db 116
db 101
db 100
db 32
db 105
db 110
db 32
db 116
db 104
db 101
db 32
db 112
db 97
db 114
db 115
db 101
db 114
db 32
db 121
db 101
db 116
db 33
..@G1542:
mov rax, ..@G1541
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
LOAD
jmp ..@G1536
..@G1534:
push RDI
mov rax, 47
..@G1536:
SAVE
jmp ..@G1544
align 8
..@G1543:
dq 16
db 69
db 88
db 80
db 82
..@G1544:
mov rax, ..@G1543
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1546
align 8
..@G1545:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1546:
mov rax, ..@G1545
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1548
align 8
..@G1547:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 87
db 79
db 82
db 68
..@G1548:
mov rax, ..@G1547
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1550
align 8
..@G1549:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1550:
mov rax, ..@G1549
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1552
align 8
..@G1551:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1552:
mov rax, ..@G1551
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
..@G1491:
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1554
align 8
..@G1553:
dq 16
db 69
db 88
db 80
db 82
..@G1554:
mov rax, ..@G1553
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
jmp ..@G1556
align 8
..@G1555:
dq 20
db 81
db 85
db 79
db 84
db 69
..@G1556:
mov rax, ..@G1555
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1558
align 8
..@G1557:
dq 16
db 76
db 73
db 83
db 84
..@G1558:
mov rax, ..@G1557
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1560
align 8
..@G1559:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1560:
mov rax, ..@G1559
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G1482
;;end of tagbody
pop r15
..@G1482:
LOAD
LEAVE
ret
.end: mov rax, G1481
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1569
align 8
..@G1568:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1569:
mov rax, ..@G1568
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1481-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1571
align 8
..@G1570:
dq 64
db 82
db 69
db 65
db 68
db 45
db 70
db 82
db 79
db 77
db 45
db 83
db 84
db 82
db 73
db 78
db 71
..@G1571:
mov rax, ..@G1570
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1572.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1572:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1575
align 8
..@G1574:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1575:
mov rax, ..@G1574
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1577
align 8
..@G1576:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1577:
mov rax, ..@G1576
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
jmp ..@G1579
align 8
..@G1578:
dq 0
..@G1579:
mov rax, ..@G1578
or rax, 3
mov RDI, rax
push RDI
mov RSI, 0
pop RDI
extern CONS
call CONS
push rax
SAVE
jmp ..@G1581
align 8
..@G1580:
dq 44
db 84
db 79
db 75
db 69
db 78
db 45
db 73
db 78
db 68
db 69
db 88
..@G1581:
mov rax, ..@G1580
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1583
align 8
..@G1582:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1583:
mov rax, ..@G1582
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G1584:
SAVE
jmp ..@G1589
align 8
..@G1588:
dq 44
db 84
db 79
db 75
db 69
db 78
db 45
db 73
db 78
db 68
db 69
db 88
..@G1589:
mov rax, ..@G1588
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1591
align 8
..@G1590:
dq 44
db 84
db 79
db 75
db 69
db 78
db 45
db 73
db 78
db 68
db 69
db 88
..@G1591:
mov rax, ..@G1590
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1593
align 8
..@G1592:
dq 12
db 83
db 84
db 82
..@G1593:
mov rax, ..@G1592
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1595
align 8
..@G1594:
dq 88
db 82
db 69
db 65
db 68
db 45
db 84
db 79
db 75
db 69
db 78
db 45
db 70
db 82
db 79
db 77
db 45
db 83
db 84
db 82
db 73
db 78
db 71
..@G1595:
mov rax, ..@G1594
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
SAVE
jmp ..@G1600
align 8
..@G1599:
dq 44
db 84
db 79
db 75
db 69
db 78
db 45
db 73
db 78
db 68
db 69
db 88
..@G1600:
mov rax, ..@G1599
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1602
align 8
..@G1601:
dq 0
..@G1602:
mov rax, ..@G1601
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1596
..@G1597:
jmp ..@G1585
jmp ..@G1598
..@G1596:
SAVE
SAVE
jmp ..@G1604
align 8
..@G1603:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1604:
mov rax, ..@G1603
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1606
align 8
..@G1605:
dq 44
db 84
db 79
db 75
db 69
db 78
db 45
db 73
db 78
db 68
db 69
db 88
..@G1606:
mov rax, ..@G1605
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1608
align 8
..@G1607:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1608:
mov rax, ..@G1607
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1584
..@G1598:
..@G1585:
SAVE
jmp ..@G1610
align 8
..@G1609:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1610:
mov rax, ..@G1609
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1612
align 8
..@G1611:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1612:
mov rax, ..@G1611
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1614
align 8
..@G1613:
dq 48
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
..@G1614:
mov rax, ..@G1613
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
..@G1586:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1619
align 8
..@G1618:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1619:
mov rax, ..@G1618
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1621
align 8
..@G1620:
dq 16
db 78
db 85
db 76
db 76
..@G1621:
mov rax, ..@G1620
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1615
..@G1616:
jmp ..@G1587
jmp ..@G1617
..@G1615:
push RDI
mov rax, 47
..@G1617:
SAVE
SAVE
SAVE
jmp ..@G1626
align 8
..@G1625:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1626:
mov rax, ..@G1625
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1628
align 8
..@G1627:
dq 4
db 40
..@G1628:
mov rax, ..@G1627
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1622
..@G1623:
SAVE
SAVE
jmp ..@G1630
align 8
..@G1629:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1630:
mov rax, ..@G1629
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1632
align 8
..@G1631:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1632:
mov rax, ..@G1631
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1634
align 8
..@G1633:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 76
db 73
db 83
db 84
..@G1634:
mov rax, ..@G1633
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1636
align 8
..@G1635:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1636:
mov rax, ..@G1635
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1638
align 8
..@G1637:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1638:
mov rax, ..@G1637
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1640
align 8
..@G1639:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1640:
mov rax, ..@G1639
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1642
align 8
..@G1641:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1642:
mov rax, ..@G1641
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1644
align 8
..@G1643:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1644:
mov rax, ..@G1643
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1586
jmp ..@G1624
..@G1622:
push RDI
mov rax, 47
..@G1624:
SAVE
SAVE
SAVE
jmp ..@G1649
align 8
..@G1648:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1649:
mov rax, ..@G1648
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1651
align 8
..@G1650:
dq 4
db 39
..@G1651:
mov rax, ..@G1650
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1645
..@G1646:
SAVE
SAVE
jmp ..@G1653
align 8
..@G1652:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1653:
mov rax, ..@G1652
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1655
align 8
..@G1654:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1655:
mov rax, ..@G1654
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1657
align 8
..@G1656:
dq 44
db 80
db 65
db 82
db 83
db 69
db 45
db 81
db 85
db 79
db 84
db 69
..@G1657:
mov rax, ..@G1656
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1659
align 8
..@G1658:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1659:
mov rax, ..@G1658
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1661
align 8
..@G1660:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1661:
mov rax, ..@G1660
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1663
align 8
..@G1662:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1663:
mov rax, ..@G1662
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
SAVE
SAVE
jmp ..@G1665
align 8
..@G1664:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1665:
mov rax, ..@G1664
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1667
align 8
..@G1666:
dq 32
db 84
db 77
db 80
db 45
db 67
db 79
db 78
db 83
..@G1667:
mov rax, ..@G1666
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1586
jmp ..@G1647
..@G1645:
push RDI
mov rax, 47
..@G1647:
SAVE
SAVE
SAVE
jmp ..@G1672
align 8
..@G1671:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1672:
mov rax, ..@G1671
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
jmp ..@G1674
align 8
..@G1673:
dq 4
db 96
..@G1674:
mov rax, ..@G1673
or rax, 3
mov RSI, rax
pop RDI
extern STRING_EQ
call STRING_EQ
LOAD
cmp rax, 47
je ..@G1668
..@G1669:
SAVE
jmp ..@G1676
align 8
..@G1675:
dq 220
db 81
db 117
db 97
db 115
db 105
db 113
db 117
db 111
db 116
db 101
db 115
db 32
db 104
db 97
db 118
db 101
db 110
db 39
db 116
db 32
db 98
db 101
db 101
db 110
db 32
db 105
db 109
db 112
db 108
db 101
db 109
db 101
db 110
db 116
db 101
db 100
db 32
db 105
db 110
db 32
db 116
db 104
db 101
db 32
db 112
db 97
db 114
db 115
db 101
db 114
db 32
db 121
db 101
db 116
db 33
..@G1676:
mov rax, ..@G1675
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
LOAD
jmp ..@G1670
..@G1668:
push RDI
mov rax, 47
..@G1670:
SAVE
jmp ..@G1678
align 8
..@G1677:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1678:
mov rax, ..@G1677
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1680
align 8
..@G1679:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1680:
mov rax, ..@G1679
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1682
align 8
..@G1681:
dq 40
db 80
db 65
db 82
db 83
db 69
db 45
db 87
db 79
db 82
db 68
..@G1682:
mov rax, ..@G1681
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1684
align 8
..@G1683:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1684:
mov rax, ..@G1683
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1686
align 8
..@G1685:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1686:
mov rax, ..@G1685
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1688
align 8
..@G1687:
dq 40
db 84
db 79
db 75
db 69
db 78
db 45
db 76
db 73
db 83
db 84
..@G1688:
mov rax, ..@G1687
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G1586
..@G1587:
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1690
align 8
..@G1689:
dq 40
db 70
db 73
db 78
db 65
db 76
db 45
db 76
db 73
db 83
db 84
..@G1690:
mov rax, ..@G1689
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1692
align 8
..@G1691:
dq 48
db 76
db 73
db 83
db 84
db 45
db 82
db 69
db 86
db 69
db 82
db 83
db 69
..@G1692:
mov rax, ..@G1691
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G1573
;;end of tagbody
pop r15
..@G1573:
LOAD
LEAVE
ret
.end: mov rax, G1572
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1709
align 8
..@G1708:
dq 12
db 83
db 84
db 82
..@G1709:
mov rax, ..@G1708
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1572-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1711
align 8
..@G1710:
dq 16
db 82
db 69
db 65
db 68
..@G1711:
mov rax, ..@G1710
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1712.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1712:
ENTER
SAVE
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1715
align 8
..@G1714:
dq 36
db 82
db 69
db 65
db 68
db 45
db 76
db 73
db 78
db 69
..@G1715:
mov rax, ..@G1714
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1717
align 8
..@G1716:
dq 64
db 82
db 69
db 65
db 68
db 45
db 70
db 82
db 79
db 77
db 45
db 83
db 84
db 82
db 73
db 78
db 71
..@G1717:
mov rax, ..@G1716
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
..@G1713:
LOAD
LEAVE
ret
.end: mov rax, G1712
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
pop rsi
pop rdi
;;end of list
mov [G1712-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1719
align 8
..@G1718:
dq 56
db 78
db 85
db 77
db 66
db 69
db 82
db 45
db 84
db 79
db 45
db 67
db 72
db 65
db 82
..@G1719:
mov rax, ..@G1718
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1720.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1720:
ENTER
SAVE
SAVE
SAVE
jmp ..@G1723
align 8
..@G1722:
dq 4
db 78
..@G1723:
mov rax, ..@G1722
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 40
pop RDI
extern REM
call REM
add rax, 0x30<<2
shl rax, 6
or rax, 1111b
..@G1721:
LOAD
LEAVE
ret
.end: mov rax, G1720
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1725
align 8
..@G1724:
dq 4
db 78
..@G1725:
mov rax, ..@G1724
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1720-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1727
align 8
..@G1726:
dq 64
db 78
db 85
db 77
db 66
db 69
db 82
db 45
db 84
db 79
db 45
db 83
db 84
db 82
db 73
db 78
db 71
..@G1727:
mov rax, ..@G1726
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1728.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1728:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 4
push rax
SAVE
jmp ..@G1731
align 8
..@G1730:
dq 4
db 73
..@G1731:
mov rax, ..@G1730
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1733
align 8
..@G1732:
dq 12
db 76
db 83
db 84
..@G1733:
mov rax, ..@G1732
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1735
align 8
..@G1734:
dq 4
db 78
..@G1735:
mov rax, ..@G1734
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 0
pop RDI
extern LESSER
call LESSER
push rax
SAVE
jmp ..@G1737
align 8
..@G1736:
dq 12
db 78
db 69
db 71
..@G1737:
mov rax, ..@G1736
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
jmp ..@G1739
align 8
..@G1738:
dq 0
..@G1739:
mov rax, ..@G1738
or rax, 3
push rax
SAVE
jmp ..@G1741
align 8
..@G1740:
dq 12
db 83
db 84
db 82
..@G1741:
mov rax, ..@G1740
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
SAVE
SAVE
jmp ..@G1746
align 8
..@G1745:
dq 12
db 78
db 69
db 71
..@G1746:
mov rax, ..@G1745
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G1742
..@G1743:
SAVE
SAVE
jmp ..@G1748
align 8
..@G1747:
dq 4
db 78
..@G1748:
mov rax, ..@G1747
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov RDI, 0
push RDI
SAVE
SAVE
jmp ..@G1750
align 8
..@G1749:
dq 4
db 78
..@G1750:
mov rax, ..@G1749
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern SUB
call SUB
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1744
..@G1742:
push RDI
mov rax, 47
..@G1744:
;;start of tagbody
..@G1751:
SAVE
jmp ..@G1753
align 8
..@G1752:
dq 12
db 76
db 83
db 84
..@G1753:
mov rax, ..@G1752
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1755
align 8
..@G1754:
dq 4
db 78
..@G1755:
mov rax, ..@G1754
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 40
pop RDI
extern REM
call REM
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1757
align 8
..@G1756:
dq 12
db 76
db 83
db 84
..@G1757:
mov rax, ..@G1756
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1759
align 8
..@G1758:
dq 4
db 78
..@G1759:
mov rax, ..@G1758
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1761
align 8
..@G1760:
dq 4
db 78
..@G1761:
mov rax, ..@G1760
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 40
pop RDI
extern DIV
call DIV
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1766
align 8
..@G1765:
dq 4
db 78
..@G1766:
mov rax, ..@G1765
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 0
pop RDI
extern EQ
call EQ
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1768
align 8
..@G1767:
dq 12
db 78
db 79
db 84
..@G1768:
mov rax, ..@G1767
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1762
..@G1763:
jmp ..@G1751
jmp ..@G1764
..@G1762:
push RDI
mov rax, 47
..@G1764:
;;end of tagbody
SAVE
SAVE
jmp ..@G1777
align 8
..@G1776:
dq 12
db 78
db 69
db 71
..@G1777:
mov rax, ..@G1776
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G1773
..@G1774:
SAVE
SAVE
jmp ..@G1779
align 8
..@G1778:
dq 12
db 76
db 83
db 84
..@G1779:
mov rax, ..@G1778
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov RDI, 4
push RDI
SAVE
SAVE
jmp ..@G1781
align 8
..@G1780:
dq 12
db 76
db 83
db 84
..@G1781:
mov rax, ..@G1780
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern CONS
call CONS
mov RSI, rax
pop RDI
extern SETL
call SETL
LOAD
jmp ..@G1775
..@G1773:
push RDI
mov rax, 47
..@G1775:
SAVE
jmp ..@G1783
align 8
..@G1782:
dq 12
db 83
db 84
db 82
..@G1783:
mov rax, ..@G1782
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1785
align 8
..@G1784:
dq 12
db 76
db 83
db 84
..@G1785:
mov rax, ..@G1784
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1787
align 8
..@G1786:
dq 44
db 76
db 73
db 83
db 84
db 45
db 76
db 69
db 78
db 71
db 84
db 72
..@G1787:
mov rax, ..@G1786
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1789
align 8
..@G1788:
dq 12
db 76
db 83
db 84
..@G1789:
mov rax, ..@G1788
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1791
align 8
..@G1790:
dq 56
db 78
db 85
db 77
db 66
db 69
db 82
db 45
db 84
db 79
db 45
db 67
db 72
db 65
db 82
..@G1791:
mov rax, ..@G1790
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern MAKE_STRING
call MAKE_STRING
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1793
align 8
..@G1792:
dq 12
db 76
db 83
db 84
..@G1793:
mov rax, ..@G1792
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1795
align 8
..@G1794:
dq 12
db 76
db 83
db 84
..@G1795:
mov rax, ..@G1794
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1800
align 8
..@G1799:
dq 20
db 68
db 73
db 71
db 73
db 84
..@G1800:
mov rax, ..@G1799
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1802
align 8
..@G1801:
dq 12
db 76
db 83
db 84
..@G1802:
mov rax, ..@G1801
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
push rax
SAVE
jmp ..@G1804
align 8
..@G1803:
dq 20
db 71
db 49
db 55
db 57
db 54
..@G1804:
mov rax, ..@G1803
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G1805:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1810
align 8
..@G1809:
dq 20
db 71
db 49
db 55
db 57
db 54
..@G1810:
mov rax, ..@G1809
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1812
align 8
..@G1811:
dq 16
db 78
db 85
db 76
db 76
..@G1812:
mov rax, ..@G1811
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1806
..@G1807:
mov rax, 47
jmp ..@G1798
jmp ..@G1808
..@G1806:
push RDI
mov rax, 47
..@G1808:
SAVE
jmp ..@G1814
align 8
..@G1813:
dq 20
db 68
db 73
db 71
db 73
db 84
..@G1814:
mov rax, ..@G1813
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1816
align 8
..@G1815:
dq 20
db 71
db 49
db 55
db 57
db 54
..@G1816:
mov rax, ..@G1815
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
jmp ..@G1818
align 8
..@G1817:
dq 12
db 83
db 84
db 82
..@G1818:
mov rax, ..@G1817
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1820
align 8
..@G1819:
dq 4
db 73
..@G1820:
mov rax, ..@G1819
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
push RSI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1822
align 8
..@G1821:
dq 12
db 76
db 83
db 84
..@G1822:
mov rax, ..@G1821
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1824
align 8
..@G1823:
dq 56
db 78
db 85
db 77
db 66
db 69
db 82
db 45
db 84
db 79
db 45
db 67
db 72
db 65
db 82
..@G1824:
mov rax, ..@G1823
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDX, rax
pop RSI
pop RDI
extern SET_CHAR
call SET_CHAR
SAVE
jmp ..@G1826
align 8
..@G1825:
dq 12
db 76
db 83
db 84
..@G1826:
mov rax, ..@G1825
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1828
align 8
..@G1827:
dq 12
db 76
db 83
db 84
..@G1828:
mov rax, ..@G1827
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1830
align 8
..@G1829:
dq 4
db 73
..@G1830:
mov rax, ..@G1829
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1832
align 8
..@G1831:
dq 4
db 73
..@G1832:
mov rax, ..@G1831
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 4
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G1834
align 8
..@G1833:
dq 20
db 71
db 49
db 55
db 57
db 54
..@G1834:
mov rax, ..@G1833
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1836
align 8
..@G1835:
dq 20
db 71
db 49
db 55
db 57
db 54
..@G1836:
mov rax, ..@G1835
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G1805
;;end of tagbody
pop r15
..@G1798:
SAVE
SAVE
jmp ..@G1849
align 8
..@G1848:
dq 12
db 78
db 69
db 71
..@G1849:
mov rax, ..@G1848
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G1845
..@G1846:
SAVE
SAVE
SAVE
jmp ..@G1851
align 8
..@G1850:
dq 12
db 83
db 84
db 82
..@G1851:
mov rax, ..@G1850
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
mov RSI, 0
push RSI
mov RDX, 11535
pop RSI
pop RDI
extern SET_CHAR
call SET_CHAR
LOAD
jmp ..@G1847
..@G1845:
push RDI
mov rax, 47
..@G1847:
SAVE
SAVE
jmp ..@G1853
align 8
..@G1852:
dq 12
db 83
db 84
db 82
..@G1853:
mov rax, ..@G1852
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
pop r15
..@G1729:
LOAD
LEAVE
ret
.end: mov rax, G1728
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1855
align 8
..@G1854:
dq 4
db 78
..@G1855:
mov rax, ..@G1854
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1728-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1857
align 8
..@G1856:
dq 20
db 76
db 73
db 83
db 84
db 80
..@G1857:
mov rax, ..@G1856
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1858.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1858:
ENTER
SAVE
SAVE
SAVE
SAVE
jmp ..@G1867
align 8
..@G1866:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1867:
mov rax, ..@G1866
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern ATOM
call ATOM
LOAD
cmp rax, 47
je ..@G1863
..@G1864:
push RDI
mov rax, 47
jmp ..@G1865
..@G1863:
push RDI
mov rax, 4
..@G1865:
cmp rax, 47
je ..@G1860
..@G1861:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1872
align 8
..@G1871:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1872:
mov rax, ..@G1871
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1874
align 8
..@G1873:
dq 16
db 78
db 85
db 76
db 76
..@G1874:
mov rax, ..@G1873
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1868
..@G1869:
push RDI
mov rax, 4
jmp ..@G1870
..@G1868:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1876
align 8
..@G1875:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1876:
mov rax, ..@G1875
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1878
align 8
..@G1877:
dq 20
db 76
db 73
db 83
db 84
db 80
..@G1878:
mov rax, ..@G1877
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
..@G1870:
jmp ..@G1862
..@G1860:
push RDI
mov rax, 47
..@G1862:
..@G1859:
LOAD
LEAVE
ret
.end: mov rax, G1858
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G1880
align 8
..@G1879:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1880:
mov rax, ..@G1879
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1858-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G1882
align 8
..@G1881:
dq 32
db 80
db 82
db 73
db 78
db 84
db 45
db 78
db 76
..@G1882:
mov rax, ..@G1881
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G1883.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G1883:
ENTER
SAVE
SAVE
SAVE
jmp ..@G1888
align 8
..@G1887:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1888:
mov rax, ..@G1887
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern STRINGP
call STRINGP
cmp rax, 47
je ..@G1886
mov RDI, 8719
extern WRITE_CHAR
call WRITE_CHAR
SAVE
SAVE
jmp ..@G1890
align 8
..@G1889:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1890:
mov rax, ..@G1889
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
mov RDI, 8719
extern WRITE_CHAR
call WRITE_CHAR
jmp ..@G1885
..@G1886:
SAVE
SAVE
jmp ..@G1893
align 8
..@G1892:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1893:
mov rax, ..@G1892
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CHARP
call CHARP
cmp rax, 47
je ..@G1891
mov rax, 47
push rax
mov rax, 4
mov rdi, rax
pop rsi
call CONS
push rax
jmp ..@G1895
align 8
..@G1894:
dq 20
db 84
db 69
db 83
db 84
db 50
..@G1895:
mov rax, ..@G1894
or rax, 3
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1897
align 8
..@G1896:
dq 32
db 80
db 82
db 73
db 78
db 84
db 45
db 78
db 76
..@G1897:
mov rax, ..@G1896
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G1899
align 8
..@G1898:
dq 8
db 35
db 92
..@G1899:
mov rax, ..@G1898
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
SAVE
SAVE
jmp ..@G1901
align 8
..@G1900:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1901:
mov rax, ..@G1900
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern WRITE_CHAR
call WRITE_CHAR
jmp ..@G1885
..@G1891:
SAVE
SAVE
jmp ..@G1904
align 8
..@G1903:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1904:
mov rax, ..@G1903
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern NUMBERP
call NUMBERP
cmp rax, 47
je ..@G1902
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1906
align 8
..@G1905:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1906:
mov rax, ..@G1905
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1908
align 8
..@G1907:
dq 64
db 78
db 85
db 77
db 66
db 69
db 82
db 45
db 84
db 79
db 45
db 83
db 84
db 82
db 73
db 78
db 71
..@G1908:
mov rax, ..@G1907
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
jmp ..@G1885
..@G1902:
SAVE
SAVE
jmp ..@G1911
align 8
..@G1910:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1911:
mov rax, ..@G1910
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern SYMBOLP
call SYMBOLP
cmp rax, 47
je ..@G1909
SAVE
SAVE
jmp ..@G1913
align 8
..@G1912:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1913:
mov rax, ..@G1912
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern SYMBOL_NAME
call SYMBOL_NAME
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
jmp ..@G1885
..@G1909:
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1916
align 8
..@G1915:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1916:
mov rax, ..@G1915
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1918
align 8
..@G1917:
dq 20
db 76
db 73
db 83
db 84
db 80
..@G1918:
mov rax, ..@G1917
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G1914
jmp ..@G1920
align 8
..@G1919:
dq 4
db 40
..@G1920:
mov rax, ..@G1919
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G1925
align 8
..@G1924:
dq 16
db 73
db 84
db 69
db 77
..@G1925:
mov rax, ..@G1924
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1927
align 8
..@G1926:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1927:
mov rax, ..@G1926
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
push rax
SAVE
jmp ..@G1929
align 8
..@G1928:
dq 20
db 71
db 49
db 57
db 50
db 49
..@G1929:
mov rax, ..@G1928
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G1930:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1935
align 8
..@G1934:
dq 20
db 71
db 49
db 57
db 50
db 49
..@G1935:
mov rax, ..@G1934
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1937
align 8
..@G1936:
dq 16
db 78
db 85
db 76
db 76
..@G1937:
mov rax, ..@G1936
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G1931
..@G1932:
mov rax, 47
jmp ..@G1923
jmp ..@G1933
..@G1931:
push RDI
mov rax, 47
..@G1933:
SAVE
jmp ..@G1939
align 8
..@G1938:
dq 16
db 73
db 84
db 69
db 77
..@G1939:
mov rax, ..@G1938
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1941
align 8
..@G1940:
dq 20
db 71
db 49
db 57
db 50
db 49
..@G1941:
mov rax, ..@G1940
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern SETL
call SETL
mov rax, 47
push rax
mov rax, 47
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1943
align 8
..@G1942:
dq 16
db 73
db 84
db 69
db 77
..@G1943:
mov rax, ..@G1942
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1945
align 8
..@G1944:
dq 32
db 80
db 82
db 73
db 78
db 84
db 45
db 78
db 76
..@G1945:
mov rax, ..@G1944
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, 8207
extern WRITE_CHAR
call WRITE_CHAR
SAVE
jmp ..@G1947
align 8
..@G1946:
dq 20
db 71
db 49
db 57
db 50
db 49
..@G1947:
mov rax, ..@G1946
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G1949
align 8
..@G1948:
dq 20
db 71
db 49
db 57
db 50
db 49
..@G1949:
mov rax, ..@G1948
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G1930
;;end of tagbody
pop r15
..@G1923:
mov RDI, 2063
extern WRITE_CHAR
call WRITE_CHAR
jmp ..@G1958
align 8
..@G1957:
dq 4
db 41
..@G1958:
mov rax, ..@G1957
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
jmp ..@G1885
..@G1914:
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G1961
align 8
..@G1960:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1961:
mov rax, ..@G1960
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1963
align 8
..@G1962:
dq 16
db 78
db 85
db 76
db 76
..@G1963:
mov rax, ..@G1962
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G1959
jmp ..@G1965
align 8
..@G1964:
dq 12
db 78
db 73
db 76
..@G1965:
mov rax, ..@G1964
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
jmp ..@G1885
..@G1959:
SAVE
SAVE
SAVE
jmp ..@G1971
align 8
..@G1970:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1971:
mov rax, ..@G1970
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern ATOM
call ATOM
LOAD
cmp rax, 47
je ..@G1967
..@G1968:
push RDI
mov rax, 47
jmp ..@G1969
..@G1967:
push RDI
mov rax, 4
..@G1969:
cmp rax, 47
je ..@G1966
jmp ..@G1973
align 8
..@G1972:
dq 4
db 40
..@G1973:
mov rax, ..@G1972
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
mov rax, 47
push rax
mov rax, 47
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1975
align 8
..@G1974:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1975:
mov rax, ..@G1974
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1977
align 8
..@G1976:
dq 32
db 80
db 82
db 73
db 78
db 84
db 45
db 78
db 76
..@G1977:
mov rax, ..@G1976
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G1979
align 8
..@G1978:
dq 12
db 32
db 46
db 32
..@G1979:
mov rax, ..@G1978
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
mov rax, 47
push rax
mov rax, 47
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1981
align 8
..@G1980:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1981:
mov rax, ..@G1980
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G1983
align 8
..@G1982:
dq 32
db 80
db 82
db 73
db 78
db 84
db 45
db 78
db 76
..@G1983:
mov rax, ..@G1982
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G1985
align 8
..@G1984:
dq 4
db 41
..@G1985:
mov rax, ..@G1984
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
jmp ..@G1885
..@G1966:
SAVE
SAVE
jmp ..@G1988
align 8
..@G1987:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G1988:
mov rax, ..@G1987
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern FUNCTIONP
call FUNCTIONP
cmp rax, 47
je ..@G1986
jmp ..@G1990
align 8
..@G1989:
dq 32
db 70
db 85
db 78
db 67
db 84
db 73
db 79
db 78
..@G1990:
mov rax, ..@G1989
or rax, 3
mov RDI, rax
extern WRITE_STRING
call WRITE_STRING
jmp ..@G1885
..@G1986:
mov rax, 4
cmp rax, 47
je ..@G1991
jmp ..@G1993
align 8
..@G1992:
dq 152
db 85
db 110
db 107
db 110
db 111
db 119
db 110
db 32
db 111
db 98
db 106
db 101
db 99
db 116
db 32
db 116
db 121
db 112
db 101
db 32
db 112
db 97
db 115
db 115
db 101
db 100
db 32
db 116
db 111
db 32
db 112
db 114
db 105
db 110
db 116
db 45
db 110
db 108
..@G1993:
mov rax, ..@G1992
or rax, 3
mov RDI, rax
extern ERROR
call ERROR
jmp ..@G1885
..@G1991:
..@G1885:
SAVE
SAVE
jmp ..@G1998
align 8
..@G1997:
dq 8
db 78
db 76
..@G1998:
mov rax, ..@G1997
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
cmp rax, 47
je ..@G1994
..@G1995:
SAVE
mov RDI, 2575
extern WRITE_CHAR
call WRITE_CHAR
LOAD
jmp ..@G1996
..@G1994:
push RDI
mov rax, 47
..@G1996:
SAVE
SAVE
jmp ..@G2000
align 8
..@G1999:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G2000:
mov rax, ..@G1999
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
..@G1884:
LOAD
LEAVE
ret
.end: mov rax, G1883
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G2002
align 8
..@G2001:
dq 8
db 78
db 76
..@G2002:
mov rax, ..@G2001
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
mov rsi, rax
push rsi
SAVE
jmp ..@G2004
align 8
..@G2003:
dq 24
db 79
db 66
db 74
db 69
db 67
db 84
..@G2004:
mov rax, ..@G2003
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G1883-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G2006
align 8
..@G2005:
dq 20
db 80
db 82
db 73
db 78
db 84
..@G2006:
mov rax, ..@G2005
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G2007.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G2007:
ENTER
SAVE
mov rax, 47
push rax
mov rax, 4
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2010
align 8
..@G2009:
dq 12
db 79
db 66
db 74
..@G2010:
mov rax, ..@G2009
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2012
align 8
..@G2011:
dq 32
db 80
db 82
db 73
db 78
db 84
db 45
db 78
db 76
..@G2012:
mov rax, ..@G2011
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
..@G2008:
LOAD
LEAVE
ret
.end: mov rax, G2007
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G2014
align 8
..@G2013:
dq 12
db 79
db 66
db 74
..@G2014:
mov rax, ..@G2013
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G2007-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G2016
align 8
..@G2015:
dq 16
db 69
db 86
db 65
db 76
..@G2016:
mov rax, ..@G2015
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G2017.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G2017:
ENTER
SAVE
SAVE
SAVE
jmp ..@G2022
align 8
..@G2021:
dq 4
db 69
..@G2022:
mov rax, ..@G2021
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern SYMBOLP
call SYMBOLP
cmp rax, 47
je ..@G2020
SAVE
SAVE
jmp ..@G2024
align 8
..@G2023:
dq 4
db 69
..@G2024:
mov rax, ..@G2023
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
jmp ..@G2019
..@G2020:
SAVE
SAVE
jmp ..@G2027
align 8
..@G2026:
dq 4
db 69
..@G2027:
mov rax, ..@G2026
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern ATOM
call ATOM
cmp rax, 47
je ..@G2025
SAVE
SAVE
jmp ..@G2029
align 8
..@G2028:
dq 4
db 69
..@G2029:
mov rax, ..@G2028
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G2019
..@G2025:
SAVE
SAVE
jmp ..@G2032
align 8
..@G2031:
dq 4
db 69
..@G2032:
mov rax, ..@G2031
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern STRINGP
call STRINGP
cmp rax, 47
je ..@G2030
mov rax, 47
push rax
jmp ..@G2034
align 8
..@G2033:
dq 8
db 100
db 101
..@G2034:
mov rax, ..@G2033
or rax, 3
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2036
align 8
..@G2035:
dq 20
db 80
db 82
db 73
db 78
db 84
..@G2036:
mov rax, ..@G2035
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
SAVE
SAVE
jmp ..@G2038
align 8
..@G2037:
dq 4
db 69
..@G2038:
mov rax, ..@G2037
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
jmp ..@G2019
..@G2030:
SAVE
SAVE
jmp ..@G2041
align 8
..@G2040:
dq 4
db 69
..@G2041:
mov rax, ..@G2040
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
extern ATOM
call ATOM
cmp rax, 47
je ..@G2039
SAVE
SAVE
jmp ..@G2045
align 8
..@G2044:
dq 4
db 69
..@G2045:
mov rax, ..@G2044
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2047
align 8
..@G2046:
dq 20
db 81
db 85
db 79
db 84
db 69
..@G2047:
mov rax, ..@G2046
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2043
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2049
align 8
..@G2048:
dq 4
db 69
..@G2049:
mov rax, ..@G2048
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2051
align 8
..@G2050:
dq 16
db 67
db 65
db 68
db 82
..@G2051:
mov rax, ..@G2050
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G2042
..@G2043:
SAVE
SAVE
jmp ..@G2054
align 8
..@G2053:
dq 4
db 69
..@G2054:
mov rax, ..@G2053
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2056
align 8
..@G2055:
dq 16
db 65
db 84
db 79
db 77
..@G2056:
mov rax, ..@G2055
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2052
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2058
align 8
..@G2057:
dq 4
db 69
..@G2058:
mov rax, ..@G2057
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2060
align 8
..@G2059:
dq 16
db 67
db 65
db 68
db 82
..@G2060:
mov rax, ..@G2059
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2062
align 8
..@G2061:
dq 16
db 69
db 86
db 65
db 76
..@G2062:
mov rax, ..@G2061
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
extern ATOM
call ATOM
jmp ..@G2042
..@G2052:
SAVE
SAVE
jmp ..@G2065
align 8
..@G2064:
dq 4
db 69
..@G2065:
mov rax, ..@G2064
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2067
align 8
..@G2066:
dq 8
db 69
db 81
..@G2067:
mov rax, ..@G2066
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2063
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2069
align 8
..@G2068:
dq 4
db 69
..@G2069:
mov rax, ..@G2068
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2071
align 8
..@G2070:
dq 16
db 67
db 65
db 68
db 82
..@G2071:
mov rax, ..@G2070
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2073
align 8
..@G2072:
dq 16
db 69
db 86
db 65
db 76
..@G2073:
mov rax, ..@G2072
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2075
align 8
..@G2074:
dq 4
db 69
..@G2075:
mov rax, ..@G2074
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2077
align 8
..@G2076:
dq 20
db 67
db 65
db 68
db 68
db 82
..@G2077:
mov rax, ..@G2076
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2079
align 8
..@G2078:
dq 16
db 69
db 86
db 65
db 76
..@G2079:
mov rax, ..@G2078
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern EQ
call EQ
jmp ..@G2042
..@G2063:
SAVE
SAVE
jmp ..@G2082
align 8
..@G2081:
dq 4
db 69
..@G2082:
mov rax, ..@G2081
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2084
align 8
..@G2083:
dq 12
db 67
db 65
db 82
..@G2084:
mov rax, ..@G2083
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2080
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2086
align 8
..@G2085:
dq 4
db 69
..@G2086:
mov rax, ..@G2085
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2088
align 8
..@G2087:
dq 16
db 67
db 65
db 68
db 82
..@G2088:
mov rax, ..@G2087
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2090
align 8
..@G2089:
dq 16
db 69
db 86
db 65
db 76
..@G2090:
mov rax, ..@G2089
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
extern CAR
call CAR
jmp ..@G2042
..@G2080:
SAVE
SAVE
jmp ..@G2093
align 8
..@G2092:
dq 4
db 69
..@G2093:
mov rax, ..@G2092
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2095
align 8
..@G2094:
dq 12
db 67
db 68
db 82
..@G2095:
mov rax, ..@G2094
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2091
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2097
align 8
..@G2096:
dq 4
db 69
..@G2097:
mov rax, ..@G2096
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2099
align 8
..@G2098:
dq 16
db 67
db 65
db 68
db 82
..@G2099:
mov rax, ..@G2098
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2101
align 8
..@G2100:
dq 16
db 69
db 86
db 65
db 76
..@G2101:
mov rax, ..@G2100
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
extern CDR
call CDR
jmp ..@G2042
..@G2091:
SAVE
SAVE
jmp ..@G2104
align 8
..@G2103:
dq 4
db 69
..@G2104:
mov rax, ..@G2103
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2106
align 8
..@G2105:
dq 16
db 67
db 79
db 78
db 83
..@G2106:
mov rax, ..@G2105
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2102
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2108
align 8
..@G2107:
dq 4
db 69
..@G2108:
mov rax, ..@G2107
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2110
align 8
..@G2109:
dq 16
db 67
db 65
db 68
db 82
..@G2110:
mov rax, ..@G2109
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2112
align 8
..@G2111:
dq 16
db 69
db 86
db 65
db 76
..@G2112:
mov rax, ..@G2111
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2114
align 8
..@G2113:
dq 4
db 69
..@G2114:
mov rax, ..@G2113
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2116
align 8
..@G2115:
dq 20
db 67
db 65
db 68
db 68
db 82
..@G2116:
mov rax, ..@G2115
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2118
align 8
..@G2117:
dq 16
db 69
db 86
db 65
db 76
..@G2118:
mov rax, ..@G2117
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G2042
..@G2102:
SAVE
SAVE
jmp ..@G2121
align 8
..@G2120:
dq 4
db 69
..@G2121:
mov rax, ..@G2120
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2123
align 8
..@G2122:
dq 16
db 67
db 79
db 78
db 68
..@G2123:
mov rax, ..@G2122
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2119
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2125
align 8
..@G2124:
dq 4
db 69
..@G2125:
mov rax, ..@G2124
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2127
align 8
..@G2126:
dq 20
db 69
db 86
db 67
db 79
db 78
..@G2127:
mov rax, ..@G2126
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G2042
..@G2119:
SAVE
SAVE
jmp ..@G2130
align 8
..@G2129:
dq 4
db 69
..@G2130:
mov rax, ..@G2129
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2132
align 8
..@G2131:
dq 12
db 83
db 69
db 84
..@G2132:
mov rax, ..@G2131
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2128
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2134
align 8
..@G2133:
dq 4
db 69
..@G2134:
mov rax, ..@G2133
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2136
align 8
..@G2135:
dq 16
db 67
db 65
db 68
db 82
..@G2136:
mov rax, ..@G2135
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2138
align 8
..@G2137:
dq 16
db 69
db 86
db 65
db 76
..@G2138:
mov rax, ..@G2137
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2140
align 8
..@G2139:
dq 4
db 69
..@G2140:
mov rax, ..@G2139
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2142
align 8
..@G2141:
dq 16
db 67
db 68
db 68
db 82
..@G2142:
mov rax, ..@G2141
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2144
align 8
..@G2143:
dq 16
db 69
db 86
db 65
db 76
..@G2144:
mov rax, ..@G2143
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SET
call SET
jmp ..@G2042
..@G2128:
SAVE
SAVE
jmp ..@G2147
align 8
..@G2146:
dq 4
db 69
..@G2147:
mov rax, ..@G2146
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RDI, rax
push RDI
SAVE
jmp ..@G2149
align 8
..@G2148:
dq 16
db 83
db 69
db 84
db 76
..@G2149:
mov rax, ..@G2148
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
cmp rax, 47
je ..@G2145
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2151
align 8
..@G2150:
dq 4
db 69
..@G2151:
mov rax, ..@G2150
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2153
align 8
..@G2152:
dq 16
db 67
db 65
db 68
db 82
..@G2153:
mov rax, ..@G2152
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2155
align 8
..@G2154:
dq 16
db 69
db 86
db 65
db 76
..@G2155:
mov rax, ..@G2154
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2157
align 8
..@G2156:
dq 4
db 69
..@G2157:
mov rax, ..@G2156
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2159
align 8
..@G2158:
dq 16
db 67
db 68
db 68
db 82
..@G2159:
mov rax, ..@G2158
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2161
align 8
..@G2160:
dq 16
db 69
db 86
db 65
db 76
..@G2161:
mov rax, ..@G2160
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G2042
..@G2145:
mov rax, 4
cmp rax, 47
je ..@G2162
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2164
align 8
..@G2163:
dq 4
db 69
..@G2164:
mov rax, ..@G2163
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2166
align 8
..@G2165:
dq 16
db 69
db 86
db 65
db 76
..@G2166:
mov rax, ..@G2165
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2168
align 8
..@G2167:
dq 4
db 69
..@G2168:
mov rax, ..@G2167
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2170
align 8
..@G2169:
dq 20
db 69
db 86
db 76
db 73
db 83
..@G2170:
mov rax, ..@G2169
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern APPLY
call APPLY
jmp ..@G2042
..@G2162:
..@G2042:
jmp ..@G2019
..@G2039:
..@G2019:
..@G2018:
LOAD
LEAVE
ret
.end: mov rax, G2017
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G2172
align 8
..@G2171:
dq 4
db 69
..@G2172:
mov rax, ..@G2171
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G2017-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G2174
align 8
..@G2173:
dq 20
db 69
db 86
db 67
db 79
db 78
..@G2174:
mov rax, ..@G2173
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G2175.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G2175:
ENTER
SAVE
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2180
align 8
..@G2179:
dq 4
db 67
..@G2180:
mov rax, ..@G2179
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2182
align 8
..@G2181:
dq 16
db 67
db 65
db 65
db 82
..@G2182:
mov rax, ..@G2181
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2184
align 8
..@G2183:
dq 16
db 69
db 86
db 65
db 76
..@G2184:
mov rax, ..@G2183
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G2178
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2186
align 8
..@G2185:
dq 4
db 67
..@G2186:
mov rax, ..@G2185
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2188
align 8
..@G2187:
dq 20
db 67
db 65
db 68
db 65
db 82
..@G2188:
mov rax, ..@G2187
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2190
align 8
..@G2189:
dq 16
db 69
db 86
db 65
db 76
..@G2190:
mov rax, ..@G2189
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G2177
..@G2178:
mov rax, 4
cmp rax, 47
je ..@G2191
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2193
align 8
..@G2192:
dq 4
db 67
..@G2193:
mov rax, ..@G2192
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2195
align 8
..@G2194:
dq 20
db 69
db 86
db 67
db 79
db 78
..@G2195:
mov rax, ..@G2194
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
jmp ..@G2177
..@G2191:
..@G2177:
..@G2176:
LOAD
LEAVE
ret
.end: mov rax, G2175
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G2197
align 8
..@G2196:
dq 4
db 67
..@G2197:
mov rax, ..@G2196
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G2175-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G2199
align 8
..@G2198:
dq 20
db 69
db 86
db 76
db 73
db 83
..@G2199:
mov rax, ..@G2198
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G2200.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G2200:
ENTER
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2205
align 8
..@G2204:
dq 4
db 77
..@G2205:
mov rax, ..@G2204
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2207
align 8
..@G2206:
dq 16
db 78
db 85
db 76
db 76
..@G2207:
mov rax, ..@G2206
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
cmp rax, 47
je ..@G2203
mov rax, 47
jmp ..@G2202
..@G2203:
mov rax, 4
cmp rax, 47
je ..@G2208
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2210
align 8
..@G2209:
dq 4
db 77
..@G2210:
mov rax, ..@G2209
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2212
align 8
..@G2211:
dq 16
db 69
db 86
db 65
db 76
..@G2212:
mov rax, ..@G2211
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2214
align 8
..@G2213:
dq 4
db 77
..@G2214:
mov rax, ..@G2213
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2216
align 8
..@G2215:
dq 20
db 69
db 86
db 76
db 73
db 83
..@G2216:
mov rax, ..@G2215
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern CONS
call CONS
jmp ..@G2202
..@G2208:
..@G2202:
..@G2201:
LOAD
LEAVE
ret
.end: mov rax, G2200
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
mov rsi, rax
push rsi
SAVE
jmp ..@G2218
align 8
..@G2217:
dq 4
db 77
..@G2218:
mov rax, ..@G2217
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov rdi, rax
pop rsi
extern CONS
call CONS
pop rsi
pop rdi
;;end of list
mov [G2200-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
SAVE
jmp ..@G2220
align 8
..@G2219:
dq 16
db 82
db 69
db 80
db 76
..@G2220:
mov rax, ..@G2219
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
jmp G2221.end
align 8
nop
nop
nop
nop
nop
nop
nop
nop
G2221:
ENTER
SAVE
push r15
mov rax, r15
push rax
mov rax, 0
push rax
SAVE
jmp ..@G2224
align 8
..@G2223:
dq 4
db 73
..@G2224:
mov rax, ..@G2223
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 0
push rax
SAVE
jmp ..@G2226
align 8
..@G2225:
dq 12
db 76
db 69
db 78
..@G2226:
mov rax, ..@G2225
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
mov rax, 47
push rax
SAVE
jmp ..@G2228
align 8
..@G2227:
dq 20
db 69
db 88
db 80
db 82
db 83
..@G2228:
mov rax, ..@G2227
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G2230:
mov RDI, 15887
extern WRITE_CHAR
call WRITE_CHAR
SAVE
jmp ..@G2232
align 8
..@G2231:
dq 20
db 69
db 88
db 80
db 82
db 83
..@G2232:
mov rax, ..@G2231
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2234
align 8
..@G2233:
dq 16
db 82
db 69
db 65
db 68
..@G2234:
mov rax, ..@G2233
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G2236
align 8
..@G2235:
dq 12
db 76
db 69
db 78
..@G2236:
mov rax, ..@G2235
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2238
align 8
..@G2237:
dq 20
db 69
db 88
db 80
db 82
db 83
..@G2238:
mov rax, ..@G2237
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2240
align 8
..@G2239:
dq 44
db 76
db 73
db 83
db 84
db 45
db 76
db 69
db 78
db 71
db 84
db 72
..@G2240:
mov rax, ..@G2239
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G2242
align 8
..@G2241:
dq 4
db 73
..@G2242:
mov rax, ..@G2241
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov RSI, 0
pop RDI
extern SETL
call SETL
push r15
mov rax, r15
push rax
mov rax, 47
push rax
SAVE
jmp ..@G2247
align 8
..@G2246:
dq 16
db 69
db 88
db 80
db 82
..@G2247:
mov rax, ..@G2246
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2249
align 8
..@G2248:
dq 20
db 69
db 88
db 80
db 82
db 83
..@G2249:
mov rax, ..@G2248
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
push rax
SAVE
jmp ..@G2251
align 8
..@G2250:
dq 20
db 71
db 50
db 50
db 52
db 51
..@G2251:
mov rax, ..@G2250
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
pop rsi
mov rdi, rax
extern CONS
call CONS
mov rdi, rax
pop rsi
call CONS
mov r15, rax
;;start of tagbody
..@G2252:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2257
align 8
..@G2256:
dq 20
db 71
db 50
db 50
db 52
db 51
..@G2257:
mov rax, ..@G2256
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2259
align 8
..@G2258:
dq 16
db 78
db 85
db 76
db 76
..@G2259:
mov rax, ..@G2258
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
cmp rax, 47
je ..@G2253
..@G2254:
mov rax, 47
jmp ..@G2245
jmp ..@G2255
..@G2253:
push RDI
mov rax, 47
..@G2255:
SAVE
jmp ..@G2261
align 8
..@G2260:
dq 16
db 69
db 88
db 80
db 82
..@G2261:
mov rax, ..@G2260
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G2263
align 8
..@G2262:
dq 20
db 71
db 50
db 50
db 52
db 51
..@G2263:
mov rax, ..@G2262
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CAR
call CAR
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
jmp ..@G2265
align 8
..@G2264:
dq 4
db 73
..@G2265:
mov rax, ..@G2264
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
mov RDI, 4
push RDI
SAVE
SAVE
jmp ..@G2267
align 8
..@G2266:
dq 4
db 73
..@G2267:
mov rax, ..@G2266
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern ADD
call ADD
mov RSI, rax
pop RDI
extern SETL
call SETL
SAVE
SAVE
SAVE
jmp ..@G2272
align 8
..@G2271:
dq 12
db 76
db 69
db 78
..@G2272:
mov rax, ..@G2271
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G2274
align 8
..@G2273:
dq 4
db 73
..@G2274:
mov rax, ..@G2273
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RSI, rax
pop RDI
extern EQ
call EQ
LOAD
cmp rax, 47
je ..@G2268
..@G2269:
SAVE
mov rax, 47
push rax
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2276
align 8
..@G2275:
dq 16
db 69
db 88
db 80
db 82
..@G2276:
mov rax, ..@G2275
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2278
align 8
..@G2277:
dq 16
db 69
db 86
db 65
db 76
..@G2278:
mov rax, ..@G2277
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2280
align 8
..@G2279:
dq 20
db 80
db 82
db 73
db 78
db 84
..@G2280:
mov rax, ..@G2279
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
jmp ..@G2270
..@G2268:
SAVE
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2282
align 8
..@G2281:
dq 16
db 69
db 88
db 80
db 82
..@G2282:
mov rax, ..@G2281
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
call CONS
push rax
SAVE
SAVE
jmp ..@G2284
align 8
..@G2283:
dq 16
db 69
db 86
db 65
db 76
..@G2284:
mov rax, ..@G2283
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
LOAD
..@G2270:
SAVE
jmp ..@G2286
align 8
..@G2285:
dq 20
db 71
db 50
db 50
db 52
db 51
..@G2286:
mov rax, ..@G2285
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
push RDI
SAVE
SAVE
jmp ..@G2288
align 8
..@G2287:
dq 20
db 71
db 50
db 50
db 52
db 51
..@G2288:
mov rax, ..@G2287
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_LEXICAL_VALUE
call SYMBOL_LEXICAL_VALUE
LOAD
mov RDI, rax
extern CDR
call CDR
mov RSI, rax
pop RDI
extern SETL
call SETL
jmp ..@G2252
;;end of tagbody
pop r15
..@G2245:
jmp ..@G2230
;;end of tagbody
..@G2229:
pop r15
..@G2222:
LOAD
LEAVE
ret
.end: mov rax, G2221
or rax, 2
push rax
;;list
push rdi
push rsi
mov rax, 47
pop rsi
pop rdi
;;end of list
mov [G2221-8], rax
pop rax
mov RSI, rax
pop RDI
extern SET
call SET
mov rax, 47
push rax
SAVE
SAVE
jmp ..@G2304
align 8
..@G2303:
dq 16
db 82
db 69
db 80
db 76
..@G2304:
mov rax, ..@G2303
or rax, 3
mov RDI, rax
push RDI
mov RSI, 47
pop RDI
extern MAKE_SYMBOL
call MAKE_SYMBOL
LOAD
mov RDI, rax
extern SYMBOL_VALUE
call SYMBOL_VALUE
LOAD
mov rdi, rax
pop rsi
extern APPLY
call APPLY
;;Compiled code ends here
LEAVE
ret
