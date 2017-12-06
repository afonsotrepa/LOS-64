# LOS-64
Small LISP-1 OS for x86-64 written in C, asm and Common Lisp

## Features:
- Cross-compiler for LISP-1 written in Common Lisp
- Primitive REPL in LISP-1

## Requires:
- Nasm
- SBCL (for Common Lisp, but any other implementation of CL should do)
- qemu-system-x86_64
- grub-mkrescue
- x86_64-elf-gcc (included)
- GNU Make

## Run:
simply do 'make run' and it should compile the OS and run it under Qemu
