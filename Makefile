    OBJECTS = bin/kernel.o bin/boot.o \
    	      bin/tty.o bin/idt.o bin/io.o \
	      bin/lib.o bin/irq.o bin/mem.o \
	      bin/kbd.o bin/lisp.o bin/compiler/primitives.o \
	      bin/gdt.o
    CC =./x86_64-elf-4.9.1-Linux-x86_64/bin/x86_64-elf-gcc #comment this entire line out to get ycm-generator to work!
    CFLAGS = -m64 -std=gnu99 -ffreestanding -Wall -Wextra -fno-use-linker-plugin -I./bin/include -masm=intel -O2 #-ggdb (breaks radare2) #-Og for debugging (O3 seems to break the code)
    LDFLAGS = -ffreestanding -nostdlib -lgcc -fno-use-linker-plugin -z max-page-size=0x1000 
    AS = nasm
    ASFLAGS = -f elf64 -I bin/include/
    SHELL = bash
    SBCL = sbcl

    .PHONY = all run clean lisp

    all: lisp kernel.elf

    kernel.elf: $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -T linker.ld -o kernel.elf


    os.iso: kernel.elf
	cp kernel.elf isodir/boot/kernel.elf
	grub-mkrescue -o kernel.iso isodir

    run: os.iso
	qemu-system-x86_64 -cdrom kernel.iso -no-reboot --enable-kvm #-curses #-s -S & gdb -quiet #do ALT-2 then type "quit" to exit qemu 

    %.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

    %.o: %.s
	$(AS) $(ASFLAGS) $< -o $@

    lisp: bin/compiler/*.lisp bin/compiler/*.s
	$(SBCL) --script bin/compiler/compiler.lisp bin/compiler/repl.lisp

    clean:
	rm bin/*.o bin/compiler/*.o kernel.elf kernel.iso
