#include <tty.h>
#include <idt.h>
#include <lib.h>
#include <mem.h>
#include <kbd.h>
#include <repl.h>

/* Check if the compiler thinks we are targeting the wrong operating system. */
#if defined(__linux__)
#error "You are not using a cross-compiler, you will most certainly run into trouble"
#endif


void kernel_main(void)
{
	tty_initialize();
	idt_initialize();
	heap_initialize();
	kbd_initialize();
	// malloc seems to crash after being called too may times (20 for
	// malloc_aligned)
	// for (int i = 0; i < 100; i++) {
	// print(itoa((int) malloc(16), str, 10));
	//}

	uint32_t tmp;
	tmp = repl_initialize();
	print("\n");
	print("REPL RETURN: ");
	char str[20];
	print(itoa(tmp >> 2, str, 10));
	print("\n");


	for (;;) {
		asm volatile("sti");
		asm volatile("hlt");
	}
}
