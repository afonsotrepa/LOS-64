#include <kbd.h>
/// Keyboard related functions

// static uint8_t kbd_buff_top = 0;    // points to the top of the buff
// static uint8_t kbd_buff_bottom = 0; // points to the bottom of the buff

void kbd_initialize(void)
{
	print("Kbd Initialize\n");

	// kbd_buff = malloc(256);

	/* Disabled because it's not needed atm
	asm("cli");
	// use scan code set 1
	outb(PS2_DATA_PORT, 0xF0);
	outb(PS2_DATA_PORT, 0x01);
	// disable "translation"
	outb(PS2_COMMAND_PORT, 0x20);
	outb(PS2_COMMAND_PORT, inb(PS2_DATA_PORT) ^ 1 << 6);

	asm("sti");
	if (inb(PS2_DATA_PORT) == 0xFA) print("ACK\n");*/


	scancode_in = '\0';
	// Initialization of the scan code table
	ascii = malloc(256);
	memset(ascii, '\0', 256);
	memcpy(ascii + 0x10, "qwertyuiop", 10);
	memcpy(ascii + 0x1E, "asdfghjkl", 9);
	memcpy(ascii + 0x2C, "zxcvbnm", 7);
	memcpy(ascii + 0x02, "1234567890'", 11);
	ascii[0x39] = ' ';
	ascii[0x1C] = '\n';
	ascii[0x0E] = '\b';
	// for when the shift key is pressed:
	shift_ascii = malloc(256);
	memset(shift_ascii, '\0', 256);
	memcpy(shift_ascii + 0x10, "QWERTYUIOP", 10);
	memcpy(shift_ascii + 0x1E, "ASDFGHJKL", 9);
	memcpy(shift_ascii + 0x2C, "ZXCVBNM", 7);
	memcpy(shift_ascii + 0x02, "!\"#$%&/()=", 10);
	shift_ascii[0x39] = ' ';
	shift_ascii[0x1C] = '\n';
	shift_ascii[0x0E] = '\b';
}

// Lisp primitive
uint64_t READ_CHAR(void)
{
	static bool shift_pressed = false;
	for (;;) {
		asm volatile("hlt"); // wait for the next interrput
		if (scancode_in == 0x2A) {
			shift_pressed = true;
		} else if (scancode_in == 0xAA) {
			shift_pressed = false;
			// ((scan_code & 128) != 128) checks if the key has been
			// pressed or released
		} else if (((scancode_in & 128) != 128) &&
			   ((scancode_in ^ (1 << 8)) != 0)) {
			if (shift_pressed == false)
				return (ascii[scancode_in] << 8) | 0b1111;
			else
				return (shift_ascii[scancode_in] << 8) | 0b1111;
		}
	}
}
