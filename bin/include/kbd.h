#ifndef KDB_H_INCLUDED
#define KDB_H_INCLUDED
#include <mem.h>
#include <stddef.h>
#include <tty.h>
#include <io.h>
#include <idt.h>
#include <stdbool.h>

uint16_t volatile scancode_in;
char *ascii;
char *shift_ascii;

void kbd_initialize(void);
void kbd_handler(void);
#endif
