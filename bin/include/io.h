#ifndef IO_H_INCLUDED
#define IO_H_INCLUDED
#include <stdint.h>

#define PS2_DATA_PORT 0x60
#define PS2_COMMAND_PORT 0x64

void outb(uint16_t port, uint8_t value);
uint8_t inb(uint16_t port);
#endif
