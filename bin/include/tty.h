#ifndef TTY_H_INCLUDED
#define TTY_H_INCLUDED
#include <stddef.h>
#include <stdint.h>
#include <lib.h>
#include <io.h>

#define FRAME_BUFFER (void *) 0xB8000

/* Hardware text mode color constants. */
enum vga_color;

// static inline uint8_t vga_entry_color(enum vga_color fg, enum vga_color bg);
// static inline uint16_t vga_entry(unsigned char uc, uint8_t color);

static const size_t VGA_WIDTH = 80;
static const size_t VGA_HEIGHT = 25;

size_t fb_row;
size_t fb_column;
uint8_t fb_color;
uint16_t *frame_buffer;

void tty_initialize(void);

void setcolor(uint8_t color);

void put_entry_at(char c, uint8_t color, size_t x, size_t y);

void scroll_down(void);
void put_char(char c);

void write(const char *data, size_t size);

void print(const char *data);
#endif
