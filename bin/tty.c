#include <tty.h>
/* Hardware text mode color constants. */
enum vga_color {
	VGA_COLOR_BLACK = 0,
	VGA_COLOR_BLUE = 1,
	VGA_COLOR_GREEN = 2,
	VGA_COLOR_CYAN = 3,
	VGA_COLOR_RED = 4,
	VGA_COLOR_MAGENTA = 5,
	VGA_COLOR_BROWN = 6,
	VGA_COLOR_LIGHT_GREY = 7,
	VGA_COLOR_DARK_GREY = 8,
	VGA_COLOR_LIGHT_BLUE = 9,
	VGA_COLOR_LIGHT_GREEN = 10,
	VGA_COLOR_LIGHT_CYAN = 11,
	VGA_COLOR_LIGHT_RED = 12,
	VGA_COLOR_LIGHT_MAGENTA = 13,
	VGA_COLOR_LIGHT_BROWN = 14,
	VGA_COLOR_WHITE = 15,
};

static inline uint8_t vga_entry_color(enum vga_color fg, enum vga_color bg)
{
	return fg | bg << 4;
}

static inline uint16_t vga_entry(unsigned char uc, uint8_t color)
{
	return (uint16_t) uc | (uint16_t) color << 8;
}

void tty_initialize(void)
{
	fb_row = 0;
	fb_column = 0;
	fb_color = vga_entry_color(VGA_COLOR_LIGHT_GREY, VGA_COLOR_BLACK);
	frame_buffer = (uint16_t *) FRAME_BUFFER;
	for (size_t y = 0; y < VGA_HEIGHT; y++) {
		for (size_t x = 0; x < VGA_WIDTH; x++) {
			const size_t index = y * VGA_WIDTH + x;
			frame_buffer[index] = vga_entry(' ', fb_color);
		}
	}
}

void put_entry_at(char c, uint8_t color, size_t x, size_t y)
{
	const size_t index = y * VGA_WIDTH + x;
	frame_buffer[index] = vga_entry(c, color);
}

void scroll_down(void)
{
	for (size_t i = VGA_WIDTH; i < VGA_HEIGHT * VGA_WIDTH; i++)
		frame_buffer[i - 80] = frame_buffer[i];

	for (size_t i = (VGA_HEIGHT - 1) * VGA_WIDTH;
	     i < VGA_HEIGHT * VGA_WIDTH; i++)
		frame_buffer[i] = 0;

	fb_row--;
}
void auto_scroll(void)
{
	if (fb_column >= VGA_WIDTH) {
		fb_column = 0;
		fb_row++;
	}
	if (fb_row >= VGA_HEIGHT) scroll_down();
}
void update_cursor(uint8_t row, uint8_t col)
{
	unsigned short pos = row * 80 + col;

	outb(0x3D4, 0x0F);
	outb(0x3D5, (uint8_t) pos & 0xFF);
	outb(0x3D4, 0x0E);
	outb(0x3D5, (uint8_t)(pos >> 8) & 0xFF);
}
void put_char(char c)
{
	if (c == '\n') {
		fb_column = 0;
		fb_row++;
		auto_scroll();
	} else if (c == '\0') {
		return;
	} else if (c == '\b') {
		fb_column--;
	} else {
		put_entry_at(c, fb_color, fb_column++, fb_row);
	}
	// might not be a good idea to update the cursor after each char since
	// VGA in/out is slow
	update_cursor(fb_row, fb_column);
	auto_scroll();
}

void write(const char *data, size_t size)
{
	for (size_t i = 0; i < size; i++) {
		put_char(data[i]);
	}
}

void print(const char *data)
{
	write(data, str_len(data));
}
