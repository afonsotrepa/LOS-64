#ifndef KLIB_H_INCLUDED
#define KLIB_H_INCLUDED
#include <stddef.h>
#include <stdint.h>
#include <mem.h>
#include <tty.h>

#define MAXINT 2147483647


unsigned int log_2(uint32_t n);
unsigned int log(uint64_t n, unsigned int base);
char *itoa(uint32_t value, char *str, int base);
size_t str_len(const char *str);
int64_t expt(int64_t base, int64_t exp);
#endif
