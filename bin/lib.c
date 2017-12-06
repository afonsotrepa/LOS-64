#include <lib.h>
unsigned int log_2(uint32_t n)
{
	const unsigned int b[] = {0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000};
	const unsigned int S[] = {1, 2, 4, 8, 16};

	unsigned int r = 0; // return value
	for (int i = 4; i >= 0; i--) {
		if (n & b[i]) {
			n >>= S[i];
			r |= S[i];
		}
	}
	return r;
}

unsigned int log(uint64_t n, unsigned int base)
{
	return log_2(n) / log_2(base);
}

char *itoa(uint32_t value, char str[], int base)
{
	if (value == 0) {
		str[0] = '0';
		str[1] = '\0';
		return str;
	}

	char array[] = "0123456789ABCDEF";

	uint32_t tmp = log(value, base);
	for (unsigned int i = 0; i <= tmp; i++) {
		str[tmp - i] = array[value % base];
		value /= base;
	}

	str[tmp + 1] = '\0';
	if (str[0] == '0') memcpy(str, str + 1, tmp + 1);
	return str;
}

size_t str_len(const char *str)
{
	size_t len = 0;
	while (str[len]) len++;
	return len;
}

int64_t expt(int64_t base, int64_t exp)
{
	int result = 1;
	while (exp) {
		if (exp & 1) result *= base;
		exp >>= 1;
		base *= base;
	}

	return result;
}
