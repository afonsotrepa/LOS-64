#ifndef MEM_H_INCLUDED
#define MEM_H_INCLUDED
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// The packed attribute is unnecessary here and actually slows down the struct
// in certain architectures
typedef struct /*__attribute__((__packed__))*/ node {
	uint16_t size;     // size of the allocated space
	struct node *next; // next node in the linked list
} node_t;

/* Declared in the boot.s */
extern void *heap_bottom;
extern void *heap_top;

#define HEAP_SIZE 4000

void heap_initialize(void); // need to run this once for malloc to work

void *malloc(size_t size);
void *malloc_aligned(size_t size); // same as malloc, but ptr is 8bytes aligned

void free(void *ptr);



void *memset(void *pointer, unsigned char element, size_t len);
void *memcpy(void *dst, const void *src, size_t len);

// Lisp heap:
#define LISP_HEAP_SIZE 100000 // 100000 qwords
uint64_t lisp_alloc(uint64_t size);
uint64_t gc_in_root(uint64_t ptr, uint64_t env);
volatile uint64_t lisp_heap[LISP_HEAP_SIZE] __attribute__((aligned(8)));
// A pointer that is used to allocate memory in lisp_heap
volatile uint64_t
	lisp_heap_ptr; // not defined as a pointer because of ptr arithmetic
#endif
