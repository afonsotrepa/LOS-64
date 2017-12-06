#include <mem.h>

/// Might want to make a merge() function to merge adjacent deallocated cells,
/// simply to prevent fragmentation of the heap that might eventually make it
/// unusuable otherwise.

node_t *head = NULL;

void heap_initialize(void)
{
	if (head == NULL) { // If heap not initialized:
		head = (node_t *) heap_bottom;
		head->size = HEAP_SIZE - sizeof(node_t);
		head->next = NULL;
	}
}

bool inside_heap(void *ptr)
{
	if (ptr > heap_bottom || ptr < (heap_top))
		return true;
	else
		return false;
}

// Returns a pointer to the best matching node
node_t *find_size(uint16_t size)
{
	node_t *current = head;
	node_t *best = current; // node pointing to the best cell to use

	do {
		if (current->size < best->size && current->size >= size &&
		    inside_heap(current + size)) {
			best = current;
		}
	} while (current->next != NULL);

	return best;
}

/* Returns pointer to the last node in the linked list */
node_t *find_last(void)
{
	node_t *current = head;
	while (current->next != NULL) {
		*current = *(current->next);
	}
	return current;
}

node_t *find_previous(node_t *node)
{
	node_t *current = head;

	while (current->next != NULL) {
		if (current->next == node) return current;
	}
	return head; // if previous not found, return the head of the list
}

/* Inserts node at the end of the linked list */
void push(size_t size, node_t *new)
{
	node_t *last = find_last();

	new->size = size;
	new->next = NULL;
	last->next = new;
}

void *malloc(size_t size)
{
	// Make sure we allocate enough space to save the node needed to
	// deallocate the memory later
	if (size + sizeof(size) < sizeof(node_t))
		size = sizeof(node_t) - sizeof(size);

	node_t *old = find_size(size + sizeof(size));

	node_t *previous = find_previous(old);

	if (old->size > size + sizeof(size)) {
		if (previous->next != NULL) {
			previous->next += size + sizeof(size);
			previous->next->size =
				old->size - (size + sizeof(size));
			previous->next->next = old->next;
		} else {
			head += size + sizeof(size);
			head->size -= size + sizeof(size);
			head->next = NULL;
		}
	} else {
		// may need to test if previous->next is null too!
		previous->next = old->next;
	}


	size_t *tmp = (size_t *) old;
	*tmp = size;
	return tmp + sizeof(size);
}

// Same as malloc, but the pointer is aligned by 4bytes
// Should really make it so it creates an aligned pointer if none is avaliable,
// instead of just returnign NULL!!!
void *malloc_aligned(size_t size)
{
	for (int i = 0; i < 8; i++) {
		// If aligned
		if (((uint64_t) find_size(size + sizeof(size) + i) &
		     (uint64_t) 0b000) == 0)
			return malloc(size + i);
	}
	return NULL;
}

// It currently doesn't check if the pointer passed to it was allocated
// or not,
// which might result in bad deallocations, resulting in corruption of
// allocated
// memory and/or creation of partially or completely duplicated nodes in
// the
// list.
// I presume that the user is smart enough to call free() with the
// pointer
// returned by malloc() and nothing else, this might turn out to be a
// mistake.
void free(void *ptr)
{
	if (inside_heap(ptr))
		push(*((size_t *) (ptr - sizeof(size_t))),
		     (node_t *) (ptr - sizeof(size_t)));
}



void *memset(void *pointer, uint8_t element, size_t len)
{
	for (unsigned int i = 0; i < len; i++)
		((uint8_t *) pointer)[i] = element;
	return pointer;
}

// Not working for pointers atm (works for arrays though)
void *memcpy(void *dst, const void *src, size_t len)
{
	uint8_t *d = dst;
	const uint8_t *s = src;
	while (len--) {
		*d++ = *s++;
	}

	return dst;
}


// Helper function for lisp_alloc
// Used to check if an area in memory is in the root-set (dyn and lex env)
uint64_t gc_in_root(uint64_t ptr, uint64_t env)
{
	// if env isn't a string or cons, return 0
	if ((uint64_t) env % 8 != 0b001 && (uint64_t) env % 8 != 0b011)
		return 0;
	// 0b001 == cons tag; 0b011 == string tag
	if (ptr == env - 0b001 || ptr == env - 0b011) return env;
	// if env is a cons
	if ((uint64_t) env % 8 == 0b001) {
		// test the car first
		if (gc_in_root(ptr, *((uint64_t *) (env - 0b001))) != 0)
			return gc_in_root(ptr, *((uint64_t *) (env - 0b001)));
		else // return the result from testing the cdr if the car fails
			return gc_in_root(ptr,
					  *((uint64_t *) (env - 0b001 + 8)));
	} else { // if ptr != env (without the tag) and env not a cons, return 0
		return 0;
	}
}
// The heap used to store lisp objects over 8 bytes (1 qword)
volatile uint64_t lisp_heap[LISP_HEAP_SIZE] __attribute__((aligned(8))) = {0};
// A pointer that is used to allocate memory in lisp_heap
volatile uint64_t lisp_heap_ptr = 0;
// Used to allocate space for lisp objects
// Only checks the dynamic and lexical variables
uint64_t lisp_alloc(uint64_t size)
{
	uint64_t lexical_env;
	asm("mov %0, r15;" : "=r"(lexical_env) :); // lexical_env = r15
	// uint64_t dynamic_env = *alist_ptr;
	uint64_t dynamic_env;
	asm("mov %0, [alist_ptr];" : "=r"(dynamic_env) :); // dynamic_env= alist

	uint64_t offset = 0; // used after the loop too
	for (; offset < size;) {
		// check the lex env
		uint64_t tmp = gc_in_root((uint64_t) lisp_heap + lisp_heap_ptr +
						  offset,
					  lexical_env);
		// check the dyn env if it's not found in the lex
		if (!tmp)
			tmp = gc_in_root((uint64_t) lisp_heap + lisp_heap_ptr +
						 offset,
					 dynamic_env);
		// if found in any env:
		if (tmp != 0) {
			tmp &= 0b111; // only care about the last 3 bits
			// cons
			if (tmp == 0b001) lisp_heap_ptr += 16; // 2 qwords
			// string
			else {
				//>>2 removes the fixnum tag from the str length
				lisp_heap_ptr +=
					(*((uint64_t *) ((uint64_t) lisp_heap +
							 lisp_heap_ptr +
							 offset)) >>
					 2) +
					8;
				// align the lisp_heap_ptr (if necessary)
				if (lisp_heap_ptr % 8 != 0)
					lisp_heap_ptr += 8 - lisp_heap_ptr % 8;
			}
			offset = 0; // reset the offset if found
		} else {
			offset += 8; // check the next qword if not found
		}
		// overflow check:
		if (lisp_heap_ptr + offset >= LISP_HEAP_SIZE) {
			lisp_heap_ptr = 0; // reset the pointer
			offset = 0;	// reset the offset
			print("\n###LISP heap overflow (not a crash)###\n");
		}
	}
	lisp_heap_ptr += offset;
	return (uint64_t) lisp_heap + lisp_heap_ptr - offset;
}
