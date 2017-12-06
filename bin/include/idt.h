#ifndef IDT_H_INCLUDED
#define IDT_H_INCLUDED
#include <io.h>
#include <lib.h>
#include <stddef.h>
#include <stdint.h>
#include <mem.h>
#include <tty.h>

#define MASTER_PIC_CONTROL_PORT 0x20
#define SLAVE_PIC_CONTROL_PORT 0xA0
#define KBD_PORT 0x60
#define EOI_COMMAND 0x20
#define NUMBER_OF_IDT_ENTRIES 256
#define TRAP_GATE 0xF

struct idt_ptr {
	uint16_t limit;
	uint64_t base;
} __attribute__((packed));
typedef struct idt_ptr idt_ptr_t;

struct idt_entry {
	uint16_t offset_low;
	uint16_t selector;
	uint8_t zero; /* unused, set to 0 */
	uint8_t type_attr;
	uint16_t offset_high;
	uint32_t offset_top;
	uint32_t res;
} __attribute__((packed));
typedef struct idt_entry idt_entry_t;

idt_ptr_t idt_ptr;				// aka IDTR
idt_entry_t idt_entries[NUMBER_OF_IDT_ENTRIES]; // the actual IDT


/* Send "End of interrupt" to master PIC */
void eoi_master();
/* Send "End of interrupt" to slave PIC */
void eoi_slave();
/* Called when PIT sends an interrupt */
// void pit_handler(void);
/* Just a standard handler for interrupts we want to ignore */
void empty_handler();
/* Initializes the IDT */
void idt_initialize();

uint8_t read_master_pic(void);

// From irq.s:
/* Enables a specific IRQ */
void enable_irq(unsigned int irq);
/* Disables a specific IRQ */
void disable_irq(unsigned int irq);
/* Used to load the IDT table (from gdt_idt.s file) */
void load_idt(void *ptr);

void empty_isr(void);
void empty_isr1(void);
void empty_isr2(void);
void pit_isr(void);
void kbd_isr(void);
#endif
