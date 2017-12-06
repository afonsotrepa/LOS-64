#include <idt.h>

// Send End of interrupt signal to pic
void eoi_master(void)
{
	outb(MASTER_PIC_CONTROL_PORT, EOI_COMMAND);
}

void eoi_slave(void)
{
	outb(SLAVE_PIC_CONTROL_PORT, EOI_COMMAND);
}


uint8_t read_master_pic(void)
{
	return inb(0x60);
}

void pit_handler(void)
{
	print("PIT");
	eoi_master();
}


void empty_handler(void)
{
}

void empty_handler1(void)
{
	eoi_master();
}

void empty_handler2(void)
{
	eoi_master();
	eoi_slave();
}

// A lot of code taken from
// https://github.com/heatd/Onyx/blob/mainline/kernel/arch/x86_64/idt.c
void load_idt_entry(uint8_t entry, uint64_t offset, uint16_t selector,
		    uint8_t flags)
{
	idt_entries[entry].offset_low = offset & 0xFFFF;
	idt_entries[entry].offset_high = (offset >> 16) & 0xFFFF;
	idt_entries[entry].offset_top = (offset >> 32);
	idt_entries[entry].selector = selector;

	idt_entries[entry].zero = 0;
	idt_entries[entry].type_attr = flags | 0x60;
}
void idt_initialize()
{
	print("IDT_initialize\n");
	asm volatile("cli");

	for (uint64_t i = 0; i <= NUMBER_OF_IDT_ENTRIES; i++) {
		// doesn't matter if empty_isr or &empty_isr is used
		load_idt_entry(i, (uint64_t) empty_isr, 0x08, 0x8E);
	}

	// 0x20 is the first available entry after the PIC remapping
	load_idt_entry(0x20, (uint64_t) pit_isr, 0x08, 0x8E);

	load_idt_entry(0x21, (uint64_t) kbd_isr, 0x08, 0x8E);
	enable_irq(0);

	print("LoadIDT\n");
	idt_ptr.limit = sizeof(idt_entry_t) * 256 - 1;
	idt_ptr.base = (uint64_t) &idt_entries;
	load_idt(&idt_ptr);
	asm volatile("sti");
	// asm volatile("int 0x20");
}
