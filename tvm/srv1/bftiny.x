/* Customized linker script 
 * 
 * Blackfin 'tiny' memory model
 *
 * (c) Unknown author
 * 2004, 2005, Martin Strubel <hackfin@section5.ch>
 *
 * $Id: bftiny.x,v 1.1 2005/08/17 13:36:08 strubi Exp $
 *
 * This linker script is for standalone "bare metal" code
 * that does neither depend on a newlib or uses a crt0.s.
 *
 * The user has to write supply his startup code in startup.o
 * and is thus supposed to have total control over the Blackfin :-)
 *
 */

OUTPUT_FORMAT("elf32-bfin", "elf32-bfin", "elf32-bfin")
OUTPUT_ARCH(bfin)
ENTRY(start)

MEMORY
{
	ram(rwx)            : ORIGIN = 0x00000000, LENGTH = 0x02000000
	l1code(x)           : ORIGIN = 0xffa00000, LENGTH = 0xc000
	l1code_cache(x)     : ORIGIN = 0xffa10000, LENGTH = 0x4000
	/* BF533, BF537, BF539 */
	l1data_a(rw)        : ORIGIN = 0xff800000, LENGTH = 0x4000
	l1data_cache_a(rw)  : ORIGIN = 0xff804000, LENGTH = 0x4000
	l1data_b(rw)        : ORIGIN = 0xff900000, LENGTH = 0x4000
	l1data_cache_b(rw)  : ORIGIN = 0xff904000, LENGTH = 0x4000
	scratchpad(rw)      : ORIGIN = 0xffb00000, LENGTH = 0x1000
}

SECTIONS
{
	.l1code         : { startup.o(STARTUP_SECTION) *(.l1code) } > l1code
	.text           : { *(.text) *(.gnu.warning) } > l1code
	.l1data         :
	{
		*(.data) *(.l1data)*(.l1data.a)
	} > l1data_a
	.l1data.b       : { *(.l1data.b) } > l1data_b
	.rodata         : { *(.rodata .rodata.* .gnu.linkonce.r.*) } > l1data_a
	.data           :
	{
		*(.data .data.* .gnu.linkonce.d.*) SORT(CONSTRUCTORS)
	} > l1data_a
	.ctors          :
	{
		KEEP (*crtbegin*.o(.ctors))
		KEEP (*(EXCLUDE_FILE (*crtend*.o ) .ctors))
		KEEP (*(SORT(.ctors.*)))
		KEEP (*(.ctors))
	} > l1data_a
	.dtors          :
	{
		KEEP (*crtbegin*.o(.dtors))
		KEEP (*(EXCLUDE_FILE (*crtend*.o ) .dtors))
		KEEP (*(SORT(.dtors.*)))
		KEEP (*(.dtors))
	} > l1data_a
	/* Special case: Some BSS blocks are too big to fit into L1 RAM.
	   See crt0.asm:clear_bss() for BSS initialization.
	 */

	.sdram.bss          : {
		__sdram_bss_start = .;
		*(.bss.sdram)
		. = ALIGN(32 / 8);
		__sdram_bss_end = .;
	} > ram

	.bss            :
	{
		/* PROVIDE(__bss_start = .); */
		__bss_start = .;
		*(.dynbss)
		*(.bss .bss.* .gnu.linkonce.b.*)
		*(COMMON)
		. = ALIGN(32 / 8);
		__bss_end = .;
		/* PROVIDE (__bss_end = .); */
	} > l1data_a
	. = 0xffb00ffc;
	PROVIDE (_supervisor_stack = .);
}
