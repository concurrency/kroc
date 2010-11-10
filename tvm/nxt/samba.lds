/* Copyright (c) 2007,2008 the NxOS developers
 *
 * See AUTHORS for a full list of the developers.
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

/* NXT linker script source.
 *
 *  This file must be run through a sed script to generate an ldscript
 *  suitable for either a SAM-BA or rom image.
 *
 *  The sed script achieves this by selectively copying the lines with
 *
 *  Notes on alignment:
 *  1) Sections should be 4-byte aligned otherwise ARM fetches will be
 *     misaligned.
 *  2) The stack should be 8-byte aligned for the APCS. That's because
 *     STRD and LDRD assume that they are on 8-byte boundaries.
 */

/*
 * Memory definitions.
 */

MEMORY {
vector_ram : ORIGIN = 2M, LENGTH = 64
ram : ORIGIN = 2M + 64, LENGTH = 64K - 64 + 21
}

ROM_BASE = 1M;
ROM_SIZE = 256k;
RAM_BASE = 2M;
RAM_SIZE = 64k;
SAMBA_RESERVED_SIZE = 8k;

SECTIONS {
  /*
   * Interrupt vectors. These are loaded to the bottom of memory at
   * boot time.
   */
  .vectors : {
    KEEP(vectors.o (*.text *.text.*))
  } > vector_ram


  /*
   * This section contains code that is relocated to RAM before
   * execution.
   *
   * This section is used for code that has to reside in ram to
   * function correctly (like flash writing code)
   */
  .ram_text : {
    . = ALIGN(4);
    *(.text.ram)
  } > ram


  /*
   * Read/Write initialized data. Runs from RAM, gets copied there
   * from ROM if booting from flash.
   */
  .data : {
    . = ALIGN(4);
    * (*.data *.data.*)
  } > ram


  /*
   * The main kernel executable code, as well as all the read-only
   * data. If possible, this goes into flash, to free up more RAM.
   */
  .text : {
    . = ALIGN(4);
    * (*.text *.text.* *.glue*)
    . = ALIGN(4);
    * (*.rodata *.rodata.*)
  }
   > ram


  /*
   * The BSS section is zero-initialized data. The section does not
   * take any space in the final image, but the linker helpfully
   * defines the symbols we need to be able to initialize the section
   * properly.
   */
  .bss : {
    . = ALIGN(4);
    * (*.bss *.bss.*)
  } > ram


  /*
   * The various kernel stacks.
   *
   * Although not strictly required, this zone is zeroed out at boot
   * time to provide a clean environment for the kernel.
   */
  .stack  : {
    /* irq stack */
    . = ALIGN(8);
    . += 0x60; /* 96b, which is 3*8*4, the maximum irq stack size. */
    . = ALIGN(8);
    __irq_stack__ = .;

    /* supervisor stack */
    __supervisor_stack_bottom__ = . ;
    . += 0x400; /* 1k supervisor stack. */
    . = ALIGN(8);
    __supervisor_stack__ = .;
    __supervisor_stack_top__ = . ;
  } > ram


  /* SAMBA stage 0.5 */
   .samba_boot ADDR(.bss) : {
     KEEP(samba_init.o (.samba_init));
   }


  /*
   * The magic metadata struct is tacked onto the start of the kernel binary.
   */
   .magic ADDR(.samba_boot) + SIZEOF(.samba_boot) : {
    /* Magic identifier */
    LONG(0xDEADBEEF);
    /* Kernel in-ram size */
    LONG(SIZEOF(.vectors) + SIZEOF(.ram_text) +
         SIZEOF(.data) + SIZEOF(.bss) + SIZEOF(.stack)
     + SIZEOF(.text));
    /* Kernel in-rom size */
     LONG(0);
    /* Write address */
     LONG(RAM_BASE + SAMBA_RESERVED_SIZE);
    /* Bootup address */
     LONG(ADDR(.samba_boot) + SAMBA_RESERVED_SIZE);
    /* SAM-BA kernel? */
     BYTE(1);
  }
   > ram

  /* Discard anything not mapped at this point. */
  /DISCARD/ : {
    * (*);
  }

  /*
   * Symbol definitions for the use of the kernel code.
   */
  __vectors_ram_start__  = ADDR(.vectors);
  __vectors_ram_end__    = (ADDR(.vectors) + SIZEOF(.vectors));
  __vectors_load_start__ = LOADADDR(.vectors);
  __vectors_load_end__   = (LOADADDR(.vectors) + SIZEOF(.vectors));

  __ramtext_ram_start__ = ADDR(.ram_text);
  __ramtext_ram_end__   = ADDR(.ram_text) + SIZEOF(.ram_text);
  __ramtext_load_start__ = LOADADDR(.ram_text);
  __ramtext_load_end__ = __ramtext_load_start__ + SIZEOF(.ram_text) ;

  __data_ram_start__ = ADDR(.data);
  __data_ram_end__   = ADDR(.data) + SIZEOF(.data);
  __data_load_start__ = LOADADDR(.data);
  __data_load_end__ = __data_load_start__ + SIZEOF(.data) ;

  __text_start__ = ADDR(.text);
  __text_end__ = ADDR(.text) + SIZEOF(.text);

  __bss_start__ = ADDR(.bss);
  __bss_end__   = (ADDR(.bss) + SIZEOF(.bss));

  __stack_start__ = ADDR(.stack);
  __stack_end__ = (ADDR(.stack) + SIZEOF(.stack));

   __boot_from_samba__ = 1;

   __rom_userspace_start__ = 0;
  __rom_userspace_end__ = (ROM_BASE + ROM_SIZE);

  __ram_userspace_start__ = __stack_end__;
  __ram_userspace_end__ = (RAM_BASE + RAM_SIZE);

   __kernel_ram_load_size__ = __text_end__ - __vectors_ram_start__;
}
