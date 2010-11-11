.text
.align 4
.globl _start

_start:
	mov sp, #0x210000
	stmfd sp!, {lr}
	bl flash_driver
	ldmfd sp!, {pc}

