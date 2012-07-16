#include "tvm-scc.h"
#include <bytecode.h>

//#define PREREAD 1

static tvm_t tvm;
tvm_ectx_t context;

// 50k WORDS = 200kB -> Leaves 56kB for TVM + bytecode to fit in L2 cache (256kB)
#define MEM_WORDS 512000
static WORD raw_memory[MEM_WORDS + 1];
static WORDPTR memory;

/* The bytecode file, loaded into flash at a fixed address. */
static const BYTE *tbc_data = transputerbytecode;
static BYTEPTR initial_iptr;

WORD get_time(ECTX ectx) {
	return (WORD)time_millis();
}

static void modify_sync_flags(ECTX ectx, WORD set, WORD clear) {
	ectx->sflags = (ectx->sflags & (~clear)) | set;
}

int main()
{
	int i;
#ifdef PREREAD
	unsigned char tmp;
	unsigned char* preread_ptr;
#endif

	enable();

	/* The Transputer memory must be word-aligned. */
	memory = (WORDPTR) (((int) (raw_memory + 1)) & ~1);

	for (i = 0; i < MEM_WORDS; i++) {
		memory[i] = MIN_INT;
	}

	tvm_init (&tvm);
	tvm_ectx_init (&tvm, &context);

	if (init_context_from_tbc(&context, tbc_data, memory, MEM_WORDS) != 0) {
		printf("ERRO: init_context_from_tbc() failed.\n");
	}
	initial_iptr = context.iptr;

	context.get_time = get_time;
	context.modify_sync_flags = modify_sync_flags;
	context.sffi_table = sffi_table;
	context.sffi_table_length = sffi_table_length;

#ifdef PREREAD
	printf("INFO: Prereading execution context ...\n");
	
	preread_ptr = (unsigned char*)&context;
	for(i = 0; i < sizeof(context); i++)
	{
		tmp = *preread_ptr;
		preread_ptr++;
	}
	
	preread_ptr = (unsigned char*)transputerbytecode;
	for(i = 0; i < sizeof(transputerbytecode); i++)
	{
		tmp = *preread_ptr;
		preread_ptr++;
	}
/*	
	preread_ptr = (unsigned char*)raw_memory;
	for(i = 0; i < sizeof(raw_memory); i++)
	{
		tmp = *preread_ptr;
		preread_ptr++;
	}*/
#endif
	
	printf("INFO: Starting TVM ...\n");

	while (1) {
		int ret = tvm_run (&context);

		switch (ret) {
			case ECTX_PREEMPT:
			case ECTX_TIME_SLICE: {
				break;
			}
			case ECTX_SLEEP: {
				WORD next = context.tnext;
				WORD now;
				do {
					now = time_millis();
				} while (TIME_AFTER (next, now));
				break;
			}
			case ECTX_INTERRUPT: {
				break;
			}
			case ECTX_EMPTY: {
				break;
			}
			case ECTX_SHUTDOWN: {
				printf("INFO: End of program reached.\n");
			}
		}
	}

	/* NOTREACHED */
	return 0;
}
