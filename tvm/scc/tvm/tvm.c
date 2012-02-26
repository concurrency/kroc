#include "tvm-scc.h"

static tvm_t tvm;
tvm_ectx_t context;

#define MEM_WORDS 5120
static WORD raw_memory[MEM_WORDS + 1];
static WORDPTR memory;

/* The bytecode file, loaded into flash at a fixed address. */
extern const unsigned char transputerbytecode[];
static const BYTE *tbc_data = transputerbytecode;
static BYTEPTR initial_iptr;

//TODO modify for use on SCC
/* Time is in milliseconds, since microseconds wrap round too fast in
   16 bits to be useful. */
static WORD get_time(ECTX ectx) {
	return time_millis();
}

//TODO modify for use on SCC
static void modify_sync_flags(ECTX ectx, WORD set, WORD clear) {
//	cli ();
	ectx->sflags = (ectx->sflags & (~clear)) | set;
//	sei ();
}

static void dump_machine_state() {
/*	WORDPTR wp;
	BYTEPTR bp;
	const BYTE *file;
	UWORD line;
	const UWORD iptr_offset = (UWORD) (context.iptr - initial_iptr);

	if (tbc_file_and_line (tbc_data, iptr_offset, &file, &line) == 0) { //TODO TBC USED
		printf_P (PSTR ("\nfile=%S line=%d"), file, (int) line);
	}

	printf_P (PSTR ("\nwptr=%04x (rel=%04x)  iptr=%04x (rel=%04x)  "
	                "eflags=%04x sflags=%04x\n"
	                "areg=%04x breg=%04x creg=%04x  oreg=%04x\n"),
	          (int) context.wptr, (int) (context.wptr - memory),
	          (int) context.iptr, (int) (context.iptr - initial_iptr),
	          context.eflags, context.sflags,
	          context.areg, context.breg, context.creg, context.oreg);

	for (wp = context.wptr - 7; wp < context.wptr + 7; ++wp) {
		if (wp == context.wptr) {
			printf_P (PSTR ("wptr>"));
		}
		printf_P (PSTR ("%04x "), read_word (wp));
	}
	printf_P (PSTR ("\n"));

	for (bp = context.iptr - 12; bp < context.iptr + 12; ++bp) {
		if (bp == context.iptr) {
			printf_P (PSTR ("iptr>"));
		}
		printf_P (PSTR ("%02x "), read_byte (bp));
	}
	printf_P (PSTR ("\n"));
*/}

int main () {
	int i;

	init_clock();
	init_idt();

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

	printf("INFO: SFFI table length is %d.\n", sffi_table_length);
	
	printf("INFO: Starting TVM ...\n");

	while(1){}

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
			default: {
				printf("ERRO: Exit status is %x.\n", &ret);
			}
		}
	}

	/* NOTREACHED */
	return 0;
}
