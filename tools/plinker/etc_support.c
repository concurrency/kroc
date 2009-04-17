
#include <stdlib.h>
#include <stdio.h>

#include <ccsp.h>

#define WS_SEP 32
#define VS_SEP 32

int kroc_argc		= 0;
char **kroc_argv	= NULL;

void etc_error_bounds (void *sched, word *Wptr, char *file, int line)
{
	fprintf (stderr, "Bounds error at %s:%d (Wptr = %p)\n", file, line, Wptr);
	exit (1);
}

void etc_error_overflow (void *sched, word *Wptr, char *file, int line)
{
	fprintf (stderr, "Arithmetic overflow at %s:%d (Wptr = %p)\n", file, line, Wptr);
	exit (1);
}

void _println (word *ws)
{
	char *str 	= (char *) ws[0];
	word len	= ws[1];
	while (len--)
		fputc (*(str++), stdout);
	fputc ('\n', stdout);
}

void occam_debug (word val)
{
	fprintf (stderr, "occam_debug %d (%08x)\n", val, val);
}

int occam_start (int argc, char **argv, void *code_entry, void *start_proc, int ws, int vs, int ms)
{
	void (*entry_fp)(void *, word *) = (void (*)(void *, word *)) code_entry;
	word *Wptr, *Wptr_data;
	int arg_ptr = 1;
	int i, words;

	fprintf (stderr, "occam_start %p, %p, %d, %d, %d\n", code_entry, start_proc, ws, vs, ms);

	/* Stash Command Line */

	kroc_argc = argc;
	kroc_argv = argv;


	/* Setup CCSP */

	ccsp_set_branding ("KRoC/LLVM");

	if (!ccsp_init (entry_fp)) {
		fprintf (stderr, "ccsp_init failed\n");
		return 1;
	}
	

	/* Setup Initial Workspace */

	words 		= ws + WS_SEP + vs + VS_SEP;
	Wptr_data 	= (word *) dmem_alloc (words * sizeof (word));

	for (i = 0; i < words; ++i) {
		Wptr_data[i] = MostNeg;
	}

	Wptr 		= Wptr_data + (ws + (WS_SEP / 2));
	if (vs) {
		Wptr[arg_ptr++] = (word) (Wptr_data + (ws + WS_SEP + (VS_SEP / 2)));
	}
	/* FIXME: ms */

	Wptr[Iptr]	= (word) start_proc;


	/* Enter the CCSP Kernel */

	fprintf (stderr, "entering kernel... (Wptr = %p)\n", Wptr);
	
	ccsp_kernel_entry (Wptr, NotProcess_p);

	fprintf (stderr, "left kernel...\n");
	
	return 0;
}

