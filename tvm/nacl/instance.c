/*
 * main.c - NaCl TVM Wrapper
 *
 * Copyright (C) 2011  Carl G. Ritson
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "tvm_nacl.h"

static const char *prog_name = "TVM";

#if defined(TVM_DYNAMIC_OCCAM_PI)
#define MT_DEFINES      1
#define MT_TVM          1
#include <mobile_types.h>
#define TLP_MT_CB_TYPE	(MT_SIMPLE |\
			MT_MAKE_TYPE (MT_CB) |\
			MT_CB_SHARED |\
			(1 << MT_CB_CHANNELS_SHIFT))
#endif /* TVM_DYNAMIC_OCCAM_PI */ 


/*{{{  tvm_sleep */
static void tvm_sleep (tvm_instance_t *tvm)
{
	ECTX firmware	= tvm->firmware;
	ECTX user	= tvm->user;
	WORD now	= firmware->get_time (firmware);
	WORD timeout	= 0;
	int set		= 0;

	if (firmware->tptr != NOT_PROCESS_P) {
		timeout = firmware->tnext;
		set++;
	}

	if (user->tptr != NOT_PROCESS_P) {
		if (!set || TIME_BEFORE(user->tnext, timeout)) {
			timeout = user->tnext;
			set++;
		}
	}

	if (set && TIME_AFTER(timeout, now)) {
		unsigned int period = timeout - now;
		
		if (period > 0) {
			struct timespec to;

			to.tv_sec = (period / 1000000);
			to.tv_nsec = ((period % 1000000) * 1000);
			
			nanosleep (&to, 0);
		}
	}
}
/*}}}*/

static void v_error_out (const char *fmt, va_list ap)
{
	fprintf (stderr, "%s: ", prog_name);
	vfprintf (stderr, fmt, ap);
	if (errno != 0) {
		fprintf (stderr, ": ");
		perror ("");
	} else {
		fprintf (stderr, ".\n");
	}
}

static int error_out (const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	v_error_out (fmt, ap);
	va_end (ap);

	return -1;
}

static int error_out_no_errno (const char *fmt, ...)
{
	va_list ap;

	errno = 0;

	va_start (ap, fmt);
	v_error_out (fmt, ap);
	va_end (ap);

	return -1;
}

tvm_instance_t *tvm_alloc_instance(void)
{
	tvm_instance_t *tvm = malloc(sizeof(tvm_instance_t));

	// FIXME: allocate contexts

	tvm_init(&(tvm->tvm));

	tvm->fw_bc = NULL;
	tvm->us_bc = NULL;

	tvm->kyb_channel = NOT_PROCESS_P;
	tvm->scr_channel = NOT_PROCESS_P;
	tvm->err_channel = NOT_PROCESS_P;

	return tvm;
}

void tvm_free_instance(tvm_instance_t *tvm)
{
	// FIXME: release contexts
	if (tvm->fw_bc)
		tvm_free_bytecode(tvm->fw_bc);
	if (tvm->us_bc)
		tvm_free_bytecode(tvm->us_bc);

	free(tvm);
}

int tvm_load_bytecode(tvm_instance_t *tvm, uint8_t *tbc, size_t tbc_len)
{
	if (tvm->us_bc)
		tvm_free_bytecode(tvm->us_bc);

	tvm->us_bc = tvm_alloc_bytecode(tbc, tbc_len);

	if (tvm->us_bc) {
		return 0;
	} else {
		return -1;
	}
}
