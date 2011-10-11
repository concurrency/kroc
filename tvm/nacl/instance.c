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

static void v_error_out (tvm_instance_t *tvm, const char *fmt, va_list ap)
{
	const int bufsize = 4095;
	char *buffer = malloc(bufsize + 1);
	int pos = 0;

	pos += snprintf (buffer + pos, bufsize - pos, "%s: ", prog_name);
	pos += vsnprintf (buffer + pos, bufsize - pos, fmt, ap);
	if (errno != 0) {
		pos += snprintf (buffer + pos, bufsize - pos, ": %s", strerror(errno));
	} else {
		pos += snprintf (buffer + pos, bufsize - pos, ".");
	}

	buffer[pos] = '\0';
	fprintf (stderr, "%s\n", buffer);

	if (tvm->last_error)
		free(tvm->last_error);
	tvm->last_error = strdup(buffer);
	
	free(buffer);
}

static int error_out (tvm_instance_t *tvm, const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	v_error_out (tvm, fmt, ap);
	va_end (ap);

	return -1;
}

static int error_out_no_errno (tvm_instance_t *tvm, const char *fmt, ...)
{
	va_list ap;

	errno = 0;

	va_start (ap, fmt);
	v_error_out (tvm, fmt, ap);
	va_end (ap);

	return -1;
}

tvm_instance_t *tvm_alloc_instance(void)
{
	tvm_instance_t *tvm = malloc(sizeof(tvm_instance_t));

	tvm_init(&(tvm->tvm));

	tvm->last_error = NULL;
	tvm->fw_bc = NULL;
	tvm->us_bc = NULL;

	tvm->user = NULL;
	tvm->firmware = NULL;

	tvm->kyb_channel = NOT_PROCESS_P;
	tvm->scr_channel = NOT_PROCESS_P;
	tvm->err_channel = NOT_PROCESS_P;

	return tvm;
}

void tvm_free_instance(tvm_instance_t *tvm)
{
	if (tvm->user)
		tvm_free_ectx(tvm->user);
	if (tvm->firmware)
		tvm_free_ectx(tvm->firmware);
	if (tvm->fw_bc)
		tvm_free_bytecode(tvm->fw_bc);
	if (tvm->us_bc)
		tvm_free_bytecode(tvm->us_bc);
	if (tvm->last_error)
		free(tvm->last_error);

	free(tvm);
}

static int install_user_ctx (tvm_instance_t *tvm, bytecode_t *us_bc)
{
	const char *const tlp_fmt = "?!!";
	int kyb_p = -1, scr_p = -1, err_p = -1, valid_tlp = 1;
	tbc_t *tbc = us_bc->tbc;
	WORD *argv;
	char *tlp;
	int i, tlp_len;

	if (tbc->tlp != NULL) {
		tlp = tbc->tlp->fmt;
	} else {
		tlp = (char *) tlp_fmt;
	}

	tvm->tlp_argv[0] = (WORD) &(tvm->kyb_channel);
	tvm->tlp_argv[1] = (WORD) &(tvm->scr_channel);
	tvm->tlp_argv[2] = (WORD) &(tvm->err_channel);

	for (tlp_len = 0; valid_tlp && (tlp[tlp_len] != '\0'); ++tlp_len) {
		char arg = tlp[tlp_len];
		if (arg == '.' || arg == 'F') {
			/* OK */
		} else if (kyb_p < 0 && (arg == '?' || arg == 'S')) {
			kyb_p = tlp_len;
			if (arg == 'S')
				kyb_p |= 0x100;
		} else if (scr_p < 0 && (arg == '!' || arg == 'C')) {
			scr_p = tlp_len;
			if (arg == 'C')
				scr_p |= 0x100;
		} else if (err_p < 0 && (arg == '!' || arg == 'C')) {
			err_p = tlp_len;
			if (arg == 'C')
				err_p |= 0x100;
		} else {
			valid_tlp = 0;
		}	
	}
	
	if (tlp_len > TVM_ECTX_TLP_ARGS)
		valid_tlp = 0;

	#if !defined(TVM_DYNAMIC_OCCAM_PI)
	if ((kyb_p | scr_p | err_p) & 0x100)
		valid_tlp = 0;
	#endif

	if (!valid_tlp) {
		error_out_no_errno (tvm, "unsupported top-level-process format: \"%s\"", tlp);
		return -1;
	}

	#if defined(TVM_DYNAMIC_OCCAM_PI)
	if (kyb_p & 0x100)
		tvm->tlp_argv[0] = (WORD) tvm_mt_alloc (NULL, TLP_MT_CB_TYPE, 1);
	if (scr_p & 0x100)
		tvm->tlp_argv[1] = (WORD) tvm_mt_alloc (NULL, TLP_MT_CB_TYPE, 1);
	if (err_p & 0x100)
		tvm->tlp_argv[2] = (WORD) tvm_mt_alloc (NULL, TLP_MT_CB_TYPE, 1);
	kyb_p &= 0xff;
	scr_p &= 0xff;
	err_p &= 0xff;
	#endif /* TVM_DYNAMIC_OCCAM_PI */

	argv = (WORD *) malloc (sizeof (WORD) * tlp_len);

	for (i = 0; i < tlp_len; ++i) {
		if (i == kyb_p) {
			argv[i] = tvm->tlp_argv[0];
		} else if (i == scr_p) {
			argv[i] = tvm->tlp_argv[1];
		} else if (i == err_p) {
			argv[i] = tvm->tlp_argv[2];
		} else {
			argv[i] = (WORD) MIN_INT;
		}
	}

	tvm->user = tvm_allocate_ectx (tvm, us_bc, tlp, argv);
	
	if (argv != NULL)
		free (argv);
	if (tvm->user == NULL)
		return -1;

	return 0;
}

int tvm_load_bytecode(tvm_instance_t *tvm, uint8_t *tbc, size_t tbc_len)
{
	bytecode_t *us_bc;

	if (tvm->us_bc) {
		tvm_free_bytecode(tvm->us_bc);
		tvm_free_ectx(tvm->user);
		tvm->us_bc = NULL;
		tvm->user = NULL;
	}

	if ((us_bc = tvm_alloc_bytecode(tbc, tbc_len)) == NULL) {
		return -1;
	}

	if (install_user_ctx (tvm, us_bc) < 0) {
		tvm_free_bytecode(us_bc);
		return -2;
	}

	tvm->us_bc = us_bc;

	return 0;
}
	
