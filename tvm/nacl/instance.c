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

	tvm->stop = 0;
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

static void remove_firmware_and_ectx(tvm_instance_t *tvm)
{
	if (tvm->fw_bc) {
		tvm_free_bytecode(tvm->fw_bc);
		tvm->fw_bc = NULL;
	}
	if (tvm->firmware) {
		tvm_free_ectx(tvm->firmware);
		tvm->firmware = NULL;
	}
	if (tvm->us_bc) {
		tvm_free_bytecode(tvm->us_bc);
		tvm->us_bc = NULL;
	}
	if (tvm->user) {
		tvm_free_ectx(tvm->user);
		tvm->user = NULL;
	}
}

void tvm_free_instance(tvm_instance_t *tvm)
{
	remove_firmware_and_ectx(tvm);
	
	if (tvm->last_error)
		free(tvm->last_error);

	free(tvm);
}

static int install_firmware_ctx (tvm_instance_t *tvm)
{
	const uint8_t *tbc;
	bytecode_t *fw_bc;
	size_t tbc_len;

	tbc = tvm_nacl_firmware(&tbc_len);

	if ((fw_bc = tvm_alloc_bytecode(tbc, tbc_len)) == NULL) {
		error_out_no_errno (tvm, "failed to load/decode firmware image");
		return -1;
	}

	if ((tvm->firmware = tvm_allocate_ectx (tvm, fw_bc, "!??", tvm->tlp_argv)) == NULL) {
		error_out_no_errno (tvm, "unable to allocate firmware execution context");
		tvm_free_bytecode (fw_bc);
		return -1;
	}

	tvm->fw_bc = fw_bc;

	return 0;
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
	if (tvm->user == NULL) {
		error_out_no_errno (tvm, "unable to allocate user execution context");
		return -1;
	}

	return 0;
}


int tvm_load_bytecode(tvm_instance_t *tvm, uint8_t *tbc, size_t tbc_len)
{
	bytecode_t *us_bc;

	remove_firmware_and_ectx(tvm);

	if ((us_bc = tvm_alloc_bytecode(tbc, tbc_len)) == NULL) {
		return -1;
	}

	if (install_user_ctx (tvm, us_bc) < 0) {
		tvm_free_bytecode(us_bc);
		return -2;
	} else {
		tvm->us_bc = us_bc;
	}

	if (install_firmware_ctx (tvm) < 0) {
		remove_firmware_and_ectx(tvm);
		return -3;
	}

	tvm->stop = 0;

	return 0;
}

static inline int run_firmware (ECTX firmware)
{
	int ret = tvm_run (firmware);

	if (ret == ECTX_SLEEP) {
		return ret; /* OK - timer sleep */
	} else if (ret == ECTX_EMPTY) {
		/* FIXME: check deadlock */
		return ret;
	}

	/* Being here means something unexpected happened... */
	fprintf (stderr, "Firmware failed; state = %c\n", firmware->state);
	
	return ECTX_ERROR;
}

static inline int run_user (ECTX user)
{
	int ret = tvm_run_count (user, 10000);

	switch (ret) {
		case ECTX_PREEMPT:
		case ECTX_SHUTDOWN:
		case ECTX_SLEEP:
		case ECTX_TIME_SLICE:
			return ret; /* OK */
		case ECTX_EMPTY:
			if (tvm_ectx_waiting_on (user, user->priv.memory, user->priv.memory_length)) {
				return ret; /* OK - waiting for firmware */
			}
			break;
		default:
			break;
	}

	return ECTX_ERROR;
}

int tvm_run_instance(tvm_instance_t *tvm)
{
	ECTX firmware = tvm->firmware;
	ECTX user = tvm->user;
	int f_ret, u_ret;
	
	while (!tvm->stop) {
		f_ret = run_firmware (firmware);
		u_ret = run_user (user);

		if ((f_ret == ECTX_EMPTY || f_ret == ECTX_SLEEP) &&
			(u_ret == ECTX_EMPTY || u_ret == ECTX_SLEEP)) {
			if (firmware->fptr == NOT_PROCESS_P && user->fptr == NOT_PROCESS_P) {
				tvm_sleep (tvm);
			}
		} else if (f_ret == ECTX_ERROR || u_ret == ECTX_ERROR) {
			break;
		} else if (u_ret == ECTX_SHUTDOWN) {
			/* Run firmware to clear buffers */
			run_firmware (firmware);
			break;
		}
	}
	
	if ((!tvm->stop) && (u_ret == ECTX_ERROR)) {
		tbc_t *tbc = user->priv.bytecode->tbc;

		if (tbc->debug) {
			tbc_dbg_t	*dbg = tbc->debug;
			tbc_lnd_t	*ln;
			tenc_str_t 	*file;
			int offset = user->iptr - tbc->bytecode;
			int i = 0;

			while (i < dbg->n_lnd) {
				if (dbg->lnd[i].offset > offset) {
					break;
				}
				i++;
			}
			ln = &(dbg->lnd[i - 1]);

			file = dbg->files;
			for (i = 0; i < ln->file; ++i) {
				file = file->next;
			}

			error_out_no_errno(
				tvm, 
				"Program failed at %s:%d, state = %c, eflags = %08x",
				file->str, ln->line,
				user->state, user->eflags
			);
		} else {
			error_out_no_errno(
				tvm, 
				"Program failed, state = %c, eflags = %08x",
				user->state, user->eflags
			);
		}

		return 1;
	} else {
		return 0;
	}
}
