/*
 * main.c - POSIX libtvm Wrapper
 *
 * Copyright (C) 2004-2008  Christian L. Jacobsen, Matthew C. Jadud
 *
 */

#include "tvm_posix.h"
#include <tvm_tbc.h>

#include <pthread.h>


/*{{{  Global State */
char			*prog_name 	= NULL;
int			tvm_argc	= 0;
char			**tvm_argv	= NULL;
int                     numCore         = 4;
/*}}}*/

/*{{{  Static State  */
static ECTX         ea[numCore+1]; /* firmware plus number of cores */

static bytecode_t	*fw_bc, *us_bc;
/*}}}*/

/*{{{  Firmware */
#if defined(TVM_DYNAMIC_OCCAM_PI)
#define MT_DEFINES      1
#define MT_TVM          1
#include <mobile_types.h>
#define TLP_MT_CB_TYPE	(MT_SIMPLE |\
			MT_MAKE_TYPE (MT_CB) |\
			MT_CB_SHARED |\
			(1 << MT_CB_CHANNELS_SHIFT))
#endif /* TVM_DYNAMIC_OCCAM_PI */ 
static WORD		kyb_channel = NOT_PROCESS_P;
static WORD		scr_channel = NOT_PROCESS_P;
static WORD 		err_channel = NOT_PROCESS_P;
static WORD 		tlp_argv[3];
/*}}}*/

/*{{{  tvm_sleep */
static void tvm_sleep (int t)
{
	WORD now	= ea[0]->get_time (ea[0]);
	WORD timeout	= 0;
	int set		= 0;

	if (ea[0]->tptr != NOT_PROCESS_P) {
		timeout = ea[0]->tnext;
		set++;
	}

	if (ea[t]->tptr != NOT_PROCESS_P) {
		if (!set || TIME_BEFORE(ea[t]->tnext, timeout)) {
			timeout = ea[t]->tnext;
			set++;
		}
	}

	if (set && TIME_AFTER(timeout, now)) {
		unsigned int period = timeout - now;
		
		if (period > 0) {
			#if defined(HAVE_NANOSLEEP)
			struct timespec to;

			to.tv_sec = (period / 1000000);
			to.tv_nsec = ((period % 1000000) * 1000);
			
			nanosleep (&to, 0);
			#elif defined(HAVE_SLEEP)
			Sleep (period / 1000);
			#else
			#warning "Don't know how to sleep..."
			#endif
		}
	}
}
/*}}}*/


static void ectx_error_found(int t){
    tbc_t *tbc = ea[t]->priv.bytecode->tbc;

    if (tbc->debug) {
        tbc_dbg_t	*dbg = tbc->debug;
        tbc_lnd_t	*ln;
        tenc_str_t 	*file;
        int offset = ea[t]->iptr - tbc->bytecode;
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

        fprintf (stderr,
                "Error at %s:%d\n",
                file->str, ln->line
        );
    }

    /* FIXME: more debugging */
    fprintf (stderr, 
        "Program failed, state = %c, eflags = %08x\n",
        ea[t]->state, ea[t]->eflags
    );

}

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

static void usage (FILE *out)
{
	fprintf (out, "Usage: %s <filename>\n", prog_name);
}

static int file_exists (const char *fn)
{
	/* fopen is more portable than say stat(2) */
	FILE *fh = fopen (fn, "r");

	if (fh != NULL) {
		fclose (fh);
		return 1;
	}

	return 0;
}

static int install_firmware_ctx (void)
{
	static const char *firmware_path 	= TVM_FIRMWARE_PATH "tvm-posix.tbc";
	static const char *posix_tbc		= "tvm-posix.tbc";
	char *firmware_file			= getenv ("TVM_FIRMWARE_FILE");

	if (firmware_file == NULL) {
		firmware_file = (char *) firmware_path;

		if (file_exists (firmware_file)) {
			/* do nothing */
		} else if (file_exists (posix_tbc)) {
			firmware_file = (char *) posix_tbc;
		}
	}

	if ((fw_bc = load_bytecode (firmware_file)) == NULL) {
		fprintf (stderr, 
			"Failed to load/decode %s\n",
			firmware_file
		);
		return -1;
	}

	if ((ea[0] = allocate_ectx (fw_bc, "!??", tlp_argv)) == NULL) {
		return -1;
	}

	return 0;
}

static EXT_CHAN_ENTRY ext_chans[] = {
#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
	{
		.typehash	= 0,
		.in		= NULL,
		.out		= NULL,
		.mt_in		= vc0_mt_in,
		.mt_out		= NULL
	}
#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */
};
static const int ext_chans_length = sizeof (ext_chans) / sizeof (EXT_CHAN_ENTRY);

static int install_user_ctx (const char *fn, int t)
{
	const char *const tlp_fmt = "?!!";
	int kyb_p = -1, scr_p = -1, err_p = -1, valid_tlp = 1;
	tbc_t *tbc;
	WORD *argv;
	char *tlp;
	int i, tlp_len;

	if ((us_bc = load_bytecode (fn)) == NULL) {
		fprintf (stderr, 
			"Failed to load/decode %s\n",
			fn
		);
		return -1;
	}

	tbc = us_bc->tbc;

	if (tbc->tlp != NULL) {
		tlp = tbc->tlp->fmt;
	} else {
		tlp = (char *) tlp_fmt;
	}

	tlp_argv[0] = (WORD) &kyb_channel;
	tlp_argv[1] = (WORD) &scr_channel;
	tlp_argv[2] = (WORD) &err_channel;

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
		error_out_no_errno ("unsupported top-level-process format: \"%s\"", tlp);
		return -1;
	}

	#if defined(TVM_DYNAMIC_OCCAM_PI)
	if (kyb_p & 0x100)
		tlp_argv[0] = (WORD) tvm_mt_alloc (NULL, TLP_MT_CB_TYPE, 1);
	if (scr_p & 0x100)
		tlp_argv[1] = (WORD) tvm_mt_alloc (NULL, TLP_MT_CB_TYPE, 1);
	if (err_p & 0x100)
		tlp_argv[2] = (WORD) tvm_mt_alloc (NULL, TLP_MT_CB_TYPE, 1);
	kyb_p &= 0xff;
	scr_p &= 0xff;
	err_p &= 0xff;
	#endif /* TVM_DYNAMIC_OCCAM_PI */

	argv = (WORD *) malloc (sizeof (WORD) * tlp_len);

	for (i = 0; i < tlp_len; ++i) {
		if (i == kyb_p) {
			argv[i] = tlp_argv[0];
		} else if (i == scr_p) {
			argv[i] = tlp_argv[1];
		} else if (i == err_p) {
			argv[i] = tlp_argv[2];
		} else {
			argv[i] = (WORD) MIN_INT;
		}
	}

	ea[t] = allocate_ectx (us_bc, tlp, argv);
	
	if (argv != NULL)
		free (argv);
	if (ea[t] == NULL)
		return -1;

	ea[t]->ext_chan_table		= ext_chans;
        ea[t]->ext_chan_table_length	= ext_chans_length;

	return 0;
}

static int run_firmware (void)
{
	int ret = tvm_run (ea[0]);

	if (ret == ECTX_SLEEP) {
		return ret; /* OK - timer sleep */
	} else if (ret == ECTX_EMPTY) {
		/* FIXME: check deadlock */
		return ret;
	}

	/* Being here means something unexpected happened... */
	fprintf (stderr, "Firmware failed; state = %c\n", ea[0]->state);
	
	return ECTX_ERROR;
}

static int run_user (void)
{
	int ret = tvm_run_count (ea[1], 10000);

	switch (ret) {
		case ECTX_PREEMPT:
		case ECTX_SHUTDOWN:
		case ECTX_SLEEP:
		case ECTX_TIME_SLICE:
			return ret; /* OK */
		case ECTX_EMPTY:
			if (tvm_ectx_waiting_on (ea[1], ea[1]->priv.memory, ea[1]->priv.memory_length)) {
				return ret; /* OK - waiting for firmware */
			}
			break;
		default:
			break;
	}

	return ECTX_ERROR;
}
static void *run_multicore(void *tv)
{
    int t = ((int) tv)+1;
    int ret = tvm_run(ea[t]);

    switch (ret) {
        case ECTX_PREEMPT:
        case ECTX_SLEEP:
        case ECTX_EMPTY:
            if (!tvm_ectx_waiting_on(ea[t], ea[t]->priv.memory, 
                                    ea[t]->priv.memory_length)) {
                 /* not waiting, there is an error*/
                 ectx_error_found(t);
                 break;
            }
        case ECTX_TIME_SLICE:
            tvm_sleep(t);
            break; /* OK */
        case ECTX_SHUTDOWN:
            /* not really sure how to handle this yet */ 
            break;
        case ECTX_ERROR:
            ectx_error_found(t);
            break;
        default:
            /* also there is an error if this is hit  */ 
            ectx_error_found(t);
            break;
    }
}

#ifdef TVM_PROFILING
static void output_profile (ECTX ectx)
{
	const int n_pri = sizeof(ectx->profile.pri) / sizeof(UWORD);
	const int n_sec = sizeof(ectx->profile.sec) / sizeof(UWORD);
	UWORD total = 0;
	int i;

	for (i = 0; i < n_pri; ++i) {
		total += ectx->profile.pri[i];
	}
	for (i = 0; i < n_sec; ++i) {
		total += ectx->profile.sec[i];
	}

	for (i = 0; i < n_pri; ++i) {
		if (ectx->profile.pri[i] > 0) {
			fprintf (stderr,
				"pri  0x%03x  %-12s = %-7d (%.2f%%)\n", 
				i, tvm_instr_pri_name (i), ectx->profile.pri[i],
				((double) ectx->profile.pri[i] / (double) total) * 100.0
			);
		}
	}
	for (i = 0; i < n_sec; ++i) {
		if (ectx->profile.sec[i] > 0) {
			fprintf (stderr, 
				"sec  0x%03x  %-12s = %-7d (%.2f%%)\n", 
				i, tvm_instr_sec_name (i), ectx->profile.sec[i],
				((double) ectx->profile.sec[i] / (double) total) * 100.0
			);
		}
	}
}

static void output_profiling (void)
{
	fprintf (stderr, "-- Firmware Bytecode Profile\n");
	output_profile (&firmware_ctx);
	fprintf (stderr, "-- User Bytecode Profile\n");
	output_profile (&user_ctx);
}
#endif /* TVM_PROFILING */

int main (int argc, char *argv[])
{
    char *fn;
    int f_ret, u_ret;
    int t;
    int rc;
    int first_run = 1;
    
    pthread_t threads[numCore];

    prog_name	= argv[0]; 
    tvm_argc	= argc;
    tvm_argv	= argv;

    if (argc < 2) {
            usage (stderr);
            return 1;
    } else {
            fn = argv[1];
    }

    init_vm ();
    
    if(1==1){ /*multiple cores */
        for(t=0; t<numCore;t++){
            if (install_user_ctx (fn, t+1) < 0) {
                error_out_no_errno ("failed to load user bytecode");
                return 1;
            }
        }
    }else{
        if (install_user_ctx (fn, 1) < 0) {
                error_out_no_errno ("failed to load user bytecode");
                return 1;
        }
    }
    if (install_firmware_ctx () < 0) {
            error_out_no_errno ("failed to install firmware");
            return 1;
    }

    kyb_channel = NOT_PROCESS_P;
    scr_channel = NOT_PROCESS_P;
    err_channel = NOT_PROCESS_P;

    for (;;) {
        f_ret = run_firmware ();
        u_ret = ECTX_SLEEP;


        if(1==1) {   /* multiple core */
          if (first_run) {
          first_run = 0;
            for(t=0; t<numCore;t++){
                rc = pthread_create(&threads[t], NULL, run_multicore, (void *)t);
                if (rc){
                    printf("ERROR; return code from pthread_create() is %d\n", rc);
                    // exit(-1);
                    u_ret= ECTX_ERROR;
                }
            }
            
            }
        } else {  /* single core machine */
            u_ret = run_user();
            if ((f_ret == ECTX_EMPTY || f_ret == ECTX_SLEEP) &&
                (u_ret == ECTX_EMPTY || u_ret == ECTX_SLEEP)) {
                if(ea[0]->fptr == NOT_PROCESS_P && ea[1]->fptr == NOT_PROCESS_P){
                    tvm_sleep (1);
                }
            } else if (f_ret == ECTX_ERROR || u_ret == ECTX_ERROR) {
                break;
            } else if (u_ret == ECTX_SHUTDOWN) {
                /* Run firmware to clear buffers */
                run_firmware ();
                break;
            }
        }
    }
    
    if (u_ret == ECTX_ERROR) {
        ectx_error_found(1);
        return 1;
    }
    if(1==1) {  /* multiple cores */
        for(t=0;t<numCore;t++){
            free_ectx(ea[t+1]);
        }
    }else {
        free_ectx (ea[1]);
    }
    free_ectx (ea[0]);
    free_bytecode (fw_bc);
    free_bytecode (us_bc);

    #ifdef TVM_PROFILING
    output_profiling ();
    #endif

    return 0;
}

