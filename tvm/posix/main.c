/*
 * main.c - POSIX libtvm Wrapper
 *
 * Copyright (C) 2004-2008  Christian L. Jacobsen, Matthew C. Jadud
 *
 */

#include "tvm_posix.h"

/*{{{  Global State */
char			*prog_name 	= NULL;
int			tvm_argc	= 0;
char			**tvm_argv	= NULL;
/*}}}*/

/*{{{  Static State  */
static tvm_t		tvm;
static tvm_ectx_t	firmware_ctx, user_ctx;
static volatile WORD	alarm_set	= 0;
static volatile WORD	alarm_time	= 0;
/*}}}*/

/*{{{  Firmware */
static WORDPTR		firmware_memory;
static WORD		firmware_memory_len;
static WORD		kyb_channel, scr_channel, err_channel;
static WORD 		tlp_argv[] = { (WORD) &kyb_channel, (WORD) &scr_channel, (WORD) &err_channel };

#define ws_size		firmware_ws_size
#define vs_size		firmware_vs_size
#define ms_size		firmware_ms_size
#define inst_size	firmware_bytecode_len
#define transputercode	firmware_bytecode
#include "firmware.h"
#undef ws_size
#undef vs_size
#undef ms_size
#undef inst_size
#undef transputercode
/*}}}*/

/*{{{  User */
static WORDPTR		user_memory;
static WORD		user_memory_len;
static WORD		ws_size, vs_size, ms_size;
static BYTEPTR		bytecode;
static WORD		bytecode_len;
/*}}}*/

/*{{{  tvm_get_time */
#if defined(HAVE_GETTIMEOFDAY)
static WORD tvm_get_time (ECTX ectx)
{
	struct timeval t;

	gettimeofday (&t, 0);

	return (WORD) ((t.tv_sec * 1000000) + t.tv_usec);
}
#elif defined(WIN32)
static WORD tvm_get_time (ECTX ectx)
{
	/*
	http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/filetime_str.asp
	"The FILETIME structure is a 64-bit value representing the number of 
	 100-nanosecond intervals since January 1, 1601 (UTC)."
	*/
	ULARGE_INTEGER usecs;
	FILETIME time;
	
	/* Get the system time */
	GetSystemTimeAsFileTime (&time);
	
	/* Put it into a structure we can work with, and turn it into usecs
	 * See: 
	 *  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winprog/winprog/ularge_integer_str.asp
	 */
	memcpy (&usecs, &time, sizeof(FILETIME));
	usecs.QuadPart = usecs.QuadPart / 10;

	/* Return the clock, just the lower parts thankyou */
	return usecs.LowPart;
}
#elif defined(HAVE_FTIME)
static WORD tvm_get_time (ECTX ectx)
{
	struct timeb milli;

	ftime (&milli);
	
	return (WORD) (milli.millitm + (milli.time * 1000));
}
#endif
/*}}}*/

/*{{{  tvm_set_alarm */
#if defined(HAVE_SETITIMER)
#define HAVE_SET_ALARM
static void tvm_set_alarm (ECTX ectx)
{
	struct itimerval timeout;
	unsigned int t;
	WORD now;

	if (alarm_set && TIME_AFTER(ectx->tnext, alarm_time)) {
		return;
	}

	now = ectx->get_time (ectx);
	t = ectx->tnext - now;

	if (TIME_AFTER(now, ectx->tnext) || (t == 0)) {
		ectx->modify_sync_flags (ectx, SFLAG_TQ, 0);
		alarm_set = 0;
	}

	timeout.it_interval.tv_sec	= 0;
	timeout.it_interval.tv_usec	= 0;
	timeout.it_value.tv_sec		= t / 1000000;
	timeout.it_value.tv_usec	= t % 1000000;

	alarm_set = 1;
	alarm_time = ectx->tnext;

	if (setitimer (ITIMER_REAL, &timeout, NULL) != 0) {
		/* Behave like busy-wait if setitimer fails. */
		ectx->modify_sync_flags (ectx, SFLAG_TQ, 0);
		alarm_set = 0;
	}
}
#endif
/*}}}*/

/*{{{  tvm_sleep */
static void tvm_sleep (void)
{
	WORD now	= firmware_ctx.get_time (&firmware_ctx);
	WORD timeout	= 0;
	int set		= 0;

	if (firmware_ctx.tptr != NOT_PROCESS_P) {
		timeout = firmware_ctx.tnext;
		set++;
	}

	if (user_ctx.tptr != NOT_PROCESS_P) {
		if (!set || TIME_BEFORE(user_ctx.tnext, timeout)) {
			timeout = user_ctx.tnext;
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

/*{{{  Old stuff for recycling */
/* Not sure where to define these, they could be platform specific */
#define EXIT_SIGSEGV 139
#define EXIT_SIGBUS 138

#if 0
static void sigsegv_handler(int num)
{
	/* Set the signal back to the default so we dont get into
	 * some kind of ugly loop should we segfault in here */
	signal(SIGSEGV, SIG_DFL);

	print_crash(EXIT_SIGSEGV);
	printf("Segmentation fault (%d)\n", num);
	printf("\n");
	print_state();
	printf("exiting... (segmentation fault)\n");

	if(code_start <= iptr && iptr < code_end)
	{
		print_debug_info(filename);
	}

	/* 139 */
	exit(EXIT_SIGSEGV); /* This seems to be what a segfaulted program returns */
}

#if !defined(WIN32) && !defined(CYGWIN)
/* Borland does not have sigbus */
static void sigbus_handler(int num)
{
	/* Set the signal back to the default so we dont get into
	 * some kind of ugly loop should we segfault in here */
	signal(SIGBUS, SIG_DFL);

	print_crash(EXIT_SIGBUS);
	printf("Bus error (%d)\n", num);
	printf("\n");
	print_state();
	printf("exiting... (bus error)\n");

	if(code_start <= iptr && iptr < code_end)
	{
		print_debug_info(filename);
	}

	/* 138 */
	exit(EXIT_SIGBUS); /* This seems to be what a buserrored program returns */
}
#endif
#endif

/*{{{  sigalarm_handler */
static void sigalrm_handler (int num)
{
	firmware_ctx.sflags	|= SFLAG_TQ;
	user_ctx.sflags		|= SFLAG_TQ;
	alarm_set		= 0;
}
/*}}}*/

static void v_error_out (const char *fmt, va_list ap)
{
	fprintf (stderr, "%s: ", prog_name);
	vfprintf (stderr, fmt, ap);
	if (errno != 0) {
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

static void add_system_functions (ECTX ectx)
{
	ectx->get_time = tvm_get_time;
	#ifdef HAVE_SET_ALARM
	ectx->set_alarm = tvm_set_alarm;
	#endif
}

static int install_firmware_ctx (void)
{
	WORDPTR ws, vs, ms;
	ECTX firmware = &firmware_ctx;

	tvm_ectx_init (&tvm, firmware);
	add_system_functions (firmware);
	install_sffi (firmware);

	/* Do initial layout with NULL to get memory size. */
	tvm_ectx_layout (
		firmware, NULL,
		"!??", 3,
		firmware_ws_size, 
		firmware_vs_size, 
		firmware_ms_size,
		&firmware_memory_len, &ws, &vs, &ms
	);

	firmware_memory = malloc (sizeof(WORD) * firmware_memory_len);
	if (firmware_memory == NULL) {
		return error_out ("unable to allocate firmware memory (%d words)", firmware_memory_len);
	}

	/* This is the real layout. */
	tvm_ectx_layout (
		firmware, firmware_memory,
		"!??", 3,
		firmware_ws_size, 
		firmware_vs_size, 
		firmware_ms_size,
		&firmware_memory_len, &ws, &vs, &ms
	);
	
	return tvm_ectx_install_tlp (
		firmware, (BYTEPTR) firmware_bytecode, ws, vs, ms,
		"!??", 3, tlp_argv
	);
}

static int read_big_endian_int (FILE *fh, int *value)
{
	int ret;

	if ((ret = fread (value, sizeof(int), 1, fh)) != 1) {
		return error_out ("read error (%d)", ret);
	}
	
	#if !defined(TVM_BIG_ENDIAN)
	*value = SwapFourBytes (*value);
	#endif

	return 0;
}

static int read_bytecode (const char *fn)
{
	FILE *fh = fopen (fn, "rb");
	char header[4];
	int len, ret;
	int bc_start, bc_length;

	if (fh == NULL) {
		return error_out ("unable to open \"%s\"", fn);
	}

	if (fseek (fh, 0, SEEK_END) < 0) {
		return error_out ("unable to seek to file end", fn);
	}

	len = (int) ftell (fh);

	if (fseek (fh, 0, SEEK_SET) < 0) {
		return error_out ("unable to seek to file start", fn);
	}

	/* 'TBZ\0' + 2x4 + (4, 4, 4) + (4, 4, 4) + (4, 4, 4) + (4, 4, 4, 4) = 64 */
	if (len <= 64) {
		return error_out_no_errno ("file size %d is too small", len);
	}

	if ((ret = fread (header, sizeof(header), 1, fh)) != 1) {
		return error_out ("read incorrect (%d)", ret);
	}

	if (memcmp (header, "tbz\0", 4)) {
		return error_out_no_errno ("file header does not match");
	}

	if (fseek (fh, 3 * 4, SEEK_SET) < 0) {
		return error_out ("unable to seek to bytecode descriptor", fn);
	}
	
	if (read_big_endian_int (fh, &bc_start)) {
		return -1;
	}
	if (read_big_endian_int (fh, &bc_length)) {
		return -1;
	}

	if (fseek (fh, bc_start, SEEK_SET) < 0) {
		return error_out ("unable to seek to bytecode", fn);
	}

	if ((ret = fread (header, sizeof(header), 1, fh)) != 1) {
		return error_out ("read incorrect (%d)", ret);
	}

	if (memcmp (header, "tvm\2", 4)) {
		fprintf (stderr, "%02x%02x%02x%02x\n", header[0], header[1], header[2], header[3]);
		return error_out_no_errno ("bytecode header does not match");
	}

	if (read_big_endian_int (fh, &ws_size)) {
		return -1;
	}
	if (read_big_endian_int (fh, &vs_size)) {
		return -1;
	}
	if (read_big_endian_int (fh, &ms_size)) {
		return -1;
	}

	bytecode_len	= bc_length - (4 * sizeof(int));
	bytecode 	= (BYTEPTR) malloc (bytecode_len);

	if (bytecode == NULL) {
		return error_out ("unable to allocate bytecode memory (%d bytes)", bytecode_len);
	}

	if ((ret = fread (bytecode, 1, bytecode_len, fh)) != bytecode_len) {
		return error_out ("failed reading bytecode (%d of %d bytes)", ret, bytecode_len);
	}

	fclose (fh);

	return 0;
}

static int install_user_ctx (const char *fn)
{
	#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	const char *tlp_fmt = "?!!F";
	const int tlp_argc = 4;
	#else
	const char *tlp_fmt = "?!!";
	const int tlp_argc = 3;
	#endif /* !TVM_DYNAMIC_MEMORY || !TVM_OCCAM_PI */

	WORDPTR ws, vs, ms;
	ECTX user = &user_ctx;

	if (read_bytecode (fn)) {
		error_out_no_errno ("unable to read bytecode");
		return -1;
	}

	tvm_ectx_init (&tvm, user);
	user->pri = 1; /* lower priority */
	add_system_functions (user);
	install_sffi (user);

	/* Do initial layout with NULL to get memory size. */
	tvm_ectx_layout (
		user, NULL,
		tlp_fmt, tlp_argc, 
		ws_size, 
		vs_size, 
		ms_size,
		&user_memory_len, &ws, &vs, &ms
	);

	user_memory = (WORDPTR) malloc (sizeof (WORD) * user_memory_len);
	if (user_memory == NULL) {
		return error_out ("failed to allocate user memory (%d words)", user_memory_len);
	}

	/* This is the real layout. */
	tvm_ectx_layout (
		user, user_memory,
		tlp_fmt, tlp_argc, 
		ws_size, 
		vs_size, 
		ms_size,
		&user_memory_len, &ws, &vs, &ms
	);

	return tvm_ectx_install_tlp (
		user, bytecode, ws, vs, ms,
		tlp_fmt, tlp_argc, tlp_argv
	);
}

static int run_firmware (void)
{
	int ret = tvm_run (&firmware_ctx);

	if (ret == ECTX_SLEEP) {
		return ret; /* OK - timer sleep */
	} else if (ret == ECTX_EMPTY) {
		/* FIXME: check deadlock */
		return ret;
	}

	/* Being here means something unexpected happened... */
	fprintf (stderr, "Firmware failed; state = %c\n", firmware_ctx.state);
	
	return ECTX_ERROR;
}

static int run_user (void)
{
	int ret = tvm_run_count (&user_ctx, 10000);

	switch (ret) {
		case ECTX_PREEMPT:
		case ECTX_SHUTDOWN:
		case ECTX_SLEEP:
		case ECTX_TIME_SLICE:
			return ret; /* OK */
		case ECTX_EMPTY:
			if (tvm_ectx_waiting_on (&user_ctx, user_memory, user_memory_len)) {
				return ret; /* OK - waiting for firmware */
			}
			break;
		default:
			break;
	}

	return ECTX_ERROR;
}

int main (int argc, char *argv[])
{
	char *fn;
	int f_ret, u_ret;

	prog_name	= argv[0]; 
	tvm_argc	= argc;
	tvm_argv	= argv;

	if (argc < 2) {
		usage (stderr);
		return 1;
	} else {
		fn = argv[1];
	}

	tvm_init (&tvm);

	if (install_firmware_ctx () < 0) {
		error_out_no_errno ("failed to install firmware");
		return 1;
	}
	if (install_user_ctx (fn) < 0) {
		error_out_no_errno ("failed to load user bytecode");
		return 1;
	}

	kyb_channel = NOT_PROCESS_P;
	scr_channel = NOT_PROCESS_P;
	err_channel = NOT_PROCESS_P;

	#if defined(SIGALRM) && defined(HAVE_SET_ALARM)
	signal (SIGALRM, sigalrm_handler);
	#endif

	for (;;) {
		f_ret = run_firmware ();
		u_ret = run_user ();

		if ((f_ret == ECTX_EMPTY || f_ret == ECTX_SLEEP) &&
			(u_ret == ECTX_EMPTY || u_ret == ECTX_SLEEP)) {
			if (firmware_ctx.fptr == NOT_PROCESS_P && user_ctx.fptr == NOT_PROCESS_P) {
				tvm_sleep ();
			}
		} else if (f_ret == ECTX_ERROR || u_ret == ECTX_ERROR) {
			break;
		} else if (u_ret == ECTX_SHUTDOWN) {
			/* Run firmware to clear buffers */
			run_firmware ();
			break;
		}
	}
	
	if (u_ret == ECTX_ERROR) {
		/* FIXME: more debugging */
		fprintf (stderr, 
			"Program failed, state = %c, eflags = %08x\n",
			user_ctx.state, user_ctx.eflags
		);

		return 1;
	}
	
	free (firmware_memory);
	free (user_memory);
	free (bytecode);

	return 0;
}

