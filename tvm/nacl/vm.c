#include "tvm_nacl.h"

/*{{{  Static State  */
static volatile WORD	alarm_set	= 0;
static volatile WORD	alarm_time	= 0;
/*}}}*/

/*{{{  tvm_get_time */
static WORD tvm_get_time (ECTX ectx)
{
	struct timeval t;

	gettimeofday (&t, 0);

	return (WORD) ((t.tv_sec * 1000000) + t.tv_usec);
}
/*}}}*/

#if 0
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

/*{{{  sigalarm_handler */
static void sigalrm_handler (int num)
{
	ECTX ectx = tvm.head;

	while (ectx != NULL) {
		ectx->sflags |= SFLAG_TQ;
		if (ectx == tvm.tail)
			break;
		ectx = ectx->next;
	}

	alarm_set		= 0;
}
/*}}}*/
#endif

static void add_system_functions (ECTX ectx)
{
	ectx->get_time = tvm_get_time;
	#ifdef HAVE_SET_ALARM
	ectx->set_alarm = tvm_set_alarm;
	#endif
}

#if 0
void init_vm (void)
{
	tvm_init (&tvm);
	
	#if defined(SIGALRM) && defined(HAVE_SET_ALARM)
	{
		struct sigaction sa;

		sa.sa_handler	= sigalrm_handler;
		sa.sa_flags	= 0;
		sigemptyset (&sa.sa_mask);

		sigaction (SIGALRM, &sa, NULL);
	}
	#endif
}
#endif

ECTX tvm_allocate_ectx (tvm_instance_t *tvm, bytecode_t *bc, const char *tlp, WORD *argv)
{
	WORDPTR mem, vs, ws;
	WORD mem_len;
	ECTX vm;

	vm = (ECTX) malloc (sizeof (tvm_ectx_t));
	if (vm == NULL)
		return NULL;

	tvm_ectx_init (&(tvm->tvm), vm);
	add_system_functions (vm);
	tvm_install_sffi (vm);

	vm->ffi_table 		= bc->ffi_table;
	vm->ffi_table_length 	= bc->ffi_table_length;

	mem_len = tvm_ectx_memory_size (
		vm,
		tlp, strlen (tlp), 
		bc->tbc->ws, bc->tbc->vs
	);

	mem = (WORDPTR) malloc (sizeof (WORD) * mem_len);
	if (mem == NULL) {
		free (vm);
		return NULL;
	}

	tvm_ectx_layout (
		vm, mem,
		tlp, strlen (tlp), 
		bc->tbc->ws, bc->tbc->vs, 
		&ws, &vs
	);

	/* Setup the type shadow before installing the TLP */
	#ifdef TVM_TYPE_SHADOW
	vm->shadow_start	= (unsigned int) mem;
	vm->shadow_end		= vm->shadow_start + (mem_len << WSH);
	vm->type_store		= malloc (mem_len);
	fill_type_shadow (vm, (BYTEPTR) vm->shadow_start, mem_len << WSH, STYPE_UNDEF);
	#endif

	if (tvm_ectx_install_tlp (
		vm,
		bc->tbc->bytecode, ws, vs,
		tlp, strlen (tlp), argv
	)) {
		free (mem);
		free (vm);
		return NULL;
	}

	bc->refcount++;

	vm->priv.bytecode	= bc;
	vm->priv.memory		= mem;
	vm->priv.memory_length	= mem_len;
	vm->priv.instance	= tvm;

	return vm;
}

void tvm_free_ectx (ECTX vm)
{
	/* FIXME: deal with stray mobiles */
	tvm_ectx_release (vm);
	tvm_free_bytecode (vm->priv.bytecode);
	free (vm->priv.memory);
	free (vm);
}

