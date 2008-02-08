/*{{{  LO/HI assembly macros that might not be defined */
#ifndef LO
#define LO(x) ((x) & 0xffff)
#endif
#ifndef HI
#define HI(x) (((x) >> 16) & 0xffff)
#endif
/*}}}*/

/*{{{  Assembly macros for saving and restoring blackfin processor state */
.macro save_context
	[--sp] = SYSCFG;
	[--sp] = (r7:0, p5:0);
	[--sp] = fp;

	[--sp] = i0;
	[--sp] = i1;
	[--sp] = i2;
	[--sp] = i3;

	[--sp] = m0;
	[--sp] = m1;
	[--sp] = m2;
	[--sp] = m3;

	[--sp] = l0;
	[--sp] = l1;
	[--sp] = l2;
	[--sp] = l3;

	[--sp] = b0;
	[--sp] = b1;
	[--sp] = b2;
	[--sp] = b3;
	[--sp] = a0.x;
	[--sp] = a0.w;
	[--sp] = a1.x;
	[--sp] = a1.w;

	[--sp] = LC0;
	[--sp] = LC1;
	[--sp] = LT0;
	[--sp] = LT1;
	[--sp] = LB0;
	[--sp] = LB1;

	[--sp] = ASTAT;

	[--sp] = RETS;
	r0 = RETI;
	[--sp] = r0;
	[--sp] = RETX;
	[--sp] = RETN;
	[--sp] = RETE;
	[--sp] = SEQSTAT;
	
	/* clear L registers.  */
	r0 = 0 (x);
	l0 = r0;
	l1 = r0;
	l2 = r0;
	l3 = r0;
.endm

.macro restore_context
	SEQSTAT = [sp++];
	RETE = [sp++];
	RETN = [sp++];
	RETX = [sp++];
	r0 = [sp++];
	RETI = r0;
	RETS = [sp++];

	ASTAT = [sp++];

	LB1 = [sp++];
	LB0 = [sp++];
	LT1 = [sp++];
	LT0 = [sp++];
	LC1 = [sp++];
	LC0 = [sp++];

	a1.w = [sp++];
	a1.x = [sp++];
	a0.w = [sp++];
	a0.x = [sp++];
	b3 = [sp++];
	b2 = [sp++];
	b1 = [sp++];
	b0 = [sp++];

	l3 = [sp++];
	l2 = [sp++];
	l1 = [sp++];
	l0 = [sp++];

	m3 = [sp++];
	m2 = [sp++];
	m1 = [sp++];
	m0 = [sp++];

	i3 = [sp++];
	i2 = [sp++];
	i1 = [sp++];
	i0 = [sp++];

	fp = [sp++];

	(r7:0, p5:0) = [sp++];
	SYSCFG = [sp++];
.endm
/*}}}*/
