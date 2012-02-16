(module c-code mzscheme
  (provide (all-defined))
  (define
   code
   (let ((c (make-hash-table)) (h (make-hash-table)))
     (hash-table-put! h 'and "extern void ins_and();")
     (hash-table-put! h 'norm "extern void ins_norm();")
     (hash-table-put! h 'or "extern void ins_or();")
     (hash-table-put! h 'mul "extern void ins_mul();")
     (hash-table-put! h 'talt "extern void ins_talt();")
     (hash-table-put! h 'extin "extern void ins_extin();")
     (hash-table-put! h 'out "extern void ins_out();")
     (hash-table-put! h 'extout "extern void ins_extout();")
     (hash-table-put! h 'getpri "extern void ins_getpri();")
     (hash-table-put! h 'startp "extern void ins_startp();")
     (hash-table-put! h 'seterr "extern void ins_seterr();")
     (hash-table-put! h 'nfix "extern void ins_nfix();")
     (hash-table-put! h 'ldnl "extern void ins_ldnl();")
     (hash-table-put! h 'diff "extern void ins_diff();")
     (hash-table-put! h 'in "extern void ins_in();")
     (hash-table-put! h 'ldnlp "extern void ins_ldnlp();")
     (hash-table-put! h 'call "extern void ins_call();")
     (hash-table-put! h 'csub0 "extern void ins_csub0();")
     (hash-table-put! h 'bsub "extern void ins_bsub();")
     (hash-table-put! h 'ladd "extern void ins_ladd() ;")
     (hash-table-put! h 'ldlp "extern void ins_ldlp();")
     (hash-table-put! h 'prod "extern void ins_prod();")
     (hash-table-put! h 'stnl "extern void ins_stnl();")
     (hash-table-put! h 'lshr "extern void ins_lshr();")
     (hash-table-put! h 'endp "extern void ins_endp();")
     (hash-table-put! h 'ldpi "extern void ins_ldpi();")
     (hash-table-put! h 'xdble "extern void ins_xdble();")
     (hash-table-put! h 'taltwt "extern void ins_taltwt();")
     (hash-table-put! h 'pfix "extern void ins_pfix();")
     (hash-table-put! h 'alt "extern void ins_alt();")
     (hash-table-put! h 'ajw "extern void ins_ajw();")
     (hash-table-put! h 'mint "extern void ins_mint();")
     (hash-table-put! h 'cj "extern void ins_cj();")
     (hash-table-put! h 'sub "extern void ins_sub();")
     (hash-table-put! h 'eqc "extern void ins_eqc();")
     (hash-table-put! h 'j "extern void ins_j();")
     (hash-table-put! h 'ldc "extern void ins_ldc();")
     (hash-table-put! h 'adc "extern void ins_adc();")
     (hash-table-put! h 'ldl "extern void ins_ldl();")
     (hash-table-put! h 'stl "extern void ins_stl();")
     (hash-table-put! h 'opr "extern void ins_opr();")
     (hash-table-put! h 'rev "extern void ins_rev();")
     (hash-table-put! h 'enbt "extern void ins_enbt();")
     (hash-table-put! h 'lb "extern void ins_lb();")
     (hash-table-put! h 'lsum "extern void ins_lsum();")
     (hash-table-put! h 'add "extern void ins_add();")
     (hash-table-put! h 'lsub "extern void ins_lsub();")
     (hash-table-put! h 'gt "extern void ins_gt();")
     (hash-table-put! h 'rem "extern void ins_rem();")
     (hash-table-put! h 'altend "extern void ins_altend();")
     (hash-table-put! h 'ret "extern void ins_ret();")
     (hash-table-put! h 'ldtimer "extern void ins_ldtimer();")
     (hash-table-put! h 'div "extern void ins_div();")
     (hash-table-put! h 'xor "extern void ins_xor();")
     (hash-table-put! h 'disc "extern void ins_disc();")
     (hash-table-put! h 'sb "extern void ins_sb();")
     (hash-table-put! h 'wsub "extern void ins_wsub();")
     (hash-table-put! h 'diss "extern void ins_diss();")
     (hash-table-put! h 'shr "extern void ins_shr();")
     (hash-table-put! h 'ccnt1 "extern void ins_ccnt1();")
     (hash-table-put! h 'dist "extern void ins_dist();")
     (hash-table-put! h 'stoperr "extern void ins_stoperr();")
     (hash-table-put! h 'ldiv "extern void ins_ldiv();")
     (hash-table-put! h 'altwt "extern void ins_altwt();")
     (hash-table-put! h 'outbyte "extern void ins_outbyte();")
     (hash-table-put! h 'cword "extern void ins_cword();")
     (hash-table-put! h 'boolinvert "extern void ins_boolinvert();")
     (hash-table-put! h 'move "extern void ins_move();")
     (hash-table-put! h 'enbc "extern void ins_enbc();")
     (hash-table-put! h 'widenshort "extern void ins_widenshort();")
     (hash-table-put! h 'dup "extern void ins_dup();")
     (hash-table-put! h 'tin "extern void ins_tin();")
     (hash-table-put! h 'enbs "extern void ins_enbs();")
     (hash-table-put! h 'outword "extern void ins_outword();")
     (hash-table-put! h 'not "extern void ins_not();")
     (hash-table-put! h 'csngl "extern void ins_csngl();")
     (hash-table-put! h 'lend "extern void ins_lend();")
     (hash-table-put! h 'fficall "extern void ins_fficall();")
     (hash-table-put! h 'extvrfy "extern void ins_extvrfy();")
     (hash-table-put! h 'lmul "extern void ins_lmul();")
     (hash-table-put! h 'shl "extern void ins_shl();")
     (hash-table-put! h 'lshl "extern void ins_lshl();")
     (hash-table-put!
       c
       'and
       "\n{\n\tSTACK(areg & breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'norm
       "\n{\n\t/* Too complicated (looking) for now */\n\tins_not_implemented();\n}")
     (hash-table-put!
       c
       'or
       "\n{\n\tSTACK(areg | breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'mul
       "\n{\n\t/* FIXME: Int bounds check */\n\tSTACK(areg * breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'talt
       "\n{\n\t/* Set the alt state as enabeling */\n\tWORKSPACE_SET(wptr, WS_ALT_STATE, ENABELING_P);\n\t/* Set up the timer */\n\tWORKSPACE_SET(wptr, WS_ALT_T, TIME_NOT_SET_P);\n}")
     (hash-table-put!
       c
       'extin
       "\n{\n\t/* Due to the fact that the KRoC uses the least significant\n\t * bit set to one to indicate a EXTERNAL channel, which is fine\n\t * if this is an address, but for the interpreter it is NOT an\n\t * address, but an INDEX, so we need to do some shifting to get\n\t * around that...\n\t */\n\t/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values\n\t * is implementation defined */\n\text_chan_table[(UWORD)breg >> 1](areg, (BPOOTER)creg);\n}")
     (hash-table-put!
       c
       'out
       "\n{\n\t/* FIXME: These instructions are interutable in the transputer,\n\t * ie a message of size X (words) would take X copies to complete,\n\t * and as X could be arbitrarily large, this instruciton can suspend\n\t * during the copying.\n\t *\n\t * In soccam Matt claims that I (clj) claim that this is easy to do,\n\t * which I a lie! I think Matt should be given the task of implementing\n\t * this functionality as punishment for trying smear my good reputation.\n\t */\n\n\t/* Makes this code look a bit nicer, and more like soccam */\n\t/* FIXME: But what the heck are those numbers next to these declerations in\n\t * soccam????? */\n\tWORD num_bytes     = areg;\n\tPOOTER chan_ptr    = (POOTER)breg;\n\tBPOOTER read_start = (BPOOTER)creg;\n\n\t/* Deal with special channels: top level SCR! and ERR! */\n\tif(chan_ptr == (POOTER)ext_chan_table[EXT_CHAN_SCR] \n\t\t\t|| chan_ptr == (POOTER)ext_chan_table[EXT_CHAN_ERR])\n\t{\n\t\t/* This is a bit ugly, but it casts chan_ptr into an ext_chan function which\n\t\t * we then call with the arguments to the ins_out */\n\t\t((void (*)(WORD len, BPOOTER ptr))chan_ptr)(num_bytes, read_start);\n\n\t\t/* FIXME: THIS IS A COMMENT FROM SOCCAM:\n      ;; WARNING 20040225\n      ;; It may be, when blasting things to the screen,\n      ;; that we need to reschedule things so other people\n      ;; can get in between our slow printing process.\n      ;; (add-to-queue wptr (get 'iptr))\n      ;; (run-next-on-queue)\n\t\t*/\n\t}\n\t/* If nobody is waiting on the channel... */\n\telse if(read_mem(chan_ptr) == NOT_PROCESS_P)\n\t{\n\t\t/* FIXME: This could be a function shared between in and out */\n\t\t/* ...Put this process('s wptr) into the channel word, */\n\t\twrite_mem(chan_ptr, (WORD)wptr);\n\t\t/* store our state */ \n\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\tWORKSPACE_SET(wptr, WS_CHAN, (WORD)read_start);\n\t\t/* and reschedule */\n\t\trun_next_on_queue();\n\t}\n\telse if((read_mem(chan_ptr) != NOT_PROCESS_P) && \n\t\t\t(WORKSPACE_GET((POOTER)read_mem(chan_ptr), WS_ALT_STATE) == ENABELING_P))\n\t{\n\t\tWORKSPACE_SET((POOTER)read_mem(chan_ptr), WS_ALT_STATE, DISABELING_P);\n\t\tWORKSPACE_SET(chan_ptr, WS_TOP, (WORD)wptr);\n\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\t/* FIXME: This used to be WS_ALT_STATE (which is == WS_CHAN), though\n\t\t * possibly not as semanticalyly clear, a leftover from soccam?\n\t\t */\n\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, (WORD)read_start);\n\t\trun_next_on_queue();\n\t}\n\telse if((read_mem(chan_ptr) != NOT_PROCESS_P) &&\n\t\t\t(WORKSPACE_GET((POOTER)read_mem(chan_ptr), WS_ALT_STATE) == WAITING_P))\n\t{\n\t\tPOOTER old_chan_word = (POOTER)read_mem(chan_ptr);\n\n\t\tWORKSPACE_SET((POOTER)read_mem(chan_ptr), WS_ALT_STATE, DISABELING_P);\n\t\tWORKSPACE_SET(chan_ptr, WS_TOP, (WORD)wptr);\n\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\tWORKSPACE_SET(wptr, WS_CHAN, (WORD)read_start);\n\n\t\t/* The alt process is rescheduled, and this process is descheduled */\n\t\twptr = old_chan_word;\n\t\tiptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);\n\t}\n\telse if((read_mem(chan_ptr) != NOT_PROCESS_P) &&\n\t\t(WORKSPACE_GET((POOTER)read_mem(chan_ptr), WS_ALT_STATE) == DISABELING_P))\n\t{\n\t\tWORKSPACE_SET(chan_ptr, WS_TOP, (WORD)wptr);\n\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\tWORKSPACE_SET(wptr, WS_CHAN, (WORD)read_start);\n\t\trun_next_on_queue();\n\t}\n\telse\n\t{\n\t\t/* FIXME: Optimise this so if we are reading by a multiple of WORD, use\n\t\t * read_mem rather than read_byte */\n\n\t\t/* Where we start reading from */\n\t\tBPOOTER write_start = (BPOOTER)WORKSPACE_GET((POOTER)read_mem(chan_ptr), WS_CHAN);\n\t\tWORD count = 0;\n\n\t\t/* Copy the data */\n\t\twhile(count < num_bytes)\n\t\t{\n\t\t\twrite_byte(bpooter_plus(write_start, count), \n\t\t\t\t\tread_byte(bpooter_plus(read_start, count)));\n\t\t\tcount = count + 1;\n\t\t}\n\n\t\t/* Add ourselves to the back of the runqueue */\n\t\tadd_to_queue((WORD)wptr, (WORD)iptr);\n\t\t/* Reschedule the process at the other end of the channel */\n\t\twptr = (POOTER)read_mem(chan_ptr);\n\t\tiptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);\n\t\t/* Set the channel word to NotProcess.p */\n\t\twrite_mem(chan_ptr, NOT_PROCESS_P);\n\n\t}\n\n\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'extout
       "\n{\n\t/* Due to the fact that the KRoC uses the least significant\n\t * bit set to one to indicate a EXTERNAL channel, which is fine\n\t * if this is an address, but for the interpreter it is NOT an\n\t * address, but an INDEX, so we need to do some shifting to get\n\t * around that...\n\t */\n\t/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values\n\t * is implementation defined */\n\text_chan_table[(UWORD)breg >> 1](areg, (BPOOTER)creg);\n}")
     (hash-table-put!
       c
       'getpri
       "\n{\n\t/* FIXME: This will need to be fixed should we ever support prioritues */\n\tSTACK(0, areg, breg);\n}")
     (hash-table-put!
       c
       'startp
       "\n{\n\tadd_to_queue(areg, ((WORD) iptr) + breg);\n\t/* FIXME: Due to the different semantics of the C add_to_queue, and the\n\t * soccam add to queue, (which we need to bring in sync) I have this line\n\t * which is not currently present in soccam */\n\tSTACK(creg, UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'seterr
       "\n{\n\tset_error_flag();\n\n\t/* FIXME: I am loaving this in until I make the handling of the error flag\n\t * more inteligent */\n\tins_invalid();\n}")
     (hash-table-put!
       c
       'nfix
       "\n{\n\t/* Negate the value in the operand register, and shift it up */\n\t/* FIXME: Wordisze problems here... */\n\toreg = (WORD)((~(UWORD)oreg) << 4);\n}")
     (hash-table-put!
       c
       'ldnl
       "\n{\n\t/* Read from memory(areg+oreg) */\n\t/* areg = *(((WORD *)areg) + oreg); */\n\tareg = read_mem(pooter_plus((POOTER)areg, oreg));\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'diff
       "\n{\n\t/* Unsigned subtract */\n\tSTACK((WORD)((UWORD) breg) - ((UWORD) areg), creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'in
       "\n{\n\t/* FIXME: These instructions are interutable in the transputer,\n\t * ie a message of size X (words) would take X copies to complete,\n\t * and as X could be arbitrarily large, this instruciton can suspend\n\t * during the copying.\n\t *\n\t * In soccam Matt claims that I (clj) claim that this is easy to do,\n\t * which I a lie! I think Matt should be given the task of implementing\n\t * this functionality as punishment for trying smear my good reputation.\n\t */\n\n\t/* Makes this code look a bit nicer, and more like soccam */\n\tWORD num_bytes      = areg;\n\tPOOTER chan_ptr     = (POOTER)breg;\n\tBPOOTER write_start = (BPOOTER)creg;\n\n\tif(chan_ptr == (POOTER)ext_chan_table[EXT_CHAN_KYB])\n\t{\n\t\t((void (*)(WORD len, BPOOTER ptr))chan_ptr)(num_bytes, write_start);\n\t} \n\t/* If nobody is waiting on the channel... */\n\telse if(read_mem(chan_ptr) == NOT_PROCESS_P)\n\t{\n\t\t/* FIXME: This could be a function shared between in and out */\n\t\t/* ...Put this process('s wptr) into the channel word, */\n\t\twrite_mem(chan_ptr, (WORD)wptr);\n\t\t/* store our state */ \n\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\tWORKSPACE_SET(wptr, WS_CHAN, (WORD)write_start);\n\t\t/* and reschedule */\n\t\trun_next_on_queue();\n\t} \n\telse\n\t{\n\t\t/* FIXME: Optimise this so if we are reading by a multiple of WORD, use\n\t\t * read_mem rather than read_byte */\n\n\t\t/* Where we start reading from */\n\t\t/* FIXME: The code in soccam does the following, which is wrong:\n\t\t   BPOOTER read_start = (BPOOTER)read_mem(chan_ptr);\n\t\t   It should be doing: */\n\t\tBPOOTER read_start = (BPOOTER)WORKSPACE_GET((POOTER)read_mem(chan_ptr), WS_CHAN);\n\t\tWORD count = 0;\n\n\t\t/* Copy the data */\n\t\twhile(count < num_bytes)\n\t\t{\n\t\t\twrite_byte(bpooter_plus(write_start, count), \n\t\t\t\t\tread_byte(bpooter_plus(read_start, count)));\n\t\t\tcount = count + 1;\n\t\t}\n\n\t\t/* Add ourselves to the back of the runqueue */\n\t\tadd_to_queue((WORD)wptr, (WORD)iptr);\n\t\t/* Reschedule the process at the other end of the channel */\n\t\twptr = (POOTER)read_mem(chan_ptr);\n\t\tiptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);\n\t\t/* Set the channel word to NotProcess.p */\n\t\twrite_mem(chan_ptr, NOT_PROCESS_P);\n\t}\n\n\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'ldnlp
       "\n{\n\t/* Add the oreg to the areg and store it in the areg */\n\t/* areg = (WORD)(((WORD *)areg) + oreg); */\n\tareg = (WORD)pooter_plus((POOTER)areg, oreg);\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'call
       "\n{\n\t/* Store registers in a new stack frame */\n\t/*\n\t*(wptr - 4 + 0) = (int)iptr;\n\t*(wptr - 4 + 1) = areg;\n\t*(wptr - 4 + 2) = breg;\n\t*(wptr - 4 + 3) = creg;\n\t*/\n\twrite_mem(pooter_minus(wptr, 4 - 0), (WORD)iptr);\n\twrite_mem(pooter_minus(wptr, 4 - 1), areg);\n\twrite_mem(pooter_minus(wptr, 4 - 2), breg);\n\twrite_mem(pooter_minus(wptr, 4 - 3), creg);\n\t/* Actually allocate the stack frame */\n\twptr = pooter_minus(wptr, 4);\n\n\t/* Set the areg to the old iptr */\n\tareg = (WORD)iptr;\n\n\t/* Set the new iptr from the oreg */\n\tiptr = bpooter_plus(iptr, oreg);\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'csub0
       "\n{\n\t/* FIXME: The implementation of this is incorrect in soccam, the\n\t * error flag can be cleared in soccam, this is wrong.\n\t */\n\tif(((UWORD) breg) >= ((UWORD) areg))\n\t{\n\t\tset_error_flag();\n\t}\n\tSTACK(breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'bsub
       "\n{\n\t/* FIXME: Does it make sense to implement this like so: \n\t * ie using the (BYTE *) to ensure that things are incremented by\n\t * the correct amount? I think so.*/\n\t/* STACK((WORD)((BYTE *) areg) + breg, creg, UNDEFINE(creg)); */\n\tSTACK((WORD)bpooter_plus((BPOOTER) areg, breg), creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'ladd
       "\n{\n\t/* FIXME: no check for overflow!! */\t\n\t/* FIXME: UNTESTED */\t\n\tSTACK((breg + areg + (creg & 1)), UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'ldlp
       "\n{\n\t/* Push the stack down */\n\tSTACK(UNDEFINE(areg), areg, breg);\n\n\t/* Add the oreg to the wptr */\n\t/*areg = (WORD)(wptr + oreg);*/\n\tareg = (WORD)pooter_plus(wptr, oreg);\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'prod
       "\n{\n\tSTACK(areg * breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'stnl
       "\n{\n\t/* *(((WORD *)areg) + oreg) = breg; */\n\twrite_mem(pooter_plus((POOTER)areg, oreg), breg);\n\n\tSTACK(creg, UNDEFINE(breg), UNDEFINE(creg));\n\t\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'lshr
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\t/*FIXME: not tested...*/\n\t/* lo does not need to be long, bits rightshifted out of lo are supposed to \n\t* fall out into the void anyway */\n\tunsigned int lo;  \n\tunsigned long long hi;\n\tif(areg <= 2* WORDSIZE_BITS  && areg >= 0 )\n        {\n                hi = (unsigned int) creg;\n                hi = hi << (WORDSIZE_BITS - areg); /*make hi really be the 'hi' and then shift it.*/\n                /* shift lo - done on seperage line coz shifting breg directly buggered up\n\t\t * coz it's unsigned, I think.  Maybe not though.  This seems to work, and\n\t\t * shifting breg directly didn't.  */\n                lo = breg;\n                lo = lo >> areg;  \n                lo += hi;  /*add what 'overflowed' from lo to hi.*/\n                hi = hi >> WORDSIZE_BITS; /*set hi back to what it would have been in a real reg.*/\n                STACK((WORD)lo,(WORD) hi, 0);\n        }\n#endif\n}")
     (hash-table-put!
       c
       'endp
       "\n{\n\t/* Check the process count */\n\t/* if(((WORD *)areg)[1] == 1) */\n\tif(read_mem(pooter_plus((POOTER)areg, 1)) == 1) \n\t{\n\t\t/* No more child processes, continue as the parent process */\n\n\t\t/* Set the process count to zero */\n\t\t/* ((WORD *)areg)[1] = 0; */\n\t\twrite_mem(pooter_plus((POOTER)areg, 1), 0);\n\t\t/* Get the resume address from the workspace */\n\t\t/* iptr = (BYTE *)((WORD *)areg)[0]; */\n\t\tiptr = (BPOOTER)read_mem((POOTER)areg);\n\t\t/* The areg becomes the new wptr */\n\t\twptr = (POOTER)areg;\n\t\t/* The entire stack becomes undefined */\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\t}\n\telse\n\t{\n\t\t/* Terminate current process, and reschedule another from the queue */\n\n\t\t/* Subtract one from the process count */\n\t\t/*((WORD *)areg)[1] = ((WORD *)areg)[1] - 1;*/\n\t\twrite_mem(pooter_plus((POOTER)areg, 1), \n\t\t\t\tread_mem(pooter_plus((POOTER)areg, 1)) - 1);\n\t\t/* The entire stack becomes undefined */\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\t\t/* Run the next process */\n\t\trun_next_on_queue();\n\t}\n}")
     (hash-table-put!
       c
       'ldpi
       "\n{\n\t/*STACK((WORD)(iptr + areg), breg, creg);*/\n\tSTACK((WORD)bpooter_plus(iptr, areg), breg, creg);\n}")
     (hash-table-put!
       c
       'xdble
       "\n{\n\tif(areg < 0)\n\t{\n\t\tSTACK(areg, -1, breg);\n\t}\n\telse\n\t{\n\t\t/* areg >= 0 */\n\t\tSTACK(areg, 0, breg);\n\t}\n}")
     (hash-table-put!
       c
       'taltwt
       "\n{\n\t/* FIXME: !!! */\n\tint pri = 0;\n\n\tgoto two;\n\none:\n\tif(WORKSPACE_GET(wptr, WS_ALT_STATE) == DISABELING_P)\n\t{\n\t\tWORKSPACE_SET(wptr, WS_TIMEOUT, get_time());\n\t}\n\telse if(WORKSPACE_GET(wptr, WS_ALT_T) == TIME_NOT_SET_P)\n\t{\n\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);\n\n\t\trun_next_on_queue();\n\t}\n\telse if(WORKSPACE_GET(wptr, WS_ALT_T) == TIME_SET_P)\n\t{\n\t\tif(AFTER(get_time(), WORKSPACE_GET(wptr, WS_TIMEOUT)))\n\t\t{\n\t\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, DISABELING_P);\n\t\t\tWORKSPACE_SET(wptr, WS_TIMEOUT, get_time());\n\t\t}\n\t\telse\n\t\t{\n\t\t\tWORKSPACE_SET(wptr, WS_TIMEOUT, WORKSPACE_GET(wptr, WS_TIMEOUT) + 1);\n\t\t\ttraverse_and_insert(tptr[pri], tptr[pri]);\n\t\t\tif(WORKSPACE_GET(wptr, WS_ALT_STATE) != DISABELING_P)\n\t\t\t{\n\t\t\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);\n\n\t\t\t\trun_next_on_queue();\n\t\t\t}\n\t\t}\n\t}\n\n\tWORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);\n\treturn;\ntwo:\n\t/* FIXME: Document this better */\n\t//if((WORKSPACE_GET(wptr, WS_ALT_STATE) == DISABELING_P) ||\n\tif((WORKSPACE_GET(wptr, WS_ALT_STATE) == ENABELING_P) ||\n\t\t\t((WORKSPACE_GET(wptr, WS_ALT_T) == TIME_SET_P) &&\n\t\t\t (BEFORE(WORKSPACE_GET(wptr, WS_TIMEOUT), get_time()))))\n\t{\n\t\t/* Undefine the stack */\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\t\t/* FIXME: a better description here? */\n\t\t/* Blast the workspace */\n\t\tWORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);\n\t}\n\t/*\n\telse if(WORKSPACE_GET(wptr, WS_NEXT_T) == TIME_NOT_SET_P)\n\t{\n\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\n\t\trun_next_on_queue();\n\t}*/\n\telse\n\t{\n\t\t/* Insert outselves into the timer queue */\n\t\ttraverse_and_insert(tptr[pri], tptr[pri]);\n\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\t\tWORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);\n\n\t\trun_next_on_queue();\n\t}\n}")
     (hash-table-put!
       c
       'pfix
       "\n{\n\t/* Shift the operand register up */\n\t/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values\n\t * is implementation defined */\n\toreg = (WORD)((UWORD)oreg << 4);\n}")
     (hash-table-put!
       c
       'alt
       "\n{\n\t/* Set the alt state as enabeling */\n\tWORKSPACE_SET(wptr, WS_ALT_STATE, ENABELING_P);\n}")
     (hash-table-put!
       c
       'disc
       "\n{\n\tWORD wptr_deref = WORKSPACE_GET(wptr, WS_TOP);\n\tWORD creg_deref = read_mem((POOTER)creg);\n\t/* FIXME: There dont seem that much point in doing this the same way\n\t * as with soccam, ie by hacing the fired variable, as we dont seem to \n\t * gain anything, as we still need to do the test in a big ig. Ie we\n\t * might therefore as well do the test in the if, and do the action\n\t * depending on the result of that, rather than storing the result of\n\t * the if in a variable and then doing another if to do the work */\n\tint fired = 0;\n\n\t/* If fired? */\n\tif((breg != 0) && \n\t\t\t(wptr_deref == NONE_SELECTED_O) && \n\t\t\t(creg_deref != NOT_PROCESS_P) && \n\t\t\t((POOTER)creg_deref != wptr))\n\t{\n\t\tWORKSPACE_SET(wptr, WS_TOP, areg);\n\t\tfired = 1;\n\t}\n\n\t/* Additionally\n\t * If this test is true, then it implies we are looking\n\t * at ourselves. So, we should destroy evidence of \n\t * a waiting process (US!) before doing everything else.\n\t */\n\tif((breg != 0) && ((POOTER)creg_deref == wptr))\n\t{\n\t\t/* FIXME: Here and in soccam would it be more clear to use \n\t\t * wptr rather than creg??? */\n\t\twrite_mem((POOTER)creg, NOT_PROCESS_P);\n\t}\n\n\tSTACK((WORD)fired, UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'ajw
       "\n{\n\t/* Add the value in the operand register to the workspace\n\t * pointer. */\n\twptr = pooter_plus(wptr, oreg);\n\n\tCLEAR(oreg);\n}")
     (hash-table-put! c 'mint "\n{\n\tSTACK(MIN_INT, areg, breg);\n}")
     (hash-table-put!
       c
       'cj
       "\n{\n\t/* If areg is = 0 then we jump, otherwise pop the stack */\n\tif(areg == 0)\n\t{\n\t\t/* Set the iptr to the new address, found in oreg */\n\t\t/* FIXME: Check this, I think we need to add */\n\t\tiptr = bpooter_plus(iptr, oreg);\n\n\t\t/* Stack is left untouched */\n\t\tSTACK(areg, breg, creg);\n\t}\n\telse\n\t{\n\t\t/* Pop the stack */\n\t\tSTACK(breg, creg, UNDEFINE(creg));\n\t}\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'sub
       "\n{\n\t/* FIXME: This instruction should set the error flag on overflow */\n\tSTACK(breg - areg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'eqc
       "\n{\n\t/* Check if areg is equal to the oreg, set areg accordingly */\n\tif(areg == oreg)\n\t{\n\t\tareg = 1; /* Set areg to true */\n\t}\n\telse\n\t{\n\t\tareg = 0; /* Set areg to false */\n\t}\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'j
       "\n{\n\tiptr = bpooter_plus(iptr, oreg);\n\n\t/* FIXME: This ought to be a propoper breakpoint hook perhaps, though\n\t * I am sure this will do for now.\n\t */\n\t/* FIXME: This does not work with unoptimal optimisations at the moment, as\n\t * that seems to leave some J 0's in the code...\n\t */\n\t/*\n\tif(oreg == 0)\n\t{\n\t\texit_runloop(EXIT_DEBUG_TRAP);\n\t}\n\t*/\n\n\t/* This instruction undefines all of the stack */\n \tSTACK(UNDEFINE(areg),\n\t\t\t  UNDEFINE(breg),\n\t\t\t\tUNDEFINE(creg));\n\t\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'ldc
       "\n{\n\t/* Push the stack down */\n\tSTACK(UNDEFINE(areg), areg, breg);\n\n\t/* Add the constant on the top of the stack, ie the areg */\n\tareg = oreg;\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'adc
       "\n{\n\t/* Add the operand register to areg */\n\t/* TODO: This instruction does checks and potentially\n\t * sets the error flag. */\n\t/* FIXME: Soccam has no note about checking for overflow\n\t * for this instruciton, add */\n\tareg = areg + oreg;\n\n\t/* breg and creg stay the same */\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'ldl
       "\n{\n\t/* Push the stack down */\n\tSTACK(UNDEFINE(areg), areg, breg);\n\t\n\t/* Read from memory(oreg+wptr) */\n\t/*areg = *(wptr + oreg);*/\n\tareg = read_mem(pooter_plus(wptr, oreg));\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'stl
       "\n{\n\t/* *(wptr + oreg) = areg; */\n\twrite_mem(pooter_plus(wptr, oreg), areg);\n\n\tSTACK(breg, creg, UNDEFINE(creg));\n\n\tCLEAR(oreg);\n}")
     (hash-table-put!
       c
       'opr
       "\n{\n\tsecondaries[oreg]();\n\n\tCLEAR(oreg);\n}")
     (hash-table-put! c 'rev "\n{\n\tSTACK(breg, areg, creg);\n}")
     (hash-table-put!
       c
       'enbt
       "\n{\n\t/* FIXME: This is badly coded in soccam and here */\n\t/* FIXME: There is a redundant STACK macro call */\n\tif(areg == 0)\n\t{\n\t\t/* The quard is disabled, do nothing apart from changeing the stack */\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\t}\n\telse\n\t{\n\t\tif(WORKSPACE_GET(wptr, WS_ALT_T) == TIME_NOT_SET_P)\n\t\t{\n\t\t\t/* If nobody else has set a timeout in this alt yet, set one */\n\t\t\tWORKSPACE_SET(wptr, WS_ALT_T, TIME_SET_P);\n\t\t\tWORKSPACE_SET(wptr, WS_TIMEOUT, breg);\n\t\t}\n\t\telse if(AFTER(WORKSPACE_GET(wptr, WS_TIMEOUT), breg))\n\t\t{\n\t\t\t/* Otherwise if the timeout of this enbt is earlier then the stored one */\n\t\t\tWORKSPACE_SET(wptr, WS_TIMEOUT, breg);\n\t\t}\n\t\telse\n\t\t{\n\t\t\t/* Do nothing I guess... */\n\t\t}\n\t}\n\t\n\t/* Remove the time in breg from the stack */\n\tSTACK(areg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'lb
       "\n{\n  STACK((WORD)read_byte((BPOOTER)areg),breg,creg);\n}")
     (hash-table-put!
       c
       'lsum
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\t/*FIXME: not tested...*/\n\t/* seems to work according to examples.... but issues warning as with lmul...*/\n\tunsigned long long result;\n\tint carry = 0;\n\tresult = areg + breg + (creg & 1);\n\tif(result & LONG_HI_MASK) carry = 1;\n\tSTACK(result, carry, UNDEFINE(creg));\n#endif\n}")
     (hash-table-put!
       c
       'add
       "\n{\n\t/* FIXME: The error flag could be set (by the \"checked plus\") on overflow */\n\tSTACK(areg + breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'lsub
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\t/*FIXME: not tested...*/\n\t/*FIXME: does not check for overflow...*/\n\t/*FIXME: requires the LDIFF instruction to be useful !...*/\n\tSTACK(((breg - areg) - (creg & 1)), UNDEFINE(breg), UNDEFINE(creg));\n#endif\n}")
     (hash-table-put!
       c
       'gt
       "\n{\n\tif(breg > areg)\n\t{\n\t\tSTACK(1, creg, UNDEFINE(creg));\n\t}\n\telse\n\t{\n\t\tSTACK(0, creg, UNDEFINE(creg));\n\t}\n}")
     (hash-table-put!
       c
       'not_implemented
       "\n{\n\tif(not_implemented)\n\t{\n\t\tnot_implemented();\n\t}\n\texit_runloop(EXIT_INS_NOT_IMP);\n}")
     (hash-table-put!
       c
       'rem
       "\n{\n\tif((areg == 0) || ((areg == -1) && (breg == MIN_INT)))\n\t{\n\t\tSTACK(UNDEFINE(areg), creg, UNDEFINE(creg));\n\t\tset_error_flag();\n\t}\n\telse\n\t{\n\t  STACK((breg % areg), creg, UNDEFINE(creg));\t\n\t}\n}")
     (hash-table-put!
       c
       'altend
       "\n{\n\t/* Add the jump offset which has been stored at the top of the workspace by\n\t * one of the disabeling instructions to the current iptr */\n\tiptr = bpooter_plus(iptr, WORKSPACE_GET(wptr, WS_TOP));\n}")
     (hash-table-put!
       c
       'ret
       "\n{\n\t/* Check if this was the last instruction */\n\t/* FIXME: I dont like having this here, there must be a better and cheaper way\n\t * of doing this. */\n\t/* FIXME: This needs wordsize fixing */\n\t/*if((BYTE *)*wptr == (BYTE *)0xffffffff)*/\n\tif((BPOOTER)read_mem(wptr) == (BPOOTER)0xffffffff)\n\t{\n\t\texit_runloop(EXIT_STACK_BOTTOM);\n\t}\n\n\t/*\n\tiptr = (BYTE *)*wptr;\n\twptr = wptr + 4;\n\t*/\n\tiptr = (BPOOTER)read_mem(wptr);\n\twptr = pooter_plus(wptr, 4);\n}")
     (hash-table-put!
       c
       'ldtimer
       "\n{\n\tif(get_time != 0)\n\t{\n\t\tSTACK(get_time(), areg, breg);\n\t}\n\telse\n\t{\n\t\tins_not_implemented();\n\t}\n}")
     (hash-table-put!
       c
       'div
       "\n{\n\tif((areg == 0) || ((areg == -1) && (breg == MIN_INT)))\n\t{\n\t\tSTACK(UNDEFINE(areg), creg, UNDEFINE(creg));\n\t\tset_error_flag();\n\t}\n\telse\n\t{\n\t\tSTACK(breg / areg, creg, UNDEFINE(creg));\n\t}\n}")
     (hash-table-put!
       c
       'ldinf
       "\n{\n#if (defined LEGO)\n\tins_not_implemented();\n#else\n\tSTACK(0x7F800000, areg, breg);\n#endif\n}")
     (hash-table-put!
       c
       'xor
       "\n{\n\tSTACK(areg ^ breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put! c 'restorecreg "\n{\n\tcreg = saved_creg;\n}")
     (hash-table-put!
       c
       'sb
       "\n{\n\twrite_byte((BPOOTER)areg, (BYTE)breg);\n\n\tSTACK(creg, UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'wsub
       "\n{\n\t/* FIXME: Same check as for bsub */\n\t/*STACK((WORD)((WORD *) areg) + breg, creg, UNDEFINE(creg));*/\n\tSTACK((WORD)pooter_plus((POOTER)areg, breg), creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'diss
       "\n{\n\t/* FIXME: ditto as for disc */\n\tint fired = 0;\n\n\t/* Fired? */\n\tif((breg != 0) && (WORKSPACE_GET(wptr, WS_TOP) == NONE_SELECTED_O))\n\t{\n\t\tWORKSPACE_SET(wptr, WS_TOP, areg);\n\t\tfired = 1;\n\t}\n\n\tSTACK((WORD)fired, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'shr
       "\n{\n\tSTACK( ((UWORD) ((UWORD) breg) >> ((UWORD) areg)), creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'ccnt1
       "\n{\n\tif((breg == 0) || (((UWORD) breg) > ((UWORD) areg)))\n\t{\n\t\tset_error_flag();\n\t}\n\t\n\tSTACK(breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'dist
       "\n{\n\t/* FIXME: !!! */\n\tint pri = 0;\n\n\tWORD current_time = get_time();\n\tint fired = (((breg != 0) && (WORKSPACE_GET(wptr, WS_TOP) == NONE_SELECTED_O)) &&\n\t\tAFTER(current_time, creg));\n\n\tif(fired)\n\t{\n\t\t/* We fired, set the top of the workspace to the address of code to run */\n\t\tWORKSPACE_SET(wptr, WS_TOP, areg);\n\t}\n\n\t/* Aditionally */\n\tif((breg != 0) && (BEFORE(current_time, creg)) && (tptr[pri] != (POOTER)NOT_PROCESS_P))\n\t{\n\t\tPOOTER loop_wptr = tptr[pri];\n\t\tPOOTER loop_next = (POOTER)WORKSPACE_GET(tptr[pri], WS_NEXT_T);\n\t\tPOOTER loop_prev = 0;\nins_dist_loop:\n\t\tif((loop_wptr == tptr[pri]) && (loop_wptr == wptr))\n\t\t{\n\t\t\t/* HEAD CHECK */\n\t\t\t/* We are at the head of the list */\n\t\t\tif(loop_next == (POOTER)NOT_PROCESS_P)\n\t\t\t{\n\t\t\t\t/* If there is nobody after this process, we can nuke the tptr */\n\t\t\t\ttptr[pri] = (POOTER)NOT_PROCESS_P;\n\t\t\t}\n\t\t\telse\n\t\t\t{\n\t\t\t\t/* If there are others after this process, we need to put them at the\n\t\t\t\t * head of the queue, and also set the timeout to their timeout value.\n\t\t\t\t */\n\t\t\t\ttptr[pri] = (POOTER)WORKSPACE_GET(loop_wptr, WS_NEXT_T);\n\t\t\t\ttnext[pri] = WORKSPACE_GET(tptr[pri], WS_TIMEOUT);\n\t\t\t}\n\t\t}\n\t\telse if(loop_wptr == wptr)\n\t\t{\n\t\t\t/* TERMINATION */\n\t\t\t/* If we find ourselves on the queue... */\n\t\t\t/* FIXME: check this is correct */\n\t\t\t/* ... take us out of the queue */\n\t\t\tWORKSPACE_SET(loop_prev, WS_NEXT_T, WORKSPACE_GET(loop_wptr, WS_NEXT_T));\n\t\t}\n\t\telse if(loop_next == (POOTER)NOT_PROCESS_P)\n\t\t{\n\t\t\t/* AT THE END OF THE LIST */\n\t\t\t/* Do nothing */\n\t\t}\n\t\telse\n\t\t{\n\t\t\t/* KEEP LOOKING */\n\t\t\tloop_prev = loop_wptr; /* FIXME: different order from soccam */\n\t\t\tloop_wptr = (POOTER)WORKSPACE_GET(loop_wptr, WS_NEXT_T);\n\t\t\tloop_next = (POOTER)WORKSPACE_GET(loop_wptr, WS_NEXT_T); /* FIXME: different from soccam */\n\t\t\tgoto ins_dist_loop;\n\t\t}\n\t}\n\n\tSTACK(fired, UNDEFINE(breg), UNDEFINE(creg));\n\n}")
     (hash-table-put!
       c
       'stoperr
       "\n{\n\tif(error_flag)\n\t{\n\t\t/* Undefine the whole stack */\n\t\tSTACK(UNDEFINE(areg), UNDEFINE(areg), UNDEFINE(breg));\n\n\t\t/* Store the instruction pointer at wptr-1 */\n\t\twrite_mem(pooter_minus(wptr, 1), (WORD)iptr);\n\n\t\t/* Run a new process */\n\t\trun_next_on_queue();\n\t}\n\t/*\n\telse\n\t{\n\t\tSTACK(areg, breg, creg);\n\t}\n\t*/\n}")
     (hash-table-put!
       c
       'ldiv
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\tunsigned long long result, tmp;\n\t/* FIXME: UNTESTED */\t\n\t/* Maybe the below should not be cast to unsigned word.... */\n\tif(((UWORD) creg) >= ((UWORD) areg ))\n\t{\n\t\tset_error_flag();\n\t} \n\ttmp = (unsigned int)creg;\n\tresult = (tmp << WORDSIZE_BITS) + ((unsigned int)breg);\n\tSTACK( result / areg, result % areg, UNDEFINE(creg));\n#endif\n}")
     (hash-table-put!
       c
       'altwt
       "\n{\n\t/* FIXME: in both soccam and here, the set of WS_TOP happens in both branches,\n\t * so it should be taken out of the if.\n\t * */\n\t/* DISABELING_P is also (in the T9000 book) READY_P, so should we use that\n\t * instead? \n\t */ \n\tif(WORKSPACE_GET(wptr, WS_ALT_STATE) == DISABELING_P)\n\t{\n\t\tWORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);\n\t}\n\telse\n\t{\n\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);\n\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\tWORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);\n\t\t/* FIXME: Soccam contains a warning here questioning the need to \n\t\t * reschedule here, considering that things seem to work, I would have\n\t\t * thought that the reschedule is good! */\n\t\trun_next_on_queue();\n\t}\n\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(breg));\n}")
     (hash-table-put!
       c
       'outbyte
       "\n{\n\t/* Put the byte to be transfered at the top of the workspace */\n\twrite_byte((BPOOTER)wptr, (BYTE)areg);\n\t/* Set up the stack to the correct values for an OUT instruction, we are\n\t * trasnfereing 1 byte, at channel BREG, from the top of the\n\t * workspace (address of which is stored in CREG) */\n\tSTACK(1, breg, (WORD)wptr);\n\t/* Do the OUT */\n\tins_out();\n}")
     (hash-table-put!
       c
       'cword
       "\n{\n\tif((breg >= areg) || (breg < -areg))\n\t{\n\t\tset_error_flag();\n\t}\n\n\tSTACK(breg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'boolinvert
       "\n{\n\tSTACK((areg == 0?1:0), breg, creg);\n}")
     (hash-table-put!
       c
       'move
       "\n{\n\t\n\t/* Areg has the count, we use that as our counter variable and count down to 0\n\t * We also modify breg (dest) and creg (src) by adding one to them each\n\t * time through the loop in order to perform the move */\n\t/* FIXME: Optimise this for WORDALIGNED WORDLENGTH moves? */\n\tfor(; areg > 0 ; areg--)\n\t{\n\t\twrite_byte((BPOOTER) breg++, read_byte((BPOOTER) creg++));\n\t}\n\t\t\n\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'enbc
       "\n{\n\t/* Is guard enabled? if not, do nothing */\n\t/* FIXME: This is the other way around from how the test is done in soccam */\n\tif(areg != 0)\n\t{\n\t\tif(read_mem((POOTER)breg) == NOT_PROCESS_P)\n\t\t{\n\t\t\twrite_mem((POOTER)breg, (WORD)wptr);\n\t\t}\n\t\telse\n\t\t{\n\t\t\tif((POOTER)read_mem((POOTER)breg) == wptr)\n\t\t\t{\n\t\t\t\t/* Another guard of the current process is waiting\n\t\t\t\t * on the channnel; do nothing.\n\t\t\t\t */\n\t\t\t}\n\t\t\telse\n\t\t\t{\n\t\t\t\t/* another process is waiting on the channel, so set a \n\t\t\t\t * flag to show that the guard is ready\n\t\t\t\t */\n\t\t\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, DISABELING_P);\n\t\t\t}\n\t\t}\n\t}\n\n\tSTACK(areg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'widenshort
       "\n{\n\t/* Kudos to Damian for suggesting casting rather than doing something stupid,\n\t * like I would have done! clj3 */\n\tSTACK(((WORD) ((HWORD)areg)), breg, creg);\n}")
     (hash-table-put! c 'dup "\n{\n\tSTACK(areg, areg, breg);\n}")
     (hash-table-put!
       c
       'tin
       "\n{\n\t/* FIXME: This is TEMPORARY in order to get this stuff to compile, \"pri\"\n\t * should be GLOBAL! */\n\tint pri = 0;\n\n\t/* ins_not_implemented(); */\n\t\n\tif(get_time != 0)\n\t{\n\t\tWORD current_time = get_time();\n\t\tWORD reschedule_time = areg;\n\n\t\tif(AFTER(current_time, reschedule_time))\n\t\t{\n\t\t\t/* Do nothing, as we have already timed out */\n\t\t}\n\t\telse\n\t\t{\n\t\t\t/* Store our reschedule time in our workspace */\n\t\t\tWORKSPACE_SET(wptr, WS_TIMEOUT, reschedule_time);\n\t\t\t/* Store the iptr in our workspace */\n\t\t\tWORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);\n\t\t\t\n\t\t\t/* We need to insert ourselves into the timer queue, this is an ordered\n\t\t\t * queue, after that we need to reschedule another process */\n\t\t\t/* Put ourselves into the timer queue */\n\t\t\t/* FIXME: This is a pretty bad name for this function in my oppinion, it\n\t\t\t * is time specific, so it should be called something related to that. */\n\t\t\t/* FIXME: I think we ought to have some kind of global priority variable\n\t\t\t * */\n\t\t\ttraverse_and_insert(tptr[pri],tptr[pri]);\n\t\t\t/* Run the next process */\n\t\t\trun_next_on_queue();\n\t\t\t/* Undefine the entire stack */\n\t\t\tSTACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));\n\t\t}\n\t}\n\telse\n\t{\n\t\tins_not_implemented();\n\t}\n}")
     (hash-table-put!
       c
       'enbs
       "\n{\n\t/* The stack is unaffected by this instruction */\n\n\t/* FIXME: in soccam and here, this does the test differently\n\t * than in enbc */\n\tif(areg == 1)\n\t{\n\t\tWORKSPACE_SET(wptr, WS_ALT_STATE, DISABELING_P);\n\t}\n}")
     (hash-table-put!
       c
       'invalid
       "\n{\n\tif(invalid)\n\t{\n\t\tinvalid();\n\t}\n\texit_runloop(EXIT_INS_INVALID);\n}")
     (hash-table-put!
       c
       'outword
       "\n{\n\t/* Put the word to be transfered at the top of the workspace */\n\twrite_mem(wptr, areg);\n\t/* Set up the stack to the correct values for an OUT instruction, we are\n\t * trasnfereing WORDLEGNTH bytes, at channel BREG, from the top of the\n\t * workspace (address of which is stored in CREG) */\n\tSTACK(WORDLENGTH, breg, (WORD)wptr);\n\t/* Do the OUT */\n\tins_out();\n}")
     (hash-table-put! c 'savecreg "\n{\n\tsaved_creg = creg;\n}")
     (hash-table-put! c 'not "\n{\n\tSTACK(~areg, breg, creg);\n}")
     (hash-table-put!
       c
       'csngl
       "\n{\n\tif((areg < 0 && breg != 1) || (areg >= 0 && breg != 0))\n\t{\n\t\tset_error_flag();\n\t}\n\n\tSTACK(areg, creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'lend
       "\n{\n\t/* FIXME: I dont think that the soccam version of this sets creg to \n\t * undefined */\n\t/* FIXME: WARNING:\n\t * If memory is not initialised to zero by the runtime, we need to\n\t * initialise it to zero for this to work. The occam compiler seems to\n\t * assume that memory is all zeros before the program runs. eg: The scheme\n\t * interpreter inits memory to <void>.\n\t */\n\tif(read_mem(pooter_plus((POOTER)breg, 1)) > 1)\n\t{\n\t\t/* FIXME: This is done the other way around in soccam, I think I followed\n\t\t * the order it was done in the book... check though */\n\t\twrite_mem((POOTER)breg, read_mem((POOTER)breg) + 1);\n\t\twrite_mem(pooter_plus((POOTER)breg, 1), \n\t\t\t\tread_mem(pooter_plus((POOTER)breg, 1)) - 1);\n\n\t\tiptr = bpooter_minus(iptr, areg);\n\t} \n\telse \n\t{\n\t\t/* Decrement the counter */\n\t\twrite_mem(pooter_plus((POOTER)breg, 1), \n\t\t\t\tread_mem(pooter_plus((POOTER)breg, 1)) - 1);\n\t }\t\t\t\n\t\t\n\t/* FIXME: I dont think the soccam instruction set the stack properly, it\n\t * should use the stack macro, it dont (this is related to the first comment\n\t * in this function  */\n\tSTACK(areg, breg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'fficall
       "\n{\n\t/* This is the prototype for occam FFI calls, it is defined, by D.C.Wood as\n\t * void name (word w[]); we are going to use a slightly different version\n\t * which has substituted int for word (they are equivalent) and we use *\n\t * instead of [], which are equivalent too */\n\tvoid (*ffi_func_addr)(int *);\n\t\n\t/* If the FFI table has not been created, then we dont want to\n\t * run this instruction as we are probably going to jump into\n\t * oblivion if that is the case */\n\tif(!ffi_table)\n\t{\n\t  /* FIXME: We need a seperate error trap for this */\n\t\tins_not_implemented();\n\t}\n\n\t/* Assume that if the FFI table has been set up that all is ok, and that we\n\t * can start jumping from it, areg contains the index we are jumping from,\n\t * each index in the table has two words, one (the first) is the one we are\n\t * jumping to, the second is */\n\tffi_func_addr = *(ffi_table + (2 * areg));\n\n\t/* Now we got the address, jump! Hold on to your hats :p */\n\t/* Though we need to make sure that we pass the correct parameter, which is\n\t * the wptr + 1 (+1 to avoid the iptr on the top of the stack). */\n\tffi_func_addr(wptr + 1);\n\t\n\t/* FFI call is done, now we need to return, use ins_ret */\n\tins_ret();\n}")
     (hash-table-put!
       c
       'extvrfy
       "\n{\n\t/* FIXME: Actually do the verify? */\n\n\t/* We should be popping two values off the stack as far as we can tell */\n\tSTACK(creg, UNDEFINE(creg), UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'lmul
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\t/*FIXME: not tested...*/\n\t/*FIXME: the hi calculation has a nasty warning that the thing we are sticking in it does not fit. */\n\t/* Multiplies areg * breg and adds creg (not sure why)\n\t * then gets the hi end of the multiplication and sticks it in hi\n\t * and gets the lo end of the result and stick it in lo.  lo goes \n\t * in areg and hi goes in breg */\n\tunsigned int hi, lo;\n\tunsigned long long result, tmp;\n\tresult = breg;\n\ttmp = (unsigned int)areg;\n\tresult = (result * tmp) + creg;\n\ttmp = (result >> WORDSIZE_BITS);\n\thi = (int) tmp;\n\tlo = (result & LONG_LO_MASK );\n\tSTACK( lo, hi, UNDEFINE(creg));\n#endif\n}")
     (hash-table-put!
       c
       'shl
       "\n{\n\tSTACK(((UWORD) breg) << ((UWORD) areg), creg, UNDEFINE(creg));\n}")
     (hash-table-put!
       c
       'lshl
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\t/*FIXME: not tested...*/\n\t/* hi does not need to be long, bits leftshifted out of hi are supposed to \n\t* fall out into the void anyway */\n\tunsigned int hi;  \n\tunsigned long long lo;\n\tif(areg <= 2* WORDSIZE_BITS  && areg >= 0 )\n\t{\n\t\thi = ((unsigned int)creg) << areg; /*shift hi*/\n\t\t/* shift lo - if breg is shifted directly then you get the result of\n\t\t * a shifted int put in a long long.  Not what we want... shift the value\n\t\t * in the long long to get what we want (doesn't overflow).  Probably not\n\t\t * very efficient.*/\n\t\tlo = breg; \n\t\tlo = lo << areg;  \n\t\thi += lo >> WORDSIZE_BITS;  /*add what 'overflowed' from lo to hi.*/\n\t\tSTACK((WORD)lo, (WORD)hi, UNDEFINE(creg));\n\t} \n#endif\n}")
     (hash-table-put!
       c
       'ldiff
       "\n{\n#if (defined WIN32) || (defined LEGO)\n\t/* FIXME: Does not work with BCC32 */\n\tins_not_implemented();\n#else\n\t/* FIXME: not tested!! */\t\n\tunsigned long long result;\n\tint carry = 0;\n\tresult = breg - areg - (creg & 1);\n\t/*This is supposed to check for overflow....  its probably\n\t* inefficient and quite possibly just plain wrong.*/\n\tif(result > MAX_INT || result < -MAX_INT) carry = 1;\n\tSTACK((int)result, carry, UNDEFINE(creg));\n#endif\n}")
     (values h c)
     c)))
