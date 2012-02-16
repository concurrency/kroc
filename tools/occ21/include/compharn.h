/*
 *	Common compiler definitions
 *	Copyright (C) 1987, 1992 Inmos Limited
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _compharn_LOADED
#define _compharn_LOADED

/*{{{  harness variables */
/* These variables should be actually defined in the same module
   as main.
   Their declarations are here to ensure consistency between the
   different INMOS compilers.
*/
extern BOOL information;
extern BOOL testflag;

extern struct optimisation_struct
  {
    BOOL flowgraph_opt_enabled;
         /* Default true. Enabled by O1, O2, ZEF. Disabled by O0, ZNF. */

    BOOL peepholer_enabled;
         /* Default true. Enabled by O1, O2, ZEP. Disabled by O0, ZNP. */

    BOOL tailcall_opt_enabled;
         /* Default true. Enabled by O1, O2, ZET. Disabled by O0, ZNT. */

    BOOL deadcode_opt_enabled;
         /* Default true. Enabled by O1, O2, ZED. Disabled by O0, ZND. */

    BOOL cse_opt_enabled;
         /* Default false. Enabled by O2, ZEC. Disabled by O0, O1, ZNC. */

    BOOL cse_heuristics_enabled;
         /* Default true, but only makes a difference when cse_enabled or
            loop_invariant_opt_enabled. Enabled by O2, ZECH. Disabled by ZNCH.*/

    BOOL loop_invariant_opt_enabled;
         /* Default false. Enabled by O2, ZEL. Disabled by O0, O1, ZNL. */

    BOOL strength_reduction_enabled;
         /* Default false. Enabled by O2, ZES. Disabled by O0, O1, ZNS. */

    BOOL allocate_workspace_by_liveness;
         /* Default true. Enabled by O1, O2, ZEW. Disabled by O0, ZNW (in which
            case workspace is allocated disjointly). */

    BOOL prefer_time_to_space;
         /* Default true. Enabled by QT. Disabled by QS. */

    BOOL use_no_side_effect_info;
         /* Only used by the C compiler, currently.
            Initially: default false. Enabled by O2, ZESE. 
                       Disabled by O0, O1, ZNSE. */

    BOOL allow_formals_overlay;
         /* Allow local variables to overlay formal parameters provided
            that their liveness doesn't clash. */
         /* Default false.  Enabled by O2, ZEFO.
                            Disabled by O0, O1, ZNFO, C pragma IMS_conn_v1 */

    BOOL use_unused_stack_args;
         /* Allocate local variables in any holes in workspace left because
            the call had less than 3 words of args */
         /* Default false.  Enabled by O2, ZEUS.
                            Disabled by O0, O1, ZNUS, C pragma IMS_conn_v1 */
         
    BOOL constant_propagation_enabled;
         /* Default false.  Enabled by O2 ZECP.
                            Disabled by O0, O1, ZNCP. */

    BOOL unroll_loops;
         /* Default false.  Enabled by UNROLL x:y. */
  } optimisations;

/* The following macro sets the initial (default) state of the optimisations
   structure. */
#define OPTIMISATION_DEFAULT_INIT \
                    {  optimisations.flowgraph_opt_enabled = TRUE; \
                       optimisations.peepholer_enabled = TRUE; \
                       optimisations.tailcall_opt_enabled = FALSE; \
                       optimisations.deadcode_opt_enabled = TRUE; \
                       optimisations.cse_opt_enabled = FALSE; \
                       optimisations.cse_heuristics_enabled = FALSE; \
                       optimisations.loop_invariant_opt_enabled = FALSE; \
                       optimisations.strength_reduction_enabled = FALSE; \
                       optimisations.allocate_workspace_by_liveness = TRUE; \
                       optimisations.prefer_time_to_space = TRUE; \
                       optimisations.use_no_side_effect_info=FALSE;\
                       optimisations.allow_formals_overlay = FALSE;\
                       optimisations.use_unused_stack_args = FALSE;\
                       optimisations.constant_propagation_enabled = FALSE;\
                       optimisations.unroll_loops = FALSE;}

/* The following macros set up the fields of the optimisations structure
   for the optimisation levels 0, 1, and 2.
   eg. When the command line option 01 is seen, it should be adequate to
   write      
        OPTIMSATION_LEVEL_1_INIT;
   Note that some fields of the optimisations structure (currently only
   prefer_time_to_space) are not set up by these macros. */

#define OPTIMISATION_LEVEL_0_INIT \
                   {  optimisations.flowgraph_opt_enabled = FALSE; \
                      optimisations.peepholer_enabled = FALSE; \
                      optimisations.tailcall_opt_enabled = FALSE; \
                      optimisations.deadcode_opt_enabled = FALSE; \
                      optimisations.cse_opt_enabled = FALSE; \
                      optimisations.cse_heuristics_enabled = FALSE; \
                      optimisations.loop_invariant_opt_enabled = FALSE; \
                      optimisations.strength_reduction_enabled = FALSE; \
                      optimisations.allocate_workspace_by_liveness = FALSE; \
                      optimisations.use_no_side_effect_info = FALSE;\
                      optimisations.allow_formals_overlay = FALSE;\
                      optimisations.use_unused_stack_args = FALSE;\
                      optimisations.constant_propagation_enabled = FALSE;\
                      optimisations.unroll_loops = FALSE; }

#define OPTIMISATION_LEVEL_1_INIT \
                   {  optimisations.flowgraph_opt_enabled = TRUE; \
                      optimisations.peepholer_enabled = TRUE; \
                      optimisations.tailcall_opt_enabled = FALSE; \
                      optimisations.deadcode_opt_enabled = TRUE; \
                      optimisations.cse_opt_enabled = FALSE; \
                      optimisations.cse_heuristics_enabled = FALSE; \
                      optimisations.loop_invariant_opt_enabled = FALSE; \
                      optimisations.strength_reduction_enabled = FALSE; \
                      optimisations.allocate_workspace_by_liveness = TRUE; \
                      optimisations.use_no_side_effect_info = FALSE;\
                      optimisations.allow_formals_overlay = FALSE; \
                      optimisations.use_unused_stack_args = FALSE;\
                      optimisations.constant_propagation_enabled = FALSE; \
                      optimisations.unroll_loops = FALSE; }


#define OPTIMISATION_LEVEL_2_INIT \
                   {  optimisations.flowgraph_opt_enabled = TRUE; \
                      optimisations.peepholer_enabled = TRUE; \
                      optimisations.tailcall_opt_enabled = TRUE; \
                      optimisations.deadcode_opt_enabled = TRUE; \
                      optimisations.cse_opt_enabled = TRUE; \
                      optimisations.cse_heuristics_enabled = TRUE; \
                      optimisations.loop_invariant_opt_enabled = TRUE; \
                      optimisations.strength_reduction_enabled = TRUE; \
                      optimisations.allocate_workspace_by_liveness = TRUE; \
                      optimisations.use_no_side_effect_info = TRUE; \
                      optimisations.allow_formals_overlay = TRUE; \
                      optimisations.use_unused_stack_args = TRUE;\
                      optimisations.constant_propagation_enabled = TRUE;\
                      optimisations.unroll_loops = FALSE; }



/*}}}*/

/*{{{  frmb -- this _seems_ like a good place to add some global flags */
extern BOOL reverse_alt_disable;
extern BOOL disable_dynmem;
extern BOOL enhanced_alt_enable;
extern BOOL alt_preenable;
extern BOOL enable_mobilespace;
extern BOOL mpp_check_at_act;		/* checks for a "terminated" mobile process at the point of activation */
extern BOOL map_all_procs;
extern BOOL target_bigendian;		/* whether the target is bigendian */
extern BOOL target_accessaligned;	/* whether the target requires aligned memory accesses */
extern BOOL main_dynentry;		/* if TRUE generates a DYNCALL entrypoint for the top-level process */

/*}}}  */

#endif /* _compharn_LOADED */
