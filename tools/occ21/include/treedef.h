/* $Id: treedef.h,v 1.3 1997/08/22 11:04:41 mdp2 Exp $ */

/*
 *	parse tree structure
 *	Copyright (C) 1987 Inmos Limited
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


/*{{{  MDP notes*/
/* added n_endposn to n_loc in n_proc in n_un in namenode */
/*}}}*/

#ifndef _treedef_LOADED
#define _treedef_LOADED

/*{{{  SOURCEPOSN*/
typedef BIT32 SOURCEPOSN;
#define FileLineOf(L) ((int)((L) & 0xfffff))
#define FileNumOf(L)  ((int)(((L) >> 20) & 0xfff))

#define SetFileLine(L,M) ((L) = ((L) & 0xfff00000) | (((BIT32)M) & 0xfffff))
#define SetFileNum(L,M)  ((L) = ((L) & 0x000fffff) | ((((BIT32)M) & 0xfff) << 20))

#define NOPOSN ((SOURCEPOSN)0)
/*}}}*/
/*{{{  tagtype_t*/
/* Many structures must have their tag as the first part of the structure,
   and they must all be the same type. This enforces this requirement.
*/

/* This used to be an unsigned char, but since all the revelant structures
    have the next item word aligned, we can make it an int to speed access.
*/

/*typedef unsigned char tagtype_t;*/
typedef int tagtype_t;
/*}}}*/
/*{{{  wordnodestruct*/
struct wordnodestruct
  {
    tagtype_t w_tag;
    struct wordnodestruct *w_next;
    int w_length;
    const char *w_name;
  };
typedef struct wordnodestruct wordnode;
/*}}}*/
/*{{{  parse tree nodes*/
struct tnode_s; /* Forward declaration */

/*{{{  structures used within tnode_s*/
/*{{{  struct actionnode_s           action*/
struct actionnode_s
  {
    struct tnode_s *act_lhs,             /* Left-hand side of the action */
                   *act_rhs;             /* Right-hand side of the action */
    struct tnode_s *act_type;            /* Type tree for comms */
    struct tnode_s *act_during;          /* Process enclosed in extended input */
    struct tnode_s *act_after;           /* Process after extended input (may be NULL) */
    int act_flags;			 /* Special flags for extended input (ActionFlag_...) */
  };
/*}}}*/
/*{{{  struct altnode_s              alternative*/
struct altnode_s
  {
    struct tnode_s *alt_guard,            /* Guard expression */
                   *alt_input,            /* Channel input */
                   *alt_body;             /* Process in the ALT */
    struct tnode_s *alt_chanexp;          /* Channel expression */
    struct tnode_s *alt_timeexp;          /* Timer value */
    union
      {
        int         alt_label;            /* Used by backend */
        void       *alt_labelptr;         /* Also used by backend */
      } alt_un;
  };
/*}}}*/
/*{{{  struct arraysubnode_s         array subscript*/
struct arraysubnode_s
  {
    struct tnode_s *as_base,         /* What is being subscripted */
                   *as_index,        /* the subscript */
                   *as_exp;          /* Transformed expression for backend */
    struct tnode_s *as_length;       /* Expression containing length, if not constant */
    INT32           as_offset;       /* Constant-folded dimension length */
  };
/*}}}*/
/*{{{  struct cnode_s                construct (SEQ, PAR, IF, ALT)*/
struct cnode_s
  {
    struct tnode_s *c_body;              /* Body of the construct */
    struct tnode_s *c_temp;              /* Space for a temporary */
  };
/*}}}*/
/*{{{  struct condnode_s             condition (guard)*/
struct condnode_s
  {
    struct tnode_s *cnd_guard,           /* Guard expression */
                   *cnd_body,            /* Guarded process */
                   *cnd_after;		 /* After process for extended variants */
  };
/*}}}*/
#ifdef CONDEXP
/*{{{  struct condexpnode_s          conditional expression*/
struct condexpnode_s
  {
    struct tnode_s *cndexp_guard;           /* Guard expression */
    struct tnode_s *cndexp_true;            /* true expression  */
    struct tnode_s *cndexp_false;           /* false expression */
    int cndexp_type;                        /* Type of operator */
  };
/*}}}*/
#endif
/*{{{  struct constexpnode_s         constant folded expression*/
struct constexpnode_s
  {
    struct tnode_s *ce_exp;              /* Unfolded expression tree */
    BIT32 ce_vhi, ce_vlo;                /* Folded value of expression */
    struct tnode_s *ce_next;             /* Link to next item in list,
                                            used by code generator */
    INT32 ce_offset;                     /* Offset from beginning of
                                            long constant table,
                                            used by code generator */
  };
/*}}}*/
/*{{{  struct consttablenode_s       constant table*/
struct consttablenode_s
  {
    union {
      wordnode       *ct_val;            /* Value of constant table */
      const char     *ct_ptr;            /* Pointer to char array */
    } ct_un;
    struct tnode_s *ct_exp,              /* Constant tree before folding */
                   *ct_next;             /* Link to next item in list,
                                            used by code generator */
    struct tnode_s *ct_type;             /* used by fe to hold the user type of
                                            the  type decoration of strings. */
    int ct_label;                        /* Label of constant table,
                                            used by code generator */
  };
/*}}}*/
/*{{{  struct declnode_s             specification*/
struct declnode_s
  {
    struct tnode_s *d_nameptr,           /* List of symbol table entries */
                   *d_v,                 /* Initializing value, if any */
                   *d_body,              /* Process following specification */
		   *d_extra;		 /* Extra tree for declarations (eg, EXTENDS list) */
  };
/*}}}*/
/*{{{  struct dopnode_s              dyadic operator*/
struct dopnode_s
  {
    struct tnode_s *dop_leftop,          /* Left-hand operand */
                   *dop_rightop;         /* Right-hand operand */
    int dop_type;                  /* Type of operator */
  };
/*}}}*/
/*{{{  struct hiddenparamnode_s      hidden parameter, used by backend only*/
#if 0
struct hiddenparamnode_s
  {
    struct tnode_s *h_exp;       /* Expression to which hidden param applies */
    BIT32 h_dimension;           /* Dimension which hidden param represents */
    int   h_lexlevel;            /* Lex level of hidden parameter */
    int   h_arg;                 /* Arg number of corresponding real arg */
#ifdef COMPILING_TO_JCODE
    void *h_binder;              /* Used for  J_CODE creation */
#endif
  };
#endif
/*}}}*/
/*{{{  struct instancenode_s         function or procedure call*/
struct instancenode_s
  {
    struct tnode_s *i_nameptr;		/* Symbol table entry for called proc */
    struct tnode_s *i_paramlist;	/* List of actual parameters */
    struct tnode_s *i_fork;		/* Associated FORKING bit of tree */
    struct tnode_s *i_dynaddr;		/* Address expression associated with a dynamic call (special) */
    unsigned int i_rinstance:1;		/* recursive instance of a PROC/FUNCTION? */
    unsigned int i_forked:1;		/* forked instance of a PROC? */
    unsigned int i_dynmem:1;		/* dynamically allocated instance of a PROC/FUNCTION? */
    unsigned int i_rparamslots:15;	/* slots required for generating/passing parameters */
#ifdef MOBILES
    unsigned int i_rmspoffset;		/* offset of recursive mobilespace (yes, really needed here..) */
#endif
    unsigned char i_loadseq;		/* Parameter loading sequence */
  };
/*}}}*/
/*{{{  struct listnode_s             list of items*/
struct listnode_s
  {
    struct tnode_s *ls_left,             /* This item on the list */
                   *ls_right;            /* Next item on the list, or another
                                            list node */
  };
/*}}}*/
/*{{{  struct litnode_s              literal*/
struct litnode_s
  {
    struct tnode_s *l_exp;         /* Expression list for constructor */
                                   /* or wordnode for literal */
    struct tnode_s *l_type;        /* Type of literal */
    BOOL           l_unspec;       /* flag to show if type specified in code */
                                   /* used to generate user defined operator */
                                   /* funciton names */
  };
/*}}}*/
/*{{{  struct mopnode_s              monadic operator*/
struct mopnode_s
  {
    struct tnode_s *mop_operand;         /* Operand */
    int mop_type;                        /* Type of operator */
    int mop_typeattr;			 /* Used when handling CHAN TYPEs, holds any TypeAttr_shared-ness from parser to (scope+)checker */
  };
/*}}}*/
#if 0 /* never used */
/*{{{  struct overlapnode_s          overlap check*/
struct overlapnode_s
  {
    struct tnode_s *o_base1,            /* base of first subscript     */
                   *o_count1,           /* length of first susbcript   */
                   *o_base2,            /* base of second subscript    */
                   *o_count2;           /* length of second subscript  */
  };
/*}}}*/
#endif
/*{{{  struct processornode_s        processor*/
struct processornode_s
  {
    struct tnode_s *p_exp;              /* processor number expression */
    wordnode       *p_type;      /* processor type              */
    struct tnode_s *p_body;             /* processor body              */
    BIT16           p_scope;            /* scope of PROCESSOR statement*/
  };
/*}}}*/
/*{{{  struct replcnode_s            replicated construct*/
struct replcnode_s
  {
    struct tnode_s *rc_nameptr;    /* Symbol table entry for replicator */
    struct tnode_s *rc_startexp,   /* Replicator start expression */
                   *rc_lengthexp,  /* Replicator count expression */
                   *rc_stepexp,    /* Replicator step expression */
                   *rc_body;       /* Replicated process */
    struct tnode_s *rc_temp;       /* Temp for replicated ALTs    */
    struct tnode_s *rc_ra_start,   /* Start expression for reversed ALT disables */
    		   *rc_ra_step;    /* Step expression for reversed ALT disables */
  };
/*}}}*/
/*{{{  struct segmentnode_s          segment instance*/
struct segmentnode_s
  {
    struct tnode_s *s_nptr,              /* Segmented element */
                   *s_startexp,          /* Segment start expression */
                   *s_lengthexp,         /* Segment length expression */
                   *s_subscriptexp,      /* Transformed subscript expression */
                   *s_checkexp;          /* Segment check expression */
    struct tnode_s *s_length;            /* Expression containing length, if not constant */
    INT32          s_offset;             /* Constant offset from subscript */

  };
/*}}}*/
/*{{{  struct spacenode_s            space usage information, used  by backend*/
struct spacenode_s
  {
    struct tnode_s *sp_body;             /* Sized process */
    INT32           sp_maxwsp,           /* space usage above wptr */
                    sp_datasize,         /* total space including below wptr */
                    sp_vsusage,          /* vector space usage */
#ifdef MOBILES
                    sp_nestedvs,         /* vs usage for nested calls */
                    sp_msusage,          /* mobile-space usage */
                    sp_nestedms;         /* ms offset in parent (ms usage/offset for nested calls) */
    void            *sp_msptr;           /* pointer to mobile-space map for a dynamic repl par */
    void            *sp_wsmap;           /* workspace-map (bind3.c) */
    int             sp_maplabel;         /* label of generated workspace-map */
#else
                    sp_nestedvs;         /* vs usage for nested calls */
#endif
    struct tnode_s *sp_namechain;        /* names declared in body */
    BIT32           sp_cpoffset;         /* constant ptr offset in repl PAR workspace */
  };
/*}}}*/
/*{{{  struct tempnode_s             temporary variable*/
#if 0 /* never used */
struct tempnode_s
  {
    int t_number;                        /* Number of temporary */
    BIT32 t_offset;                      /* Workspace offset of variable */
    struct tnode_s *t_exp;               /* Pointer to expression temporary */
                                         /* represents */
    struct tnode_s *t_next;              /* Next node in allocation list */
    unsigned char t_lexlev;              /* Lex level at which temp declared */
  };
#endif

/*}}}*/
/*{{{  struct typenode_s             constructed type definition*/
struct typenode_s
  {
    struct tnode_s *ar_type;         /* sub-type tree */
    struct tnode_s *ar_dimlength;    /* Expression tree for dimension length of an array */
    struct tnode_s *ar_traces;       /* List of traces for mobile channel-types */
    INT32           ar_dim;          /* Constant-folded dimension length */
      /* If dimension length is unknown, ar_dimlength == NULL, ar_dim == -1,
         until the workspace allocator reuses it to contain the workspace
         offset of the variable containing the array length */
    BIT32           ar_attr;         /* Attributes of the type */
    int             ar_tdlab;        /* type-descriptor label, for MOBILE channel-types */
    struct tnode_s *ar_alignment;    /* Holds alignment when allocating aligned mobile arrays */
  };
/*}}}*/
/*{{{  struct valofnode_s            valof instance*/
struct valofnode_s
  {
    struct tnode_s *vl_body,             /* VALOF body */
                   *vl_resultlist;       /* VALOF result list */
  };
/*}}}*/
#if 1 /*def CONFIG*/
/*{{{  struct confignode_s           config node*/
struct confignode_s
  {                        /* CONNECT SET        MAP */
    struct tnode_s *cnf_a; /* From    Name       Source */
    struct tnode_s *cnf_b; /* To      Attr Names Dest   */
    struct tnode_s *cnf_c; /* Arc     Attr Exps  Pri    */
  };
/*}}}*/
/*{{{  struct leafnode_s             leafnode extras*/
struct leafnode_s
  {
    struct tnode_s *link;
  };
/*}}}*/
#endif
/*}}}*/
/*{{{  struct namenode_s             symbol table entry*/
struct namenode_s
  {
    wordnode *n_name;           /* String representing name */
    struct tnode_s *n_type;     /* Type tree */
    struct tnode_s *n_decl;     /* Pointer to name declaration on parse tree */
    unsigned char n_mode;       /* Mode, ie. workspace/vecspace/pointer ... */
    unsigned char n_lexlev;     /* Lexical level of name declaration */
                                /* For any name, bit 7 set if it is used */
    BIT16 n_scope;              /* Scope of declaration */
    void *n_checker;            /* Used by usage checker */
    void *n_undef;		/* Used by undefinedness checker */
    void *n_fmcheck;		/* Used by formal-model checker */

#ifdef COMPILING_TO_JCODE
    void *n_binder;             /* Used for J_CODE creation */
    BOOL  n_addr_taken;         /* Used for J_CODE creation */
#endif

    /* The elements of the following union are arranged in such a silly
       order, because when I separated out the PROC stuff from the VAR stuff,
       I wanted to be sure I didn't break any hidden dependencies of their
       ordering.
       CO'N 3/7/90
    */
    int n_typeattr;		/* Used to put direction specifiers and shared (and claimed) on N_TYPEDECL nodes */
    union{
      /*{{{  n_proc - PROC / FUNCTION*/
      struct{ /* First we have the record for PROCs and FUNCTIONs */
        union{
          /*{{{  n_loc - local   PROC/FUNC*/
          struct {
            struct tnode_s *n_ctable;  /* pointer to constant table chain */
            INT32           n_cpoffset;/* WS offset of constant table */
            SOURCEPOSN      n_endposn; /* source position of colon */
          } n_loc;
          /*}}}*/
          /*{{{  n_lib - library PROC/FUNC*/
          struct {
            struct tnode_s *n_nextlib; /* pointer to next lib entry point */
            int             n_liboffset;/* Code offset for lib patch */
            void           *n_external;/* pointer to lib file data*/
          } n_lib;
          /*}}}*/
        } n_un1;
        int               n_label;     /* label of entry point */
	int               n_maplabel;  /* label of workspace map */
	void              *n_wsmap;    /* workspace-map (bind3.c) */
        BIT32             n_maxwsp;    /* Max workspace size */
        BIT32             n_datasize;  /* Below workspace size */
        BIT32             n_vsusage;   /* VS Usage */
      #ifdef MOBILES
        BIT32             n_msusage;   /* MS Usage */
        void              *n_msptr;    /* mobile allocation chain (bind3.c) */
      #endif
        int               n_params;    /* Parameter info */
        unsigned int      n_nestedpripar:1; /* Has a nested PRI PAR */
        unsigned int      n_nestedtimer:1;  /* Has a nested TIMER */
        unsigned int      n_nestedplace:1;  /* Has a nested PLACE */
        unsigned int      n_nestedport:1;   /* Has a nested PLACED PORT or PLACED CHAN */
        unsigned int      n_safefnresult:1; /* Cannot overwrite res ptrs */
      #ifdef OCCAM2_5
        unsigned int      n_constantfn:1;   /* Can be constant folded */
      #endif
        unsigned int      n_recursive:1;    /* Is something recursive */
	unsigned int      n_forks:1;	    /* Does something have free FORKs ? */
	unsigned int      n_suspends:1;	    /* whether something SUSPENDs */
	unsigned int      n_dyncall:1;      /* whether we must always DYNCALL something */
      } n_proc;
      /*}}}*/
      /*{{{  n_var  - variables and most others*/
      struct{ /* This is for variables (ie most others) */
        INT32             n_offset;    /* Workspace position*/
        union{
          INT32           n_vsoffset;  /* VS offset if mode = NM_VECSPACE (also MS offset if mode == NM_MOBILESPACE) */
          BOOL            n_replknown; /* whether value of N_REPL is known */
          BOOL            n_chanmark;  /* whether chan used by ASM (if type is scalar chan) */
        } n_un3;
        union{
          BIT32           n_usecount;  /* Usage count for ws allocation */
          BIT32           n_replval;   /* Value of replicator N_REPL*/
          BIT32           n_rsize;     /* Size of T_RESERVEDWS */
        } n_un4;
        unsigned int      n_shared  :1; /* Whether to usage check variable */
        unsigned int      n_aliased :1; /* Whether to alias check variable */
        unsigned int      n_rdefined:1; /* Whether to undefined-check variable (RESULT abbrs) */
        unsigned int      n_device  :1; /* Whether this is a device (PORT) */
        unsigned int      n_assumeconst:1; /* Whether this to allow as formal ASM param */
      #ifdef OCCAM2_5
        unsigned int      n_reshapes:1; /* Whether this is a RESHAPES rather than RETYPES */
      #endif
	unsigned int	  n_nameused:1;	/* Whether the name is actually used (for loopend optimisation) */
	unsigned int      n_indirect:1; /* Whether this is an indirect name, e.g. a pointer to it -- used for MOBILE PROCs / SUSPEND */
        SOURCEPOSN        n_endposn;    /* line of the process following the body of an abbreviation */
        union{
          /*{{{  n_front frontend only*/
          #if 0
          struct{ /* These are all frontend only */
            BOOL          n_shared;    /* Whether to usage check variable */
            BOOL          n_notaliased;/* Whether to alias check variable */
            BOOL          n_rdefined;  /* Whether to undefined-check variable */
          } n_front;
          #endif
          /*}}}*/
          /*{{{  n_back backend only*/
          struct { /* These are all backend only */
            struct tnode_s *n_shares;    /* Chain of vars with same ws offset */
            int             n_varnum;    /* Temporaries only */
            struct tnode_s *n_nexttemp;  /* Temporaries only */
            struct tnode_s *n_allocnext; /* used while allocating ws */
	    int             n_wsmaptag;  /* used when generating workspace maps */
          } n_back;
          /*}}}*/
        } n_un5;
      } n_var;
      /*}}}*/
      /*{{{  n_tag  - PROTOCOL tags*/
      struct{ /* This is for protocol tags */
        BIT32             n_tagval;    /* Tag value */
      } n_tag;
      /*}}}*/
    } n_un;

  };
/*}}}*/
/*{{{  struct tnode_s*/
struct tnode_s
  {
    tagtype_t tag;         /* Node tag */
    SOURCEPOSN lcn;        /* Source location corresponding to node */
#ifdef COMPILING_TO_JCODE
    union
      {
        void     *dbgplist;
        int      dbgtyperef;
      } dbg_u;
#endif
    /*{{{  union {...} n_u    union of possible node bodies*/
    union
      {
	struct leafnode_s        ln_s;		/* special for SUSPEND leafnode */
        struct actionnode_s      act_s;
        struct altnode_s         alt_s;
        struct arraysubnode_s    as_s;
        struct cnode_s           c_s;
        struct condnode_s        cnd_s;
        struct constexpnode_s    ce_s;
        struct consttablenode_s  ct_s;
        struct declnode_s        d_s;
        struct dopnode_s         dop_s;
    #if 0
        struct hiddenparamnode_s h_s;
    #endif
        struct instancenode_s    i_s;
        struct listnode_s        ls_s;
        struct litnode_s         l_s;
        struct mopnode_s         mop_s;
        struct namenode_s        n_s;
      /*struct overlapnode_s     o_s;*/ /* never used */
        struct processornode_s   p_s;
        struct replcnode_s       rc_s;
        struct segmentnode_s     s_s;
        struct spacenode_s       sp_s;
      /*struct tempnode_s        t_s;*/ /* never used */
        struct typenode_s        ar_s;
        struct valofnode_s       vl_s;
    #if 1 /* def CONFIG */
        struct confignode_s      cnf_s;
    #endif
    #ifdef CONDEXP
        struct condexpnode_s     cndexp_s;
    #endif
      } n_u;
    /*}}}*/
  };

typedef struct tnode_s treenode;

#define TREENODEBASE (offsetof(struct tnode_s, n_u))

/*}}}*/
/*{{{  nodetypeoftag_t*/
typedef enum {
  NONODE,
  ACTIONNODE,
  ALTNODE,
  ARRAYSUBNODE,
  CNODE,
  CONDNODE,
  CONSTEXPNODE,
  CONSTTABLENODE,
  DECLNODE,
  DOPNODE,
  INSTANCENODE,
  LEAFNODE,
  LISTNODE,
  LITNODE,
  MOPNODE,
  NAMENODE,
  PROCESSORNODE,
  REPLCNODE,
  SEGMENTNODE,
  SPACENODE,
  TYPENODE,
  VALOFNODE,
/*VARIANTNODE,*/
  WORDNODE,

#ifdef CONDEXP
  CONDEXPNODE,
#endif

/*  OVERLAPNODE,*/
/*  TEMPNODE,*/

#if 1 /* def CONFIG */
  CONFIGNODE
#endif
  } nodetypeoftag_t;
/*}}}*/
/*}}}*/
#endif
