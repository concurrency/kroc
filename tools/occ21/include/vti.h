/* $Id: vti.h,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	C virtual tree interface
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

/* ALL access to the parse tree should go through the virtual tree
   interface.
 */

/*{{{  keyword structure layout*/
typedef struct
  {
    int             s_tag;        /* lexical tag and/or node tag */
    const char     *s_keyword;    /* keyword, or NULL if not a keyword */
    const char     *s_tagstr;     /* 'user friendly' name */
    const char     *s_itagstr;    /* internal name */
    const char     *s_ftagstr;    /* data for error messages */
    nodetypeoftag_t s_nodetype;   /* Type of that treenode */
    int             s_valid_lang; /* valid language */
  } vti_keyword_t;

/*}}}*/

/*{{{  Node tags etc*/
void declkeywords(fe_lang_t fe_lang);

/*char *keywordstring (int t);*/ /* unused */
const char *tagstring (int t);
const char *itagstring (int t);
void  ftagstring(char *string, int t, const wordnode *lexword, int literalp, const char *literalv);
void init_nodetypeoftag(void);
nametypeoftag_t nametypeoftag (int t);

/*}}}*/

/*{{{  Macros to access treenodes*/
/*{{{  tag and location*/
#define TagOf(T)    ((T)->tag)
#define SetTag(T,V) ((T)->tag = (V))

#define LocnOf(T)    ((T)->lcn)
#define SetLocn(T,V) ((T)->lcn = (V))

#ifdef COMPILING_TO_JCODE
#define DbgPListOf(T) ((T)->dbg_u.dbgplist)
#define SetDbgPList(T, V) ((T)->dbg_u.dbgplist = (V))
#define DbgTypeRefOf(T) ((T)->dbg_u.dbgtyperef)
#define SetDbgTypeRef(T, V) ((T)->dbg_u.dbgtyperef = (V))
#endif
/*}}}*/

/*{{{  CHECKNODE, CHECKREAD and CHECKWORD*/
#ifdef CAN_USE_INLINE
extern const vti_keyword_t vti_keyword_table[];
extern int vti_max_tag;

/*{{{  inline nodetypeoftag_t nodetypeoftag*/
PRIVATE inline nodetypeoftag_t nodetypeoftag (const int t)
{
  if (t < 0 || t >= vti_max_tag) return NONODE;
  return vti_keyword_table[t].s_nodetype;
}
/*}}}*/
extern const treenode *checkread_fn (const treenode *const tptr, const nodetypeoftag_t nodetype, const char *const file, const int line);
extern treenode *checknode_fn (treenode *tptr, nodetypeoftag_t nodetype, const char *file, int line);

#if 0			/* don't want these inline regardless! */
/*{{{  inline       treenode *checknode_fn*/
PRIVATE inline treenode *checknode_fn(treenode *const tptr, const nodetypeoftag_t nodetype,
                         const char *const file, const int line)
{
  if (nodetypeoftag(TagOf(tptr)) != nodetype)
    {
      msg_out_is(SEV_INFO, ANY_MODULE, ANY_FILEINFO, NOPOSN, line, file);
      badtag(LocnOf(tptr), TagOf(tptr), "checknode_fn");
    }
  return tptr;
}
/*}}}*/
/*{{{  inline const treenode *checkread_fn*/
PRIVATE inline const treenode *checkread_fn(const treenode *const tptr, const nodetypeoftag_t nodetype,
                                            const char *const file, const int line)
{
  if (nodetypeoftag(TagOf(tptr)) != nodetype)
    {
      msg_out_is(SEV_INFO, ANY_MODULE, ANY_FILEINFO, NOPOSN, line, file);
      badtag(LocnOf(tptr), TagOf(tptr), "checkread_fn");
    }
  return tptr;
}
/*}}}*/
#endif
#else
/*{{{  extern declarations*/
nodetypeoftag_t nodetypeoftag (int t);
treenode       *checknode_fn(      treenode *tptr, nodetypeoftag_t nodetype, const char *file, int line);
const treenode *checkread_fn(const treenode *tptr, nodetypeoftag_t nodetype, const char *file, int line);
/*}}}*/
#endif

#ifdef CHECK_TREEACCESSES
  #define CHECKNODE(T, N) checknode_fn(T, N, __FILE__, __LINE__)
#else
  #define CHECKNODE(T, N) (T)
#endif

#ifdef CHECK_TREEACCESSES
  #define CHECKREAD(T, N) checkread_fn(T, N, __FILE__, __LINE__)
#else
  #define CHECKREAD(T, N) (T)
#endif

#define CHECKWORD(T, N) (T)
/*}}}*/

/*{{{  actionnode*/
#define LHSOf(T)              (CHECKREAD(T,ACTIONNODE)->n_u.act_s.act_lhs)
#define RHSOf(T)              (CHECKREAD(T,ACTIONNODE)->n_u.act_s.act_rhs)
#define ActionTypeOf(T)       (CHECKREAD(T,ACTIONNODE)->n_u.act_s.act_type)
#define ActionDuringOf(T)     (CHECKREAD(T,ACTIONNODE)->n_u.act_s.act_during)
#define ActionAfterOf(T)      (CHECKREAD(T,ACTIONNODE)->n_u.act_s.act_after)
#define ActionFlagsOf(T)      (CHECKREAD(T,ACTIONNODE)->n_u.act_s.act_flags)

#define SetLHS(T,V)           (CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_lhs  = (V))
#define SetRHS(T,V)           (CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_rhs  = (V))
#define SetActionType(T,V)    (CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_type = (V))
#define SetActionDuring(T,V)  (CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_during = (V))
#define SetActionAfter(T,V)   (CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_after = (V))
#define SetActionFlags(T,V)   (CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_flags = (V))

#define LHSAddr(T)            (&(CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_lhs))
#define RHSAddr(T)            (&(CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_rhs))
#define ActionTypeAddr(T)     (&(CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_type))
#define ActionDuringAddr(T)   (&(CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_during))
#define ActionAfterAddr(T)    (&(CHECKNODE(T,ACTIONNODE)->n_u.act_s.act_after))
/* no ActionFlagsAddr */

#define ActionFlag_default	(0x0)
#define ActionFlag_skip_xable	(0x1)		/* XABLE skip for extended rendezvous */
#define ActionFlag_decode	(0x2)		/* flag for DECODE.CHANNEL */
#define ActionFlag_encode	(0x4)		/* flag for ENCODE.CHANNEL */
#define ActionFlag_mobile	(0x8)		/* indicates a MOBILE action: for ENCODE/DECODE */
#define ActionFlag_dynmob	(0x10)		/* indicates a dynamic MOBILE (array) action: for ENCODE/DECODE */
#define ActionFlag_count	(0x20)		/* indicates the count part for a counted-array */
#define ActionFlag_case		(0x40)		/* indicates the CASE (BYTE or INT) for a tagged protocol */
#define ActionFlag_ed3seq	(0x80)		/* three INTs used for ENCODE.CHANNEL/DECODE.CHANNEL */
#define ActionFlag_edacomm	(0x100)		/* [2]INT or [3]INT link for ENCODE.CHANNEL/DECODE.CHANNEL */
#define ActionFlag_edmcomm	(0x200)		/* MOBILE [2]INT or MOBILE [3]INT link for ENCODE.CHANNEL/DECODE.CHANNEL */
#define ActionFlag_precount	(0x400)		/* indicates the count part got missed previously and we should send it with this one (ENCODE only) */
#define ActionFlag_mobproc	(0x800)		/* indicates a mobile process type: for ENCODE/DECODE */
#define ActionFlag_chantype	(0x1000)	/* indicates a mobile channel type: for ENCODE/DECODE */
#define ActionFlag_anychantypehash	(0x2000)	/* indicates communication (input) is a tag for an ANYCHANTYPE */

/*}}}*/
/*{{{  altnode*/
#define AltGuardOf(T)   (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_guard)
#define AltInputOf(T)   (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_input)
#define AltBodyOf(T)    (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_body)
#define AltChanExpOf(T) (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_chanexp)
#define AltTimeExpOf(T) (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_timeexp)
#define AltLabelOf(T)    (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_un.alt_label)
#define AltLabelPtrOf(T) (CHECKREAD(T,ALTNODE)->n_u.alt_s.alt_un.alt_labelptr)

#define SetAltGuard(T,V)   (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_guard = (V))
#define SetAltInput(T,V)   (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_input = (V))
#define SetAltBody(T,V)    (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_body = (V))
#define SetAltLabel(T,V)    (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_un.alt_label    = (V))
#define SetAltLabelPtr(T,V) (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_un.alt_labelptr = (V))
#define SetAltChanExp(T,V) (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_chanexp = (V))
#define SetAltTimeExp(T,V) (CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_timeexp = (V))

#define AltGuardAddr(T)   (&(CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_guard))
#define AltInputAddr(T)   (&(CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_input))
#define AltBodyAddr(T)    (&(CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_body))
#define AltChanExpAddr(T) (&(CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_chanexp))
#define AltTimeExpAddr(T) (&(CHECKNODE(T,ALTNODE)->n_u.alt_s.alt_timeexp))

/*}}}*/
/*{{{  arraysubnode*/
/* The following field names are used by both S_ARRAYSUB and S_ARRAYITEM */
#define ASBaseOf(T)        (CHECKREAD(T,ARRAYSUBNODE)->n_u.as_s.as_base)
#define ASIndexOf(T)       (CHECKREAD(T,ARRAYSUBNODE)->n_u.as_s.as_index)
#define ASExpOf(T)         (CHECKREAD(T,ARRAYSUBNODE)->n_u.as_s.as_exp)    /* S_ARRAYITEM only */
#define ASOffsetOf(T)      (CHECKREAD(T,ARRAYSUBNODE)->n_u.as_s.as_offset) /* S_ARRAYITEM only */
#define ASLengthOf(T)      (CHECKREAD(T,ARRAYSUBNODE)->n_u.as_s.as_length) /* S_ARRAYITEM only */

#define SetASBase(T,V)     (CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_base = (V))
#define SetASIndex(T,V)    (CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_index = (V))
#define SetASExp(T,V)      (CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_exp = (V))
#define SetASOffset(T,V)   (CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_offset = (V))
#define SetASLength(T,V)   (CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_length = (V))

#define ASBaseAddr(T)    (&(CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_base))
#define ASIndexAddr(T)   (&(CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_index))
#define ASExpAddr(T)     (&(CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_exp))
#define ASOffsetAddr(T)  (&(CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_offset))
#define ASLengthAddr(T)  (&(CHECKNODE(T,ARRAYSUBNODE)->n_u.as_s.as_length))

/*}}}*/
/*{{{  cnode*/
#define CBodyOf(T)      (CHECKREAD(T,CNODE)->n_u.c_s.c_body)
#define CTempOf(T)      (CHECKREAD(T,CNODE)->n_u.c_s.c_temp)

#define SetCBody(T,V)   (CHECKNODE(T,CNODE)->n_u.c_s.c_body = (V))
#define SetCTemp(T,V)   (CHECKNODE(T,CNODE)->n_u.c_s.c_temp = (V))

#define CBodyAddr(T)  (&(CHECKNODE(T,CNODE)->n_u.c_s.c_body))
#define CTempAddr(T)  (&(CHECKNODE(T,CNODE)->n_u.c_s.c_temp))
/*}}}*/
/*{{{  condnode*/
#define CondGuardOf(T)     (CHECKREAD(T,CONDNODE)->n_u.cnd_s.cnd_guard)
#define CondBodyOf(T)      (CHECKREAD(T,CONDNODE)->n_u.cnd_s.cnd_body)
#define CondAfterOf(T)     (CHECKREAD(T,CONDNODE)->n_u.cnd_s.cnd_after)

#define SetCondGuard(T,V)  (CHECKNODE(T,CONDNODE)->n_u.cnd_s.cnd_guard = (V))
#define SetCondBody(T,V)   (CHECKNODE(T,CONDNODE)->n_u.cnd_s.cnd_body = (V))
#define SetCondAfter(T,V)  (CHECKNODE(T,CONDNODE)->n_u.cnd_s.cnd_after = (V))

#define CondGuardAddr(T) (&(CHECKNODE(T,CONDNODE)->n_u.cnd_s.cnd_guard))
#define CondBodyAddr(T)  (&(CHECKNODE(T,CONDNODE)->n_u.cnd_s.cnd_body))
#define CondAfterAddr(T) (&(CHECKNODE(T,CONDNODE)->n_u.cnd_s.cnd_after))

#define VRTaggedListOf(T)    CondGuardOf(T)
#define VRBodyOf(T)          CondBodyOf(T)
#define VRDuringOf(T)        VRBodyOf(T)
#define VRAfterOf(T)         CondAfterOf(T)

#define SetVRTaggedList(T,V) SetCondGuard(T,V)
#define SetVRBody(T,V)       SetCondBody(T,V)
#define SetVRDuring(T,V)     SetVRBody(T,V)
#define SetVRAfter(T,V)      SetCondAfter(T,V)

#define VRTaggedListAddr(T)  CondGuardAddr(T)
#define VRBodyAddr(T)        CondBodyAddr(T)
#define VRDuringAddr(T)      VRBodyAddr(T)
#define VRAfterAddr(T)       CondAfterAddr(T)

/*}}}*/
#ifdef CONDEXP
/*{{{  condexpnode*/
#define CondExpGuardOf(T)    (CHECKREAD(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_guard)
#define CondExpTrueOf(T)     (CHECKREAD(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_true)
#define CondExpFalseOf(T)    (CHECKREAD(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_false)
#define CondExpTypeOf(T)     (CHECKREAD(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_type)

#define SetCondExpGuard(T,V)   (CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_guard = (V))
#define SetCondExpTrue(T,V)    (CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_true = (V))
#define SetCondExpFalse(T,V)   (CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_false = (V))
#define SetCondExpType(T,V)    (CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_type = (V))

#define CondExpGuardAddr(T) (&(CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_guard))
#define CondExpTrueAddr(T)  (&(CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_true))
#define CondExpFalseAddr(T) (&(CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_false))
#define CondExpTypeAddr(T)  (&(CHECKNODE(T,CONDEXPNODE)->n_u.cndexp_s.cndexp_type))
/*}}}*/
#endif
#if 1 /*def CONFIG*/
/*{{{  confignode*/
#define ConnectFromEdgeOf(T)     (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_a)
#define ConnectToEdgeOf(T)       (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_b)
#define ConnectArcOf(T)          (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_c)

#define SetConnectFromEdge(T, V) (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_a = (V))
#define SetConnectToEdge(T, V)   (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_b = (V))
#define SetConnectArc(T, V)      (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_c = (V))

#define ConnectFromEdgeAddr(T) (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_a))
#define ConnectToEdgeAddr(T)   (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_b))
#define ConnectArcAddr(T)      (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_c))

#define MapSourceOf(T)           (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_a)
#define MapDestOf(T)             (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_b)
#define MapPriOf(T)              (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_c)

#define SetMapSource(T,V)        (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_a = (V))
#define SetMapDest(T,V)          (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_b = (V))
#define SetMapPri(T,V)           (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_c = (V))

#define MapSourceAddr(T)       (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_a))
#define MapDestAddr(T)         (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_b))
#define MapPriAddr(T)          (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_c))

#define STDevOf(T)               (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_a)
#define STAttrNameOf(T)          (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_b)
#define STAttrExpOf(T)           (CHECKREAD(T,CONFIGNODE)->n_u.cnf_s.cnf_c)

#define SetSTDev(T, V)           (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_a = (V))
#define SetSTAttrName(T, V)      (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_b = (V))
#define SetSTAttrExp(T, V)       (CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_c = (V))

#define STDevAddr(T)           (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_a))
#define STAttrNameAddr(T)      (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_b))
#define STAttrExpAddr(T)       (&(CHECKNODE(T,CONFIGNODE)->n_u.cnf_s.cnf_c))
/*}}}*/
#endif
/*{{{  constexpnode*/
#define CExpOf(T)     (CHECKREAD(T,CONSTEXPNODE)->n_u.ce_s.ce_exp)
#define HiValOf(T)    (CHECKREAD(T,CONSTEXPNODE)->n_u.ce_s.ce_vhi)
#define LoValOf(T)    (CHECKREAD(T,CONSTEXPNODE)->n_u.ce_s.ce_vlo)
#define CENextOf(T)   (CHECKREAD(T,CONSTEXPNODE)->n_u.ce_s.ce_next)
#define CEOffsetOf(T) (CHECKREAD(T,CONSTEXPNODE)->n_u.ce_s.ce_offset)

#define SetCExp(T,V)     (CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_exp = (V))
#define SetHiVal(T,V)    (CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_vhi = (V))
#define SetLoVal(T,V)    (CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_vlo = (V))
#define SetCENext(T,V)   (CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_next = (V))
#define SetCEOffset(T,V) (CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_offset = (V))

#define CExpAddr(T)     (&(CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_exp))
#define HiValAddr(T)    (&(CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_vhi))
#define LoValAddr(T)    (&(CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_vlo))
#define CENextAddr(T)   (&(CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_next))
#define CEOffsetAddr(T) (&(CHECKNODE(T,CONSTEXPNODE)->n_u.ce_s.ce_offset))
/*}}}*/
/*{{{  consttablenode*/
#define CTValOf(T)     (CHECKREAD(T,CONSTTABLENODE)->n_u.ct_s.ct_un.ct_val)
#define CTPtrOf(T)     (CHECKREAD(T,CONSTTABLENODE)->n_u.ct_s.ct_un.ct_ptr)
#define CTExpOf(T)     (CHECKREAD(T,CONSTTABLENODE)->n_u.ct_s.ct_exp)
#define CTNextOf(T)    (CHECKREAD(T,CONSTTABLENODE)->n_u.ct_s.ct_next)
#define ConstTableTypeOf(T) (CHECKREAD(T,CONSTTABLENODE)->n_u.ct_s.ct_type)
#define CTLabelOf(T)   (CHECKREAD(T,CONSTTABLENODE)->n_u.ct_s.ct_label)

#define SetCTVal(T,V)     (CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_un.ct_val = (V))
#define SetCTPtr(T,V)     (CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_un.ct_ptr = (V))
#define SetCTExp(T,V)     (CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_exp   = (V))
#define SetCTNext(T,V)    (CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_next  = (V))
#define SetConstTableType(T,V)  (CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_type  = (V))
#define SetCTLabel(T,V)   (CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_label = (V))

#define CTValAddr(T)   (&(CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_un.ct_val))
#define CTExpAddr(T)   (&(CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_exp))
#define CTNextAddr(T)  (&(CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_next))
#define ConstTableTypeAddr(T)  (&(CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_type))
#define CTLabelAddr(T) (&(CHECKNODE(T,CONSTTABLENODE)->n_u.ct_s.ct_label))

/*}}}*/
/*{{{  declnode*/
#define DNameOf(T)    (CHECKREAD(T,DECLNODE)->n_u.d_s.d_nameptr)
#define DValOf(T)     (CHECKREAD(T,DECLNODE)->n_u.d_s.d_v)
#define DBodyOf(T)    (CHECKREAD(T,DECLNODE)->n_u.d_s.d_body)
#define DExtraOf(T)   (CHECKREAD(T,DECLNODE)->n_u.d_s.d_extra)

#define SetDName(T,V)    (CHECKNODE(T,DECLNODE)->n_u.d_s.d_nameptr = (V))
#define SetDVal(T,V)     (CHECKNODE(T,DECLNODE)->n_u.d_s.d_v = (V))
#define SetDBody(T,V)    (CHECKNODE(T,DECLNODE)->n_u.d_s.d_body = (V))
#define SetDExtra(T,V)   (CHECKNODE(T,DECLNODE)->n_u.d_s.d_extra = (V))

#define DNameAddr(T)  (&(CHECKNODE(T,DECLNODE)->n_u.d_s.d_nameptr))
#define DValAddr(T)   (&(CHECKNODE(T,DECLNODE)->n_u.d_s.d_v))
#define DBodyAddr(T)  (&(CHECKNODE(T,DECLNODE)->n_u.d_s.d_body))
#define DExtraAddr(T) (&(CHECKNODE(T,DECLNODE)->n_u.d_s.d_extra))

#define PlaceExpOf(T)     (CHECKREAD(T,DECLNODE)->n_u.d_s.d_v)
#define SetPlaceExp(T,V)  (CHECKNODE(T,DECLNODE)->n_u.d_s.d_v = (V))
#define PlaceExpAddr(T) (&(CHECKNODE(T,DECLNODE)->n_u.d_s.d_v))
/*}}}*/
/*{{{  dopnode*/
#define LeftOpOf(T)     (CHECKREAD(T,DOPNODE)->n_u.dop_s.dop_leftop)
#define RightOpOf(T)    (CHECKREAD(T,DOPNODE)->n_u.dop_s.dop_rightop)
#define DOpTypeOf(T)    (CHECKREAD(T,DOPNODE)->n_u.dop_s.dop_type)

#define SetLeftOp(T,V)     (CHECKNODE(T,DOPNODE)->n_u.dop_s.dop_leftop = (V))
#define SetRightOp(T,V)    (CHECKNODE(T,DOPNODE)->n_u.dop_s.dop_rightop = (V))
#define SetDOpType(T,V)    (CHECKNODE(T,DOPNODE)->n_u.dop_s.dop_type = (V))

#define LeftOpAddr(T)   (&(CHECKNODE(T,DOPNODE)->n_u.dop_s.dop_leftop))
#define RightOpAddr(T)  (&(CHECKNODE(T,DOPNODE)->n_u.dop_s.dop_rightop))
#define DOpTypeAddr(T)  (&(CHECKNODE(T,DOPNODE)->n_u.dop_s.dop_type))

/*}}}*/
/*{{{  hiddenparamnode*/
#if 0
#define HExpOf(T)       (CHECKREAD(T,HIDDENPARAMNODE)->n_u.h_s.h_exp)
#define HDimensionOf(T) ((int)(CHECKREAD(T,HIDDENPARAMNODE)->n_u.h_s.h_dimension))
/* When the hidden parameter is a function result pointer, this field
   contains the workspace offset of the parameter */
#define HOffsetOf(T)    (CHECKREAD(T,HIDDENPARAMNODE)->n_u.h_s.h_dimension)
#define HLexLevelOf(T)  (CHECKREAD(T,HIDDENPARAMNODE)->n_u.h_s.h_lexlevel)
#define HArgOf(T)       (CHECKREAD(T,HIDDENPARAMNODE)->n_u.h_s.h_arg)

#ifdef COMPILING_TO_JCODE
/* "Binder" is a type defined in the J-Code stuff */
#define HBinderOf(T)    ((Binder *)(CHECKREAD(T,HIDDENPARAMNODE)->n_u.h_s.h_binder))
#endif

#define SetHExp(T,V)       (CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_exp = (V))
#define SetHDimension(T,V) (CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_dimension = (BIT32)(V))
#define SetHOffset(T,V)    (CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_dimension = (V))
#define SetHLexLevel(T,V)  (CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_lexlevel = (V))
#define SetHArg(T,V)       (CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_arg = (V))

#ifdef COMPILING_TO_JCODE
#define SetHBinder(T,V)    (CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_binder = (void *)(V))
#endif

#define HExpAddr(T)       (&(CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_exp))
#define HDimensionAddr(T) (&(CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_dimension))
#define HOffsetAddr(T)    (&(CHECKNODE(T,HIDDENPARAMNODE)->n_u.h_s.h_dimension))
#endif
/*}}}*/
/*{{{  instancenode*/
#define INameOf(T)      	(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_nameptr)
#define IParamListOf(T) 	(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_paramlist)
#define ILoadSeqOf(T)   	(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_loadseq)
#define IRecursiveOf(T)		(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_rinstance)
#define IForkOf(T)		(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_fork)
#define IForkedOf(T)		(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_forked)
#define IDynmemOf(T)		(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_dynmem)
#define IDynaddrOf(T)		(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_dynaddr)
#define IRPSlotsOf(T)		(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_rparamslots)
#ifdef MOBILES
#define IRMSPOffsetOf(T)	(CHECKREAD(T,INSTANCENODE)->n_u.i_s.i_rmspoffset)
#endif

#define SetIName(T,V)		(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_nameptr = (V))
#define SetIParamList(T,V)	(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_paramlist = (V))
#define SetILoadSeq(T,V)	(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_loadseq = (V))
#define SetIRecursive(T,V)	(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_rinstance = (V))
#define SetIFork(T,V)		(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_fork = (V))
#define SetIForked(T,V)		(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_forked = (V))
#define SetIDynmem(T,V)		(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_dynmem = (V))
#define SetIDynaddr(T,V)	(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_dynaddr = (V))
#define SetIRPSlots(T,V)	(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_rparamslots = (V))
#ifdef MOBILES
#define SetIRMSPOffset(T,V)	(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_rmspoffset = (V))
#endif

#define INameAddr(T)      (&(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_nameptr))
#define IParamListAddr(T) (&(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_paramlist))
#define ILoadSeqAddr(T)   (&(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_loadseq))
#define IDynaddrAddr(T)   (&(CHECKNODE(T,INSTANCENODE)->n_u.i_s.i_dynaddr))
/*}}}*/
/*{{{  listnode*/
#define LeftOf(T)       (CHECKREAD(T,LISTNODE)->n_u.ls_s.ls_left)
#define RightOf(T)      (CHECKREAD(T,LISTNODE)->n_u.ls_s.ls_right)

#define SetLeft(T,V)       (CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_left = (V))
#define SetRight(T,V)      (CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_right = (V))

#define LeftAddr(T)     (&(CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_left))
#define RightAddr(T)    (&(CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_right))

#define FnTypeListOf(T)  (CHECKREAD(T,LISTNODE)->n_u.ls_s.ls_left)
#define FnParamsOf(T)    (CHECKREAD(T,LISTNODE)->n_u.ls_s.ls_right)

#define SetFnTypeList(T,V)  (CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_left = (V))
#define SetFnParams(T,V)    (CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_right = (V))

#define FnTypeListAddr(T) (&(CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_left))
#define FnParamsAddr(T)   (&(CHECKNODE(T,LISTNODE)->n_u.ls_s.ls_right))

/*}}}*/
/*{{{  litnode*/
/*{{{  COMMENT*/
/*Added by Jim 6/3/97 access macros:*/
/*LitUnspecOf()*/
/*SetLitUnspec()*/
/*to hold a boolean flag to allow the user defined operator code in chk1.c to*/
/*know if a type was explicitly specified, or the type was resolved.  This is*/
/*used to discard the resolved type in favour of the basic type (INT, BYTE,*/
/*REAL32) when generating operator function names - but keeping the resolved*/
/*types if the operator in question turns out to be not overloaded.*/
/*}}}*/
#define LitTypeOf(T)          (CHECKREAD(T,LITNODE)->n_u.l_s.l_type)
#define LitExpOf(T)           (CHECKREAD(T,LITNODE)->n_u.l_s.l_exp)
#define StringPtrOf(T)        ((wordnode *)LitExpOf(T))
#define LitUnspecOf(T)        (CHECKREAD(T,LITNODE)->n_u.l_s.l_unspec)

#define SetLitType(T,V)       (CHECKNODE(T,LITNODE)->n_u.l_s.l_type = (V))
#define SetLitExp(T,V)        (CHECKNODE(T,LITNODE)->n_u.l_s.l_exp  = (V))
#define SetStringPtr(T,V)     SetLitExp(T, (treenode *)(V))
#define SetLitUnspec(T,V)     (CHECKNODE(T,LITNODE)->n_u.l_s.l_unspec  = (V))

#define LitTypeAddr(T)        (&(CHECKNODE(T,LITNODE)->n_u.l_s.l_type))
#define LitExpAddr(T)         (&(CHECKNODE(T,LITNODE)->n_u.l_s.l_exp))

/*}}}*/
/*{{{  mopnode*/
#define OpOf(T)               (CHECKREAD(T,MOPNODE)->n_u.mop_s.mop_operand)
#define MOpTypeOf(T)          (CHECKREAD(T,MOPNODE)->n_u.mop_s.mop_type)
#define OpTypeAttrOf(T)       (CHECKREAD(T,MOPNODE)->n_u.mop_s.mop_typeattr)

#define SetOp(T,V)            (CHECKNODE(T,MOPNODE)->n_u.mop_s.mop_operand = (V))
#define SetMOpType(T,V)       (CHECKNODE(T,MOPNODE)->n_u.mop_s.mop_type = (V))
#define SetOpTypeAttr(T,V)    (CHECKNODE(T,MOPNODE)->n_u.mop_s.mop_typeattr = (V))

#define OpAddr(T)             (&(CHECKNODE(T,MOPNODE)->n_u.mop_s.mop_operand))
#define MOpTypeAddr(T)        (&(CHECKNODE(T,MOPNODE)->n_u.mop_s.mop_type))

/*}}}*/
#if 0 /* never used */
/*{{{  overlapnode*/
#define OBase1Of(T)            (CHECKREAD(T,OVERLAPNODE)->n_u.o_s.o_base1)
#define OCount1Of(T)           (CHECKREAD(T,OVERLAPNODE)->n_u.o_s.o_count1)
#define OBase2Of(T)            (CHECKREAD(T,OVERLAPNODE)->n_u.o_s.o_base2)
#define OCount2Of(T)           (CHECKREAD(T,OVERLAPNODE)->n_u.o_s.o_count2)

#define SetOBase1(T,V)         (CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_base1 = (V))
#define SetOCount1(T,V)        (CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_count1 = (V))
#define SetOBase2(T,V)         (CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_base2 = (V))
#define SetOCount2(T,V)        (CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_count2 = (V))

#define OBase1Addr(T)          (&(CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_base1))
#define OCount1Addr(T)         (&(CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_count1))
#define OBase2Addr(T)          (&(CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_base2))
#define OCount2Addr(T)         (&(CHECKNODE(T,OVERLAPNODE)->n_u.o_s.o_count2))
/*}}}*/
#endif
/*{{{  namenode*/

/* namenodes are defined in nameshdr.h */

/*}}}*/
/*{{{  processornode*/
#define ProcessorExpOf(T)      (CHECKREAD(T,PROCESSORNODE)->n_u.p_s.p_exp)
#define ProcessorTypeOf(T)     (CHECKREAD(T,PROCESSORNODE)->n_u.p_s.p_type)
#define ProcessorBodyOf(T)     (CHECKREAD(T,PROCESSORNODE)->n_u.p_s.p_body)
#define ProcessorScopeOf(T)    (CHECKREAD(T,PROCESSORNODE)->n_u.p_s.p_scope)

#define SetProcessorExp(T,V)   (CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_exp = (V))
#define SetProcessorType(T,V)  (CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_type = (V))
#define SetProcessorBody(T,V)  (CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_body = (V))
#define SetProcessorScope(T,V) (CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_scope = (V))

#define ProcessorExpAddr(T)    (&(CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_exp))
#define ProcessorTypeAddr(T)   (&(CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_type))
#define ProcessorBodyAddr(T)   (&(CHECKNODE(T,PROCESSORNODE)->n_u.p_s.p_body))
/*}}}*/
/*{{{  replcnode / arrayconstructor*/
#define ReplCNameOf(T)         (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_nameptr)
#define ReplCStartExpOf(T)     (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_startexp)
#define ReplCLengthExpOf(T)    (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_lengthexp)
#define ReplCStepExpOf(T)      (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_stepexp)
#define ReplCBodyOf(T)         (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_body)
#define ReplCTempOf(T)         (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_temp)
#define ReplCRAltStartOf(T)    (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_ra_start)
#define ReplCRAltStepOf(T)     (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_ra_step)

#define SetReplCName(T,V)      (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_nameptr = (V))
#define SetReplCStartExp(T,V)  (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_startexp = (V))
#define SetReplCLengthExp(T,V) (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_lengthexp = (V))
#define SetReplCStepExp(T,V)   (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_stepexp = (V))
#define SetReplCBody(T,V)      (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_body = (V))
#define SetReplCTemp(T,V)      (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_temp = (V))
#define SetReplCRAltStart(T,V) (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_ra_start = (V))
#define SetReplCRAltStep(T,V)  (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_ra_step = (V))

#define ReplCNameAddr(T)       (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_nameptr))
#define ReplCStartExpAddr(T)   (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_startexp))
#define ReplCLengthExpAddr(T)  (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_lengthexp))
#define ReplCStepExpAddr(T)    (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_stepexp))
#define ReplCBodyAddr(T)       (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_body))
#define ReplCTempAddr(T)       (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_temp))
#define ReplCRAltStartAddr(T)  (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_ra_start))
#define ReplCRAltStepAddr(T)   (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_ra_step))

/* These are the names for a array constructor node */

#define ACNameOf(T)      (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_nameptr)
#define ACStartExpOf(T)  (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_startexp)
#define ACLengthExpOf(T) (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_lengthexp)
#define ACValExpOf(T)    (CHECKREAD(T,REPLCNODE)->n_u.rc_s.rc_body)

#define SetACName(T,V)      (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_nameptr = (V))
#define SetACStartExp(T,V)  (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_startexp = (V))
#define SetACLengthExp(T,V) (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_lengthexp = (V))
#define SetACValExp(T,V)    (CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_body = (V))

#define ACNameAddr(T)      (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_nameptr))
#define ACStartExpAddr(T)  (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_startexp))
#define ACLengthExpAddr(T) (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_lengthexp))
#define ACValExpAddr(T)    (&(CHECKNODE(T,REPLCNODE)->n_u.rc_s.rc_body))
/*}}}*/
/*{{{  segmentnode*/
#define SNameOf(T)            (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_nptr)
#define SStartExpOf(T)        (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_startexp)
#define SLengthExpOf(T)       (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_lengthexp)
#define SSubscriptExpOf(T)    (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_subscriptexp)
#define SCheckExpOf(T)        (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_checkexp)
#define SOffsetOf(T)          (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_offset)
#define SLengthOf(T)          (CHECKREAD(T,SEGMENTNODE)->n_u.s_s.s_length)

#define SetSName(T,V)         (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_nptr = (V))
#define SetSStartExp(T,V)     (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_startexp = (V))
#define SetSLengthExp(T,V)    (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_lengthexp = (V))
#define SetSSubscriptExp(T,V) (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_subscriptexp = (V))
#define SetSCheckExp(T,V)     (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_checkexp = (V))
#define SetSOffset(T,V)       (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_offset = (V))
#define SetSLength(T,V)       (CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_length = (V))

#define SNameAddr(T)          (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_nptr))
#define SStartExpAddr(T)      (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_startexp))
#define SLengthExpAddr(T)     (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_lengthexp))
#define SSubscriptExpAddr(T)  (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_subscriptexp))
#define SCheckExpAddr(T)      (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_checkexp))
#define SOffsetAddr(T)        (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_offset))
#define SLengthAddr(T)        (&(CHECKNODE(T,SEGMENTNODE)->n_u.s_s.s_length))
/*}}}*/
/*{{{  spacenode*/
/* The Namechain field is used by the workspace allocator, (either as a list
   of names declared in the process, or a list of constant tables in the
   process if the process is a  replicated PAR body)
   and reused by the code generator as a label. */
#define SpBodyOf(T)        (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_body)
#define SpMaxwspOf(T)      (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_maxwsp)
#define SpDatasizeOf(T)    (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_datasize)
#define SpVSUsageOf(T)     (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_vsusage)
#ifdef MOBILES
#define SpMSUsageOf(T)     (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_msusage)
#define SpNestedMSOf(T)    (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_nestedms)
#define SpMSPtrOf(T)       (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_msptr)
#define SpWSMapOf(T)       (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_wsmap)
#define SpMapLabelOf(T)    (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_maplabel)
#endif
#define SpNestedVSOf(T)    (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_nestedvs)
#define SpNamechainOf(T)   (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_namechain)
#define SpLabelOf(T)       ((int)CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_namechain)
#define SpCPOffsetOf(T)    (CHECKREAD(T,SPACENODE)->n_u.sp_s.sp_cpoffset)

#define SetSpBody(T,V)        (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_body = (V))
#define SetSpMaxwsp(T,V)      (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_maxwsp = (V))
#define SetSpDatasize(T,V)    (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_datasize = (V))
#define SetSpVSUsage(T,V)     (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_vsusage = (V))
#ifdef MOBILES
#define SetSpMSUsage(T,V)     (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_msusage = (V))
#define SetSpNestedMS(T,V)    (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_nestedms = (V))
#define SetSpMSPtr(T,V)       (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_msptr = (V))
#define SetSpWSMap(T,V)       (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_wsmap = (V))
#define SetSpMapLabel(T,V)    (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_maplabel = (V))
#endif
#define SetSpNestedVS(T,V)    (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_nestedvs = (V))
#define SetSpNamechain(T,V)   (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_namechain = (V))
#define SetSpLabel(T,V)       (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_namechain = (treenode *)(V))
#define SetSpCPOffset(T,V)    (CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_cpoffset = (V))

#define SpBodyAddr(T)        (&(CHECKNODE(T,SPACENODE)->n_u.sp_s.sp_body))
/*}}}*/
/*{{{  typenode*/
#define ARDimLengthOf(T)    (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_dimlength)
#define ARTypeOf(T)         (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_type)
#define ARDimOf(T)          (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_dim)
#define TypeAttrOf(T)       (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_attr)
#define ProtocolOf(T)       ARTypeOf(T)
#define MTypeOf(T)          ARTypeOf(T)
#define TypeTracesOf(T)     (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_traces)
#define MTDLabOf(T)         (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_tdlab)
#define ARAlignmentOf(T)    (CHECKREAD(T,TYPENODE)->n_u.ar_s.ar_alignment)

#define SetARDimLength(T,V) (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_dimlength = (V))
#define SetARType(T,V)      (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_type = (V))
#define SetARDim(T,V)       (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_dim = (V))
#define SetTypeAttr(T,V)    (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_attr = (V))
#define SetProtocol(T,V)    SetARType(T,V)
#define SetMType(T,V)       SetARType(T,V)
#define SetTypeTraces(T,V)  (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_traces = (V))
#define SetMTDLab(T,V)      (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_tdlab = (V))
#define SetARAlignment(T,V) (CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_alignment = (V))

#define ARDimLengthAddr(T) (&(CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_dimlength))
#define ARTypeAddr(T)      (&(CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_type))
#define ARDimAddr(T)       (&(CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_dim))
#define ProtocolAddr(T)    ARTypeAddr(T)
#define MTypeAddr(T)       ARTypeAddr(T)
#define TypeTracesAddr(T)  (&(CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_traces))
#define MTDLabAddr(T)      (&(CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_tdlab))
#define ARAlignmentAddr(T) (&(CHECKNODE(T,TYPENODE)->n_u.ar_s.ar_alignment))

/* Bit patterns to be OR-ed together: */
#define TypeAttr_default    (0x0)
#define TypeAttr_datatype   (0x1)  /* DATA RECORDs */
#define TypeAttr_packed     (0x2)  /* PACKED RECORDs */
#define TypeAttr_chantype   (0x4)  /* CHAN RECORDs */
#define TypeAttr_wordlen    (0x8)  /* RECORD layout is sensitive to word length */
#define TypeAttr_marked_in  (0x10) /* for channels marked as input */
#define TypeAttr_marked_out (0x20) /* for channels marked as output */
#define TypeAttr_placed     (0x40) /* for channels PLACEd, which we need to handle differently */
#define TypeAttr_iospace    (0x80) /* for arrays PLACEd in io-space, which we need to output special instructions to the translator for.. */
#define TypeAttr_dynmobile (0x100) /* for dynamic MOBILE arrays, set at the MOBILE bit */
#define TypeAttr_shared    (0x200) /* for shared CHAN TYPE variables */
#define TypeAttr_claimed   (0x400) /* used on (variable) names during use4 checking to make sure CLAIM blocks are used properly */
#define TypeAttr_recursive (0x800) /* used on MOBILE nodes to indicate recursive types */
#define TypeAttr_undefined (0x1000)	/* used on formal reference parameters to indicate that PROC leaves it undefined */
#define TypeAttr_fixed     (0x2000)	/* used on N_TPROTDEF nodes to indicate that they're "fixed", and for individual values on N_TAGDEF nodes
					 * also used on MOBILE formal paramter nodes (N_PARAM, ..) to indicate that they don't get "moved" */
#define TypeAttr_ufixed    (0x4000)	/* used on MOBILE formal parameter nodes (N_PARAM, ..) to indicate a user-specified "FIXED" */
#define TypeAttr_resigned  (0x8000)	/* used on BARRIERs during type-check to indicate it's been resigned (i.e. may not be used) */
#define TypeAttr_aligned   (0x10000)    /* used on MOBILE arrays, to indicate alignment set */
#define TypeAttr_dma       (0x20000)    /* used on MOBILE arrays, to request DMA capable memory */
#define TypeAttr_empty     (0x40000)	/* used on MOBILE arrays, no data memory allocation */

/*}}}*/
/*{{{  valofnode*/
#define VLBodyOf(T)       (CHECKREAD(T,VALOFNODE)->n_u.vl_s.vl_body)
#define VLResultListOf(T) (CHECKREAD(T,VALOFNODE)->n_u.vl_s.vl_resultlist)

#define SetVLBody(T,V)       (CHECKNODE(T,VALOFNODE)->n_u.vl_s.vl_body = (V))
#define SetVLResultList(T,V) (CHECKNODE(T,VALOFNODE)->n_u.vl_s.vl_resultlist = (V))

#define VLBodyAddr(T)       (&(CHECKNODE(T,VALOFNODE)->n_u.vl_s.vl_body))
#define VLResultListAddr(T) (&(CHECKNODE(T,VALOFNODE)->n_u.vl_s.vl_resultlist))
/*}}}*/
/*{{{  wordnode*/
#define WTagOf(N)         (CHECKWORD(N,WORDNODE)->w_tag)
#define WNameOf(N)        (CHECKWORD(N,WORDNODE)->w_name)
#define WLengthOf(N)      (CHECKWORD(N,WORDNODE)->w_length)
#define WNextOf(N)        (CHECKWORD(N,WORDNODE)->w_next)

#define SetWTag(N,V)      (CHECKWORD(N,WORDNODE)->w_tag = (V))
#define SetWName(N,V)     (CHECKWORD(N,WORDNODE)->w_name = (V))
#define SetWLength(N,V)   (CHECKWORD(N,WORDNODE)->w_length = (V))
#define SetWNext(N,V)     (CHECKWORD(N,WORDNODE)->w_next = (V))

#define WNameAddr(N)      (&(CHECKWORD(N,WORDNODE)->w_name))
/*}}}*/
/*{{{  leafnode*/
#define LeafLinkOf(T)        (CHECKREAD(T,LEAFNODE)->n_u.ln_s.link)
#define SetLeafLink(T,V)     (CHECKNODE(T,LEAFNODE)->n_u.ln_s.link = (V))
#define LeafLinkAddr(T)	     (&(CHECKNODE(T,LEAFNODE)->n_u.ln_s.link))
/*}}}*/
/*}}}*/

/*{{{  Memory allocation*/
/*extern FILE *vti_outfile;*/ /* This is actually local to the vti routines */
extern INT32 vti_no_slot_value; /* ditto */

void vtiinit   (FILE *fptr, INT32 no_slot_value);
void vtifinish (FILE *fptr);
void set_vtilocn(SOURCEPOSN *locn_ptr);

void *memalloc (size_t size);
void memfree (void *ptr);
void *newvec (size_t n);

int  switch_to_temp_workspace (void);
int  switch_to_real_workspace (void);
void switch_to_prev_workspace (int old);
void freeup_temp_workspace (void);

#if 1 /*def CONFIG*/
void freeup_all_workspace (void);
void freevec (void *ptr, size_t size);
void freenode (treenode **tptr);
void freetree (treenode **tptr);
#endif

long tablesize (void);

#if 0 /*def CONFIG*/
void markws (void);
void freews (void);
#endif
/*}}}*/
/*{{{  New nodes*/
treenode *newactionnode  (int t, SOURCEPOSN ln, treenode *l, treenode *r);
treenode *newactionnodex (int t, SOURCEPOSN ln, treenode *l, treenode *r, treenode *d, treenode *a);
treenode *newaltnode  (int t, SOURCEPOSN ln, treenode *g,
                             treenode *i, treenode *b);
treenode *newarraysubnode (int t, SOURCEPOSN ln, treenode *b, treenode *i);
treenode *newcnode  (int t, SOURCEPOSN ln, treenode *b);
treenode *newcondnode  (int t, SOURCEPOSN ln, treenode *condition, treenode *process);
treenode *newcondnodex (int t, SOURCEPOSN ln, treenode *inputlist, treenode *d_proc, treenode *a_proc);
treenode *newconstexpnode  (int t, SOURCEPOSN ln, treenode *e,
                                  BIT32 h, BIT32 l);
treenode *newconstant       (BIT32 n);
treenode *newintconstant    (BIT32 n, int type);
treenode *newlongintconstant(BIT32 lo, BIT32 hi, const char *str, int type);
treenode *newrealconstant   (BIT32 lo, BIT32 hi, const char *str, int type);
treenode *newtypedconstant  (BIT32 lo, BIT32 hi, const char *str, int type);
treenode *newconsttablenode (int t, SOURCEPOSN ln, wordnode *v,
                                    treenode *e);
treenode *newdeclnode  (int t, SOURCEPOSN ln, treenode *n,
                              treenode *val, treenode *b);
treenode *newdopnode  (int t, SOURCEPOSN ln, treenode *l, treenode *r, int type);
#if 0
treenode *newhiddenparamnode  (int t, SOURCEPOSN ln, treenode *e, int d, int lexlevel, int arg);
#endif
treenode *newinstancenode  (int t, SOURCEPOSN ln, treenode *n, treenode *p);
treenode *newleafnode  (int t, SOURCEPOSN ln);
treenode *newlistnode  (int t, SOURCEPOSN ln, treenode *l, treenode *r);
treenode *newlitnode   (int t, SOURCEPOSN ln, treenode *e, treenode *type );
treenode *newmopnode   (int t, SOURCEPOSN ln, treenode *o, int type);
treenode *newnamenode (int tag, SOURCEPOSN ln,
                             wordnode *name,
                             treenode *type, treenode *spec,
                             int lexlevel, int scope,
                             int mode);
treenode *newprocessornode  (int t, SOURCEPOSN ln, treenode *exp,
                            wordnode *type,
                            treenode *process);
treenode *newreplcnode  (int t, SOURCEPOSN ln, treenode *n,
                               treenode *s, treenode *l, treenode *step, treenode *b);
treenode *newsegmentnode  (int t, SOURCEPOSN ln, treenode *n, treenode *s,
                                 treenode *l);
treenode *newspacenode  (int t, SOURCEPOSN ln, treenode *p,
                               BIT32 maxwsp, BIT32 datasize,
                               BIT32 vsusage, BIT32 nestedvs,
#ifdef MOBILES
                               BIT32 msusage, BIT32 nestedms,
#endif
                               treenode *namechain, int cpoffset);
treenode *newtypenode    (int t, SOURCEPOSN ln, treenode *d, treenode *tp);
treenode *newvalofnode  (int t,SOURCEPOSN ln, treenode *b, treenode *r);
wordnode *newwordnode  (int t, const char *name, int len, wordnode *next);

treenode *NParamListOf (treenode *n);
void SetNParamList (treenode *n, treenode *v);
int NFieldsOf(treenode *type);

#ifdef CONDEXP
treenode *newcondexpnode (int t, SOURCEPOSN ln,
                          treenode *condition, treenode *true, treenode *false);
#endif

#if 1 /*def CONFIG*/
treenode *newconfignode (int t, SOURCEPOSN ln, treenode *a, treenode *b, treenode *c);
#endif
/*}}}*/
/*{{{  Tree walking and copying*/
#define STOP_WALK     TRUE
#define CONTINUE_WALK FALSE

void applytoexp (treenode *tptr, void (*f1)(treenode *, void *), void *);

void marknametrans (void);
void freenametrans (void);
void addnametrans (treenode *o, treenode *n);
treenode *transcopytree(treenode *tptr, SOURCEPOSN locn, int init_lexlevel);
treenode *copytree     (treenode *tptr, int init_lexlevel);

treenode *expand_inline_instance(treenode *t, SOURCEPOSN locn, int lexlevel);

void initcopytree (void);

void replace_all_names(treenode **tptr, treenode *old, treenode *new);

void prewalktree        (treenode  *tptr, int (*f1)(treenode  *, void *), void *);
void prewalkproctree    (treenode  *tptr, int (*f1)(treenode  *, void *), void *);
void modprewalktree     (treenode **tptr, int (*f1)(treenode **, void *), void *);
/*void modprewalkproctree (treenode **tptr, int (*f1)(treenode **, void *), void *);*/ /* unused */
/*}}}*/
/*{{{  Pretty printing*/
void printbyte   (FILE *fptr, int b);
void printstring (FILE *fptr, const char *s, int l);
void printexp    (FILE *fptr, treenode *t);
void printtreenl (FILE *fptr, int i, treenode *t);
void printtree   (FILE *fptr, int i, treenode *t);
void printsrc    (FILE *fptr, int i, treenode *t);
/*void printstatement (int indent, treenode *s);*/
/*}}}*/
/*{{{  XML printing in vti5*/
void xml_printtree (FILE *fptr, int i, treenode *t);
void xml_printtree_wrapper (FILE *fptr, int i, treenode *t, const char *sfname);

/*}}}*/

/*{{{  decision functions on tags and trees*/
BOOL parrepl (int replc);
#if 1 /*def CONFIG*/
BOOL network_datatype (int type);
#endif

INT32 min_INT32 (INT32 x, INT32 y);
INT32 max_INT32 (INT32 x, INT32 y);

BOOL isint        (int t);
BOOL isintorbyte  (int t);
BOOL isreal       (int t);
BOOL isscalartype (int t);
BOOL isint_tree        (treenode *tptr);
BOOL isintorbyte_tree  (treenode *tptr);
BOOL isreal_tree       (treenode *tptr);
BOOL isscalartype_tree (treenode *tptr);
BOOL isdynamicmobiletype           (treenode *tptr);
BOOL isdynamicmobilearraytype      (treenode *tptr);
BOOL isdynamicmobilechantype       (treenode *tptr);
BOOL issamemobilechantype          (treenode *t1, treenode *t2);
/*int littag (int t);*/
BOOL istypetag(int tag);

BOOL check_array_overflow(SOURCEPOSN locn, BIT32 elemsize, BIT32 nelements, BOOL in_RETYPE);
int bytesinscalar (int t);
int bytesinscalar_tree(treenode *const tptr);
INT32 bytesin (const treenode *t);
#ifdef MOBILES
INT32 bytesin_raw (const treenode *t);
#endif
BIT32 wordsin (treenode *tptr);
INT32 elementsin (treenode *t);
INT32 known_bytesin(const treenode *type);

treenode *basetype_tree (treenode *tptr);
int basetype (treenode *tptr);
treenode *usertype_tree (treenode *tptr);
#ifdef OCCAM2_5
treenode *follow_user_type(treenode *tptr);
#else
#define follow_user_type(T) (T)
#endif
treenode *nameof (treenode *tptr);
treenode *basedecl (treenode *tptr);
BOOL isnotexpression(treenode *tptr);

BOOL isspecification (treenode *tptr);
treenode *skipspecifications (treenode *tptr);
BOOL separatelycompiled (treenode *nptr);
BOOL isinline (treenode *nptr);
BOOL issimple (treenode *tptr);
BOOL issame (treenode *t1, treenode *t2);
BOOL isnamedformal (const treenode *tptr);
BOOL ishiddenformal(const treenode *tptr);
BOOL ishiddenparam (const treenode *param);

treenode **dimexpaddr (treenode *tptr, int dimension);
treenode *dimexpof (treenode *tptr, int dimension);
treenode *skipevals (treenode *tptr);

INT32 typehash (treenode *tptr);
INT32 typehash_names (treenode *tptr);

/*}}}*/

/*{{{  special ASM names*/
/* The order of these matters, cos they're inserted into the name array
   in this order */
#define ASMNAME_WSSIZE               0
#define ASMNAME_VSPTR                1
#define ASMNAME_STATIC               2
#define ASMNAME_FB                   3
#ifdef MOBILES
	#define ASMNAME_MSPTR        4
	#define ASMNAME_MPPTR        5
	#define ASMNAMES_COUNT       6
#else
	#define ASMNAMES_COUNT       4
#endif

/* Contents of the 'asmvalids' array: */
#define ASMNAME_INVALID           0       /* Value not yet known */
#define ASMNAME_VALID             1       /* Value can be inserted */
#define ASMNAME_ERROR             2       /* Value cannot be determined */

extern INT32 asmvalues[];
extern int   asmvalids[];
int which_asmname (treenode *tptr);

/*}}}*/

/*{{{  side-effects pragmas*/
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)

#pragma IMS_nosideeffects (NFieldsOf)
#pragma IMS_nosideeffects (basetype_tree)
#pragma IMS_nosideeffects (basetype)
#pragma IMS_nosideeffects (isint_tree)
#pragma IMS_nosideeffects (isintorbyte_tree)
#pragma IMS_nosideeffects (isreal_tree)
#pragma IMS_nosideeffects (isscalartype_tree)
#ifdef OCCAM2_5
#pragma IMS_nosideeffects (follow_user_type)
#endif
#pragma IMS_nosideeffects (nameof)
#pragma IMS_nosideeffects (skipevals)
#pragma IMS_nosideeffects (skipspecifications)

#pragma IMS_nosideeffects (tablesize)
#pragma IMS_nosideeffects (nodetypeoftag)
#pragma IMS_nosideeffects (parrepl)
#pragma IMS_nosideeffects (network_datatype)
#pragma IMS_nosideeffects (min_INT32)
#pragma IMS_nosideeffects (max_INT32)
#pragma IMS_nosideeffects (isint)
#pragma IMS_nosideeffects (isintorbyte)
#pragma IMS_nosideeffects (isreal)
#pragma IMS_nosideeffects (isscalartype)
/*#pragma IMS_nosideeffects (littag)*/
#pragma IMS_nosideeffects (isnotexpression)
#pragma IMS_nosideeffects (separatelycompiled)
#pragma IMS_nosideeffects (issimple)
#pragma IMS_nosideeffects (inline)
#pragma IMS_nosideeffects (isnamedformal)
#pragma IMS_nosideeffects (ishiddenformal)
#pragma IMS_nosideeffects (ishiddenparam)
#pragma IMS_nosideeffects (isspecification)
#pragma IMS_nosideeffects (which_asmname)
#endif
/*}}}*/

