/* $Id: nameshdr.h,v 1.2 1997/08/22 11:05:36 mdp2 Exp $ */

/*
 *	Macro definitions for code generator fields within namenodes
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

#ifndef NAMESHDR_LOADED
#define NAMESHDR_LOADED

/*{{{  MDP notes*/
/* added access to n_endposn */
/*}}}*/

/*{{{  Fields used by all of the namenode types*/
#define NNameOf(N)              (CHECKREAD(N,NAMENODE)->n_u.n_s.n_name)
#define NTypeOf(N)              (CHECKREAD(N,NAMENODE)->n_u.n_s.n_type)
#define NDeclOf(N)              (CHECKREAD(N,NAMENODE)->n_u.n_s.n_decl)
#define NLexLevelOf(N)   ((int)((CHECKREAD(N,NAMENODE)->n_u.n_s.n_lexlev) & 0x7f))
#define NUsedOf(N)            (((CHECKREAD(N,NAMENODE)->n_u.n_s.n_lexlev) & 0x80) != 0)
#define NScopeOf(N)             (CHECKREAD(N,NAMENODE)->n_u.n_s.n_scope)
#define NModeOf(N)              (CHECKREAD(N,NAMENODE)->n_u.n_s.n_mode)

#define NTypeAttrOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_typeattr)
#define SetNTypeAttr(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_typeattr = (V))

#define NCheckerOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_checker)
#define SetNChecker(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_checker = (V))

#define NUndefOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_undef)
#define SetNUndef(N,V)		(CHECKNODE(N,NAMENODE)->n_u.n_s.n_undef = (V))

#define NFMCheckOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_fmcheck)
#define SetNFMCheck(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_fmcheck = (V))


#ifdef COMPILING_TO_JCODE
#define NAddrTakenOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_addr_taken)
#define NBindOf(N)              (CHECKREAD(N,NAMENODE)->n_u.n_s.n_binder)
#endif

#define SetNDeclType(N,V)       (CHECKNODE(N,NAMENODE)->n_u.n_s.n_decltype = (V))
#define SetNName(N,V)           (CHECKNODE(N,NAMENODE)->n_u.n_s.n_name   = (V))
#define SetNType(N,V)           (CHECKNODE(N,NAMENODE)->n_u.n_s.n_type   = (V))
#define SetNDecl(N,V)           (CHECKNODE(N,NAMENODE)->n_u.n_s.n_decl   = (V))
#define SetNLexLevel(N,V)       (CHECKNODE(N,NAMENODE)->n_u.n_s.n_lexlev = \
                                (CHECKNODE(N,NAMENODE)->n_u.n_s.n_lexlev & 0x80) | ((int)(V) & 0x7f))
#define SetNUsed(N,V)           (CHECKNODE(N,NAMENODE)->n_u.n_s.n_lexlev = \
                                (CHECKNODE(N,NAMENODE)->n_u.n_s.n_lexlev & 0x7f) | (((V) == 0) ? 0 : 0x80))
#define SetNScope(N,V)          (CHECKNODE(N,NAMENODE)->n_u.n_s.n_scope  = (V))
#define SetNMode(N,V)           (CHECKNODE(N,NAMENODE)->n_u.n_s.n_mode   = (V))

#ifdef COMPILING_TO_JCODE
#define SetNAddrTaken(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_addr_taken = (V))
#define SetNBind(N,V)           (CHECKNODE(N,NAMENODE)->n_u.n_s.n_binder     = (V))
#endif

#define NTypeAddr(N)		(&(CHECKNODE(N,NAMENODE)->n_u.n_s.n_type))
#define NDeclAddr(N)		(&(CHECKNODE(N,NAMENODE)->n_u.n_s.n_decl))
#define NNameAddr(N)		(&(CHECKNODE(N,NAMENODE)->n_u.n_s.n_name))
#define NTypeAttrAddr(N)	(&(CHECKNODE(N,NAMENODE)->n_u.n_s.n_typeattr))
/*}}}*/

/*{{{  PROC/FUNCTION node*/

#define NPLabelOf(N)            (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_label)
#define NPMapLabelOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_maplabel)
#define NPWSMapOf(N)            (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_wsmap)
#define NPMaxwspOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_maxwsp)
#define NPDatasizeOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_datasize)
#define NPVSUsageOf(N)          (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_vsusage)
/*#define NPNestedVSOf(N)       (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedvs)*/
#ifdef MOBILES
#define NPMSUsageOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_msusage)
#define NPMSPtrOf(N)            (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_msptr)
#endif
#define NPParamsOf(N)    ((int)((CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_params) & 0xffff))
#define NPSLUsageOf(N)  ((int)(((CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_params) >> 16) & 0xffff))

#define NPNestedPriParOf(N)     (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedpripar)
#define NPNestedTimerOf(N)      (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedtimer)
#define NPNestedPlaceOf(N)      (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedplace)
#define NPNestedPortOf(N)       (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedport)
#define NPSafeFnResultOf(N)     (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_safefnresult)
#ifdef OCCAM2_5
#define NPConstantFnOf(N)       (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_constantfn)
#endif
#define NPRecursiveOf(N)	(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_recursive)
#define NPForksOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_forks)
#define NPSuspendsOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_suspends)
#define NPDyncallOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_dyncall)

#define SetNPLabel(N,V)         (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_label    = (V))
#define SetNPMapLabel(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_maplabel = (V))
#define SetNPWSMap(N,V)		(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_wsmap    = (V))
#define SetNPMaxwsp(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_maxwsp   = (V))
#define SetNPDatasize(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_datasize = (V))
#define SetNPVSUsage(N,V)       (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_vsusage  = (V))
/*#define SetNPNestedVS(N,V)    (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedvs = (V))*/
#ifdef MOBILES
#define SetNPMSUsage(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_msusage  = (V))
#define SetNPMSPtr(N,V)         (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_msptr    = (V))
#endif

#define SetNPNestedPriPar(N,V)  (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedpripar = (V))
#define SetNPNestedTimer(N,V)   (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedtimer  = (V))
#define SetNPNestedPlace(N,V)   (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedplace  = (V))
#define SetNPNestedPort(N,V)    (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_nestedport   = (V))
#define SetNPSafeFnResult(N,V)  (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_safefnresult = (V))
#ifdef OCCAM2_5
#define SetNPConstantFn(N,V)    (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_constantfn   = (V))
#endif
#define SetNPRecursive(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_recursive    = (V))
#define SetNPForks(N,V)		(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_forks        = (V))
#define SetNPSuspends(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_suspends     = (V))
#define SetNPDyncall(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_dyncall      = (V))

#define SetNPParams(N,V) ( CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_params = \
        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_params & 0xffff0000) | (((int)(V)) & 0xffff) )
#define SetNPSLUsage(N,V) ( CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_params = \
        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_params & 0x0000ffff) | ((((int)(V)) & 0xffff) << 16) )

/*{{{  local PROC/FUNC*/
#define NPConstTablesOf(N)      (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_loc.n_ctable)
#define NPCPOffsetOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_loc.n_cpoffset)
#define NPEndposnOf(N)          (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_loc.n_endposn)

#define SetNPConstTables(N,V)   (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_loc.n_ctable = (V))
#define SetNPCPOffset(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_loc.n_cpoffset = (V))
#define SetNPEndposn(N,V)       (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_loc.n_endposn = (V))
/*}}}*/

/*{{{  library PROC/FUNC*/
#define NLExternalOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_lib.n_external)
#define NLEntryNextOf(N)        (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_lib.n_nextlib)
#define NLEntryOffsetOf(N)      (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_lib.n_liboffset)

#define SetNLExternal(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_lib.n_external = (V))
#define SetNLEntryNext(N,V)     (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_lib.n_nextlib = (V))
#define SetNLEntryOffset(N,V)   (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_proc.n_un1.n_lib.n_liboffset = (V))
/*}}}*/
/*}}}*/

/*{{{  variable node*/
/*{{{  all variables*/
#define NVOffsetOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_offset)
#define SetNVOffset(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_offset = (V))
/*}}}*/

/*{{{  frontend only*/
#if 0
#define NVSharedOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_front.n_shared)
#define NVNotAliasedOf(N)       (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_front.n_notaliased)
#define NVRDefinedOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_front.n_rdefined)
#define SetNVShared(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_front.n_shared     = (V))
#define SetNVNotAliased(N,V)    (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_front.n_notaliased = (V))
#define SetNVRDefined(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_front.n_rdefined = (V))
#endif
/*}}}*/

/*{{{  backend only*/
#define NVNextOf(N)             (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_shares)
#define NVVarNumOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_varnum)
#define NVNextTempOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_nexttemp)
#define NVAllocNextOf(N)        (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_allocnext)
#define NVWSMapTagOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_wsmaptag)
#define SetNVNext(N,V)          (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_shares = (V))
#define SetNVVarNum(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_varnum = (V))
#define SetNVNextTemp(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_nexttemp = (V))
#define SetNVAllocNext(N,V)     (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_allocnext = (V))
#define SetNVWSMapTag(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un5.n_back.n_wsmaptag = (V))

/*}}}*/

/*{{{  actual variables only*/
#define NVVSOffsetOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_vsoffset)
#ifdef MOBILES
#define NVMSOffsetOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_vsoffset)
#endif
/* temporaries too: */
#define NVUseCountOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un4.n_usecount)

#define SetNVVSOffset(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_vsoffset = (V))
#ifdef MOBILES
#define SetNVMSOffset(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_vsoffset = (V))
#endif
#define SetNVUseCount(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un4.n_usecount = (V))

#define NVSharedOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_shared)
#define NVAliasedOf(N)          (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_aliased)
#define NVRDefinedOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_rdefined)
#define NVDeviceOf(N)           (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_device)
#define NVAssumeConstOf(N)      (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_assumeconst)
#ifdef OCCAM2_5
#define NVReshapesOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_reshapes)
#endif
#define NVEndPosnOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_endposn)
#define NVNameUsedOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_nameused)
#define NVIndirectOf(N)		(CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_indirect)

#define SetNVShared(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_shared  = (V))
#define SetNVAliased(N,V)       (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_aliased = (V))
#define SetNVRDefined(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_rdefined = (V))
#define SetNVDevice(N,V)        (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_device  = (V))
#define SetNVAssumeConst(N,V)   (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_assumeconst = (V))
#ifdef OCCAM2_5
#define SetNVReshapes(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_reshapes = (V))
#endif
#define SetNVEndPosn(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_endposn = (V))
#define SetNVNameUsed(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_nameused = (V))
#define SetNVIndirect(N,V)	(CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_indirect = (V))

/*}}}*/

/*{{{  replicators only (frontend)*/
#define NReplKnownOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_replknown)
#define NReplValueOf(N)         (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un4.n_replval)

#define SetNReplKnown(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_replknown = (V))
#define SetNReplValue(N,V)      (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un4.n_replval = (V))
/*}}}*/

/*{{{  scalar channels only*/
/* Scalar channels only (TagOf(NTypeOf(n)) == S_CHAN) */
#define NChanMarkOf(N)          (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_chanmark)
#define SetNChanMark(N,V)       (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un3.n_chanmark = (V))
/*}}}*/

/*{{{  T_RESERVEDWS only*/
#define NVRSizeOf(N)            (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un4.n_rsize)
#define SetNVRSize(N,V)         (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_var.n_un4.n_rsize = (V))
/*}}}*/

/*{{{  hidden parameters*/
#define HExpOf(T)       NDeclOf(T)
#define HDimensionOf(T) NVVarNumOf(T)
#define HArgNoOf(T)     NVVSOffsetOf(T)

#define SetHExp(T,V)       SetNDecl(T,V)
#define SetHDimension(T,V) SetNVVarNum(T,V)
#define SetHArgNo(T,V)     SetNVVSOffset(T,V)

#define HExpAddr(T)        NDeclAddr(T)

/*}}}*/

/*}}}*/

/*{{{  tag definition node*/
#define NTValueOf(N)        (CHECKREAD(N,NAMENODE)->n_u.n_s.n_un.n_tag.n_tagval)
#define SetNTValue(N,V)     (CHECKNODE(N,NAMENODE)->n_u.n_s.n_un.n_tag.n_tagval = (V))
/*}}}*/

/*{{{  Selector values for different types of names*/
/* routine nametypeoftag(int tag) returns one of these */
typedef enum {
  NAMENODE_MISC,
  NAMENODE_VAR,
  NAMENODE_ROUTINE
  } nametypeoftag_t;
/*}}}*/

/*{{{  name modes*/
enum {
  NM_DEFAULT,
  NM_WORKSPACE,
  NM_VECSPACE,
  NM_PLACED,
  NM_POINTER,
  NM_INLINE /* never used */,
  NM_WSPLACED
  #ifdef MOBILES
    , NM_MOBILESPACE
  #endif
  };
/*}}}*/

#endif
