/* $Id: lexhdr.h,v 1.3 1997/09/04 13:45:51 jm40 Exp $ */

/*
 *	Lexical tokens
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

#define S_ADD       1
#define S_AFTER     2
#define S_ALT       3
#define S_AMPERSAND 4
#define S_AND       5
#define S_ANY       6
#define S_ASS       7
#define S_AT        8
#define S_BITAND    9
#define S_BITNOT    10
#define S_BITOR     11
#define S_BOOL      12
#define S_BOX       13
#define S_BYTE      14
#if 0
	#define S_BYTELIT   15
#endif
#define S_CASE      16
#define S_CHAN      17
#define S_COLON     18
#define S_COLON2    19
#define S_COMMA     20
#define S_DIV       21
#define S_ELSE      22
#define S_EQ        23
#define S_FALSE     24
#define S_FOR       25
#define S_FROM      26
#define S_FUNCTION  27
#define S_GE        28
#define S_GR        29
#define S_IF        30
#define S_INPUT     31
#define S_INT       32
#define S_INT16     33
#define S_INT32     34
#define S_INT64     35
#if 0
	#define S_INTLIT    36
	#define S_INT16LIT  37
	#define S_INT32LIT  38
	#define S_INT64LIT  39
#endif
#define S_IS        40
#define S_LBOX      41
#define S_LE        42
#define S_LPAREN    43
#define S_LS        44
#define S_LSHIFT    45
#define S_MINUS     46
#define S_MOSTNEG   47
#define S_MOSTPOS   48
#define S_MULT      49
#define S_NAME      50
#define S_NE        51
#define S_NOT       52
#define S_OF        53
#define S_OR        54
#define S_OUTPUT    55
#define S_PAR       56
#define S_PLACE     57
#define S_PLACED    58
#define S_PLUS      59
#define S_PORT      60
#define S_PRI       61
#define S_PROC      62
#define S_PROCESSOR 63
#define S_PROTOCOL  64
#define S_RBOX      65
#define S_REAL32    66
#define S_REAL64    67
#define S_UREALLIT  68
#if 0
	#define S_REAL32LIT 69
	#define S_REAL64LIT 70
#endif
#define S_REM       71
#define S_RESULT    72
#define S_RETYPES   73
#define S_ROUND     74
#define S_RPAREN    75
#define S_RSHIFT    76
#define S_SEMICOLON 77
#define S_SEQ       78
#define S_SIZE      79
#define S_SKIP      80
#define S_STOP      81
#define S_STRING    82
#define S_SUBTRACT  83
#define S_TIMER     84
#define S_TIMES     85
#define S_TRUE      86
#define S_TRUNC     87
#define S_VAL       88
#define S_VALOF     89
#define S_WHILE     90
#define S_XOR       91

#define S_NEWLINE   92
#define S_END       93
#define S_COMMENT   94

#define S_UINTLIT   95
#define S_STRINGCONT 96
#define S_UBYTELIT  97

#define S_ILLEGALSYMB 98
#define S_WORKSPACE 99
#define S_VECSPACE 100
#define S_IN       101

#define S_SC       102
#define S_INCLUDE  103
#define S_USE      104
#define S_INLINE   105
#define S_GUY      106
#define S_DOT      107
#define S_STEP     108
#define S_IMPORT   109
#define S_OPTION   110
#define S_HCOMMENT 111

#define S_HNETWORK 112 /* added for version 3 configurer */
#define S_ARRAYCONSTRUCTOR 113  /* [i = 0 FOR (SIZE a) | 'z'] type things */

#if 1 /*def CONFIG*/
	#define S_SET      114
	#define S_ARC      115
	#define S_EDGE     116
	#define S_NODE     117
	#define S_CONFIG   118
	#define S_NETWORK  119
	#define N_CONFIG   120
	#define N_NETWORK  121
	#define S_CONNECT  122
	#define S_TO       123
	#define S_WITH     124
	#define S_MAPPING  125
	#define N_MAPPING  126
	#define S_MAP      127
	#define S_ON       128
	#define S_ONTO     129

	#define CASE_CONFIG_SPEC case S_CONFIG: case S_NETWORK: case S_MAPPING:
	#define CASE_CONFIG_NAME case N_CONFIG: case N_NETWORK: case N_MAPPING:
	#define CASE_CONFIG_TYPE case S_NODE:   case S_ARC:     case S_EDGE:    case S_CONTROLPORT: case S_ROUTE:
#else
	#define CASE_CONFIG_SPEC /* nothing */
	#define CASE_CONFIG_NAME /* nothing */
	#define CASE_CONFIG_TYPE /* nothing */
#endif

/* From synhdr.h : */
/*{{{  parse tree tags above lexical tokens*/
#define S_LIST  130
#define S_DELAYED_INPUT 131
#define S_CASE_INPUT  132
#define S_VALABBR     133
#define S_VALRETYPE   134
#define S_DECL        135
#define S_ABBR        136
#define S_RETYPE      137
#define S_TPROTDEF    138
#define S_SPROTDEF    139
#define S_PROCDEF     140
#define S_SFUNCDEF    141
#define S_LFUNCDEF    142
#define S_ARRAYSUB    143
#define S_NEG         144
/*#define S_VAR        145*/ /* never used */
#define S_EXACT       146
#define S_SEGMENT     147
#define S_VARIANT     148
#define S_ARRAY       149
#define S_FNTYPE      150
#define N_VALABBR     151
#define N_VALRETYPE   152
#define N_ABBR        153
#define N_RETYPE      154
#define N_DECL        155
#define N_TAGDEF      156
#define N_SPROTDEF    157
#define N_TPROTDEF    158
#define N_PROCDEF     159
#define N_SFUNCDEF    160
#define N_LFUNCDEF    161
#define N_VALPARAM    162
#define N_REPL        163
#define S_ALTERNATIVE 164
#define S_CONSTRUCTOR 165
#define N_PARAM       166
#define S_CONSTEXP    167
/*#define S_LONGSTRING 168*/ /* never used */
#define S_UNDECLARED  169
#define S_WSPLACE     170
#define S_VSPLACE     171
#define N_SCPROCDEF   172
#define N_SCFUNCDEF   173
#define N_LIBPROCDEF  174
#define N_LIBFUNCDEF  175
#define N_PREDEFFUNCTION 176
#define N_PREDEFPROC  177
#define N_STDLIBPROCDEF 178
#define N_STDLIBFUNCDEF 179
#define N_LABELDEF    180
#define S_LABELDEF    181
#define S_UMINUS      182
#define S_GUYCODE     183
#define S_GUYSTEP     184
#define S_LABEL       185
#define S_PRIPAR      186
#define S_PRIREPLPAR  187
#define S_PRIALT      188
#define S_PRIREPLALT  189
#define S_PLACEDPAR   190
#define S_PLACEDREPLPAR 191
#define S_TAGGED_INPUT 192
#define S_REPLSEQ     193
#define S_REPLPAR     194
#define S_REPLIF      195
#define S_REPLALT     196
#define S_CHOICE      197
#define S_SELECTION   198
#define S_FINSTANCE   199
#define S_PINSTANCE   200

#ifdef CONDEXP
	#define S_CONDEXP     201
#endif

#if 0
	#define S_STRUCTCONSTRUCTOR 202
#endif

#define N_INLINEPROCDEF 203
#define N_INLINEFUNCDEF 204

#if 1
	#define S_RECORDSUB     205
	#define N_FIELD         206 /* Used for fields of records */
	#define S_DO            207
	#define S_REPLDO        208
#endif

#if 1 /*def CONFIG*/
	#define S_PLACEON       209
#endif
/*}}}*/

/* FROM chkhdr.h : */

#define S_UNKNOWN          210                               /* Unknown type */

#define S_CONSTCONSTRUCTOR 211
/*#define N_VALABBRLIT       212*/ /* never used */
#define S_CONSTPTR         213
#define S_CSUB0            214                  /* introduced by the backend */
#define T_TEMP             215

#define S_DUMMYEXP         216
#define S_ARRAYITEM        217
#define S_SEGMENTITEM      218
#define T_PREEVALTEMP      219
#define S_CCNT1            220
#define S_FNFORMALRESULT   221
#define S_FNACTUALRESULT   222
#define S_ELSIZE           223 /* exactly like SIZE,but introduced by backend*/
#define S_SEGSTART         224
#define S_HIDDEN_PARAM     225
#define S_PARAM_STATICLINK 226
#define S_PARAM_VSP        227
#define S_EVAL             228
#define S_OVERLAPCHECK     229
/*#define S_POSTEVAL         230*/ /* never used */
#define S_ADDRESSOF        231

/* Used in backend: */
#define S_SPACEUSAGE            236
#define T_REGTEMP               237
#define T_RESERVEDWS            238
/*#define S_BASICBLOCK            239*/ /* never used */ /* ACS */

/* Extra ones added by CO'N */
#define S_PRAGMA                240
#define S_ASM                   241
#define S_ASMNAME               242

/* NDL reader: */
#define S_CONTROLPORT           243
#define S_ROUTE                 244

#if 1 /*def OCCAM2_5*/
	/* always define the tags,
	   so that the backend etc doesn't need lots of ifdefs
	 */
	#define S_DATA                  245
	#define S_TYPE                  246
	#define S_RECORD                247
	#define S_TYPEDECL              248
	#define N_TYPEDECL              249
	#define S_RECORDITEM            250
	#define S_PACKED                251
	#define S_BYTESIN               252
	#define S_OFFSETOF              253
	#define S_RESHAPES              254
#endif

/* Max is 255 */ /* Not actually true! */
#ifdef USER_DEFINED_OPERATORS
	/* Diadic operator symbols tag values */
	#define S_DQUESTIONM            256
	#define S_DAMPERSAT             257
	#define S_DDOLLAR               258
	#define S_PERCENT               259
	#define S_DPERCENT              260
	#define S_DAMPERSAND            261
	#define S_LEFTDOLLAR            262 /* not used */
	#define S_DOLLARRIGHT           263 /* not used */
	#define S_LEFTPERCENT           264
	#define S_PERCENTRIGHT          265
	#define S_LEFTAMPERSAND         266
	#define S_AMPERSANDRIGHT        267
	#define S_DPLUS                 268
	#define S_DMINUS                269 /* not used */
	#define S_HAT                   270
	#define S_DEXCLAIMATION         271
	#define S_DEQUALS               272
	#define S_LEFTAMPERSAT          273
	#define S_AMPERSATRIGHT         274
	#define S_AMPERSAT              275
	#define S_DOLLAR                276 /* not used */
	#define S_ROTATELEFT            277
	#define S_ROTATERIGHT           278
	/* Same operators - but symbols for monadoc operators */
	#define S_M_DQUESTIONM          279
	#define S_M_DAMPERSAT           280
	#define S_M_DDOLLAR             281
	#define S_M_PERCENT             282
	#define S_M_DPERCENT            283
	#define S_M_DAMPERSAND          284
	#define S_M_LEFTDOLLAR          285 /* not used */
	#define S_M_DOLLARRIGHT         286 /* not used */
	#define S_M_LEFTPERCENT         287
	#define S_M_PERCENTRIGHT        288
	#define S_M_LEFTAMPERSAND       289
	#define S_M_AMPERSANDRIGHT      290
	#define S_M_DPLUS               291
	#define S_M_DMINUS              292 /* not used */
	#define S_M_HAT                 293
	#define S_M_DEXCLAIMATION       294
	#define S_M_DEQUALS             295
	#define S_M_LEFTAMPERSAT        296
	#define S_M_AMPERSATRIGHT       297
	#define S_M_AMPERSAT            298
	#define S_M_DOLLAR              299 /* not used */
	#define S_M_ROTATELEFT          300
	#define S_M_ROTATERIGHT         301
#endif /* USER_DEFINED_OPERATORS */
#ifdef INITIAL_DECL
	#define S_INITIAL               302
#endif /* INITIAL_DECL */

#ifdef MOBILES
	#define S_MOBILE		306
	#define S_CLONE			307
	#define S_PARAM_MSP		308 /* mobile-space pointer */
	#define S_NEW_ARRAY		309 /* for creating dynamic MOBILE arrays */
	#define S_UNDEFINED		310 /* nodes inserted by the undefined-checker */
#endif
#define S_BAR				312 /* pipe symbol (aka bar, `|') for compound array expressions */
#define N_RESULTPARAM			313
#define S_RECURSIVE			314 /* for recursive PROCs/FUNCTIONs */
#define S_REC				315
#define S_ASINPUT			316 /* for channel-direction specifiers used in parameter passing */
#define S_ASOUTPUT			317 /* removed in the usage-checker */
#define S_X_INPUT			318 /* extended input "??" */
#define S_X_TAGGED_INPUT		319 /* extended tagged input "?? CASE .." */
#define S_X_CASE_INPUT			320 /* extended CASE input "?? CASE\n" -- during/after not used here */
#define S_X_VARIANT			321 /* extended variant for CASE input  "?? CASE\n" */
#ifndef USER_DEFINED_OPERATORS
#define S_DQUESTIONM			322 /* UDO symbol for extended input (same name removes #ifdef-to-hell) */
#endif	/* !USER_DEFINED_OPERATORS */
#define S_X_FIRST_HALF			323 /* first half of extended input (no XEND) -- mopnode (X_INPUT) */
#define S_X_SECOND_HALF			324 /* second half of extended input (XEND) -- mopnode (channel) */
#define S_SCALAR			325 /* for SCALAR types */
#define N_SCALAR			326 /* for SCALAR names */
#define S_SLEEP				327 /* for SLEEPing (like after but relative) */
#ifdef MOBILES
#define S_NTH_DIMENSION			328 /* used to extract dimension sizes from dynamic MOBILE arrays */
#endif

#define S_FORKING			329 /* FORKING process */
#define S_FORK				330 /* FORK keyword (does not appear in resulting tree), instance gets flagged */
#define S_BARRIER			331 /* BARRIER type used for forking (and now for PAR/REPLPAR barriers also) */

#define S_SHARED			332 /* SHARED keyword/tag for CHAN TYPE variables */
#define S_CLAIM				333 /* CLAIM process */

#define S_UPLUS				334 /* unary `+' operator (never gets past syn) */
#define S_ANONCHANTYPE			335 /* typenode for anonymous channel-types (mangled in type-check/trans) */

#define S_PREPROC			336 /* dummy tag for the pre-processor */
#define S_X_INPUT_OUTPUT		337 /* used for ENCODE.CHANNEL and DECODE.CHANNEL -- requires extended rendezvous support */

#define S_NULLARRAY			338 /* used for the special empty-array constant */

#define S_DEFINED			339 /* MOP node (returns BOOL) for checking definedness of MOBILE things */
#define S_UDVCONSTEXP			340 /* special S_CONSTEXP used by the undefinedness-checker */

#define S_FORWDECL			341 /* declaration node for a forward declaration */
#define S_EXTENDS			342 /* keyword for protocol extension */

#define S_PROCTYPEDECL			343 /* PROC TYPE declaration */
#define N_PROCTYPEDECL			344 /* PROC TYPE name */
#define S_MPROCDECL			345 /* MOBILE PROC declaration */
#define N_MPROCDECL			346 /* MOBILE PROC name */
#define S_ALLOC_PROC			347 /* MOBILE PROC allocation (TYPENODE) */
#define S_IMPLEMENTS			348 /* IMPLEMENTS for mobile processes */
#define S_PARAM_MPP			349 /* MOBILE PROC "this" pointer parameter */
#define S_SUSPEND			350 /* MOBILE PROC "suspend" */
#define S_PARAM_FB			351 /* $fork.barrier parameter */
#define S_TRACES			352 /* TRACES */
#define S_SEQCOMP			353 /* sequential composition "->" */
#define S_PARCOMP			354 /* parallel composition "||" */
#define N_LIBMPROCDECL			355 /* MOBILE PROC name (separately-compiled/library) */
#define S_FIXED				356 /* special for MOBILE parameters -- indicates that they're not moved (but not quite VAL) */
#define S_ANYCHANTYPE			357 /* special for CHAN OF "MOBILE.CHAN" and MOBILE.CHAN vars */
#define S_SYNC				358 /* SYNC for BARRIER type */
#define S_RESIGN			359 /* RESIGN for BARRIER type */
#define S_FULLBARRIER			360 /* a "full" BARRIER type */
#define S_BAREXTEND			361 /* for PARs that declare and EXTEND barriers */
#define S_NEW_BARRIER			362 /* for allocating MOBILE BARRIERs */
#define S_HIDDEN_TYPE			363 /* hidden parameter (typehash) for MOBILE.CHAN params */
#define S_TYPEHASHOF			364 /* for extracting the typehash of a mobile channel */
#define S_ANYPROCTYPE			365 /* special for "MOBILE.PROC" generic type */
#define S_ENROLL			366 /* for enrolling BARRIERs (S_BAREXTEND) */
#define S_BUFFERED			367 /* for buffered channels */
#define S_PARAM_WS			368 /* CIF workspace size */
#define S_ALIGNMENT			369 /* ALIGNMENT type decorator */
#define S_DMA				370 /* DMA type decorator */
#define S_EMPTY				371 /* EMPTY type decorator */
#define S_ADDROF			372
#define S_HWADDROF			373
#define S_ANYMOBILETYPE			374 /* special for pre-defined PROCs taking any MOBILE */

#define S_UINT				375 /* unsigned integer types */
#define S_UINT16			376
#define S_UINT32			377
#define S_UINT64			378

#define S_DYNCALL			379 /* dynamic PROC call */
#define S_TYPEHASHCHECK			380 /* on imported DEXTERNAL declarations */


