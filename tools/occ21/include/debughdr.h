/*  (c) Copyright 1989, 1990, 1991, 1992 INMOS Limited  */

/*
 *	Common header file for debug information
 *	Copyright (C) 1989-1992 Inmos Limited
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

/*
 *
 *       Author:  Nigel Holder
 *
 *       Date:    20 April 1989
 *                 2 April 1990    Added RTL_OPTIMISED
 *                24 May   1990    Added RTL_C_PLUSPLUS_BITS
 *                26 Sept  1990    Added RTL_HARRAY
 *                27 Feb   1991    Added new FORTRAN records, renamed RTL_HARRAY
 *                29 Apr   1991    Added new variable record (RTL_KSVARIABLE)
 *                                 and RTL_COLUMN_MAJOR_BIT
 *                21 May   1991    Added RTL_CONSTVAL
 *                22 June  1992    Added RTL_INDEX_SORTED_BIT
 *
 *
 *       Module name:    debughdr.h
 *
 *       Purpose:  Common header file for debug information.
 *                 Defines RTL values as per document SW-0047-6 and SW-0264-3.
 */

#ifndef __DEBUGHDR_H
#define __DEBUGHDR_H


/*  ****  symbolic and code debug information  ****  */

#define         RTL_STARTFILE           (  0 )
#define         RTL_ENDFILE             (  1 )
#define         RTL_NEXTITEM            (  2 )
#define         RTL_WORKSPACE           (  3 )
#define         RTL_ENDSEG              (  4 )
#define         RTL_VARIABLE            (  5 )
#define         RTL_BEGSCOPE            (  6 )
#define         RTL_ENDSCOPE            (  7 )
#define         RTL_PROCEDURE           (  9 )
#define         RTL_CONSTANT            ( 10 )
#define         RTL_FILENAME            ( 11 )
#define         RTL_LIBPATCH            ( 12 )
#define         RTL_VERSION             ( 13 )
#define         RTL_CODEMARK            ( 14 )
#define         RTL_ADDRESS_FIX         ( 15 )
#define         RTL_LINEMARK            ( 17 )
#define         RTL_PROFILENEXTITEM     ( 18 )
#define         RTL_PROFILEMARK         ( 19 )
#define         RTL_NOBREAKMARK         ( 20 )
#define         RTL_NOBREAKNEXTITEM     ( 21 )
#define         RTL_TYPEDEF             ( 22 )
#define         RTL_OBJLENGTH           ( 23 )
#define         RTL_ENUMMEMBER          ( 24 )
#define         RTL_RECORDMEMBER        ( 25 )
#define         RTL_VARIANTMEMBER       ( 26 )
#define         RTL_PATHNAME            ( 27 )
#define         RTL_KSCONSTANT          ( 28 )
#define         RTL_COMMONMEMBER        ( 29 )
#define         RTL_KSVARIABLE          ( 30 )
#define         RTL_PROFILE_TABLE       ( 31 )
#define         RTL_PROFILE_COUNT       ( 32 )
#define         RTL_PROFILE_CALL_COUNT  ( 33 )
#define         RTL_ALTSTART            ( 34 )
#define         RTL_ALTGO               ( 35 )

/*  ****  end of symbolic and code debug information  ****  */



/*  ****  segment types  ****  */

#define         RTL_SEGTYPE_PROC        ( 1 )
#define         RTL_SEGTYPE_PAR         ( 2 )
#define         RTL_SEGTYPE_REPL        ( 3 )
#define         RTL_SEGTYPE_KERN        ( 4 )
#define         RTL_SEGTYPE_MAIN        ( 5 )
#define         RTL_SEGTYPE_RUN         ( 6 )
#define         RTL_SEGTYPE_PRIPAR      ( 7 )
#define         RTL_SEGTYPE_SEQ         ( 8 )

/*  ****  end of segment types  ****  */



/*  ****  procedure types  ****  */

#define         RTL_PROCFLAG              (  0 )
#define         RTL_FUNCTIONFLAG          (  1 )
#define         RTL_SCPROCFLAG            (  2 )
#define         RTL_SCFUNCTIONFLAG        (  3 )
#define         RTL_LIBPROCFLAG           (  4 )
#define         RTL_LIBFUNCTIONFLAG       (  5 )
#define         RTL_FORWARDPROCFLAG       (  6 )
#define         RTL_FORWARDFUNCTIONFLAG   (  7 )
#define         RTL_TRAPPROCFLAG          (  8 )
#define         RTL_TRAPFUNCTIONFLAG      (  9 )
#define         RTL_INTERRUPTPROCFLAG     ( 10 )
#define         RTL_INTERRUPTFUNCTIONFLAG ( 11 )

/*  ****  end of procedure types  ****  */




/*  ****  builtin types (must start at 0)  ****  */

#define         RTL_CHANNEL             (  0 )
#define         RTL_TIMER               (  1 )
#define         RTL_BOOLEAN             (  2 )
#define         RTL_BYTE                (  3 )
#define         RTL_INTEGER             (  4 )
#define         RTL_INT16               (  5 )
#define         RTL_INT32               (  6 )
#define         RTL_INT64               (  7 )
#define         RTL_REAL32              (  8 )
#define         RTL_REAL64              (  9 )
#define         RTL_PROTOCOL            ( 10 )
#define         RTL_CASETAG             ( 11 )
#define         RTL_PORT                ( 12 )
#define         RTL_CHANPOINTER         ( 13 )
#define         RTL_VOID                ( 14 )
#define         RTL_INT8                ( 15 )
#define         RTL_BIT16               ( 16 )
#define         RTL_BIT32               ( 17 )
#define         RTL_BIT64               ( 18 )
#define         RTL_UINT                ( 19 )
#define         RTL_FUNCPOINTER         ( 20 )
#define         RTL_LOGICAL16           ( 21 )
#define         RTL_LOGICAL32           ( 22 )
#define         RTL_LOGICAL             ( 23 )

#define         RTL_BUILTIN_TYPES       ( 24 )

/*  ****  end of builtin types  ****  */



/*  ****  Type constructors  ****  */

#define         RTL_ARRAY               ( 30 )
#define         RTL_POINTER             ( 31 )
#define         RTL_ENUM                ( 32 )
#define         RTL_RECORD              ( 33 )
#define         RTL_VARIANT             ( 34 )
#define         RTL_ALIAS               ( 35 )
#define         RTL_QUALIFIER           ( 36 )
#define         RTL_UNDEF               ( 37 )
#define         RTL_KSARRAY             ( 38 )
#define         RTL_COMMON              ( 39 )

/*  ****  end of Type constructors  ****  */



/*  ****  user Type reference numbers must start from this  ****  */
#define         RTL_MINUSERTYPE         ( 50 )



/*  ****  access methods  ****  */

#define         RTL_CONST               (  1 )
#define         RTL_BYVAL               (  2 )
#define         RTL_BYREF               (  3 )
#define         RTL_PLACED              (  4 )
#define         RTL_VOIDACCESS          (  5 )
#define         RTL_LSB_BYVAL           (  6 )
#define         RTL_LSB_BYREF           (  7 )
#define         RTL_GSB_BYVAL           (  8 )
#define         RTL_GSB_BYREF           (  9 )
#define         RTL_OPTIMISED           ( 10 )
#define         RTL_CONSTVAL            ( 11 )

/*  ****  end of access methods  ****  */



/*  ****  bits in Version field  ****  */

#define         RTL_NEXTITEM_BIT        ( 1 << 0 ) 
#define         RTL_LINEMARK_BIT        ( 1 << 1 ) 
#define         RTL_LANGUAGE_BITS       ( (1 << 2) | (1 << 3) | (1 << 4) | \
                                          (1 << 5) | (1 << 6) )
#define         RTL_REPLPAR_BIT         ( 1 << 7 ) 
#define         RTL_CODEADDR_BIT        ( 1 << 8 ) 
#define         RTL_PROCEDURE_BIT       ( 1 << 9 ) 
#define         RTL_FILENAME_BIT        ( 1 << 10) 
#define         RTL_COMPACTED_BIT       ( 1 << 11) 
#define         RTL_ORDER_BIT           ( 1 << 12 )
#define         RTL_3LICB_BIT           ( 1 << 13 ) 
#define         RTL_VIRTUAL_LINK_BIT    ( 1 << 14 ) 
#define         RTL_NULL_POINTER_BIT    ( 1 << 15 ) 
#define         RTL_COLUMN_MAJOR_BIT    ( 1 << 16 )
#define         RTL_INDEX_SORTED_BIT    ( 1 << 17 )


#define         RTL_OCCAM_BITS          ( 0 )
#define         RTL_C_BITS              ( 1 << 2 )
#define         RTL_PASCAL_BITS         ( 1 << 3 )
#define         RTL_FORTRAN_BITS        ( (1 << 3) | (1 << 2) )
#define         RTL_ASSEMBLER_BITS      ( 1 << 4 )
#define         RTL_ADA_BITS            ( (1 << 4) | (1 << 2) )
#define         RTL_MODULA_2_BITS       ( (1 << 4) | (1 << 3) )
#define         RTL_IMP_BITS            ( (1 << 4) | (1 << 3) | (1 << 2) )
#define         RTL_C_PLUSPLUS_BITS     ( 1 << 5 )

/*  ****  end of bits in Version field  ****  */



/*  ****  qualifier bit fields  ****  */

#define         RTL_CONSTANT_QUALIFIER  ( 1 << 0 )
#define         RTL_VOLATILE_QUALIFIER  ( 1 << 1 )
#define         RTL_NOALIAS_QUALIFIER   ( 1 << 2 )

/*  ****  end of qualifier bit fields  ****  */



#endif  /*  !__DEBUGHDR_H  */
