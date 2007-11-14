/* $Id: pdefs.h,v 1.1 1996/04/15 10:52:17 djb1 Exp $ */

/*
 *	macro's and constants used by the parse utility (text parsing in new tools)
 *	Copyright (C) 1991 Inmos Limited
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
    Declare the data structure for the parse data system.
*/

typedef struct 
  {
    int options;
    char *whole_string;
    char *string_pointer;
    char *token_pointer;
    int token_length;
    long number;
    char string[256];
  } PARSE_DATA_TYPE;

/*
    The declaration of options values, zero will imply no options at all.
*/

#define PARSE_ABREVIATIONS 1

/*
    The declaration of token values.
*/

#define PARSE_EOS      0
#define PARSE_LAMBDA   1
#define PARSE_BLANK    2
#define PARSE_ANY      3
#define PARSE_ALPHA    4
#define PARSE_DIGIT    5
#define PARSE_SYMBOL   6
#define PARSE_OSYMBOL  7
#define PARSE_DECIMAL  8
#define PARSE_HEX      9
#define PARSE_STRING   10

/*
    And finally the declaration of the macro's themselves starting
    with the begin and end, then defining the state begin and end, and
    finally the transition macro's.
*/

#define START_PARSE(s,o)         PDATA->whole_string = s; PDATA->string_pointer = s; PDATA->options = o
#define END_PARSE                PARSE_FAIL: return 0; PARSE_EXIT: return 1

#define START_ROUTINE            PARSE_DATA_TYPE saved; saved = *PDATA
#define END_ROUTINE              PARSE_FAIL: *PDATA=saved; return 0; PARSE_EXIT:PDATA->token_pointer=saved.string_pointer;PDATA->token_length=PDATA->string_pointer-PDATA->token_pointer; return 1

#define STATE(l)                 l:
#define END_STATE                goto PARSE_FAIL

#define TRAN_ON_STRING(s,l)      if( TRAN_STRING(s,PDATA) ) goto l
#define TRAN_ON_CHAR(c,l)        if( TRAN_CHAR(c,PDATA) ) goto l
#define TRAN_ON_TOKEN(t,l)       if( TRAN_TOKEN(t,PDATA) ) goto l
#define TRAN_ON_SUBROUTINE(s,l)  if( s() ) goto l 

/*
    And now some utility macro's to assist in error reporting.
*/

#define PRINT_LINE               printf( "%s\n", PDATA->whole_string )

#define PRINT_TOKEN              {int I;for(I=0;I<PDATA->token_length;printf("%c",*(PDATA->token_pointer+(I++))));}

#define PRINT_NEXT               {char *P=PDATA->string_pointer;while (*P&&(*P!=' ')&&(*P!='\t'))printf("%c",*(P++));}
