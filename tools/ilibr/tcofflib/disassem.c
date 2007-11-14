/*
 *	Disassembly functions
 *	Copyright (C) 1990 Inmos Limited
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

/* Copyright 1990 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:DISASSEM_C@505.AAAA-FILE;2(07-OCT-92)[FROZEN] */
/******************************************************************************
*
*  24 JAN 90: pauls:  increased max column width from 4 to 8.
*  25 JAN 90: pauls:  added right hand character column to output.
*     JAN 91: agw  :  modified to cope with T9000.
*   7 OCT 92: ahp  :  VAX had to have # in column 1 for #if 0 ...
*
*******************************************************************************/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <string.h>
/* #ifdef __STDC__ */
#ifdef STD_C
#include <stdlib.h>
#endif
#include "toolkit.h"

#ifdef _MSC
#define switch(x) switch ((int)(x))
#endif

#define FIELDS   8
#define SPACES   (FIELDS * 3)
/*{{{   stuff   */
#define  j      0x0
#define  pfix   0x2
#define  nfix   0x6
#define  call   0x9
#define  cj     0xA
#define  opr    0xF

PRIVATE char *direct[16] =
{
  "j",   "ldlp", "pfix", "ldnl", "ldc", "ldnlp", "pfix", "ldl",
  "adc", "call", "cj",   "ajw",  "eqc", "stl",   "stnl", "opr"
};

#define MIN_OPR_FUNC (-48)
#define MAX_OPR_FUNC 380
#define TOTAL_OPR_FUNC (MAX_OPR_FUNC + (-MIN_OPR_FUNC)) + 1

PRIVATE char *all_opr_funcs[TOTAL_OPR_FUNC] =
{
  /*{{{   vast quantity of initialisers   */
  "fdca",           /* -30  */
  "ica",            /* -2F  */
  "fdcl",           /* -2E  */
  "icl",            /* -2D  */
  "",               /* -2C  */
  "",               /* -2B  */
  "",               /* -2A  */
  "",               /* -29  */
  "ldresptr",       /* -28  */
  "stresptr",       /* -27  */
  "erdsq",          /* -26  */
  "irdsq",          /* -25  */
  "mkrc",           /* -24  */
  "unmkrc",         /* -23  */
  "",               /* -22  */
  "",               /* -21  */
  "",               /* -20  */
  "grant",          /* -1F  */
  "enbg",           /* -1E  */
  "disg",           /* -1D  */
  "readhdr",        /* -1C  */
  "writehdr",       /* -1B  */
  "initvlcb",       /* -1A  */
  "setchmode",      /* -19  */
  "sethdr",         /* -18  */
  "swapbfr",        /* -17  */
  "ldvlcb",         /* -16  */
  "stvlcb",         /* -15  */
  "vin",            /* -14  */
  "vout",           /* -13  */
  "stopch",         /* -12  */
  "",               /* -11  */
  "swapqueue",      /* -10  */
  "swaptimer",      /* -F  */
  "insertqueue",    /* -E  */
  "timeslice",      /* -D  */
  "signal",         /* -C  */
  "wait",           /* -B  */
  "",               /* -A  */
  "swapgstatus",    /* -9  */
  "syscall",        /* -8  */
  "selth",          /* -7  */
  "goprot",         /* -6  */
  "tret",           /* -5  */
  "ldshadow",       /* -4  */
  "stshadow",       /* -3  */
  "",               /* -2  */
  "",               /* -1  */
  "rev",            /*  0  */  /* <------- opr_func points here */
  "lb",             /*  1  */
  "bsub",           /*  2  */  
  "endp",           /*  3  */
  "diff",           /*  4  */
  "add",            /*  5  */
  "gcall",          /*  6  */
  "in",             /*  7  */
  "prod",           /*  8  */
  "gt",             /*  9  */
  "wsub",           /*  A  */
  "out",            /*  B  */
  "sub",            /*  C  */
  "startp",         /*  D  */
  "outbyte",        /*  E  */
  "outword",        /*  F  */
  "seterr",         /*  10  */
  "",               /*  11  */
  "resetch",        /*  12  */
  "csub0",          /*  13  */
  "",               /*  14  */
  "stopp",          /*  15  */
  "ladd",           /*  16  */
  "stlb",           /*  17  */
  "sthf",           /*  18  */
  "norm",           /*  19  */
  "ldiv",           /*  1A  */
  "ldpi",           /*  1B  */
  "stlf",           /*  1C  */
  "xdble",          /*  1D  */
  "ldpri",          /*  1E  */
  "rem",            /*  1F  */
  "ret",            /*  20  */
  "lend",           /*  21  */
  "ldtimer",        /*  22  */
  "testlds",        /*  23  */
  "testlde",        /*  24  */
  "testldd",        /*  25  */
  "teststs",        /*  26  */
  "testste",        /*  27  */
  "teststd",        /*  28  */
  "testerr",        /*  29  */
  "testpranal",     /*  2A  */
  "tin",            /*  2B  */
  "div",            /*  2C  */
  "testhardchan",   /*  2D  */
  "dist",           /*  2E  */
  "disc",           /*  2F  */
  "diss",           /*  30  */
  "lmul",           /*  31  */
  "not",            /*  32  */
  "xor",            /*  33  */
  "bcnt",           /*  34  */
  "lshr",           /*  35  */
  "lshl",           /*  36  */
  "lsum",           /*  37  */
  "lsub",           /*  38  */
  "runp",           /*  39  */
  "xword",          /*  3A  */
  "sb",             /*  3B  */
  "gajw",           /*  3C  */
  "savel",          /*  3D  */
  "saveh",          /*  3E  */
  "wcnt",           /*  3F  */
  "shr",            /*  40  */
  "shl",            /*  41  */
  "mint",           /*  42  */
  "alt",            /*  43  */
  "altwt",          /*  44  */
  "altend",         /*  45  */
  "and",            /*  46  */
  "endbt",          /*  47  */
  "endbc",          /*  48  */
  "endbs",          /*  49  */
  "move",           /*  4A  */
  "or",             /*  4B  */
  "csngl",          /*  4C  */
  "ccnt1",          /*  4D  */
  "talt",           /*  4E  */
  "ldiff",          /*  4F  */
  "sthb",           /*  50  */
  "taltwt",         /*  51  */
  "sum",            /*  52  */
  "mul",            /*  53  */
  "sttimer",        /*  54  */
  "stoperr",        /*  55  */
  "cword",          /*  56  */
  "clrhalterr",     /*  57  */
  "sethalterr",     /*  58  */
  "testhalterr",    /*  59  */
  "dup",            /*  5A  */
  "move2dinit",     /*  5B  */
  "move2dall",      /*  5C  */
  "move2dnonzero",  /*  5D  */
  "move2dzero",     /*  5E  */
  "gtu",            /*  5F  */
  "",               /*  60  */
  "",               /*  61  */
  "",               /*  62  */
  "unpacksn",       /*  63  */
  "",               /*  64  */
  "",               /*  65  */
  "",               /*  66  */
  "",               /*  67  */
  "",               /*  68  */
  "",               /*  69  */
  "",               /*  6A  */
  "",               /*  6B  */
  "postnormsn",     /*  6C  */
  "roundsn",        /*  6D  */
  "",               /*  6E  */
  "",               /*  6F  */
  "",               /*  70  */
  "ldinf",          /*  71  */
  "fmul",           /*  72  */
  "cflerr",         /*  73  */
  "crcword",        /*  74  */
  "crcbyte",        /*  75  */
  "bitcnt",         /*  76  */
  "bitrevword",     /*  77  */
  "bitrevnbits",    /*  78  */
  "pop",            /*  79  */
  "timerdisableh",  /*  7A  */
  "timerdisablel",  /*  7B  */
  "timerenableh",   /*  7C  */
  "timerenablel",   /*  7D  */
  "ldmemstartval",  /*  7E  */
  "",               /*  7F  */
  "fpsttest",       /*  80  */
  "wsubdb",         /*  81  */
  "fpldnlbi",       /*  82  */
  "fpchkerr",       /*  83  */
  "fpstnldb",       /*  84  */
  "fpldtest",       /*  85  */
  "fpldnlsni",      /*  86  */
  "fpadd",          /*  87  */
  "fpstnlsn",       /*  88  */
  "fpsub",          /*  89  */
  "fpldnldb",       /*  8A  */
  "fpmul",          /*  8B  */
  "fpdiv",          /*  8C  */
  "fprange",        /*  8D  */
  "fpldnlsn",       /*  8E  */
  "fpremfirst",     /*  8F  */
  "fpremstep",      /*  90  */
  "fpnan",          /*  91  */
  "fpordered",      /*  92  */
  "fpnotfinite",    /*  93  */
  "fpgt",           /*  94  */
  "fpeq",           /*  95  */
  "fpi32tor32",     /*  96  */
  "fpge",           /*  97  */
  "fpi32tor64",     /*  98  */
  "",               /*  99  */
  "fpb32tor64",     /*  9A  */
  "fplg",           /*  9B  */
  "fptesterr",      /*  9C  */
  "fprtoi32",       /*  9D  */
  "fpstnli32",      /*  9E  */
  "fpldzerosn",     /*  9F  */
  "fpldzerodb",     /*  A0  */
  "fpint",          /*  A1  */
  "",               /*  A2  */
  "fpdup",          /*  A3  */
  "fprev",          /*  A4  */
  "",               /*  A5  */
  "fpldnladddb",    /*  A6  */
  "",               /*  A7  */
  "fpldnlmuldb",    /*  A8  */
  "",               /*  A9  */
  "fpldnladdsn",    /*  AA  */
  "fpentry",        /*  AB  */
  "fpldnlmulsn",    /*  AC  */
  "",               /*  AD  */
  "",               /*  AE  */
  "",               /*  AF  */
  "settimeslice",   /*  B0  */
  "break",          /*  B1  */
  "clrj0break",     /*  B2  */
  "setj0break",     /*  B3  */
  "testj0break",    /*  B4  */
  "ldproc",         /*  B5  */
  "ldflags",        /*  B6  */
  "stflags",        /*  B7  */
  "xbword",         /*  B8  */
  "lbx",            /*  B9  */
  "cb",             /*  BA  */
  "cbu",            /*  BB  */
  "insphdr",        /*  BC  */
  "readbfr",        /*  BD  */
  "ldconf",         /*  BE  */
  "stconf",         /*  BF  */
  "ldcnt",          /*  C0  */
  "ssub",           /*  C1  */
  "ldth",           /*  C2  */
  "ldchstatus",     /*  C3  */
  "intdis",         /*  C4  */
  "intenb",         /*  C5  */
  "",               /*  C6  */
  "cir",            /*  C7  */
  "ss",             /*  C8  */
  "chantype",       /*  C9  */
  "ls",             /*  CA  */
  "fpseterr",       /*  CB  */
  "ciru",           /*  CC  */
  "",               /*  CD  */
  "",               /*  CE  */
  "fprem",          /*  CF  */
  "fprn",           /*  D0  */
  "fpdivby2",       /*  D1  */
  "fpmulby2",       /*  D2  */
  "fpsqrt",         /*  D3  */
  "fprp",           /*  D4  */
  "fprm",           /*  D5  */
  "fprz",           /*  D6  */
  "fpr32tor64",     /*  D7  */
  "fpr64tor32",     /*  D8  */
  "fpexpdec32",     /*  D9  */
  "fpexpinc32",     /*  DA  */
  "fpabs",          /*  DB  */
  "fpclrerr",       /*  DC  */
  "fpadddbsn",      /*  DD  */
  "fpchki32",       /*  DE  */
  "fpchki64",       /*  DF  */
  "",               /*  E0  */
  "",               /*  E1  */
  "",               /*  E2  */
  "",               /*  E3  */
  "",               /*  E4  */
  "",               /*  E5  */
  "",               /*  E6  */
  "",               /*  E7  */
  "",               /*  E8  */
  "",               /*  E9  */
  "",               /*  EA  */
  "",               /*  EB  */
  "",               /*  EC  */
  "",               /*  ED  */
  "",               /*  EE  */
  "",               /*  EF  */
  "",               /*  F0  */
  "",               /*  F1  */
  "",               /*  F2  */
  "",               /*  F3  */
  "",               /*  F4  */
  "",               /*  F5  */
  "",               /*  F6  */
  "",               /*  F7  */
  "xsword",         /*  F8  */
  "lsx",            /*  F9  */
  "cs",             /*  FA  */
  "csu",            /*  FB  */
  "",               /*  FC  */ /* Tons of space 'til lddevid */
  "",               /*  FD  */
  "",               /*  FE  */
  "",               /*  FF  */
  "",               /* 100 */
  "",               /* 101 */
  "",               /* 102 */
  "",               /* 103 */
  "",               /* 104 */
  "",               /* 105 */
  "",               /* 106 */
  "",               /* 107 */
  "",               /* 108 */
  "",               /* 109 */
  "",               /* 10A */
  "",               /* 10B */
  "",               /* 10C */
  "",               /* 10D */
  "",               /* 10E */
  "",               /* 10F */
  "",               /* 110 */
  "",               /* 111 */
  "",               /* 112 */
  "",               /* 113 */
  "",               /* 114 */
  "",               /* 115 */
  "",               /* 116 */
  "",               /* 117 */
  "",               /* 118 */
  "",               /* 119 */
  "",               /* 11A */
  "",               /* 11B */
  "",               /* 11C */
  "",               /* 11D */
  "",               /* 11E */
  "",               /* 11F */
  "",               /* 120 */
  "",               /* 121 */
  "",               /* 122 */
  "",               /* 123 */
  "",               /* 124 */
  "",               /* 125 */
  "",               /* 126 */
  "",               /* 127 */
  "",               /* 128 */
  "",               /* 129 */
  "",               /* 12A */
  "",               /* 12B */
  "",               /* 12C */
  "",               /* 12D */
  "",               /* 12E */
  "",               /* 12F */
  "",               /* 130 */
  "",               /* 131 */
  "",               /* 132 */
  "",               /* 133 */
  "",               /* 134 */
  "",               /* 135 */
  "",               /* 136 */
  "",               /* 137 */
  "",               /* 138 */
  "",               /* 139 */
  "",               /* 13A */
  "",               /* 13B */
  "",               /* 13C */
  "",               /* 13D */
  "",               /* 13E */
  "",               /* 13F */
  "",               /* 140 */
  "",               /* 141 */
  "",               /* 142 */
  "",               /* 143 */
  "",               /* 144 */
  "",               /* 145 */
  "",               /* 146 */
  "",               /* 147 */
  "",               /* 148 */
  "",               /* 149 */
  "",               /* 14A */
  "",               /* 14B */
  "",               /* 14C */
  "",               /* 14D */
  "",               /* 14E */
  "",               /* 14F */
  "",               /* 150 */
  "",               /* 151 */
  "",               /* 152 */
  "",               /* 153 */
  "",               /* 154 */
  "",               /* 155 */
  "",               /* 156 */
  "",               /* 157 */
  "",               /* 158 */
  "",               /* 159 */
  "",               /* 15A */
  "",               /* 15B */
  "",               /* 15C */
  "",               /* 15D */
  "",               /* 15E */
  "",               /* 15F */
  "",               /* 160 */
  "",               /* 161 */
  "",               /* 162 */
  "",               /* 163 */
  "",               /* 164 */
  "",               /* 165 */
  "",               /* 166 */
  "",               /* 167 */
  "",               /* 168 */
  "",               /* 169 */
  "",               /* 16A */
  "",               /* 16B */
  "",               /* 16C */
  "",               /* 16D */
  "",               /* 16E */
  "",               /* 16F */
  "",               /* 170 */
  "",               /* 171 */
  "",               /* 172 */
  "",               /* 173 */
  "",               /* 174 */
  "",               /* 175 */
  "",               /* 176 */
  "",               /* 177 */
  "",               /* 178 */
  "",               /* 179 */
  "",               /* 17A */
  "",               /* 17B */
  "lddevid",        /* 17C */
  /*}}}*/
};

PRIVATE char **opr_funcs = &all_opr_funcs[-MIN_OPR_FUNC];

/*}}}*/
/*{{{   PUBLIC void disassemble (pos, fs, print_rel)   */
PUBLIC void disassemble (pos, fs, print_rel)
const long int pos;
FILE *fs;
int print_rel;
{
  /*{{{   setup   */
  int c, print_pos, space, cpos;
  char blanks[SPACES], chars[FIELDS + 1];
  long int i, Oreg, op, val, posn;
  print_pos = TRUE;
  posn = pos;
  space = SPACES;
  for (i = 0; i < SPACES; i++) blanks[i] = ' ';
  Oreg = 0L;
  cpos = 0;
  c = dis_get_char ();
  
  /*}}}*/
  
  while (c != EOF)
  {
    op = ((long int) c) >> 4L;
    val = ((long int) c) & 0xFL;
    /*{{{   fill Oreg   */
    switch (op)
    {
      case (nfix):   Oreg = (~(Oreg | (long int) val)) << 4;    break;
      case (pfix):   Oreg = (Oreg | (long int) val) << 4;       break;
      default:       Oreg = Oreg | (long int) val;              break;
    }
    /*}}}*/
    /*{{{   print field   */
    if ((print_pos) || (space == 0))
    {
      if (space == 0)
      {
        chars[cpos] = '\0';
        fprintf (fs, "                                 %s", chars);
        fprintf (fs, "\n%08lX ", posn);
        space = SPACES;
        cpos = 0;
      }
      else fprintf (fs, "%08lX ", posn); /* initial case */
    }
    if ((c >= ' ') && (c <= '~')) chars[cpos++] = c;
    else chars[cpos++] = '.';
    fprintf (fs, "%02X ", c);
    space = space - 3;
    /*}}}*/
    /*{{{   print op   */
    if (((op != pfix) && (op != nfix)) ||
        ((op == pfix) && (val == 0L) && (Oreg == 0L)))
    {
      blanks[space] = '\0';
      fputs (blanks, fs);
      switch (op)
      {
        case j:
        case cj:
        case call:
          if (print_rel) fprintf (fs, "%-9s%-12ld (%08lX) ", direct[op], Oreg, posn + Oreg + 1L);
          else fprintf (fs, "%-9s%-12ld            ", direct[op], Oreg);
          Oreg = 0L;
          break;
        case pfix:
          if (val == 0L) fprintf (fs, "%-9s%-12ld            ", direct[op], val);
          break;
        case opr:
          if ((Oreg <= MAX_OPR_FUNC) && (Oreg >= MIN_OPR_FUNC) && (strlen (opr_funcs[Oreg]) != 0))
            fprintf (fs, "%-13s                    ", opr_funcs[Oreg]);
          else
            fprintf (fs, "%-9s%-12ld            ", direct[op], Oreg);
          Oreg = 0L;
          break;
        default:
          fprintf (fs, "%-9s%-12ld            ", direct[op], Oreg);
          Oreg = 0L;
          break;
      }
      blanks[space] = ' ';
      space = SPACES;
      chars[cpos] = '\0';
      fprintf (fs, "%s\n", chars);
      cpos = 0;
      print_pos = TRUE;
    }
    else print_pos = FALSE; /* dont print next position */
    /*}}}*/
    posn++;
    c = dis_get_char ();
  }
}
/*}}}*/
