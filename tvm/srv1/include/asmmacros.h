// $Id: asmmacros.h,v 1.1 2006/05/04 13:49:26 strubi Exp $
//
// Some asm macros that might not be defined

#ifndef LO
#define LO(x) ((x) & 0xffff)
#endif
#ifndef HI
#define HI(x) (((x) >> 16) & 0xffff)
#endif

