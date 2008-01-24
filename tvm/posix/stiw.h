
#ifndef STIW_H
#define STIW_H

#ifndef STIWTVM
extern unsigned char transputercode[];
#endif

extern int memsize;
extern int instsize;
extern int filetype;

/* Needed by borland for the ffi.c, gcc seems to not complain.. */
int get_file_size(FILE *fp, char* file_name, char* app_name);
#endif
