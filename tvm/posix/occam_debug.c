#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD 
#endif

#ifdef WIN32
#	include <io.h>
	/* The fact that these are not defined in the headers, does not
	 * leave me with a great deal of confidence.
	 */
#	ifndef F_OK
#		define F_OK  0
#	endif /* F_OK */
	/* FIXME: This is probably a bit naughty, but what the heck... */
#	ifndef R_OK
#		define R_OK  0
#	endif /* F_OK */
#else
#include <unistd.h>
#endif /* WIN32 */

#include "stiw.h"
#include "occam_debug.h"
#include "tbzutil.h"

/* FIXME: */
extern BYTEPTR code_start;
extern int file_type;

void read_and_check(unsigned int *var, FILE *fp)
{
	int result;

	result = fread(var, 4, 1, fp);
	if(result != 1)
	{
		printf("Error while reading debugging information (2)\n");
		exit(1);
	}
#ifndef HOST_BIGENDIAN	
	*var = SwapFourBytes(*var);
#endif /* WIN32 */
}

void find_dbg_info(char *tdb_filename)
{
	FILE *fp;
	char filename[5000]; /* FIXME: This needs to be limited sensibly in the slinker */
	int more_files = 1;
	int more_offsets;
	int result;

	/* iptr points to the next instruction,
	 * iptr_prime points to the current instruction 
	 */
	BYTEPTR iptr_prime = byteptr_minus(iptr, 1);
	unsigned int code_offset = iptr_prime - code_start;

	//printf("code_offset %d, iptr: %d, code_start: %d\n", code_offset, iptr, code_start);

	fp = fopen(tdb_filename, "rb");
	if(fp == 0)
	{
		printf("Error opening debug file.\n");
		exit(1);
	}

  /* Now (if TBZ) we need to seek to the right place in the file to get teh
   * debugging info... this comes from tbzutil.h */
  if(file_type == TBZ)
  {
    fseek(fp, debug_start, SEEK_SET);
    /* At this point the file should look the same as if we were reading stuff
     * from an individual file, so we just continue with the old code */
  }


	while(more_files)
	{

		/* Damn spaces in filenames */
		/* result = fscanf(fp, "%s\n", filename); */
		result = fscanf(fp, "%[^\n]\n", filename);
		
		/* Did we get to the end of the file? */
		if(feof(fp))
		{
			/* Thats it then */
			return;
		}

		/* printf("f: %s\n", filename); */
		
		/* If we got an error, but not EOF, thats bad for some reason, not quite
		 * know to us at this time...
		 */
		if(result != 1)
		{
			printf("Error while reading debugging information (1)\n");
			exit(1);
		}

		{
			/* We are storing the current line number, and the next line number, as we
			 * want to do the following:
			 *   current_offset <= iptr_offset < next_offset
			 * in order to figure out what line we are on.
			 */
			unsigned int current_offset, next_offset;
			unsigned int current_linenum, next_linenum;

			/* Read the very first linenumber and offset */
			read_and_check(&current_offset, fp);
			read_and_check(&current_linenum, fp);

			/* Check to see if it is the last offset in this file(?) Can this happen? Oh
			 * well, not going to take any chances */
			if(current_linenum == 0)
				more_offsets = 0;
			else
				more_offsets = 1;

			/* While there are more offsets, lets load and check them */
			while(more_offsets)
			{
				/* Read the next linenumber and offset */
				read_and_check(&next_offset, fp);
				read_and_check(&next_linenum, fp);

				/*
				printf("c: 0x%08x 0x%08x\n", current_offset, current_linenum);
				printf("n: 0x%08x 0x%08x\n", next_offset, next_linenum);
				*/
				
				if((current_offset <= code_offset) && (code_offset < next_offset))
				{
					printf("The error occured on or around line %d in %s\n", current_linenum, filename);
				}

				/* Was this the last linenumber in this file? */
				if(next_linenum == 0)
					more_offsets = 0;

				/* Make the 'next' (offset, linenumber) pair the current pair */
				current_offset = next_offset;
				current_linenum = next_linenum;
			}
		}
	}
}

/* Try to load in the debugging info, make sense of it, and print something
 * sensible for the user.
 */
void print_debug_info(char *tbc_filename)
{
	char *tdb_filename;

  /* If we are dealing with a TBZ type file, we'll actually just be opening the
   * normal file as opposed to a special debuginfo file */
  if(file_type == TBZ)
  {
    /* If debug_start is 0, there is no debugging information */
    if(debug_start == 0)
    {
		  printf("%s contains no debugging information\n", tbc_filename); 
      return;
    }

    tdb_filename = tbc_filename;
  }
  else
  {
    /* We need a new filename to load the tdb file, set up some memory */
    tdb_filename = (char *)malloc(strlen(tbc_filename) + 4);
    /* Copy the contents of the tbc_name in... */
    strcpy(tdb_filename, tbc_filename);
    /* Now replace the extension with .tvmdbg */
    strcpy(tdb_filename + (strlen(tdb_filename) - 3), "tvmdbg");
  }

	if(access(tdb_filename, R_OK) == 0)
	{
		/* I am guessing that the fact that we are going to print debugging info for
		 * the user, should clue them up to the fact that debugging info was found 
		 * printf("Debugging information found!\n");
		 */
		find_dbg_info(tdb_filename);
	}
	else
	{
		printf("No debugging information found\n");
	}

  if(file_type != TBZ)
    free(tdb_filename);
}
