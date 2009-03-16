#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef WIN32
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#	include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD 
#	include <io.h>
#else
#include <dlfcn.h>
#include <unistd.h>
#endif /* WIN32 */

#include "stiw.h"

#ifdef WIN32
#define LIBRARY_EXT ".dll"
#define LIBRARY_PFIX ""
#elif defined _MAC_UNIX
#define LIBRARY_EXT ".dylib"
#define LIBRARY_PFIX "lib"
#else
#define LIBRARY_EXT ".so"
#define LIBRARY_PFIX "lib"
#endif
#define DIRECTORY_PFIX "transterpreter/"

extern int get_file_size(FILE *fp, char* file_name, char* app_name);

const char *error_name = "stiw FFI loader";

#ifdef WIN32
typedef HINSTANCE LIB_HANDLE;
/* Returns error message for last error */
const char* library_error_str(void) 
{ 
    LPVOID lpMsgBuf;
    DWORD dw = GetLastError(); 

    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
        FORMAT_MESSAGE_FROM_SYSTEM,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    /* 
		printf( "failed with error %d: %s", dw, lpMsgBuf); 
    LocalFree(lpMsgBuf);
		Is it ok that we dont bother to free this anymore? The program is going 
		to quit right afterwards anyway...
		*/
		return (char *) lpMsgBuf;
}

HINSTANCE load_library(char *filename)
{
	return LoadLibrary(filename);
}

void *get_symbol(LIB_HANDLE sohandle, char *name)
{
	return GetProcAddress(sohandle, name);
}
#else
typedef void *LIB_HANDLE;
const char* library_error_str(void)
{
	return dlerror();
}

/* Wrap dlopen */
LIB_HANDLE load_library(char *filename)
{
  void *lib_ptr;
	char *newlibname;
	lib_ptr = dlopen(filename, RTLD_NOW);

  /* Attempt to load a library from the transterpreter library directory if its not found */
	if (lib_ptr == NULL) {
	  newlibname = (char *)malloc(strlen(filename) + strlen(DIRECTORY_PFIX));
		strcpy(newlibname, DIRECTORY_PFIX);
		strcat(newlibname, filename);
	  lib_ptr = dlopen(newlibname, RTLD_NOW);
		free(newlibname);
	}

	return lib_ptr; 
}

void *get_symbol(LIB_HANDLE sohandle, char *name)
{
	return dlsym(sohandle, name);
}
#endif

/* A structure which contains information about loaded libraries */
struct lib
{
	char *name;
	LIB_HANDLE solib;
};

int setup_ffi_table(char *tbc_file)
{
	FILE *ffi_file_fp;
	char *ffi_file;
	long file_size, table_start;
	FFI_TABLE_ENTRY *ffi_table_ptr;

	char header[8];
	int num_libraries;
	struct lib *libraries = NULL;
	
	int i;

	ffi_table = NULL;

	/* We need a new filename to load the ffi file, set up some memory */
	ffi_file = strdup(tbc_file);
	/* Now replace the extension with .ffi, we know that the extension is
	 * allways going to be three chars, ie .tbc, so we can just overwrite
	 * the last three chars in the string with "ffi", */
  /* But only if its not a tbz file */
  if(file_type == TBZ)
  {
    /* Is there any FFI at all??? If no, just return. */
    if(ffi_start == 0)
      return 0;
  }
  else
  {
    strcpy(ffi_file + (strlen(ffi_file) - 3), "ffi");
  }

	/* Now open in the file */
	ffi_file_fp = fopen(ffi_file, "rb");
	if(ffi_file_fp == NULL)
	{
		/* The file's not there; nothing to load. */
		return 0;
	}
  /* If we are reading from a TBZ, then seek to the right place */
  if(file_type == TBZ)
  {
    fseek(ffi_file_fp, ffi_start, SEEK_SET);
    file_size = ffi_length;
  }
  else
    file_size = get_file_size(ffi_file_fp, ffi_file, "");

	if (fread(header, 1, 8, ffi_file_fp) != 8)
	{
		printf("%s: error reading header from %s\n", error_name, ffi_file);
		goto error;
	}
	if (memcmp("ffitvm\0\0", header, 8) != 0)
	{
		printf("%s: %s contains an invalid header\n", error_name, ffi_file);
		goto error;
	}

	if (fread(&num_libraries, sizeof num_libraries, 1, ffi_file_fp) != 1)
	{
		printf("%s: error reading num_libraries from %s\n", error_name, ffi_file);
		goto error;
	}
#ifdef HOST_BIGENDIAN
	num_libraries = SwapFourBytes(num_libraries);
#endif

	libraries = (struct lib *) malloc(num_libraries * sizeof *libraries);

	/* Load the libraries */
	for (i = 0; i < num_libraries; i++)
	{
		char length;

		/* First we have the length of the string (including null) */
		if (fread(&length, 1, 1, ffi_file_fp) != 1)
		{
			printf("%s: error reading library name length from %s\n", error_name, ffi_file);
			goto error;
		}

		/* Make us some new memory to put the name in, few more bytes so we can
		 * put an extension and prefix on the name plus one for \0*/
		libraries[i].name = (char *) malloc(length + strlen(LIBRARY_EXT) + strlen(LIBRARY_PFIX));

		/* Read the name */
		if (fread(&libraries[i].name[strlen(LIBRARY_PFIX)], 1, length, ffi_file_fp) != length)
		{
			printf("%s: error reading library name from %s\n", error_name, ffi_file);
			goto error;
		}

		/* Add a prefix without \0*/
		strncpy(libraries[i].name, LIBRARY_PFIX, strlen(LIBRARY_PFIX));

		/* Add an extension */
		strcat(libraries[i].name, LIBRARY_EXT);
		// FIXME: This is debug code that should dissapear once tested on all platforms.
		/*printf("Loading %s, allocated %i\n", libraries[i].name, length + strlen(LIBRARY_EXT) + strlen(LIBRARY_PFIX) ); */

		/* Load the library */
		libraries[i].solib = load_library(libraries[i].name);
		if(libraries[i].solib == NULL)
		{
			printf("%s: failed to open library %s: %s\n", error_name, libraries[i].name, library_error_str());
			goto error;
		}
	}

	/* Advance to next 64-bit aligned address */
	table_start = ftell(ffi_file_fp);
	if (table_start % 8 != 0) {
		table_start += 8 - (table_start % 8);
		if (fseek(ffi_file_fp, table_start, SEEK_SET) == -1)
		{
			printf("%s: seek failed on %s\n", error_name, ffi_file);
			goto error;
		}
	}

	{
		int table_size = file_size;

		ffi_table = (FFI_TABLE_ENTRY *) malloc(table_size);
		memset(ffi_table, 0, table_size);
		if (fread(ffi_table, 1, table_size, ffi_file_fp) != table_size)
		{
			printf("%s: error reading FFI table from %s\n", error_name, ffi_file);
			goto error;
		}
	}

	/* Resolve all the references in the table */
	for (ffi_table_ptr = ffi_table; (int) ffi_table_ptr->name != 0; ffi_table_ptr++)
	{
		int name_offset = (int) ffi_table_ptr->name;
#ifdef HOST_BIGENDIAN
		name_offset = SwapFourBytes(name_offset);
#endif
		ffi_table_ptr->name = ((char *) ffi_table) + name_offset;
		ffi_table_ptr->func = NULL;

		/* Try resolving from all loaded libraries */
		for(i = 0; i < num_libraries; i++)
		{
			FFI_FUNCTION symbol_ptr = (FFI_FUNCTION) get_symbol(libraries[i].solib, ffi_table_ptr->name);

			if (symbol_ptr != NULL)
			{
				if (ffi_table_ptr->func != NULL)
				{
					printf("%s: symbol %s defined multiple times\n", error_name, ffi_table_ptr->name);
					goto error;
				} else {
					ffi_table_ptr->func = symbol_ptr;
				}
			}
		}

		/* Check that we resolved the symbol to an ffi call */
		if (ffi_table_ptr->func == NULL)
		{
			printf("%s: failed to resolve %s: %s\n", error_name, ffi_table_ptr->name, library_error_str());
			goto error;
		}
	}

	for (i = 0; i < num_libraries; i++)
	{
		free(libraries[i].name);
	}
	free(libraries);
	fclose(ffi_file_fp);
	free(ffi_file);
	return 0;

error:
	exit(1);
}


