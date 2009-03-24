/*
 * ffi.c - Foreign Function Interface support functions
 * 
 * Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud, Damian J. Dimmich
 * Copyright (C) 2008  Carl G. Ritson
 *
 */

#include "tvm_posix.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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

#ifdef WIN32
typedef HINSTANCE LIB_HANDLE;

static const char *library_error_str (void) 
{ 
	LPVOID lpMsgBuf;
	DWORD dw = GetLastError (); 

	FormatMessage (
			FORMAT_MESSAGE_ALLOCATE_BUFFER | 
			FORMAT_MESSAGE_FROM_SYSTEM,
			NULL,
			dw,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			(LPTSTR) &lpMsgBuf,
			0, NULL 
	);

	/* 
	   printf( "failed with error %d: %s", dw, lpMsgBuf); 
	   LocalFree(lpMsgBuf);
	   Is it ok that we dont bother to free this anymore? The program is going 
	   to quit right afterwards anyway...
	   */
	return (char *) lpMsgBuf;
}

static LIB_HANDLE load_library (char *filename)
{
	return LoadLibrary (filename);
}

static void unload_library (LIB_HANDLE handle)
{
}

static void *get_symbol (LIB_HANDLE handle, char *name)
{
	return GetProcAddress (handle, name);
}
#else /* !WIN32 */
typedef void *LIB_HANDLE;

static const char *library_error_str (void)
{
	return dlerror ();
}

static LIB_HANDLE load_library (char *filename)
{
	return dlopen (filename, RTLD_NOW);
}

static void unload_library (LIB_HANDLE handle)
{
}

static void *get_symbol (LIB_HANDLE handle, char *name)
{
	return dlsym (handle, name);
}
#endif /* !WIN32 */

int build_ffi_table (bytecode_t *bc)
{
	FFI_TABLE_ENTRY *table;
	LIB_HANDLE 	*libs;
	tbc_ffi_t	*ffi = bc->tbc->ffi;
	
	if (ffi == NULL)
		return 1;
	
	return 0;
}

void release_ffi_table (bytecode_t *bc)
{
	if (bc->ffi_table == NULL)
		return;
}

