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

#ifdef TVM_OS_WINDOWS
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#define interface WIN_interface
#	include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD
#undef interface
#	include <io.h>
#else
#include <dlfcn.h>
#include <unistd.h>
#endif /* TVM_OS_WINDOWS */

#if defined(TVM_OS_WINDOWS)
#define LIBRARY_EXT ".dll"
#define LIBRARY_PFIX ""
#elif defined(TVM_OS_DARWIN)
#define LIBRARY_EXT ".dylib"
#define LIBRARY_PFIX "lib"
#else
#define LIBRARY_EXT ".so"
#define LIBRARY_PFIX "lib"
#endif

#ifdef TVM_OS_WINDOWS
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

static LIB_HANDLE _load_library (char *filename)
{
	return LoadLibrary (filename);
}

static void unload_library (LIB_HANDLE handle)
{
	/* FIXME: add this code...? */
}

static void *get_symbol (LIB_HANDLE handle, char *name)
{
	return GetProcAddress (handle, name);
}
#else /* !TVM_OS_WINDOWS */
typedef void *LIB_HANDLE;

static const char *library_error_str (void)
{
	return dlerror ();
}

static LIB_HANDLE _load_library (char *filename)
{
	return dlopen (filename, RTLD_NOW);
}

static void unload_library (LIB_HANDLE handle)
{
	dlclose (handle);
}

static void *get_symbol (LIB_HANDLE handle, char *name)
{
	return dlsym (handle, name);
}
#endif /* !TVM_OS_WINDOWS */

static LIB_HANDLE load_library (char *name)
{
	LIB_HANDLE handle;
	char buffer[4096];
	
	buffer[sizeof(buffer) - 1] = '\0';

	snprintf (buffer, sizeof (buffer) - 1, "%s%s%s",
		LIBRARY_PFIX, name, LIBRARY_EXT
	);
	if ((handle = _load_library (buffer)) == NULL) {
		snprintf (buffer, sizeof (buffer), "%s%s%s%s",
			TVM_LIBRARY_PATH,
			LIBRARY_PFIX, name, LIBRARY_EXT
		);
		handle = _load_library (buffer);
	}

	return handle;
}

int build_ffi_table (bytecode_t *bc)
{
	FFI_TABLE_ENTRY *table		= NULL;
	LIB_HANDLE 	*libs		= NULL;
	tbc_ffi_t	*ffi 		= bc->tbc->ffi;
	char 		**lib_names	= NULL;
	int 		n_libs		= 0;	
	tenc_str_t	*str;
	int 		i;
	
	bc->ffi_table		= NULL;
	bc->ffi_table_length 	= 0;
	bc->dll_handles		= NULL;

	if (ffi == NULL)
		return 1;
	if (ffi->n_symbols <= 0)
		return 1;
	
	/* Load libraries */
	for (str = ffi->libraries; str != NULL; str = str->next) {
		if (str->str == NULL)
			break;
		n_libs++;
	}
	
	libs 		= (LIB_HANDLE *) malloc (sizeof (LIB_HANDLE) * (n_libs + 1));
	lib_names 	= (char **) malloc (sizeof (char *) * n_libs);

	for (i = 0, str = ffi->libraries; i < n_libs; ++i, str = str->next) {
		LIB_HANDLE *lib = load_library (str->str);
		
		if (lib != NULL) {
			libs[i] 	= lib;
			lib_names[i] 	= str->str;
		} else {
			fprintf (stderr,
				"Failed to open library %s: %s\n", 
				str->str, library_error_str()
			);
			goto errout;
		}
	}
	libs[n_libs] = NULL;

	/* Build FFI table */
	table = malloc (sizeof (FFI_TABLE_ENTRY) * ffi->n_symbols);
	for (i = 0; i < ffi->n_symbols; ++i) {
		table[i].func = NULL;
		table[i].name = NULL;
	}

	for (i = 0; i < ffi->n_symbols; ++i) {
		char 		*name	= ffi->map[i].symbol;
		void 		*sym	= NULL;
		int 		lib_n	= ffi->map[i].library;

		if ((lib_n >= 0) && (lib_n < n_libs)) {
			sym = get_symbol (libs[lib_n], name);
			if (sym == NULL) {
				fprintf (stderr,
					"Unable to find symbol '%s' in library '%s'.\n",
					name, lib_names[lib_n]
				);
			}
		} else {
			for (lib_n = 0; lib_n < n_libs; ++lib_n) {
				if ((sym = get_symbol (libs[lib_n], name)) != NULL)
					break;
			}
			if (sym == NULL) {
				fprintf (stderr,
					"Unable to find symbol '%s'.\n",
					name
				);
			}
		}

		if (sym != NULL) {
			int lib_len	= strlen (lib_names[lib_n]);
			int name_len 	= strlen (name);
			table[i].func	= (FFI_FUNCTION) sym;
			table[i].name	= malloc (lib_len + name_len + 2);
			sprintf (table[i].name, "%s:%s", lib_names[lib_n], name);
		} else {
			goto errout;
		}
	}

	bc->ffi_table		= table;
	bc->ffi_table_length 	= ffi->n_symbols;
	bc->dll_handles		= (void *) libs;

	free (lib_names);

	return ffi->n_symbols;

errout:
	if (table != NULL) {
		for (i = 0; i < ffi->n_symbols; ++i) {
			if (table[i].name != NULL)
				free (table[i].name);
		}
		free (table);
	}
	
	if (libs != NULL) {
		for (i = 0; i < n_libs; ++i) {
			if (libs[i] != NULL)
				unload_library (libs[i]);
		}
		free (libs);
		free (lib_names);
	}
	
	return 0;
}

void release_ffi_table (bytecode_t *bc)
{
	if (bc->ffi_table != NULL) {
		FFI_TABLE_ENTRY *table	= bc->ffi_table;
		LIB_HANDLE 	*libs	= (LIB_HANDLE *) bc->dll_handles;
		int n_symbols		= bc->ffi_table_length;
		int i;
	
		if (table != NULL) {
			for (i = 0; i < n_symbols; ++i) {
				free (table[i].name);
			}
			free (table);
		}
		
		if (libs != NULL) {
			for (i = 0; libs[i] != NULL; ++i) {
				unload_library (libs[i]);
			}
			free (libs);
		}
	}
}

