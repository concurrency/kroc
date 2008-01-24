/* External references for some of the tvm_hooks (satisfied from stiwmain.c) */
extern char **tvm_argv;
extern int tvm_argc;

/* Needs to be called from stiwmain to get the special hooks table. */
FFI_FUNCTION *get_special_hooks();

/* Hook function need to be of the same format as any occam FFI call
 * see either the swig-occam modules documentation or another resource
 * on the occam ffi mechanism. */

/* Callable from occam programs run on the tvm (currently in occama)
 * Allows the programmer to get a pointer to the cmdline arguments */
void HOOK_PTR(int w[]);
void HOOK_VER(int w[]);
void HOOK_EXIT(int w[]);

/* Shouldn't really be called from occam programs as they are used 
 * in the tvm magical procs that manage the kyb scr and err.*/
void HOOK_READCHAR(int w[]);
void HOOK_PRINTSCREEN(int w[]);
void HOOK_PRINTSCREEN(int w[]);
void HOOK_FLUSHSCREEN(int w[]);
void HOOK_PRINTERROR(int w[]);
void HOOK_FLUSHERROR(int w[]);

/* Some potentially useful debug hooks... */
void HOOK_DEBUG_STR(int w[]);
void HOOK_DEBUG_INT(int w[]);
