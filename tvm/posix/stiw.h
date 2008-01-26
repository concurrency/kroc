#ifndef STIW_H
#define STIW_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(HAVE_TVM_TVM_H)
#include <tvm/tvm.h>
#elif defined(HAVE_KROC_TVM_H)
#include <kroc/tvm.h>
#else
#include <tvm.h>
#endif

extern int memory_size;
extern int inst_size;
extern int file_type;

#ifndef WIN32
void stiw_set_alarm(WORD);
#endif
void stiw_sleep(void);

#endif
