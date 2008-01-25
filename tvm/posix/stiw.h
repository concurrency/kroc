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

extern int memsize;
extern int instsize;
extern int filetype;

#endif
