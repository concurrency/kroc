/** \file memcache.h
 *
 * Memory access caching for speedups of USB or alike transfers
 * (c) 07/2005 Martin Strubel <hackfin@section5.ch>
 *
 * $Id: memcache.h 423 2006-11-09 14:58:08Z strubi $
 *
 *
 */

#define L1PROG_CACHESIZE       0x14000
#define L1PROG_CACHE_NPAGES   (0x14000 / 8)

#define L1_VALID   0x01
#define L1_DIRTY   0x02

// Properly handle DLL import/export under windows platforms
#ifndef DLL_API
#if defined (WIN32)
#	if defined (DLL_EXPORTS)
#		define DLL_API __declspec(dllexport)
#	else
#		define DLL_API __declspec(dllimport)
#	endif
#else
#	define DLL_API
#endif
#endif

typedef struct {
	// L1 prog cache
	unsigned char  *l1prog;         /// L1 program memory
	unsigned char  *l1flags;        /// L1 cache flags

	unsigned char  *l1aux;          /// L1 auxiliary buffer
	unsigned short  l1aux_size;     /// current aux buffer size
	// generic cheap read cache
	unsigned char  *rbuf;           /// Read cache buffer
	unsigned short  rsize;          /// Buffer size
	unsigned short  rcount;         /// Number of bytes cached
	ADDR            rbase;          /// Cached base address
} MemCache;


DLL_API
int      memcache_init(MemCache *cache);

DLL_API
int      memcache_free(MemCache *cache);

DLL_API
int      set_memory_cached(CPU cpu, ADDR addr, unsigned long count,
                           const unsigned char *buf);

DLL_API
int      set_memory_queued(CPU cpu, ADDR addr, unsigned long count,
                           const unsigned char *buf);

DLL_API
int      get_memory_cached(CPU cpu, ADDR addr, unsigned long count,
                           unsigned char *buf);

/** Flush memory cache to target. This is necessary before running 'resume'.
 *
 * \return          negative error code or 0 if ok.
 */

DLL_API
int      memcache_flush(CPU cpu);

DLL_API
void     memcache_invalidate(MemCache *cache);

DLL_API
int      set_memory_cached(CPU cpu, ADDR addr, unsigned long count,
                           const unsigned char *buf);

DLL_API
int      set_memory_queued(CPU cpu, ADDR addr, unsigned long count,
                           const unsigned char *buf);

DLL_API
int      get_memory_cached(CPU cpu, ADDR addr, unsigned long count,
                           unsigned char *buf);

/** Flush memory cache to target. This is necessary before running 'resume'.
 *
 * \return          negative error code or 0 if ok.
 */

DLL_API
int      memcache_flush(CPU cpu);


/** Returns 1 if memory base is cacheable
 * 
 */

DLL_API
int      memory_is_cacheable(CPU cpu, ADDR addr);


// Non API calls
void     l1cache_invalidate(CPU cpu);


