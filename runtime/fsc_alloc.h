/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef FSC_ALLOC_H
#define FSC_ALLOC_H
#include <typeinfo.h>
#include <type_repr.h>

base_t fsc_alloc_block_with_flags(typeinfo_t, size_t, word);
base_t fsc_alloc_block(typeinfo_t, size_t);
base_t fsc_alloc_block_full(typeinfo_t type, size_t nelems, size_t remainder, word flags);
void fsc_dealloc_internal(base_t, const char *);

extern base_t fsc_alloc_varargs(size_t sz);
extern base_t fsc_alloc_block_library(typeinfo_t type, size_t len);
extern base_t fsc_alloc_static_block(typeinfo_t type, size_t len);
extern void fsc_dealloc_checktype(base_t base, word type);
extern void fsc_dealloc_varargs_finished(base_t base);

extern void *fsc_alloc_heapvar(typeinfo_t type, size_t len);
extern void *fsc_alloc_valtrampoline(typeinfo_t type);
extern void fsc_dealloc_heapvar(void *);
extern void fsc_dealloc_valtrampoline(void *);

#ifdef FSC_RUNTIME_LIBRARY
#include <fsc_autoconf.h>
#ifdef HAVE_GC_GC_H
# include <gc/gc.h>
#else
# ifdef HAVE_GC_H
#  include <gc.h>
# else
#  error gc.h location not defined in fsc_autoconf.h
# endif
#endif

extern void *fsc_alloc_raw(size_t sz);
extern void *fsc_alloc_gc(size_t sz);
extern void *fsc_realloc_raw(void *pp, size_t sz);
extern void fsc_release_raw(void *p);

void fsc_enable_signal_mutex(void);

/**************** expansible string buffer ****************/

typedef struct {
    int size;
    int len;
    char *p;
} string_buffer;

#define INIT_string_buffer { 0, 0, 0 }

extern void put_string_buffer(string_buffer *b, char c);
extern char *get_sz_string_buffer(string_buffer *b);
extern size_t get_len_string_buffer(string_buffer *b);
extern void release_string_buffer(string_buffer *b);
extern void clear_string_buffer(string_buffer *b);

#endif

#ifndef FSC_DEBUG_RUNTIME
#define INLINE extern inline
#undef FSC_ALLOC_FORCE_COMPILE_ALL
#include <fsc_alloc_inline.c>
#undef INLINE
#endif

#endif
