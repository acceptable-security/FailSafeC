/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

INLINE base_t fsc_alloc_varargs(size_t sz) {
    return fsc_alloc_block_with_flags(&fsc_typeinfo_i.val, sz, RFLAG_TYPE_VARARGS | RFLAG_NO_USER_DEALLOC);
}

INLINE base_t fsc_alloc_block_library(typeinfo_t type, size_t len) {
    return fsc_alloc_block_with_flags(type, len, RFLAG_NO_USER_DEALLOC);
}

INLINE void *fsc_alloc_heapvar(typeinfo_t type, size_t len) {
  /* address for heap var is block top, not data top, being consistent with other variables */
  return get_header(fsc_alloc_block_with_flags(type, len, RFLAG_NO_USER_DEALLOC));
}

INLINE void fsc_dealloc_heapvar(void *p) {
  /* address for heap var is block top, not data top, being consistent with other variables */
  fsc_dealloc_internal((base_t)(1 + (fsc_header *)p), "fsc_dealloc_heapvar");
}

INLINE void *fsc_alloc_valtrampoline(typeinfo_t type) {
  return (void *)fsc_alloc_block_with_flags(type, 1, RFLAG_NO_USER_DEALLOC);
}

INLINE void fsc_dealloc_valtrampoline(void *p) {
  fsc_dealloc_internal((base_t)p, "fsc_dealloc_valtrampoline");
}

INLINE base_t fsc_alloc_static_block(typeinfo_t type, size_t len) {
    return fsc_alloc_block_with_flags(type, len, RFLAG_NO_USER_DEALLOC);
}

INLINE void fsc_dealloc_checktype(base_t base, word type) {
    if ((get_header(base)->runtime_flags & RFLAG_TYPE_MASK) == type)
	fsc_dealloc_internal(base, "fsc_dealloc_checktype");
    else
	fsc_raise_error_library(base, 0, ERR_INVALIDARGS, "fsc_dealloc: type check failed: ");
}

INLINE void fsc_dealloc_varargs_finished(base_t base) {
#ifdef FSC_OLD_VARARG_CHECK
    fsc_dealloc_checktype(base, RFLAG_TYPE_VARARGS_FINISHED_BY_USER);
#else
    fsc_dealloc_checktype(base, RFLAG_TYPE_VARARGS);
#endif
}

#if defined(FSC_RUNTIME_LIBRARY) || defined(FSC_ALLOC_FORCE_COMPILE_ALL)
# include <fsc_autoconf.h>
#ifdef HAVE_GC_GC_H
# include <gc/gc.h>
#else
# ifdef HAVE_GC_H
#  include <gc.h>
# else
#  error location of gc.h not defined in fsc_autoconf.h
# endif
#endif

#include <fsc_alloc_critical.h>

INLINE void *fsc_alloc_raw(size_t sz) {
  void *p;
  if (sz == 0) sz = 1;
  ENTER_CRITICAL_SECTION
    p = malloc(sz);
  END_CRITICAL_SECTION
    if (!p)
	fsc_raise_error_library(0, sz, ERR_OUTOFMEMORY, "fsc_alloc_raw");
    return p;
}

INLINE void *fsc_alloc_gc(size_t sz) {
  void *p;
  if (sz == 0) sz = 1;
  ENTER_CRITICAL_SECTION
    p = GC_malloc(sz);
  END_CRITICAL_SECTION
    if (!p)
	fsc_raise_error_library(0, sz, ERR_OUTOFMEMORY, "fsc_alloc_gc");
    return p;
}

INLINE void *fsc_realloc_raw(void *pp, size_t sz) {
  void *p;
  ENTER_CRITICAL_SECTION
    p = realloc(pp, sz);
  END_CRITICAL_SECTION
    if (!p && sz)
	fsc_raise_error_library(0, sz, ERR_OUTOFMEMORY, "fsc_realloc_raw");
    return p;
}

INLINE void fsc_release_raw(void *p) {
    free(p);
}

/**************** expansible string buffer ****************/

INLINE void put_string_buffer(string_buffer *b, char c) {
    if (b->len >= b->size - 1) {
	int new_size = b->size * 2 + 1;
	assert (new_size > b->len);
	b->p = fsc_realloc_raw(b->p, new_size);
	b->size = new_size;
    }
    b->p[b->len++] = c;
}

INLINE char *get_sz_string_buffer(string_buffer *b) {
    put_string_buffer(b, '\0');
    return b->p;
}

INLINE size_t get_len_string_buffer(string_buffer *b) {
    return b->len;
}

INLINE void release_string_buffer(string_buffer *b) {
    fsc_release_raw(b->p);
}

INLINE void clear_string_buffer(string_buffer *b) {
    b->len = 0;
}

#endif
