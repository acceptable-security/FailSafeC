/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef WRAPPER_HELPER_H
#define WRAPPER_HELPER_H
#include <fsc_runtime.h>
#include <stddef.h>

/*** boundary check helpers ***/

#define FSC_FIELD_NAME(m) fld_##m

extern int fsc_is_nullpointer(base_t base, ofs_t ofs);
 /* strictly equivalent to p == 0 in FSC */

extern void fsc_ensure_nonnull(base_t base, ofs_t ofs, const char *libloc);
 /* rejects all invalid (integer) pointers, even if it is not 0 */

extern void fsc_assert_accessible(base_t base0, register ofs_t ofs, register ofs_t size, const char *libloc);

char *wrapper_get_string_z(base_t, ofs_t, void **_to_discard, const char *);
char *wrapper_get_string_zn(base_t, ofs_t, void **_to_discard, size_t, const char *);
char *wrapper_get_rawimage(base_t, ofs_t, void **_to_discard, size_t, const char *);
void *wrapper_get_read_buffer(base_t, ofs_t, void **_to_discard, size_t, const char *);
void wrapper_writeback_release_tmpbuf(base_t, ofs_t, void *, word);
void wrapper_release_tmpbuf(void *);
ptrvalue wrapper_make_new_static_string(const char *);

#ifdef FSC_DEBUG_RUNTIME
extern void assert_all_buffers_are_released();
#endif

#ifdef FSC_RUNTIME_LIBRARY
#ifndef FSC_DEBUG_RUNTIME
#undef INLINE
#define INLINE extern inline
#include <wrapper_helper_inline.c>
#undef INLINE
#endif
#endif

#endif
