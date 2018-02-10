/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef __FSC_VARARG_H
#define __FSC_VARARG_H

#ifdef FSC_DEBUG_RUNTIME
#include <fsc_debug.h>
#include <stdio.h>
#endif

extern void fsc_finish_varargs(base_t b0);
extern void fsc_va_end_base_ofs(base_t b, ofs_t o);

#ifndef FSC_RUNTIME_LIBRARY
#define FS_F_Pi___builtin_va_start() (ptrvalue_of_base_ofs(FAva_b, FAva_v))
#define FS_FPi_v___builtin_va_end(b,v) fsc_va_end_base_ofs(b,v)
#endif

#ifndef FSC_DEBUG_RUNTIME
#define INLINE extern inline
#include <fsc_vararg_inline.c>
#undef INLINE
#endif

#endif
