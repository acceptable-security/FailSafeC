/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef __FUNCPTR_H
#define __FUNCPTR_H

#include <fsc_runtime.h>

struct fsc_function_stub_init {
    struct fsc_header header;
    struct fsc_function_stub {
	void (*spec)();
	dvalue (*gen)(base_t, base_t);
    } val;
};

extern void *get_realoffset_PF(base_t b);
extern dvalue fsc_invoke_func_with_varargs(base_t, base_t, ofs_t, base_t, ofs_t);

#ifndef FSC_DEBUG_RUNTIME
#define INLINE extern inline
#include <funcptr_inline.c>
#undef INLINE
#endif

#endif
