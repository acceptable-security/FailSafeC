/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <stdio.h>

dvalue fsc_invoke_func_with_varargs(base_t tb, base_t fb, ofs_t fo, base_t ab, ofs_t ao) {
    struct fsc_header *h = get_header(fb);
    if (!base_remove_castflag(fb))
	fsc_raise_error(fb, fo, ERR_NULLPTR);
    /* fprintf(stderr, "kind = %#x\n", h->tinfo->kind); */
    if ((h->tinfo->kind & TI_KIND_MASK) != (TI_FUNCTION & TI_KIND_MASK))
	fsc_raise_error(fb, fo, ERR_TYPEMISMATCH);
    if (fo != 0)
	fsc_raise_error(fb, fo, ERR_OUTOFBOUNDS);
    if (ao != 0)
	fsc_raise_error_library(ERR_INVALIDARGS, ab, ao,
				"fsc_invoke_func_with_varargs");
    {
	struct fsc_function_stub *p =
	    (struct fsc_function_stub *)base_remove_castflag(fb);
	return p->gen(tb,ab);
    }
}

#define INLINE
#include <funcptr_inline.c>
#include <fsc_vararg_inline.c>
