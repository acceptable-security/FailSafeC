/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007. */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <stdio.h>
#include <setjmp.h>
#include <fsc_setjmp_handle.h>

struct typeinfo_init fsc_typeinfo_Xjmpbuf_ = {
    EMIT_HEADER_FOR_TYPEINFO,
    {
	"stdlib_jmpbuf",
	TI_SPECIAL,
	NULL,
	sizeof (struct fsc_jmpbuf),
	sizeof (struct fsc_jmpbuf),
	EMIT_TYPEINFO_ACCESS_METHODS_TABLE_NOACCESS
    }};

/* return uninitialized jmpbuf */
value fsc_create_jmpbuf(int jmpbuf_type) {
    base_t j = fsc_alloc_block_library(&fsc_typeinfo_Xjmpbuf_.val, 1);
    struct fsc_jmpbuf *p = (struct fsc_jmpbuf *)j;

    p->initialized = 0;
    p->is_sigsetjmp = jmpbuf_type;
    p->frame = fsc_stack_current_frame;

    return value_of_base_ofs(j, 0);
}

/* there is a small race condition between setting p->initialized and actual setjmp().
   To avoid this, we call setjmp TWICE.
 */

static inline struct fsc_jmpbuf *fsc_get_jmpbuf_direct(base_t b0, ofs_t o, int f) {
    /* f == 0: uninitialized buffer. initialized: 0 -> 1. */
    /* f == 1: partly-initialized buffer. initialized: 1 -> 2. */
    /* f == 2: fully-initialized buffer. initialized: 2 -> 2. */

    base_t b = base_remove_castflag(b0);
    fsc_header *h = 0;

    if (o != 0)
	fsc_raise_error_library(b, o, ERR_INVALIDARGS, "setjmp::fsc_get_jmpbuf_1");
    if (b == 0)
	fsc_raise_error_library(b, o, ERR_NULLPTR, "setjmp::fsc_get_jmpbuf_1");
    h = get_header_fast(b);
    if (h->tinfo != &fsc_typeinfo_Xjmpbuf_.val)
	fsc_raise_error_library(b, o, ERR_TYPEMISMATCH, "setjmp::fsc_get_jmpbuf_1");
    if (h->runtime_flags & RFLAG_DEALLOCED)
	fsc_raise_error_library(b, o, ERR_INVALIDARGS, "setjmp::fsc_get_jmpbuf_1: longjmping into dead frame");
    struct fsc_jmpbuf *p = (struct fsc_jmpbuf *)b;
    
    if (p->initialized != f)
	fsc_raise_error_library(b, o, ERR_INVALIDARGS, "setjmp::fsc_get_jmpbuf_1(2)");

    if (f == 0) {
	p->initialized = 1;
    } else if (f == 1) {
	register_stack_unwind_object(b, 0);
	p->initialized = 2;
    }

    return p;
}

struct fsc_jmpbuf *fsc_get_jmpbuf(base_t pb, ofs_t po, int i)
{
    value v = read_word(pb, po);
    return fsc_get_jmpbuf_direct(base_of_value(v), ofs_of_value(v), i);
}

