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

void FS_FPPvi_v_longjmp(base_t pb, ofs_t po, base_t ib, int i) {
    struct fsc_jmpbuf *jmpbuf = fsc_get_jmpbuf(pb, po, 2);
    struct fsc_stack_record *s;

    if (jmpbuf->is_sigsetjmp)
	fsc_raise_error_library(pb, po, ERR_INVALIDARGS, "longjmp: jmpbuf created by sigsetjmp");

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr,
		 "longjmp: invoked: arg %10d, current frame %s (id %p),\n"
		 "                                  target frame %s (id %p)\n", 
		 i, fsc_stack_current_frame->name, fsc_stack_current_frame,
		 jmpbuf->frame->name, jmpbuf->frame);
#endif

    /* unwind stacks */
    while(jmpbuf->frame != fsc_stack_current_frame)
	exit_stack_unwind_area();

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "longjmp: unwinding finished. jumping.\n");
#endif

    longjmp(jmpbuf->j.jmpbuf, i);
}

void FS_FPPvi_v_siglongjmp(base_t pb, ofs_t po, base_t ib, int i) {
    struct fsc_jmpbuf *jmpbuf = fsc_get_jmpbuf(pb, po, 2);
    struct fsc_stack_record *s;

    if (!jmpbuf->is_sigsetjmp)
	fsc_raise_error_library(pb, po, ERR_INVALIDARGS, "siglongjmp: jmpbuf created by setjmp");

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr,
		 "siglongjmp: invoked: arg %10d, current frame %s (id %p),\n"
		 "                                     target frame %s (id %p)\n", 
		 i, fsc_stack_current_frame->name, fsc_stack_current_frame,
		 jmpbuf->frame->name, jmpbuf->frame);
#endif

    /* unwind stacks */
    while(jmpbuf->frame != fsc_stack_current_frame)
	exit_stack_unwind_area();

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "siglongjmp: unwinding finished. jumping.\n");
#endif

    siglongjmp(jmpbuf->j.sigjmpbuf, i);
}
