/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2007 AIST.

   This file is written by Yutaka Oiwa in 2007.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_debug.h>
#include <fsc_stackrec.h>
#include <fsc_alloc.h>

struct fsc_stack_frame *fsc_stack_current_frame = 0; /* should be thread-local when REENTRANT */

void enter_stack_unwind_area(struct fsc_stack_frame *p,
			     const char *name,
			     volatile void **local_variables)
{
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "fsc_stackrec: entered %s (id %p)\n", name, p);
#endif
    p->name = name;
    p->parent = fsc_stack_current_frame;
    p->local_variables = local_variables;
    p->unwind_obj = 0;

    fsc_stack_current_frame = p;
}

static void unwind_stack_frame(struct fsc_stack_frame *p)
{
    struct fsc_stack_unwind_object *q = p->unwind_obj, *r;
    volatile void **lv;

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "fsc_stackrec: unwinding %s (id %p)\n", p->name, p);
#endif

    if (p->local_variables) {
	for(lv = p->local_variables; *lv; lv++) {
#ifdef FSC_DEBUG_RUNTIME
	    if (fsc_debug_flag['s'])
		fprintf (stderr, "fsc_stackrec: releasing local variable %p\n", *lv);
#endif
	    fsc_dealloc_heapvar((void *)*lv);
	}
    }

    while(q) {
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "fsc_stackrec: releasing object %p (fin=%p)\n", (void *)q->o, (void *)q->finalizer);
#endif

	if (q->finalizer)
	    (*q->finalizer)(q->o);
	fsc_dealloc_internal(q->o, "fsc_stackrec::unwind_stack_frame(obj)");
	r = q;
	q = q->next;
	GC_free(r);
    }
}

void exit_stack_unwind_area(void)
{
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "fsc_stackrec: exitting %s (id %p)\n", fsc_stack_current_frame->name, fsc_stack_current_frame);
#endif
    unwind_stack_frame(fsc_stack_current_frame);
    fsc_stack_current_frame = fsc_stack_current_frame->parent;
}

void register_stack_unwind_object(base_t o, void (*finalizer)(base_t))
{
    struct fsc_stack_unwind_object *p = GC_malloc(sizeof(struct fsc_stack_unwind_object));

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['s'])
	fprintf (stderr, "fsc_stackrec: registering %p (ff %p) on %s (id %p)\n",
		 (void *)o, (void *)finalizer,
		 fsc_stack_current_frame->name, fsc_stack_current_frame);
#endif

    p->o = o;
    p->finalizer = finalizer;
    p->next = fsc_stack_current_frame->unwind_obj;
    fsc_stack_current_frame->unwind_obj = p;
}
