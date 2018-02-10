/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef _FSC_STACKREC_H
#define _FSC_STACKREC_H
struct fsc_stack_unwind_object {
    void (*finalizer)(base_t);
    base_t o;
    struct fsc_stack_unwind_object *next;
};

struct fsc_stack_frame {
    const char *name;
    struct fsc_stack_frame *parent;
    volatile void **local_variables;
    struct fsc_stack_unwind_object *unwind_obj;
};

extern struct fsc_stack_frame *fsc_stack_current_frame; /* should be thread-local when REENTRANT */

extern void enter_stack_unwind_area(struct fsc_stack_frame *, const char *, volatile void **);
extern void register_stack_unwind_object(base_t, void (*)(base_t));
extern void exit_stack_unwind_area(void);
#endif
