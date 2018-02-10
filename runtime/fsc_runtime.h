/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef FSC_RUNTIME_H
#define FSC_RUNTIME_H
#include <fsc_debug.h>
#include <primitive_ops.h>
#include <block.h>
#include <typeinfo.h>
#include <fsc_error.h>
#include <fsc_alloc.h>
#include <fsc_vararg.h>
#include <fsc_stackrec.h>
#include <funcptr.h>
#ifndef FSC_RUNTIME_LIBRARY
#include <stdlib/fsc_stdlib.h>
#include <fsc_setjmp_handle.h>
#endif

void fsc_init_runtime(void);
#endif
