/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef __FSC_DEBUG_H
#define __FSC_DEBUG_H

#ifdef FSC_DEBUG_RUNTIME
#define CAUTIONAL_GETHEADER
#include <assert.h>
extern char fsc_debug_flag[256];
extern void initialize_fsc_debug_flag();
#define inline
#else
#ifndef assert
#define NDEBUG
#include <assert.h>
#else
#error fsc_debug.h should be included before assert.h
#endif
#endif

#endif
