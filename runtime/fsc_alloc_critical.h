/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef FSC_ALLOC_CRITICAL_H
#define FSC_ALLOC_CRITICAL_H
#include <signal.h>

extern int fsc_alloc_use_sigprocmask;
extern sigset_t fsc_alloc_blocking_sigset;

#define ENTER_CRITICAL_SECTION {			\
  sigset_t olds;					\
  if (fsc_alloc_use_sigprocmask)			\
    sigprocmask(SIG_BLOCK, &fsc_alloc_blocking_sigset, &olds);;

#define END_CRITICAL_SECTION  			\
  if (fsc_alloc_use_sigprocmask)		\
    sigprocmask(SIG_SETMASK, &olds, NULL);	\
}

#endif
