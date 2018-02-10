/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef _FSC_NULLPTRHANDLER_H
#define _FSC_NULLPTRHANDLER_H
void fsc_nullptr_handler(int sig, siginfo_t *sinfo, void *p);
int install_nullhandler(void);
#endif
