/* -*- c -*- */
/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006-2008 Lepidum Co., Ltd.

   This file is written by Lepidum Co., Ltd. in 2008.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/

/**
 * @file stdlib/unistd_2.sc
 */
#include <sys/types.h>

/**
 * @fn void swab(const void *from, void *to, ssize_t n)
 * @author Lepidum Co., Ltd.
 */
void swab(const void *from, void *to, ssize_t n)
{
  const char *f = from;
  char *t = to;
  ssize_t i;
  for (i = 0; i < n - 1; i += 2) {
    t[i] = f[i + 1];
    t[i + 1] = f[i];
  }
}
