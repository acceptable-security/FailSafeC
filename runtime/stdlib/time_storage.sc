/* -*- c -*- */
/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
#include <time.h>

static struct tm  __tm_storage;

long int timezone = 0;
int daylight = 0;
char *tzname[2] = {0};

struct tm *__stdlib_get_tm_storage(void)
{
  return &__tm_storage;
}

void __stdlib_set_timezone(long tz, int dl, char *tz1, char *tz2)
{
  timezone = tz;
  daylight = dl;
  tzname[0] = tz1;
  tzname[1] = tz2;
}
