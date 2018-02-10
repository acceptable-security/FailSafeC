/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/utimes.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <sys/time.h>

/**
 * @fn int utimes(const char *path, const struct timeval times[2]);
 * @brief set file time.
 * @param path file path.
 * @param times time.
 * @retval 0 success
 * @retval -1 failed
 *
 * @crashcase illegal pointer, buffer overflow, non-terminated string
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPSn21stdlib_select_timeval__i_utimes(base_t path_b, ofs_t path_o,
                                                 base_t times_b, ofs_t times_o)
{
  void *t1, *t2 = NULL;
  char *path;
  struct timeval *times;
  int ret;

  path = wrapper_get_string_z(path_b, path_o, &t1, "utimes");

  if (fsc_is_nullpointer(times_b, times_o)) {
    times = NULL;
  }else{
    times = (struct timeval*)wrapper_get_rawimage(times_b, times_o, &t2, sizeof (struct timeval[2]), "utimes");
  }
  ret = utimes(path, times);
  wrapper_release_tmpbuf(t1);
  if (base_remove_castflag(times_b) != 0) {
    wrapper_release_tmpbuf(t2);
  }
  return value_of_int(ret);
}
