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
 * @file stdlib/ftime.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <sys/timeb.h>

/**
 * @fn int ftime(struct timeb *t);
 * @brief get system clock in millisecond granularity.
 * @param t pointer to timeb structure.
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype atomic, struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn12stdlib_timeb__i_ftime(base_t timeb_b, ofs_t timeb_o)
{
  void *buf, *tmp;
  int ret;

  buf = wrapper_get_read_buffer(timeb_b, timeb_o, &tmp, sizeof(struct timeb), "ftime");
  ret = ftime(buf);

  wrapper_writeback_release_tmpbuf(timeb_b, timeb_o, tmp, sizeof(struct timeb));
  return value_of_int(ret);
}
