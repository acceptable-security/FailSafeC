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
 * @file stdlib/resource.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <copydata.h>

#include <sys/resource.h>
/**
 * @fn int getrlimit(int res, struct rlimit *lim)
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn13stdlib_rlimit__i_getrlimit(base_t res_b, int res_o,
                                           base_t lim_b, ofs_t lim_o)
{
  struct rlimit buf;
  int ret;

  ret = getrlimit(res_o, &buf);
  if(ret == 0){
    fsc_copy_from_raw(lim_b, lim_o, &buf, sizeof(struct rlimit));
  }
  return value_of_int(ret);
}

/**
 * @fn int setrlimit(int res, const struct rlimit *lim)
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn13stdlib_rlimit__i_setrlimit(base_t res_b, int res_o,
                                           base_t lim_b, ofs_t lim_o)
{
  struct rlimit buf;
  int ret;

  fsc_copy_to_raw(&buf, lim_b, lim_o, sizeof(struct rlimit));

  ret = setrlimit(res_o, &buf);
  return value_of_int(ret);
}
