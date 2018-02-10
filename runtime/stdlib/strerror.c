/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <string.h>

/**
 * @fn char *strerror(int err)
 * @brief get error message.
 * @param err error number.
 * @return error message string.
 *
 * @crashcase write to returned string.
 * @fsctype return_string
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FS_Fi_Pc_strerror(base_t err_b, unsigned int err_o)
{
  char *ret = strerror(err_o);
  return wrapper_make_new_static_string(ret);
}
