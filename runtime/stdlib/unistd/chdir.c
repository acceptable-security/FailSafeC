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
 * @file stdlib/unistd/chdir.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>

/**
 * @fn int chdir(const char *dir)
 * @brief change working directory.
 * @param dir directory.
 * @retval 0 success.
 * @retval -1 fail.
 *
 * @crashcase invalid pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPc_i_chdir(base_t dir_b, ofs_t dir_o)
{
  void *t1;
  char *dir = wrapper_get_string_z(dir_b, dir_o, &t1, "chdir");
  int ret = chdir(dir);
  wrapper_release_tmpbuf(t1);
  return value_of_int(ret);
}
