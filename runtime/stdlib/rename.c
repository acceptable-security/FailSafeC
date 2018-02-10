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
 * @file stdlib/rename.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <stdio.h>

/**
 * @fn int rename(const char *old_path, const char *new_path);
 * @brief rename file.
 * @param old_path old file name.
 * @param new_path new file name.
 * @retval 0 success.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, non-terminated string.
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPc_i_rename(base_t old_path_b, ofs_t old_path_o,
                        base_t new_path_b, ofs_t new_path_o)
{
  void *t1, *t2;
  char *old_path = wrapper_get_string_z(old_path_b, old_path_o, &t1, "rename");
  char *new_path = wrapper_get_string_z(new_path_b, new_path_o, &t2, "rename");
  int ret = rename(old_path, new_path);
  wrapper_release_tmpbuf(t1);
  wrapper_release_tmpbuf(t2);
  return value_of_int(ret);
}

