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
 * @file stdlib/unistd/symlink.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>

/**
 * @fn int symlink(const char *path1, const char *path2)
 * @brief make symbolic link to file.
 * @param path1 symlink target name
 * @param path2 symlink name
 * @retval 0 success.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, non-terminated string.
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPc_i_symlink(base_t path1_b, ofs_t path1_o,
                         base_t path2_b, ofs_t path2_o)
{
  void *t1, *t2;
  char *path1 = wrapper_get_string_z(path1_b, path1_o, &t1, "symlink");
  char *path2 = wrapper_get_string_z(path2_b, path2_o, &t2, "symlink");
  int ret = symlink(path1, path2);
  wrapper_release_tmpbuf(t1);
  wrapper_release_tmpbuf(t2);
  return value_of_int(ret);
}

/**
 * @fn int link(const char *path1, const char *path2)
 * @brief make link to file.
 * @param path1 link target name
 * @param path2 link name
 * @retval 0 success.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, non-terminated string.
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPc_i_link(base_t path1_b, ofs_t path1_o,
                      base_t path2_b, ofs_t path2_o)
{
  void *t1, *t2;
  char *path1 = wrapper_get_string_z(path1_b, path1_o, &t1, "link");
  char *path2 = wrapper_get_string_z(path2_b, path2_o, &t2, "link");

  int ret = link(path1, path2);

  wrapper_release_tmpbuf(t1);
  wrapper_release_tmpbuf(t2);
  return value_of_int(ret);
}
