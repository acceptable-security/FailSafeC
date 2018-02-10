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
 * @file stdlib/unistd/chroot.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>

/**
 * @fn int chroot(const char *path)
 * @brief change root directory
 * @param path new root directory path
 * @retval 0 success.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPc_i_chroot(base_t path_b, ofs_t path_o)
{
  void *tmp;
  char *path;
  int ret;

  path = wrapper_get_string_z(path_b, path_o, &tmp, "chroot");
  ret = chroot(path);
  wrapper_release_tmpbuf(tmp);

  return value_of_int(ret);
}
