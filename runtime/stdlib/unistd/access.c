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
 * @file unistd/access.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>

/**
 * @fn int access(const char *path, int mode);
 * @brief check file access permission.
 * @param path path of file to check.
 * @param mode bit or of R_OK, W_OK, X_OK and F_OK.
 * @retval 0 specified access mode permitted.
 * @retval -1 access not permitted or error occured.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPci_i_access(base_t path_b, off_t path_o,
                       base_t mode_b, off_t mode_o
                       )
{
  int ret;
  void *tmp;
  char *path = wrapper_get_string_z(path_b, path_o, &tmp, "access");

  ret = access(path, mode_o);

  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}
