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
 * @file stdlib/open.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <fcntl.h>

/**
 * @fn int open(const char *path, int flag, ...)
 * @brief open a file.
 * @param path filename to open
 * @param flag optional flag and one of O_RDONLY, O_WRONLY or O_RDWR.
 * @param ... mode_t mode. newly created file mode if O_CREAT is specified (optional).
 * @retval >=0 success, file descriptor is returned.
 * @retval -1  fail.
 *
 * @crashcase illegal pointer, non-terminated string, too few arguments.
 * @fsctype va, string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPciV_i_open(base_t path_b, ofs_t path_o,
                      base_t flag_b, unsigned int flag_o,
                      base_t va_b, ofs_t va_o)
{
  void *tmp;
  const char *path;
  int ret;

  path = wrapper_get_string_z(path_b, path_o, &tmp, "open");

  if(flag_o & O_CREAT){
    mode_t mode = read_word(va_b, va_o);
    ret = open(path, flag_o, mode);
  }else{
    ret = open(path, flag_o);
  }

  wrapper_release_tmpbuf(tmp);
  fsc_va_end_base_ofs(va_b, va_o);
  return value_of_int(ret);
}
