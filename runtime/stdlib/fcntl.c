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
 * @file stdlib/fcntl.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <copydata.h>

#include <fcntl.h>
#include <errno.h>

/**
 * @fn int fcntl(int fd, int cmd, ...)
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype va, struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiiV_i_fcntl(base_t fd_b, int fd_o,
                      base_t cmd_b, int cmd_o,
                      base_t va_b, ofs_t va_o)
{
  int ret;

  switch(cmd_o){
  case F_DUPFD:
  case F_SETFD:
  case F_SETFL:
  case F_SETOWN:
    {
      int arg = int_of_value(read_word(va_b, va_o));
      ret = fcntl(fd_o, cmd_o, arg);
    }
    break;
  case F_GETFD:
  case F_GETFL:
  case F_GETOWN:
    {
      ret = fcntl(fd_o, cmd_o);
    }
    break;

  case F_GETLK:
  case F_SETLK:
  case F_SETLKW:
    {
      value p = read_word(va_b, va_o);
      base_t sb = base_of_value(p);
      ofs_t so = ofs_of_value(p);
      struct flock buf;

      fsc_copy_to_raw(&buf, sb, so, sizeof(struct flock));
      ret = fcntl(fd_o, cmd_o, &buf);

      if(cmd_o == F_GETLK){
        fsc_copy_from_raw(sb, so, &buf, sizeof(struct flock));
      }
    }
    break;

  default:
    {
      ret = -1;
      errno = EINVAL;
    }
    break;
  }

  fsc_va_end_base_ofs(va_b, va_o);
  return value_of_int(ret);
}
