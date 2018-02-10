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
 * @file stdlib/ioctl.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <copydata.h>

#include <errno.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <stropts.h>
#include <stdio.h>

#define IOC_MAX_LEN (256-8)
#define IOC_MAGIC 0xabad10c0
struct ioc_buf {
  char buf[IOC_MAX_LEN];
  unsigned int magic_1;
  unsigned int magic_2;
};

static int ioctl_ptr(int fd, int req, base_t buf_b, ofs_t buf_o, size_t len)
{
  if (fsc_is_nullpointer(buf_b, buf_o)) {
    return ioctl(fd, req, (void *)0);
  } else {
    struct ioc_buf b;
    int ret;
    
    if (len > IOC_MAX_LEN) {
      errno = EINVAL;
      return -1;
    }
    fsc_copy_to_raw(&b.buf[IOC_MAX_LEN-len], buf_b, buf_o, len);
    b.magic_1 = IOC_MAGIC;
    b.magic_2 = IOC_MAGIC;
    ret = ioctl(fd, req, &b.buf[IOC_MAX_LEN-len]);
    if (ret < 0 && errno == EFAULT) {
      fsc_raise_error_library(0, req, ERR_SYSERROR, "ioctl returns EFAULT");
    }
    if (b.magic_1 != IOC_MAGIC || b.magic_2 != IOC_MAGIC) {
      fsc_raise_error_library(0, req, ERR_SYSERROR, "ioctl corrupts memory");
    }
    fsc_copy_from_raw(buf_b, buf_o, &b.buf[IOC_MAX_LEN-len], len);
    return value_of_int(ret);
  }
}

static int ioctl_value(int fd, int req, unsigned int value)
{
  int ret = ioctl(fd, req, value);
  if (ret < 0 && errno == EFAULT) {
    fsc_raise_error_library(0, req, ERR_SYSERROR, "ioctl returns EFAULT");
  }
  return ret;
}

/**
 * @fn int ioctl(int fd, int req, ...)
 * @param fd file descriptor.
 * @param req function specifier.
 * @param ... request specific arguments.
 *
 * @crashcase unknown
 * @fsctype special
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiiV_i_ioctl(base_t fd_b, unsigned int fd_o,
                      base_t req_b, unsigned int req_o,
                      base_t va_b, unsigned int va_o)
{
#define IOCTL_ENTRY(req, name, is_ptr, tp, condition) \
  IOCTL_ENTRY_##is_ptr(req, name, tp, condition)
#define IOCTL_ENTRY_PTR(req, name, tp, condition) \
  case req: \
    if (condition) { \
      return ioctl_ptr(fd_o, name, value_b, value_o, sizeof (tp)); \
    } \
    break;
#define IOCTL_ENTRY_VALUE(req, name, tp, condition) \
  case req: \
    if (condition) { \
      return ioctl_value(fd_o, name, value_o); \
    } \
    break;
#define IOCTL_ENTRY_NONE(req, name, tp, condition) \
  case req: \
    if (condition) { \
      return ioctl_value(fd_o, name, 0); \
    } \
    break;
  
  base_t value_b = 0;
  ofs_t  value_o = 0;
  int FIFO = 0;
  int SOCK = 0;
  int TTY  = 0;
  struct stat st;

  if (va_b != 0 && is_offset_ok(va_b, va_o)) {
    /* ioctl(fd, req, value) */
    value v = read_word(va_b, va_o);
    value_b = base_remove_castflag(base_of_value(v));
    value_o = ofs_of_value(v);
  }

  if (fstat(fd_o, &st) < 0) {
    return value_of_int(-1);
  }
  FIFO = S_ISFIFO(st.st_mode);
  SOCK = S_ISSOCK(st.st_mode);
  if (S_ISCHR(st.st_mode)) {
    TTY = isatty(fd_o);
  }
  
  switch (req_o) {
#include "ioctl_list.h"
  }
  errno = ENOTTY;
  return value_of_int(-1);
}
