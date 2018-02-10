/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006      Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/io.c
 */
#define _XOPEN_SOURCE 500

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>
#include <errno.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <limits.h>

#if 0
/**
 * @fn int dup(int fd)
 * @brief duplicate file descriptor.
 * @param fd file descriptor to duplicate.
 * @retval >=0 success and new file descriptor returned.
 * @retval -1 error occured.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fi_i_dup(base_t fd_b, unsigned int fd_o)
{
  return value_of_int(dup(fd_o));
}

/**
 * @fn int dup2(int fd, int fd2)
 * @brief duplicate file descriptor.
 * @param fd file descriptor to duplicate.
 * @param fd2 file descriptor to create. closed if necessary.
 * @retval fd2 success.
 * @retval -1 error occured.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fii_i_dup2(base_t fd_b, unsigned int fd_o,
                    base_t fd2_b, unsigned int fd2_o)
{
  return value_of_int(dup2(fd_o, fd2_o));
}

/**
 * @fn int close(int fd)
 * @brief close file descriptor.
 * @param fd file descriptor to close.
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fi_i_close(base_t fd_b, unsigned int fd_o)
{
  return value_of_int(close(fd_o));
}
#endif

/**
 * @fn int read(int fd, void *buf, size_t count)
 * @brief read bytes from file.
 * @param fd file descriptor to read.
 * @param buf destination byte array 
 * @param count number of bytes.
 * @retval >=0 number of bytes read from file.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPvi_i_read(base_t fd_b, unsigned int  fd_o,
                      base_t buf_b, ofs_t buf_o,
                      base_t count_b, unsigned int count_o)
{
  void *buf, *tmp;
  int ret;

  if(count_o == 0){ return value_of_int(0); }

  buf = wrapper_get_read_buffer(buf_b, buf_o, &tmp, count_o, "read");
  ret = read(fd_o, buf, count_o);
  wrapper_writeback_release_tmpbuf(buf_b, buf_o, tmp, ret);
  return value_of_int(ret);
}

/**
 * @fn int write(int fd, const void *buf, size_t count)
 * @brief write bytes to file.
 * @param fd file descriptor to write to.
 * @param buf byte array to be written.
 * @param count number of bytes.
 * @retval >=0 number of bytes written to file.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPvi_i_write(base_t fd_b, unsigned int fd_o,
                       base_t buf_b, ofs_t buf_o,
                       base_t count_b, unsigned int count_o)
{
  void *buf, *tmp;
  int ret;

  if (count_o == 0){ return value_of_int(0); }

  buf = wrapper_get_rawimage(buf_b, buf_o, &tmp, count_o, "write");
  ret = write(fd_o, buf, count_o);
  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn int pipe(int fd[2])
 * @brief create new pipe.
 * @param fd two file descriptor is returned.
 * @retval 0 success.
 * @retval -1 fail.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype array(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPi_i_pipe(base_t fd_b, ofs_t fd_o)
{
  int ret, fd[2];

  ret = pipe(fd);
  write_word(fd_b, fd_o, value_of_int(fd[0]), NULL);
  write_word(fd_b, fd_o + sizeof(word), value_of_int(fd[1]), NULL);

  return value_of_int(ret);
}

/**
 * @fn ssize_t writev(int fd, const struct iovec *iov, int len)
 * @brief write a vector of buffer.
 * @param fd file descriptor
 * @param iov vector of buffer.
 * @param len length of vector.
 * @retval >=0 number of bytes written.
 * @retval -1 error.
 *
 * @crashcase invalid pointer, buffer overflow
 * @fsctype struct(pointer)
 *
 * @author Lepidum Co,. Ltd.
 */
value FS_FiPSn12stdlib_iovec_i_i_writev(base_t fd_b, unsigned int fd_o,
                                        base_t iov_b, ofs_t iov_o,
                                        base_t len_b, unsigned int len_o)
{
  if (len_o == 0 || len_o > IOV_MAX) {
    errno = EINVAL;
    return value_of_int(-1);
  } else {
    struct iovec iov[IOV_MAX];
    void *t[IOV_MAX];
    int i;
    ssize_t ret, total;

    total = 0;
    for (i = 0; i < len_o; i++) {
      size_t iov_len = int_of_value(read_word(iov_b, iov_o + i * 8 + 4));

      if(iov_len > SSIZE_MAX || total + (ssize_t)iov_len < total){
        errno = EINVAL;
        return value_of_int(-1);
      }

      iov[i].iov_len = iov_len;
      total += iov_len;
    }

    for (i = 0; i < len_o; i++) {
      value  iov_base_v = read_word(iov_b, iov_o + i * 8);
      iov[i].iov_base   = wrapper_get_rawimage(base_of_value(iov_base_v),
                                               ofs_of_value(iov_base_v),
                                               &t[i],
                                               iov[i].iov_len,
                                               "writev"
                                               );
    }
    ret = writev(fd_o, iov, len_o);
    for (i = 0; i < len_o; i++) {
      wrapper_release_tmpbuf(t[i]);
    }
    return value_of_int(ret);
  }
}

/**
 * @fn ssize_t readv(int fd, const struct iovec *iov, int len)
 * @brief read to a vector of buffer.
 * @param fd file descriptor
 * @param iov vector of buffer.
 * @param len length of vector.
 * @retval >=0 number of bytes read.
 * @retval -1 error.
 *
 * @crashcase invalid pointer, buffer overflow
 * @fsctype struct(pointer)
 *
 * @author Lepidum Co,. Ltd.
 */
value FS_FiPSn12stdlib_iovec_i_i_readv(base_t fd_b, unsigned int fd_o,
                                       base_t iov_b, ofs_t iov_o,
                                       base_t len_b, unsigned int len_o)
{
  if (len_o == 0 || len_o > IOV_MAX) {
    errno = EINVAL;
    return value_of_int(-1);
  } else {
    struct iovec iov[IOV_MAX];
    void *t[IOV_MAX];
    int i;
    ssize_t ret, total, writeback;

    total = 0;
    for (i = 0; i < len_o; i++) {
      size_t iov_len = int_of_value(read_word(iov_b, iov_o + i * 8 + 4));

      if(iov_len > SSIZE_MAX || total + (ssize_t)iov_len < total){
        errno = EINVAL;
        return value_of_int(-1);
      }

      iov[i].iov_len = iov_len;
      total += iov_len;
    }

    for (i = 0; i < len_o; i++) {
      value  iov_base_v = read_word(iov_b, iov_o + i * 8);
      iov[i].iov_base   = wrapper_get_read_buffer(base_of_value(iov_base_v),
                                                  ofs_of_value(iov_base_v),
                                                  &t[i],
                                                  iov[i].iov_len,
                                                  "readv"
                                                  );
    }
    ret = readv(fd_o, iov, len_o);
    writeback = ret < 0 ? 0 : ret;
    for (i = 0; i < len_o; i++) {
      value  iov_base_v = read_word(iov_b, iov_o + i * 8);
      size_t iov_len = int_of_value(read_word(iov_b, iov_o + i * 8 + 4));
      size_t len = writeback < iov_len ? writeback : iov_len;
      if (len > 0) {
        wrapper_writeback_release_tmpbuf(base_of_value(iov_base_v), ofs_of_value(iov_base_v),
                                         t[i], len);
      } else {
        wrapper_release_tmpbuf(t[i]);
      }
      writeback -= len;
    }
    return value_of_int(ret);
  }
}

/**
 * @fn int truncate(const char *path, off_t length)
 * @brief truncate specified file to specified length
 * @param path file to truncate
 * @param length file truncated to this length
 * @retval 0 successed.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, non-terminated string.
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPci_i_truncate(base_t path_b, ofs_t path_o,
                         base_t length_b, unsigned int length)
{
  void *tmp;
  char *path = wrapper_get_string_z(path_b, path_o, &tmp, "truncate");

  int ret = truncate(path, length);

  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn int mkdir(const char *path, mode_t mode)
 * @brief make a directory
 * @param path directory name to create
 * @param mode access mode
 * @retval 0 success.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, non-terminated string.
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPci_i_mkdir(base_t path_b, ofs_t path_o,
                      base_t mode_b, unsigned int mode)
{
  void *tmp;
  char *path = wrapper_get_string_z(path_b, path_o, &tmp, "mkdir");

  int ret = mkdir(path, mode);

  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

#if 0
/**
 * @fn int fsync(int fd)
 * @brief
 * @param fd file descriptor
 * @retval 0 success.
 * @retval -1 failed.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fi_i_fsync(base_t fd_b, unsigned int fd)
{
  int ret = fsync(fd);
  return value_of_int(ret);
}
#endif

/**
 * @fn int pread(int fd, void *buf, size_t count, off_t offset)
 * @brief read bytes from file without changing the file pointer.
 * @param fd file descriptor to read
 * @param buf destination byte array
 * @param count number of bytes
 * @param offset position in the file
 * 
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPvii_i_pread(base_t fd_b, int fd_o,
                        base_t buf_b, ofs_t buf_o,
                        base_t count_b, unsigned int count_o,
                        base_t offset_b, int offset_o)
{
  void *buf, *t;
  ssize_t ret;

  if (count_o == 0){ return value_of_int(0); }

  buf = wrapper_get_read_buffer(buf_b, buf_o, &t, count_o, "pread");
  ret = pread(fd_o, buf, count_o, offset_o);
  wrapper_writeback_release_tmpbuf(buf_b, buf_o, t, ret);
  return value_of_int(ret);
}

/**
 * @fn int pwrite(int fd, const void *buf, size_t count, off_t offset)
 * @brief write bytes to file without changing the file pointer.
 * @param fd file descriptor to write to.
 * @param buf byte array to be written.
 * @param count number of bytes.
 * @param offset position in the file.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPvii_i_pwrite(base_t fd_b, int fd_o,
                         base_t buf_b, ofs_t buf_o,
                         base_t count_b, unsigned int count_o,
                         base_t offset_b, int offset_o)
{
  void *buf, *t;
  int ret;

  if (count_o == 0){ return value_of_int(0); }

  buf = wrapper_get_rawimage(buf_b, buf_o, &t, count_o, "pwrite");
  ret = pwrite(fd_o, buf, count_o, offset_o);
  wrapper_release_tmpbuf(t);
  return value_of_int(ret);
}
