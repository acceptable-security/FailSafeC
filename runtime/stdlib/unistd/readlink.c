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
 * @file stdlib/unistd/readlink.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>


/**
 * @fn ssize_t readlink(const char *path, char *buf, size_t buf_len)
 * @brief read symbolic link.
 * @param path path of symbolic link.
 * @param buf destination buffer.
 * @param buf_len size of buffer.
 * @retval >=0 success, number of bytes written to buffer.
 * @retval -1  error.
 *
 * @crashcase illegal pointer, buffer overflow, non-terminated string
 * @fsctype string(rw)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPci_i_readlink(base_t path_b, ofs_t path_o,
                           base_t buf_b,  ofs_t buf_o,
                           base_t buf_len_b, unsigned int buf_len_o)
{
  void *t1, *t2;
  char *path = wrapper_get_string_z(path_b, path_o, &t1, "readlink");
  char *buf  = wrapper_get_read_buffer(buf_b, buf_o, &t2, buf_len_o, "readlink");
  ssize_t sz = readlink(path, buf, buf_len_o);
  wrapper_writeback_release_tmpbuf(buf_b, buf_o, t2, sz);
  wrapper_release_tmpbuf(t1);
  return value_of_int(sz);
}
