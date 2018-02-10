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
 * @file stdlib/unistd/getwd.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>
#include <string.h>

/**
 * @fn char *getcwd(char *buf, size_t len)
 * @brief retrieve current working directory.
 * @param buf distination buffer.
 * @param len buffer length.
 * @return buf is returned if success. NULL on error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype string(rw)
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FS_FPci_Pc_getcwd(base_t buf_b, ofs_t buf_o, base_t len_b, unsigned int len_o)
{
  void *t1;
  char *buf = wrapper_get_read_buffer(buf_b, buf_o, &t1, len_o, "getcwd");
  char *ret = getcwd(buf, len_o);
  if (ret) {
    wrapper_writeback_release_tmpbuf(buf_b, buf_o, t1, strlen(ret) + 1);
    return ptrvalue_of_base_ofs(buf_b, buf_o);
  } else {
    wrapper_release_tmpbuf(t1);
    return ptrvalue_of_base_ofs(0, 0);
  }
}

