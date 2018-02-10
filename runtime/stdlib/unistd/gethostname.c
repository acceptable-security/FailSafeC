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
 * @file stdlib/unistd/gethostname.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>

/**
 * @fn int gethostname(char *name, size_t len)
 * @brief get host name.
 * @param name destination buffer.
 * @param buffer length.
 * @retval 0 success.
 * @retval -1 fail.
 *
 * @crashcase invalid pointer, buffer overflow
 * @fsctype string(wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPci_i_gethostname(base_t name_b, ofs_t name_o, base_t len_b, unsigned int len_o)
{
  void *t1;
  char *name = wrapper_get_read_buffer(name_b, name_o, &t1, len_o, "gethostname");
  int ret = gethostname(name, len_o);
  wrapper_writeback_release_tmpbuf(name_b, name_o, t1, ret);
  return value_of_int(ret);
}
