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
 * @file stdlib/if.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <net/if.h>

/**
 * @fn unsigned int if_nametoindex(const char *name)
 * @brief get network device index from name
 * @param name network device name
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPc_i_if_nametoindex(base_t name_b, ofs_t name_o)
{
  void *tmp;
  char *name = wrapper_get_string_z(name_b, name_o, &tmp, "if_nametoindex");

  int ret = if_nametoindex(name);

  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}
