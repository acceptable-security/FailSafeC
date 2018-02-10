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
 * @file stdlib/unistd/groups.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <sys/param.h>
#include <sys/types.h>
#include <unistd.h>
#include <grp.h>

/**
 * @fn int getgroups(int len, gid_t* list)
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype pointer(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPi_i_getgroups(base_t len_b, int len,
                          base_t buf_b, ofs_t buf_o)
{
  int ret;

  if(len < 0){
    fsc_raise_error_library(0, len, ERR_OUTOFBOUNDS, "getgroups: negative buffer length");
  }

  if(len == 0){
    ret = getgroups(0, NULL);
  }else{
    void *tmp, *buf;
    int size;

    size = len * 4;
    if (size / 4 != len){
      fsc_raise_error_library(0, len, ERR_OUTOFBOUNDS, "getgroups: buffer length exceeds integer");
    }
    buf = wrapper_get_read_buffer(buf_b, buf_o, &tmp, size, "getgroups");

    ret = getgroups(len, buf);

    wrapper_writeback_release_tmpbuf(buf_b, buf_o, tmp, size);
  }

  return value_of_int(ret);
}

/**
 * @fn int setgroups(int len, const gid_t* buf)
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype pointer(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPi_i_setgroups(base_t len_b, int len,
                          base_t buf_b, ofs_t buf_o)
{
  int ret;

  if(len < 0){
    fsc_raise_error_library(0, len, ERR_OUTOFBOUNDS, "setgroups: negative buffer length");
  }

  if(len == 0){
    ret = setgroups(0, NULL);
  }else{
    void *tmp, *buf;
    int size;

    size = len * 4;
    if (size / 4 != len){
      fsc_raise_error_library(0, len, ERR_OUTOFBOUNDS, "setgroups: buffer length exceeds integer");
    }
    buf = wrapper_get_rawimage(buf_b, buf_o, &tmp, size, "setgroups");

    ret = setgroups(len, buf);

    wrapper_release_tmpbuf(tmp);
  }

  return value_of_int(ret);
}

/**
 * @fn initgroups(const char *user, gid_t group)
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPci_i_initgroups(base_t user_b, ofs_t user_o,
                           base_t group_b, int group_o)
{
  void *tmp;
  char *user;
  int ret;

  user = wrapper_get_string_z(user_b, user_o, &tmp, "initgroups");

  ret = initgroups(user, group_o);

  wrapper_release_tmpbuf(tmp);
  return ret;
}
