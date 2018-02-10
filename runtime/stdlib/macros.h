/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
#ifndef __MACROS_H
#define __MACROS_H

/*
 * below experimental
 */

#include <fsc_runtime.h>
#include <wrapper_helper.h>

/**
 * return value type.
 */
#define RET_CHAR      unsigned char
#define RET_SHORT     unsigned short
#define RET_INT       value
#define RET_LONG      value
#define RET_LONGLONG  dvalue

#define RET_FLOAT     float
#define RET_DOUBLE    double

#define RET_POINTER   ptrvalue

/**
 * parameters.
 */
#define PARAM_CHAR(x)     unsigned char x##_v
#define PARAM_SHORT(x)    unsigned short x##_v
#define PARAM_INT(x)      base_t x##_b, unsigned int x##_v
#define PARAM_LONG(x)     base_t x##_b, unsigned int x##_v
#define PARAM_LONGLONG(x) base_t x##_b, unsigned long long x##_v

#define PARAM_FLOAT(x)    float x##_v
#define PARAM_DOUBLE(x)   double x##_v

#define PARAM_POINTER(x)  base_t x##_b, ofs_t x##_o

/**
 * function declaration
 */
#define FSC_FUNCTION(f, t, ...)  FS_##t##_##f(__VA_ARGS__)

/**
 * unwrap parameters.
 */
#define GET_CHAR(x)      char x  = x##_v;
#define GET_SHORT(x)     short x = x##_v;
#define GET_INT(x)       int x  = x##_v;
#define GET_LONG(x)      long x = x##_v;
#define GET_LONGLONG(x)  long long x = x##_v;
#define GET_FLOAT(x)     float x = x##_v;
#define GET_DOUBLE(x)    double x = x##_v;

/**
 * wrap return value.
 */
#define WRAP_CHAR(x)       ((unsigned char)x)
#define WRAP_SHORT(x)      ((unsigned short)x)
#define WRAP_INT(x)        value_of_base_vaddr(0, (unsigned int)x)
#define WRAP_LONG(x)       value_of_base_vaddr(0, (unsigned int)x)
#define WRAP_LONGLONG(x)   dvalue_of_base_vaddr(0, (unsigned long long)x)

#define WRAP_FLOAT(x)      ((float)x)
#define WRAP_DOUBLE(x)     ((double)x)

#define WRAP_POINTER(x)    ptrvalue_of_base_ofs(x, 0)

/**
 * architecture independent section.
 */

#define IS_NULL(x) \
  (base_remove_castflag(x##_b) == 0)

#define GET_POINTER_STRING(x) \
  if(0){ goto must_release_##x##_string; } \
  void *x##_stmp; \
  char *x = wrapper_get_string_z(x##_b, x##_o, &x##_stmp, __func__)
#define RELEASE_STRING(x) \
must_release_##x##_string: \
  wrapper_release_tmpbuf(x##_stmp)

#define GET_POINTER_BUFFER(x, s) \
  if(0){ goto must_release_##x##_buffer; } \
  void *x##_btmp; \
  char *x = wrapper_get_rawimage(x##_b, x##_o, &x##_btmp, s, __func__)
#define RELEASE_BUFFER(x) \
must_release_##x##_buffer: \
  wrapper_release_tmpbuf(x##_btmp)

#define GET_POINTER_READ_BUFFER(x, s) \
  if(0){ goto must_release_##x##_read_buffer; } \
  void *x##_rtmp; \
  char *x = wrapper_get_read_buffer(x##_b, x##_o, &x##_rtmp, s, __func__)
#define WRITEBACK_RELEASE_READ_BUFFER(x, s) \
must_release_##x##_read_buffer: \
  wrapper_writeback_release_tmpbuf(x##_b, x##_o, x##_rtmp, s)


#define GET_POINTER_FILE(x) \
  FILE *x = get_FILE_pointer(x##_b, x##_o)
#define GET_POINTER_DIR(x) \
  DIR *x = (DIR*)get_FILE_pointer(x##_b, x##_o)

#endif

/**
 * EXAMPLE
 */
#if 0

/**
 * TODO: getpid's return value is pid_t, not int.
 */
RET_INT FSC_FUNCTION(getpid, F_i)
{
  return WRAP_INT(getpid());
}

RET_INT FSC_FUNCTION(rename, FPcPc_i
                     PARAM_POINTER(old_path),
                     PARAM_POINTER(new_path)
                     )
{
  GET_POINTER_STRING(old_path);
  GET_POINTER_STRING(new_path);

  int ret = rename(old_path, new_path);

  RELEASE_STRING(old_path);
  RELEASE_STRING(new_path);

  return WRAP_INT(ret);
}

#endif
