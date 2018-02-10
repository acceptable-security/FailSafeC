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
 * @file stdlib/unistd/getdtablesize.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <unistd.h>

/**
 * @fn int getdtablesize(void)
 * @brief get descpritor table size
 * @return descpritor table size
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_F_i_getdtablesize(){
  return value_of_int(getdtablesize());
}
