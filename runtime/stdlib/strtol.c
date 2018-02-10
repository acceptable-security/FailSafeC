/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa and Lepidum Co. Ltd. in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/strtol.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <stdio.h>
#include <stdlib.h>

/**
 * @fn unsigned long strtoul(const char *str, char **endptr, int radix)
 * @brief convert string to unsigned long with specified radix.
 * @param str input string.
 * @param endptr conversion terminated point returned.
 * @param radix specify radix used in conversion.
 * @return converted unsigned long value returned.
 *
 * @crashcase illegal pointer, buffer overflow, non-terminated string
 * @fsctype atomic, pointer(atomic, wo), string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPPci_i_strtoul(base_t str_b, ofs_t str_o,
                           base_t end_b, ofs_t end_o,
                           base_t base_b, unsigned int base_o)
{
  void *tmp;
  char *str = wrapper_get_string_z(str_b, str_o, &tmp, "strtoul");
  char *end = NULL;

  unsigned long ret = strtoul(str, &end, base_o);

  if(end != NULL && base_remove_castflag(end_b) != 0){
    ofs_t end_ret = str_o + (end - str);
    write_word(end_b, end_o, value_of_base_ofs(str_b, end_ret), NULL);
  }
  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn long strtol(const char *str, char **endptr, int radix)
 * @brief convert string to long with specified radix.
 * @param str input string.
 * @param endptr conversion terminated point returned.
 * @param radix specify radix used in conversion.
 * @return converted long value returned.
 *
 * @crashcase illegal pointer, buffer overflow, non-terminated string
 * @fsctype atomic, pointer(atomic, wo), string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPcPPci_i_strtol(base_t str_b, ofs_t str_o,
                          base_t end_b, ofs_t end_o,
                          base_t base_b, unsigned int base_o)
{
  void *tmp;
  char *str = wrapper_get_string_z(str_b, str_o, &tmp, "strtol");
  char *end = NULL;

  long ret = strtol(str, &end, base_o);

  if(end != NULL && base_remove_castflag(end_b) != 0){
    ofs_t end_ret = str_o + (end - str);
    write_word(end_b, end_o, value_of_base_ofs(str_b, end_ret), NULL);
  }
  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn long long strtoll(const char *str, char **endptr, int radix)
 * @brief convert string to long long with specified radix.
 * @param str input string.
 * @param endptr conversion terminated point returned.
 * @param radix specify radix used in conversion.
 * @return converted long long value returned.
 *
 * @crashcase illegal pointer, buffer overflow, non-terminated string
 * @fsctype atomic, pointer(atomic, wo), string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
dvalue FS_FPcPPci_q_strtoll(base_t str_b, ofs_t str_o,
                            base_t end_b, ofs_t end_o,
                            base_t base_b, unsigned int base_o)
{
  void *tmp;
  char *str = wrapper_get_string_z(str_b, str_o, &tmp, "strtoll");
  char *end = NULL;

  long long ret = strtoll(str, &end, base_o);

  if(end != NULL && base_remove_castflag(end_b) != 0){
    ofs_t end_ret = str_o + (end - str);
    write_word(end_b, end_o, value_of_base_ofs(str_b, end_ret), NULL);
  }
  wrapper_release_tmpbuf(tmp);
  return dvalue_of_base_vaddr(0, ret);
}
