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
 * @file stdlib/locale.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <locale.h>

#include "env_helper.h"

/**
 * @fn char *setlocale(int category, const char *locale)
 * @brief set locale
 * @param category one of LC_* symbols.
 * @param locale locale name or one of "POSIX", "C", "" and NULL
 * @retval !=NULL locale specifier string for new locale
 * @retval NULL failed to set locale.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro), return_string
 *
 * @author Lepidum Co., Ltd.
 */

ptrvalue FS_FiPc_Pc_setlocale(base_t category_b, unsigned int category_o,
                              base_t locale_b, ofs_t locale_o
                              )
{
  char *locale, *ret;
  void *tmp = NULL;

  if(base_remove_castflag(locale_b) || locale_o != 0){
    locale = wrapper_get_string_z(locale_b, locale_o, &tmp, "setlocale");
  }else{
    locale = NULL;
  }

  if (locale && *locale == 0) {
      /* refer environments */
      fsc_set_system_env("LANG");
      fsc_set_system_env("LC_ALL");
      fsc_set_system_env("LC_COLLATE");
      fsc_set_system_env("LC_CTYPE");
      fsc_set_system_env("LC_MESSAGES");
      fsc_set_system_env("LC_MONETARY");
      fsc_set_system_env("LC_NUMERIC");
      fsc_set_system_env("LC_TIME");
  }

  ret = setlocale(category_o, locale);

  if (base_remove_castflag(locale_b) || locale_o != 0) {
    wrapper_release_tmpbuf(tmp);
  }

  if(ret){
    return wrapper_make_new_static_string(ret);
  }else{
    return ptrvalue_of_base_ofs(0, 0);
  }
}
