/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2003.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/gets.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <fileptr.h>

#if 0
#include <wrapper/fscw.h>
#define FSCW_NEED_fscw_size_t
#define FSCW_NEED_fgets
#include <wrapper/stdlib/stdio.h>
#define fscw_get_FILE_pointer(p) get_FILE_pointer(p##_base, p##_ofs)
#endif

#include <stdio.h>
#include <stdlib.h>

/**
 * @fn char *fgets(char *s, int size, FILE *fp)
 * @author Yutaka Oiwa.
 */
#if 1
value FS_FPciPSn10stdio_FILE__Pc_fgets(base_t s_base, ofs_t s_ofs, 
				       base_t size_base, int size,
				       base_t fp_base, ofs_t fp_ofs)
{
    int charcount = 0;

    FILE *fp = get_FILE_pointer(fp_base, fp_ofs);

    while(charcount < size - 1) {
	int c = getc(fp);
	if (c == EOF)
	    break;
	write_byte(s_base, s_ofs + charcount, (byte)c, NULL);
	charcount++;
	if (c == '\n') break;
    }
    if (charcount == 0)
	return ptrvalue_of_base_ofs(0, 0);
    write_byte(s_base, s_ofs + charcount, (byte)'\0', NULL);
    return ptrvalue_of_base_ofs(s_base, s_ofs);
}
#else


fscw_pointer fscw_fgets(fscw_pointer_param(dst),
                        fscw_int_param(size),
                        fscw_pointer_param(fp_p))
{
  int charcount = 0;

  FILE *fp = fscw_get_FILE_pointer(fp_p);

  while(charcount < size - 1) {
    int c = getc(fp);
    if (c == EOF)
      break;
    fscw_char_write_pointer_param(dst, charcount, (char)c);
    charcount++;
    if (c == '\n') break;
  }
  if (charcount == 0)
    return fscw_pointer_wrap(0);
  fscw_char_write_pointer_param(dst, charcount, (char)'\0');
  return fscw_pointer_wrap_param(dst);
}
#endif

/**
 * @fn char *gets(char *s)
 * @author Yutaka Oiwa.
 */
value FS_FPc_Pc_gets(base_t s_base, ofs_t s_ofs)
{
    int charcount = 0, c = 0;
    
    while(1) {
	c = getchar();
	if (c == EOF)
	    break;
	if (c == '\n')
	    break;
	write_byte(s_base, s_ofs + charcount, (byte)c, NULL);
	charcount++;
    }
    if (charcount == 0 && c == EOF)
	return ptrvalue_of_base_ofs(0, 0);
    write_byte(s_base, s_ofs + charcount, (byte)'\0', NULL);
    return ptrvalue_of_base_ofs(s_base, s_ofs);
}

/**
 * @fn int fgetc(FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE__i_fgetc(base_t fp_base, ofs_t fp_ofs) {
    FILE *fp = get_FILE_pointer(fp_base, fp_ofs);
    return value_of_int(getc(fp));
}

/**
 * @fn int getchar(void)
 * @author Yutaka Oiwa.
 */
value FS_F_i_getchar() {
    return value_of_int(getchar());
}

/**
 * @fn int ungetc(int v, FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FiPSn10stdio_FILE__i_ungetc(base_t vb, int vo, base_t fp_base, ofs_t fp_ofs) {
    FILE *fp = get_FILE_pointer(fp_base, fp_ofs);
    return value_of_int(ungetc(vo,fp));
}

