/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2003-2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/puts.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <fileptr.h>

#include <stdio.h>
#include <stdlib.h>

/**
 * @fn int puts(const char *s)
 * @author Yutaka Oiwa.
 */
value FS_FPc_i_puts(base_t base0, ofs_t ofs)
{
    void *tb0 = NULL;

    char *p0 = wrapper_get_string_z(base0, ofs, &tb0, "puts");
    int r = puts(p0);
    wrapper_release_tmpbuf(tb0);
    return value_of_base_vaddr(0, r);
}

/**
 * @fn int fputs(const char *s, FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FPcPSn10stdio_FILE__i_fputs(base_t s_base0, ofs_t s_ofs, base_t fp_base0, ofs_t fp_ofs)
{
    void *tb0 = NULL;

    FILE *fp = get_FILE_pointer(fp_base0, fp_ofs);
    char *p0 = wrapper_get_string_z(s_base0, s_ofs, &tb0, "fputs");
    int r = fputs(p0, fp);
    wrapper_release_tmpbuf(tb0);
    return value_of_int(r);
}

/**
 * @fn int putchar(int c)
 * @author Yutaka Oiwa.
 */
value FS_Fi_i_putchar(base_t cb0, ofs_t c) {
    return value_of_int(putchar(c));
}

/**
 * @fn int fputc(int c, FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FiPSn10stdio_FILE__i_fputc(base_t cb0, ofs_t c, base_t fp_base0, ofs_t fp_ofs) {
    FILE *fp = get_FILE_pointer(fp_base0, fp_ofs);
    return value_of_int(fputc(c, fp));
}
