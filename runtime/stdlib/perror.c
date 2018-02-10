/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/perror.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <stdio.h>
#include <fileptr.h>
#include <wrapper_helper.h>

/**
 * @fn void perror(const char *s)
 * @author Yutaka Oiwa.
 */
void FS_FPc_v_perror(base_t base, ofs_t ofs) {
    void *p0;

    char *s = wrapper_get_string_z(base, ofs, &p0, "perror");

    perror(s);

    wrapper_release_tmpbuf(p0);

    return;
}

