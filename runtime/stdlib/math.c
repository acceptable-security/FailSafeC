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
 * @file stdlib/math.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#if 0
#include <wrapper/fscw.h>
#include <wrapper/stdlib/math.h>
#endif

#define FORCE_COMPILE_INLINE
#include "math_inline.c"

/**
 * @fn double atof(const char *s)
 * @author Yutaka Oiwa.
 */
#if 1
double FS_FPc_d_atof(base_t base, ofs_t ofs) {
    void *rp;
    char *s = wrapper_get_string_z(base, ofs, &rp, "atof");
    double r = atof(s);
    wrapper_release_tmpbuf(rp);
    return r;
}
#else
fscw_double fscw_stdlib_atof(fscw_pointer_param(s))
{
  fscw_string_buf buf = fscw_string_get_pointer_param(s, "atof");;
  double r = atof(buf.s);
  fscw_string_release(buf);
  return fscw_double_wrap(r);
}
#endif
