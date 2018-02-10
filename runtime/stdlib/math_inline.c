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
 * @file stdlib/math_inline.c
 */
#if defined(FORCE_COMPILE_INLINE) || !defined(FSC_DEBUG_RUNTIME)

#ifdef FORCE_COMPILE_INLINE
#define INLINE
#else
#define INLINE static inline
#endif

#include <math.h>
#if 0
#include <wrapper/fscw.h>
#include <wrapper/stdlib/math.h>
#endif
#if 1
#define __DEF_INLINE_FLOATFUNC(f) INLINE double FS_Fd_d_##f(double x) { return f(x); }
#else
#define __DEF_INLINE_FLOATFUNC(f) INLINE fscw_double fscw_stdlib_math(f)(fscw_double_param(x)) { return fscw_double_wrap(f(x)); }
#endif
#if 0
/**
 * @fn double fabs(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(fabs)

/**
 * @fn double sin(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(sin)

/**
 * @fn double cos(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(cos)

/**
 * @fn double tan(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(tan)

/**
 * @fn double sqrt(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(sqrt)

/**
 * @fn double exp(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(exp)

/**
 * @fn double log(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(log)

/**
 * @fn double log10(double x)
 * @author Yutaka Oiwa.
 */
__DEF_INLINE_FLOATFUNC(log10)

/**
 * @fn double pow(double x, double y)
 * @author Yutaka Oiwa.
 */
INLINE double FS_Fdd_d_pow(double x, double y){return pow(x,y);}

#endif
#undef __DEF_INLINE_FLOATFUNC
#endif
