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
 * @file stdlib/stdlib_int_inline.c
 */
#if defined(FORCE_COMPILE_INLINE) || !defined(FSC_DEBUG_RUNTIME)

#ifdef FORCE_COMPILE_INLINE
#define INLINE
#else
#define INLINE static inline
#endif

/**
 * @fn int abs(int i)
 * @author Yutaka Oiwa.
 */
INLINE value FS_Fi_i_abs(base_t b, unsigned int o) {
    return value_of_int(abs((int)o));
}
#endif
