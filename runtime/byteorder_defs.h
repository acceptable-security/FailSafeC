/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef BYTEORDER_DEFS_H
#define BYTEORDER_DEFS_H

#if defined __sparc
#define FSC_IS_BIG_ENDIAN
#elif defined __i386
#define FSC_IS_LITTLE_ENDIAN
#elif defined __MIPSEL
#define FSC_IS_LITTLE_ENDIAN
#elif defined __ARMEL__
#define FSC_IS_LITTLE_ENDIAN
#elif defined __powerpc__
#define FSC_IS_BIG_ENDIAN
#else
/* put definition here */
#error unknown byte order
#endif


#ifdef FSC_IS_BIG_ENDIAN
#define OFS_WORD_HIGH 0
#define OFS_WORD_HIGH2 1
#define OFS_WORD_LOW2 2
#define OFS_WORD_LOW 3
#define OFS_SHORT_HIGH 0
#define OFS_SHORT_LOW 1
#define EMIT_INIT_TWO_WORDS(h,l) { (h), (l) }
#define EMIT_DECL_TWO_WORDS(h,l) h; l
#endif

#ifdef FSC_IS_LITTLE_ENDIAN
#ifdef FSC_IS_BIG_ENDIAN
#error byteorder redefined
#endif
#define OFS_WORD_HIGH 3
#define OFS_WORD_HIGH2 2
#define OFS_WORD_LOW2 1
#define OFS_WORD_LOW 0
#define OFS_SHORT_HIGH 1
#define OFS_SHORT_LOW 0
#define EMIT_INIT_TWO_WORDS(h,l) { (l), (h) }
#define EMIT_DECL_TWO_WORDS(h,l) l; h
#endif

#ifndef OFS_SHORT_LOW
#error byteorder unknown
#endif

/* white list of misaligned read/write capability */
#if defined __i386
#undef FSC_REQUIRE_ALIGNED_ACCESS_TO_RAW_WORD
#else
#define FSC_REQUIRE_ALIGNED_ACCESS_TO_RAW_WORD
#endif

#endif
