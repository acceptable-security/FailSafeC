/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef __COPYDATA_H__
#define __COPYDATA_H__

extern void fsc_copy_to_raw(void *buf0, base_t base, ofs_t ofs, size_t count);
extern void fsc_copy_from_raw(base_t base, ofs_t ofs, void *buf0, size_t count);
extern void fsc_copy_slowly(base_t d_base, ofs_t d_ofs, base_t s_base, ofs_t s_ofs, size_t count);
extern void fsc_copy_slowly_reverse(base_t d_base, ofs_t d_ofs, base_t s_base, ofs_t s_ofs, size_t count);
#endif
