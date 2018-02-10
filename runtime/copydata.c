/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <stdint.h>
#include <fsc_runtime.h>
#include <fsc_alloc.h>
#include <wrapper_helper.h>
#include <assert.h>

void fsc_copy_from_raw(base_t base, ofs_t ofs, void *buf0, size_t count) {
    char *buf = buf0;
#ifdef FSC_REQUIRE_ALIGNED_ACCESS_TO_RAW_WORD
    if (ofs % sizeof(word) != (word)buf0 % sizeof(word)) {
        for(; count > 0; ofs++, count--, buf++)
            write_byte(base, ofs, *buf, NULL);
        return;
    }
#endif
    for(; ofs % sizeof(word) != 0 && count > 0; ofs++, count--, buf++)
	write_byte(base, ofs, *buf, NULL);
    for(; count >= sizeof(word); ofs += sizeof(word), count -= sizeof(word), buf += sizeof(word))
	write_word(base, ofs, value_of_int(*(word *)buf), NULL);
    for(; count > 0; ofs++, count--, buf++)
	write_byte(base, ofs, *buf, NULL);
}

void fsc_copy_to_raw(void *buf0, base_t base, ofs_t ofs, size_t count) {
    char *buf = buf0;

#ifdef FSC_REQUIRE_ALIGNED_ACCESS_TO_RAW_WORD
    if (ofs % sizeof(word) != (word)buf0 % sizeof(word)) {
        for(; count > 0; ofs++, count--, buf++)
	    *buf = read_byte(base, ofs);
        return;
    }
#endif
    for(; ofs % sizeof(word) != 0 && count > 0; ofs++, count--, buf++)
	*buf = read_byte(base, ofs);
    for(; count >= sizeof(word); ofs += sizeof(word), count -= sizeof(word), buf += sizeof(word))
	*(word *)buf = read_word(base, ofs);
    for(; count > 0; ofs++, count--, buf++)
	*buf = read_byte(base, ofs);
}

void fsc_copy_slowly(base_t d_base, ofs_t d_ofs, base_t s_base, ofs_t s_ofs, size_t count) {
    for(; d_ofs % sizeof(word) != 0 && count > 0; d_ofs++, s_ofs++, count--)
	write_byte(d_base, d_ofs, read_byte(s_base, s_ofs), NULL);
    for(; count >= sizeof(word); d_ofs += sizeof(word), s_ofs += sizeof(word), count -= sizeof(word))
	write_word(d_base, d_ofs, read_word(s_base, s_ofs), NULL);
    for(; count > 0; d_ofs++, s_ofs++, count--)
	write_byte(d_base, d_ofs, read_byte(s_base, s_ofs), NULL);
}

void fsc_copy_slowly_reverse(base_t d_base, ofs_t d_ofs, base_t s_base, ofs_t s_ofs, size_t count) {
    d_ofs += count; s_ofs += count;
    for(; d_ofs % sizeof(word) != 0 && count > 0; d_ofs--, s_ofs--, count--)
	write_byte(d_base, d_ofs - 1, read_byte(s_base, s_ofs - 1), NULL);
    for(; count >= sizeof(word); d_ofs -= sizeof(word), s_ofs -= sizeof(word), count -= sizeof(word))
	write_word(d_base, d_ofs - sizeof(word), read_word(s_base, s_ofs - sizeof(word)), NULL);
    for(; count > 0; d_ofs--, s_ofs--, count--)
	write_byte(d_base, d_ofs - 1, read_byte(s_base, s_ofs - 1), NULL);
}

