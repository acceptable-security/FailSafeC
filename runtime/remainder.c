/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#include <stdio.h>
#include <stdlib.h>
#include <byteorder_defs.h>
#include <type_repr.h>
#include <typeinfo.h>
#include <fsc_error.h>
#include <block.h>
#include <primitive_ops.h>
#include <fsc_debug.h>

#ifdef FSC_DEBUG_RUNTIME
#define REMAINDER_DEBUG(sz)					\
  if (fsc_debug_flag['m'])					\
     fprintf(stderr, "remainder %d: %#08x   ofs = %d  strlimit = %d\n", sz, base0, ofs, hdr->structured_ofslimit);
#else
#define REMAINDER_DEBUG(sz)
#endif

#define PREPARE_REMAINDER(sz)							\
  base_t base = base_remove_castflag(base0);					\
  fsc_header *hdr = get_header(base);						\
  base_t remainder_value_start;							\
  base_t remainder_base_start;							\
  ofs_t remainder_ofs;								\
										\
  REMAINDER_DEBUG(sz)								\
  dealloc_check_fast(base, ofs);						\
  assert (ofs + sz - 1 >= hdr->structured_ofslimit);				\
  if (ofs < hdr->structured_ofslimit)						\
    goto partial;								\
  if (ofs >= hdr->total_ofslimit || ofs + sz > hdr->total_ofslimit)		\
    fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);				\
										\
  remainder_value_start =							\
    (base + (hdr->structured_ofslimit / hdr->tinfo->virtual_size		\
	     * hdr->tinfo->real_size));						\
  remainder_base_start = 							\
    (((base_t)remainder_value_start) +						\
     ((hdr->total_ofslimit - hdr->structured_ofslimit + sizeof(void *) - 1)	\
      / sizeof(void *) * sizeof(void *)));					\
  remainder_ofs = ofs - hdr->structured_ofslimit;

byte read_byte_remainder(base_t base0, ofs_t ofs) {
  PREPARE_REMAINDER(1);
  return *(char *)(remainder_value_start + remainder_ofs);
 partial:
  fsc_raise_error(0, 0, ERR_UNKNOWN);
}

void write_byte_remainder(base_t base0, ofs_t ofs, byte val, typeinfo_t ty) {
  PREPARE_REMAINDER(1);
  *(char *)(remainder_value_start + remainder_ofs) = val;
  return;
 partial:
  fsc_raise_error(0, 0, ERR_UNKNOWN);
}

hword read_hword_remainder(base_t base0, ofs_t ofs) {
  PREPARE_REMAINDER(2);
  return *(short *)(remainder_value_start + remainder_ofs);
 partial:
  return read_hword_compose_byte(base, ofs);
}

void write_hword_remainder(base_t base0, ofs_t ofs, hword val, typeinfo_t ty) {
  PREPARE_REMAINDER(2);
  *(short *)(remainder_value_start + remainder_ofs) = val;
  return;
 partial:
  write_hword_to_byte(base, ofs, val, ty);
  return;
}

value read_word_remainder(base_t base0, ofs_t ofs) {
  PREPARE_REMAINDER(4);

  if (ofs % 4 == 0) {
    return value_of_base_vaddr(*(base_t *)(remainder_base_start + remainder_ofs),
			       *(word *)(remainder_value_start + remainder_ofs));
  } else {
    return value_of_int(*(word *)(remainder_value_start + remainder_ofs));
  }
 partial:
  assert (ofs % 4);
  return read_word_compose_byte(base, ofs);
}

void write_word_remainder(base_t base0, ofs_t ofs, value val, typeinfo_t ty) {
  PREPARE_REMAINDER(4);

  if (ofs % 4 == 0) {
    *(base_t *)(remainder_base_start + remainder_ofs) = base_of_value(val);
    *(word *)(remainder_value_start + remainder_ofs) = vaddr_of_value(val);
  } else {
    *(word *)(remainder_value_start + remainder_ofs) = vaddr_of_value(val);
  }
  return;
 partial:
  assert (ofs % 4);
  write_word_to_byte(base, ofs, val, ty);
  return;
}

void write_dword_remainder(base_t base, ofs_t ofs, dvalue val, typeinfo_t ty) {
  write_dword_to_word(base, ofs, val, ty);
  return;
}

dvalue read_dword_remainder(base_t base, ofs_t ofs) {
  return read_dword_by_word(base, ofs);
}

