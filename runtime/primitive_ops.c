/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

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

/* generic routine */

/* read by byte */

static inline void align_check(ofs_t ofs, ofs_t size) {
#ifdef RAISE_ON_MISALIGN
  if (ofs & (size - 1)) {
    fsc_raise_error (base, ofs, ERR_UNALIGNED);
  }
#endif
}

inline value read_word_compose_byte(base_t base, ofs_t ofs) {
  align_check(ofs, 4);
  {
    register byte b0 = read_byte(base, ofs + OFS_WORD_LOW);
    register byte b1 = read_byte(base, ofs + OFS_WORD_LOW2);
    register byte b2 = read_byte(base, ofs + OFS_WORD_HIGH2);
    register byte b3 = read_byte(base, ofs + OFS_WORD_HIGH);
    return value_of_int(b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));
  }
}

inline hword read_hword_compose_byte(base_t base, ofs_t ofs) {
  align_check(ofs, 2);
  {
    register byte b0 = read_byte(base, ofs + OFS_SHORT_LOW);
    register byte b1 = read_byte(base, ofs + OFS_SHORT_HIGH);
    return b0 | (b1 << 8);
  }
}

/* Read from word array */

inline int inside_structured_area(register fsc_header *hdr, register ofs_t ofs, register ofs_t size) {
    register ofs_t strlimit = hdr->structured_ofslimit;
    return (ofs < strlimit && ofs + size <= strlimit && ofs + size > ofs);
}

inline value read_word_offseted_word(base_t base, ofs_t ofs) {
  fsc_header *hdr = get_header(base);
  if ((ofs & 3) == 0) {
    return read_word(base, ofs);
  } else if (!inside_structured_area(hdr, ofs + 3 + (4 - (ofs & 3)), 1)) {
    return read_word_compose_byte(base, ofs);
  } else {
#ifndef RAISE_ON_MISALIGN
    register word shift = ofs & 3;
    register word wofs = ofs & ~3;
#ifdef FSC_IS_BIG_ENDIAN
    word high = int_of_value(read_word(base, wofs));
    word low = int_of_value(read_word(base, wofs + 4));
    switch(shift) {
    case 1:
      return value_of_int(high << 8 | low >> 24);
    case 2:
      return value_of_int(high << 16 | low >> 16);
    case 3:
    default:
      return value_of_int(high << 24 | low >> 8);
    }
#else
    word low = int_of_value(read_word(base, wofs));
    word high = int_of_value(read_word(base, wofs + 4));
    switch(shift) {
    case 1:
      return value_of_int(high << 24 | low >> 8);
    case 2:
      return value_of_int(high << 16 | low >> 16);
    case 3:
    default:
      return value_of_int(high << 8 | low >> 24);
    }
#endif
#else
    fsc_raise_error(base, ofs, ERR_UNALIGNED);
#endif
  }
}

inline hword read_hword_offseted_hword(base_t base, ofs_t ofs) {
  fsc_header *hdr = get_header(base);
  if ((ofs & 2) == 0) {
    return read_word(base, ofs);
  } else if (!inside_structured_area(hdr, ofs + 1 + (2 - (ofs & 1)), 1)) {
    return read_hword_compose_byte(base, ofs);
  } else {
#ifndef RAISE_ON_MISALIGN
    register word wofs = ofs & ~1;
#ifdef FSC_IS_BIG_ENDIAN
    hword high = read_hword(base, wofs);
    hword low = read_hword(base, wofs + 2);
    return high << 8 | low >> 8;
#else
    hword low = read_hword(base, wofs);
    hword high = read_word(base, wofs + 2);
    return high << 8 | low >> 8;
#endif
#else
    fsc_raise_error(base, ofs, ERR_UNALIGNED);
#endif
  }
}

hword read_hword_by_word(base_t base, ofs_t ofs) {
  register word shift = ofs & 3;
  register word wofs = ofs & ~3;

  if (!inside_structured_area(get_header(base), wofs, 4)) {
    return read_hword_remainder(base, ofs);
  } else {

  word self = int_of_value(read_word(base, wofs));
  switch(shift) {
  case OFS_SHORT_HIGH * 2:
    return self >> 16;
  case OFS_SHORT_LOW * 2:
    return self & 0xffff;
#ifdef RAISE_ON_MISALIGN
  default:
    fsc_raise_error (base, ofs, ERR_UNALIGNED);
#else
  case 1:
    return (self >> 8) & 0xffff;
  case 3: default:
    {
      word next = int_of_value(read_word(base, wofs + 4));
#ifdef FSC_IS_BIG_ENDIAN
      return (self << 8 | next >> 24) & 0xffff;
#else
      return (next << 8 | self >> 24) & 0xffff;
#endif
#endif
    }
  }
  }
}

inline byte read_byte_by_word(base_t base, ofs_t ofs) {
  register unsigned int remainder = ofs & 3;
  register unsigned int wordaddr = ofs - remainder;

  if (!inside_structured_area(get_header(base), wordaddr, 4)) {
    return read_byte_remainder(base, ofs);
  } else {

  unsigned int i = int_of_value(read_word(base, wordaddr));
  switch (remainder) {
  case OFS_WORD_HIGH:
    i >>= 24;
    break;
  case OFS_WORD_HIGH2:
    i >>= 16;
    break;
  case OFS_WORD_LOW2:
    i >>= 8;
    break;
  default:
    break;
  }
  return (byte)(i & 0xff);
  }
}

/* misc */

#ifdef UNUSED_1
inline value read_word_compose_hword(base_t base, ofs_t ofs) {
  align_check(ofs, 4);
v  if (ofs & 1) {
    return read_word_compose_byte(base, ofs);
  } else {
    register unsigned short b0 = read_hword(base, ofs + OFS_SHORT_LOW * 2);
    register unsigned short b1 = read_hword(base, ofs + OFS_SHORT_HIGH * 2);
    return value_of_int(b0 | (b1 << 16));
  }
}
#endif

dvalue read_dword_by_word(base_t base, ofs_t ofs) {
  align_check(base, 8);
  {
    register unsigned int w0 = int_of_value(read_word(base, ofs + OFS_SHORT_LOW * 4));
    register unsigned int w1 = int_of_value(read_word(base, ofs + OFS_SHORT_HIGH * 4));
    /*    printf("R: high = %x, low = %x\n", w1, w0); */
    return dvalue_of_dword((dword)w1 << 32 | w0);
  }
}

void write_hword_offseted_hword(base_t base, ofs_t ofs, hword v, typeinfo_t ty) {
  fsc_header *hdr = get_header(base);
  if ((ofs & 1) == 0) {
    return write_hword(base, ofs, v, ty);
  } else if (!inside_structured_area(hdr, ofs + 1 + (2 - (ofs & 1)), 1)) {
    return write_hword_to_byte(base, ofs, v, ty);
  } else {
    return write_hword_to_byte(base, ofs, v, ty); /* TODO */
  }
}

void write_word_offseted_word(base_t base, ofs_t ofs, value v, typeinfo_t ty) {
  fsc_header *hdr = get_header(base);
  if ((ofs & 3) == 0) {
    return write_word(base, ofs, v, ty);
  } else if (!inside_structured_area(hdr, ofs + 3 + (4 - (ofs & 3)), 1)) {
    return write_word_to_byte(base, ofs, v, ty);
  } else {
    return write_word_to_byte(base, ofs, v, ty); /* TODO */
  }
}

void write_dword_offseted_dword(base_t base, ofs_t ofs, dvalue v, typeinfo_t ty) {
  write_dword_to_word(base, ofs, v, ty);
  return;
}

#ifdef UNUSED_1
void write_byte_offseted_byte(base_t base, ofs_t ofs, byte v, typeinfo_t ty) {
    /* what happened? */
    fsc_raise_error(base, ofs, ERR_UNKNOWN);
}
#endif

void write_hword_to_byte(base_t base, ofs_t ofs, hword v, typeinfo_t ty) {
  write_byte(base, ofs + OFS_SHORT_HIGH, (byte)(v >> 8), ty);
  write_byte(base, ofs + OFS_SHORT_LOW, (byte)v, ty);
}

void write_word_to_byte(base_t base, ofs_t ofs, value va, typeinfo_t ty) {
  word v = vaddr_of_value(va);
  write_byte(base, ofs + OFS_WORD_HIGH, (byte)(v >> 24), ty);
  write_byte(base, ofs + OFS_WORD_HIGH2, (byte)(v >> 16), ty);
  write_byte(base, ofs + OFS_WORD_LOW2, (byte)(v >> 8), ty);
  write_byte(base, ofs + OFS_SHORT_LOW, (byte)v, ty);
}

void write_dword_to_word(base_t base, ofs_t ofs, dvalue dv, typeinfo_t ty) {
  dword v = vaddr_of_dvalue(dv);
  /* printf("W: high = %x, low = %x\n", (word)(v >> 32), (word)v);*/
  write_word(base, ofs + OFS_SHORT_HIGH * 4, (word)(v >> 32), ty);
  write_word(base, ofs + OFS_SHORT_LOW * 4, (word)v, ty);
}


/* Type-Specific Routine */

/* fat integer */

value read_word_fat_int(base_t b0, ofs_t ofs) {
  base_t base = base_remove_castflag(b0);
  fsc_header *hdr = get_header(base);

  dealloc_check_fast(base, ofs);
  if (!inside_structured_area(hdr, ofs, 4)) {
    return read_word_remainder(base, ofs);
  }
  if ((ofs & 3) == 0) {
      /* aligned */
      return *(value *)(base + ofs * 2);
  } else {
    /* unaligned access */
    return read_word_offseted_word(base, ofs);
  }
}

byte read_byte_fat_int(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

  dealloc_check_fast(base, ofs);
  if (!inside_structured_area(hdr, ofs, 1)) {
    return read_byte_remainder(base, ofs);
  } else {
    register unsigned int word = ofs / sizeof(int);
    register unsigned int byteofs = ofs % sizeof(int);
#ifdef FSC_IS_LITTLE_ENDIAN
    return *(byte *)(base + word * 8 + byteofs);
#endif
#ifdef FSC_IS_BIG_ENDIAN
    return *(byte *)(base + word * 8 + byteofs + 4);
#endif
  }
}

/* fat pointers */

value read_word_fat_pointer(base_t base0, ofs_t ofs) {
    base_t base = base_remove_castflag(base0);
    fsc_header *hdr = get_header(base);

    dealloc_check_fast(base, ofs);
    if (!inside_structured_area(hdr, ofs, 4)) {
      return read_word_remainder(base0, ofs);
    }
    if ((ofs & 3) == 0) {
	register ptrvalue pv = *(ptrvalue *)(base + ofs * 2);
	return value_of_ptrvalue(pv);
    } else {
	/* unaligned access */
	return read_word_offseted_word(base, ofs);
    }
}

/* fat dword */
dvalue read_dword_fat_longlong(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header_fast(base);

  dealloc_check_fast(base, ofs);
  if (!inside_structured_area(hdr, ofs, 8)) {
    return read_dword_remainder(base0, ofs);
  }
  if ((ofs & 7) == 0) {
    return *(dvalue *)(base + ofs * 2);
  } else {
    return read_dword_by_word(base, ofs);
  }
}

value read_word_fat_longlong(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header_fast(base);

  dealloc_check_fast(base, ofs);
  if (!inside_structured_area(hdr, ofs, 4)) {
    return read_word_remainder(base0, ofs);
  }
  if ((ofs & 3) == 0) {
    dvalue *d = (dvalue *)(base + (ofs & ~7) * 2);
    register int i = (ofs & 4) / 4;
    return value_of_base_vaddr(d->dv_base[i], ((word *)&d->dv_vaddr)[i]);
  } else {
    return read_word_offseted_word(base, ofs);
  }
}

void write_dword_fat_longlong(base_t base0, ofs_t ofs, dvalue v, typeinfo_t ti) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header_fast(base);

  dealloc_check_fast(base, ofs);
  if (!inside_structured_area(hdr, ofs, 8)) {
    write_dword_remainder(base0, ofs, v, ti);
    return;
  }
  if ((ofs & -8) == 0) {
    *(dvalue *)(base + ofs * 2) = v;
    return;
  } else {
    write_dword_to_word(base, ofs, v, ti);
    return;
  }
}

void write_word_fat_longlong(base_t base0, ofs_t ofs, value v, typeinfo_t ti) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header_fast(base);

  dealloc_check_fast(base, ofs);
  if (!inside_structured_area(hdr, ofs, 4)) {
    write_word_remainder(base0, ofs, v, ti);
    return;
  }
  if ((ofs & 3) == 0) {
    dvalue *d = (dvalue *)(base + (ofs & -8) * 2);
    register int i = (ofs & 4) / 4;
    d->dv_base[i] = base_of_value(v);
    ((word *)&d->dv_vaddr)[i] = vaddr_of_value(v);
    return;
  } else {
    write_word_offseted_word(base, ofs, v, ti);
    return;
  }
}

/* continuous regions (normal integer, float, double, etc...) */

inline int inside_continuous_area(register fsc_header *hdr, register ofs_t ofs, register ofs_t size) {
    register ofs_t limit = hdr->total_ofslimit;
    return (ofs < limit && ofs + size <= limit && ofs + size > ofs);
}

dvalue read_dword_continuous(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

  dealloc_check_fast(base, ofs);
  if (!inside_continuous_area(hdr, ofs, 8)) {
    fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
  }
  if ((ofs & 7) == 0) {
    return read_merge_additional_base_dword(*(dword *)(base + ofs), base, ofs);
  } else {
    /* unaligned access */
    return read_dword_by_word(base, ofs);
  }
}

value read_word_continuous(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

  dealloc_check_fast(base, ofs);
  if (!inside_continuous_area(hdr, ofs, 4)) {
    fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
  }
  if ((ofs & 3) == 0) {
    return read_merge_additional_base_word(*(word *)(base + ofs), base, ofs);
  } else {
    /* unaligned access */
    return read_word_offseted_word(base, ofs);
  }
}

hword read_hword_continuous(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

  dealloc_check_fast(base, ofs);
  if (!inside_continuous_area(hdr, ofs, 2)) {
    fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
  }
  if ((ofs & 1) == 0) {
    return *(hword *)(base + ofs);
  } else {
    /* unaligned access */
    return read_hword_compose_byte(base, ofs);
  }
}

byte read_byte_continuous(base_t base0, ofs_t ofs) {
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);
  dealloc_check_fast(base, ofs);
  if (ofs >= hdr->total_ofslimit) {
    fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
  }
  return *(byte *)(base + ofs);
}

/* WRITE */

void write_hword_to_word(base_t base, ofs_t ofs, hword val, typeinfo_t ty) {
    int wofs = ofs & ~3;
    int iofs = ofs & 3;

    assert(iofs <= 2);

    if (!inside_structured_area(get_header(base), wofs, 4)) {
      write_hword_remainder(base, ofs, val, ty);
      return;
    } else {
    word i = vaddr_of_value(read_word(base, wofs));
    *(short *)(((char *)&i)+iofs) = val;
    write_word(base, wofs, value_of_int(i), ty);
    }
}

void write_byte_to_word(base_t base, ofs_t ofs, byte val, typeinfo_t ty) {
    int wofs = ofs & ~3;
    int iofs = ofs & 3;

    if (!inside_structured_area(get_header(base), wofs, 4)) {
      write_byte_remainder(base, ofs, val, ty);
      return;
    } else {
    word i = vaddr_of_value(read_word(base, wofs));
    ((char *)&i)[iofs] = val;
    write_word(base, wofs, value_of_int(i), ty);
    }
}

static inline void write_partial_hword(base_t base, ofs_t ofs, hword val, int from) {
  hword orig, new;
  assert((ofs & 1) == 0);
  orig = read_hword(base, ofs);
  new = orig;
  switch(ofs) {
  case OFS_SHORT_HIGH:
    new = (orig & 0x00ff) | (val << 8);
    break;
  case OFS_SHORT_LOW:
    new = (orig & 0xff00) | (val << 0);
    break;
  }
  write_hword(base, ofs, new, NULL);
}

static inline void write_partial_word(base_t base, ofs_t ofs, word val, int from, int width) {
  word orig, new;
  assert((ofs & 3) == 0);
  orig = int_of_value(read_word(base, ofs));
  new = orig;
  switch(width) {
  case 1:
    switch(ofs) {
    case OFS_WORD_HIGH:
      new = (orig & 0x00ffffff) | (val << 24);
      break;
    case OFS_WORD_HIGH2:
      new = (orig & 0xff00ffff) | (val << 16);
      break;
    case OFS_WORD_LOW2:
      new = (orig & 0xffff00ff) | (val << 8);
      break;
    case OFS_WORD_LOW:
      new = (orig & 0xffffff00) | (val << 0);
      break;
    }
  case 2:
    switch(ofs) {
    case OFS_SHORT_HIGH * 2:
      new = (orig & 0x0000ffff) | (val << 16);
      break;
    case 1:
      new = (orig & 0xff0000ff) | (val << 8);
      break;
    case OFS_SHORT_LOW * 2:
      new = (orig & 0xffff0000) | (val << 0);
      break;
    }
  case 3:
    switch(ofs) {
    case OFS_SHORT_HIGH:
      new = (orig & 0x000000ff) | (val << 8);
      break;
    case OFS_SHORT_LOW:
      new = (orig & 0xff000000) | (val << 0);
      break;
    }
  }
  write_word(base, ofs, value_of_int(new), NULL);
}

void write_word_fat_int(base_t base0, ofs_t ofs, value val, typeinfo_t ty)
{
    base_t base = base_remove_castflag(base0);
    fsc_header *hdr = get_header(base);

    dealloc_check_fast(base, ofs);
    if (!inside_structured_area(hdr, ofs, 4)) {
      write_word_remainder(base0, ofs, val, ty);
      return;
    }
    if ((ofs & 3) == 0) {
	/* aligned */
	*(value *)(base + ofs * 2) = val;
	/* *(base_t *)(base + ofs * 2 + OFS_SHORT_HIGH * 4) = base_of_value(val); */
	/* *(vaddr_t *)(base + ofs * 2 + OFS_SHORT_LOW * 4) = vaddr_of_value(val); */
    } else {
	/* unaligned access */
	write_word_offseted_word(base, ofs, int_of_value(val), ty);
    }
}

void write_word_fat_pointer(base_t base0, ofs_t ofs, value val, typeinfo_t ty)
{
    base_t base = base_remove_castflag(base0);
    fsc_header *hdr = get_header(base);
    typeinfo_t typ = hdr->tinfo;
    int is_mistyped = !is_well_typed_pointer(val, typ);
    
    dealloc_check_fast(base, ofs);
    if (!inside_structured_area(hdr, ofs, 4)) {
      write_word_remainder(base0, ofs, val, ty);
      return;
    }
    if ((ofs & 3) == 0) {
	/* aligned */
	*(ptrvalue *)(base + ofs * 2) = 
	    ptrvalue_of_base_ofs(base_of_value(val) |
				 fsc_canonify_tag(is_mistyped),
				 ofs_of_value(val));
    } else {
	/* unaligned access */
	write_word_offseted_word(base, ofs, int_of_value(val), ty);
    }
}

void write_byte_continuous(base_t base0, ofs_t ofs, byte val, typeinfo_t ty)
{
    base_t base = base_remove_castflag(base0);
    fsc_header *hdr = get_header(base);

    dealloc_check_fast(base, ofs);
    if (ofs >= hdr->total_ofslimit) {
	fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
	return;
    }
    *(byte *)(base + ofs) = val;
}

void write_hword_continuous(base_t base0, ofs_t ofs, hword val, typeinfo_t ty)
{
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

    dealloc_check_fast(base, ofs);
    if (!inside_continuous_area(hdr, ofs, 2)) {
	fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
	return;
    }

    if ((ofs & 1) == 0) {
	*(hword *)(base + ofs) = val;
    } else {
	/* unaligned access */
	write_hword_to_byte(base, ofs, val, ty);
    }
}

void write_word_continuous(base_t base0, ofs_t ofs, value val, typeinfo_t ty)
{
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

    dealloc_check_fast(base, ofs);
    if (!inside_continuous_area(hdr, ofs, 4)) {
	fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
	return;
    }

    if ((ofs & 3) == 0) {
	*(word *)(base + ofs) = int_of_value(val);
	write_additional_base_word(base, ofs, base_of_value(val));
    } else {
	/* unaligned access */
	write_word_offseted_word(base, ofs, int_of_value(val), ty);
    }
}

void write_dword_continuous(base_t base0, ofs_t ofs, dvalue val, typeinfo_t ty)
{
  base_t base = base_remove_castflag(base0);
  fsc_header *hdr = get_header(base);

    dealloc_check_fast(base, ofs);
    if (!inside_continuous_area(hdr, ofs, 8)) {
	fsc_raise_error(base, ofs, ERR_OUTOFBOUNDS);
	return;
    }

    if ((ofs & 7) == 0) {
	*(dword *)(base + ofs) = dword_of_dvalue(val);
	write_additional_base_dword(base, ofs, base_of_dvalue(val));
    } else {
	/* unaligned access */
	write_dword_offseted_dword(base, ofs, val, ty);
    }
}

dvalue read_dword_offseted_dword(base_t base, ofs_t ofs) {
  return read_dword_by_word(base, ofs);
}


/*** abstract region **/

byte read_byte_noaccess(base_t base, ofs_t ofs) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}

hword read_hword_noaccess(base_t base, ofs_t ofs) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}

value read_word_noaccess(base_t base, ofs_t ofs) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}

dvalue read_dword_noaccess(base_t base, ofs_t ofs) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}

void write_byte_noaccess(base_t base, ofs_t ofs, byte val, typeinfo_t ty) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}
  
void write_hword_noaccess(base_t base, ofs_t ofs, hword val, typeinfo_t ty) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}
  
void write_word_noaccess(base_t base, ofs_t ofs, value val, typeinfo_t ty) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}
  
void write_dword_noaccess(base_t base, ofs_t ofs, dvalue val, typeinfo_t ty) {
  fsc_raise_error(base, ofs, ERR_NOACCESS);
}

#define INLINE
#include "type_repr_inline.c"
#include "primitive_ops_inline.c"
#include "block_inline.c"
