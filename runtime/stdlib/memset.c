/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/memset.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <lazytype.h>
#include <wrapper_helper.h>

#include <string.h>

/**
 * @fn void *memset(void *s, int c, size_t n)
 * @brief fill memory with specified byte.
 * @param s start address.
 * @param c filled with this byte.
 * @param n fill length.
 * @return start address returned.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype atomic, pointer(atomic, ro)
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FSP_FPvii_Pv_memset(base_t tptr,
			     base_t b0, ofs_t o,
			     base_t bc, unsigned int c,
			     base_t bn, unsigned int n)
{
  base_t b;
  fsc_header *hdr;
  unsigned char cb;
  unsigned int cw;

  ptrvalue ret = ptrvalue_of_base_ofs(b0, o);

  if (n == 0) return ret;

  fsc_assert_accessible(b0, o, n, "memset");

  b = base_remove_castflag(b0);
  hdr = get_header_fast(b);

  if (hdr->tinfo->kind & TI_CONTINUOUS 
      && hdr->ptr_additional_base == NULL) {
    memset((char *)(b + o), c, n);
    return ret;
  }
  else if ((hdr->tinfo == &fsc_typeinfo_Xud_.val) && c == 0) {
    return ret;
  }
  else if (((hdr->tinfo->kind & TI_KIND_MASK) == TI_POINTER ||
	    hdr->tinfo == &fsc_typeinfo_i.val) &&
	   (o & 3) == 0 &&
	   (n & 3) == 0 && c == 0) {
    register ofs_t limit = hdr->structured_ofslimit;
    if (o + n <= limit) {
      memset((value *)(b + o * 2), 0, n * 2);
      return ret;
    } else /* fallback to access method */;
  }

  cb = (unsigned char)c;
  memset(&cw, c, sizeof(int));

  for(; o % sizeof(word) != 0 && n > 0; o++, n--)
    write_byte(b, o, cb, NULL);
  for(; n >= sizeof(word); o += sizeof(word), n -= sizeof(word))
    write_word(b, o, value_of_int(cw), NULL);
  for(; n > 0; o++, n--)
    write_byte(b, o, cb, NULL);

  return ret;
}
