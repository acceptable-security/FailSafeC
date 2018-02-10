/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/memop.c
 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <copydata.h>
#include <lazytype.h>

#include <string.h>

/**
 * @fn void bzero(void *p, size_t n)
 * @author Yutaka Oiwa.
 */
void FS_FPvi_v_bzero(base_t b0, ofs_t o, base_t bn, unsigned int n) {
    base_t b;
    fsc_header *hdr;

    if (n == 0) return;

    fsc_assert_accessible(b0, o, n, "bzero");

    b = base_remove_castflag(b0);
    hdr = get_header_fast(b);
    if (hdr->tinfo->kind & TI_CONTINUOUS
	&& hdr->ptr_additional_base == NULL) {
	bzero((char *)(b + o), n);
	return;
    } else if (((hdr->tinfo->kind & TI_KIND_MASK) == TI_POINTER ||
		hdr->tinfo == &fsc_typeinfo_i.val) && 
	       (o & 3) == 0 &&
	       (n & 3) == 0) {
	register ofs_t limit = hdr->structured_ofslimit;
	if (o + n <= limit) {
	    bzero((value *)(b + o * 2), n * 2);
	    return;
	} else /* fallback to access method */;
    } else if (hdr->tinfo == &fsc_typeinfo_Xud_.val) {
	/* type-undecided blocks are zero-cleared */
	    return;
    }
    for(; o % sizeof(word) != 0 && n > 0; o++, n--)
	write_byte(b, o, 0, NULL);
    for(; n >= sizeof(word); o += sizeof(word), n -= sizeof(word))
	write_word(b, o, value_of_int(0), NULL);
    for(; n > 0; o++, n--)
	write_byte(b, o, 0, NULL);
    return;
}

/**
 * @fn void *memmove(void *dest, const void *src, size_t n)
 * @author Yutaka Oiwa.
 */
ptrvalue FSP_FPvPvi_Pv_memmove(base_t tptr,
			       base_t dest_b0, ofs_t dest_o,
			       base_t src_b0, ofs_t src_o,
			       base_t n_b0, unsigned int n) {
    register typeinfo_t dest_typ, src_typ;
    register int is_dest_continuous, is_src_continuous;
    void *dest_p, *src_p;
    ptrvalue ret_ptr = ptrvalue_of_base_ofs(dest_b0, dest_o);
    
    if (n == 0) return ptrvalue_of_base_ofs(dest_b0, dest_o);

    fsc_assert_accessible(dest_b0, dest_o, n, "memmove");
    fsc_assert_accessible(src_b0, src_o, n, "memmove");
    
    dest_typ = get_header(dest_b0)->tinfo;
    src_typ = get_header(src_b0)->tinfo;

    dest_p = dest_typ->ti_get_raw_image_addr(dest_b0, dest_o, n);
    src_p = src_typ->ti_get_raw_image_addr(src_b0, src_o, n);

    if (dest_p) {
	base_t dest_b = base_remove_castflag(dest_b0);
	if (src_typ == &fsc_typeinfo_Xud_.val) {
	    /* type-undecided blocks are zero-cleared */
	    bzero(dest_p, n);
	} else if (src_p) {
	    memmove(dest_p, src_p, n);
	} else {
	    fsc_copy_slowly(dest_b0, dest_o, src_b0, src_o, n);
	    /* no aliasing guaranteed by kind difference */
	    /* fsc_copy_to_raw(get_realoffset_c(dest_b, dest_o), src_b0, src_o, n); */
	}
    } else {
	if (src_typ->kind & TI_CONTINUOUS) {
	    base_t src_b = base_remove_castflag(src_b0);
	    if (src_p)
		fsc_copy_from_raw(dest_b0, dest_o, src_p, n);
	    else
		fsc_copy_slowly(dest_b0, dest_o, src_b0, src_o, n);
	} else {
	    if (n >= 16 &&
		dest_typ == src_typ &&
		(dest_typ->kind & TI_KIND_MASK) != TI_SPECIAL &&
		dest_o % dest_typ->virtual_size == 0 &&
		src_o % dest_typ->virtual_size == 0 &&
		n % dest_typ->virtual_size == 0 &&
		inside_structured_area(get_header(dest_b0), dest_o, n) &&
		inside_structured_area(get_header(src_b0), src_o, n)) {
		void *dest = (void *)(base_remove_castflag(dest_b0) 
				      + dest_o / dest_typ->virtual_size * dest_typ->real_size);
		void *src = (void *)(base_remove_castflag(src_b0)
				     + src_o / dest_typ->virtual_size * dest_typ->real_size);
		memmove(dest, src, n / dest_typ->virtual_size * dest_typ->real_size);
		return ptrvalue_of_base_ofs(dest_b0, dest_o);
	    } else if (base_remove_castflag(dest_b0) == base_remove_castflag(src_b0) &&
		       src_o < dest_o) {
		fsc_copy_slowly_reverse(dest_b0, dest_o, src_b0, src_o, n);
	    } else {
		fsc_copy_slowly(dest_b0, dest_o, src_b0, src_o, n);
	    }
	}
    }
    return ret_ptr;
}

/**
 * @fn void *memcpy(void *dest, const void *src, size_t n)
 * @author Yutaka Oiwa.
 */
ptrvalue FSP_FPvPvi_Pv_memcpy(base_t tptr,
			      base_t dest_b0, ofs_t dest_o,
			      base_t src_b0, ofs_t src_o,
			      base_t n_b0, unsigned int n) {
  return FSP_FPvPvi_Pv_memmove(tptr, dest_b0, dest_o, src_b0, src_o, n_b0, n);
}

/**
 * @fn void bcopy(const void *src, void *dest, size_t n)
 * @author Yutaka Oiwa.
 */
void FS_FPvPvi_v_bcopy(base_t src_b0, ofs_t src_o,
		       base_t dest_b0, ofs_t dest_o,
		       base_t n_b0, unsigned int n) {
  FSP_FPvPvi_Pv_memmove(0, dest_b0, dest_o, src_b0, src_o, n_b0, n);
  return;
}
