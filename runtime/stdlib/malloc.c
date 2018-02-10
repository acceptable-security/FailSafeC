/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa and Lepidum Co. Ltd. in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/malloc.c
 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <lazytype.h>
#include <copydata.h>

#include <memory.h>

static inline base_t alloc_by_type_size(typeinfo_t typ, int n, unsigned int flags) {
    if (n < 0) {
	fsc_raise_error_library(0, n, ERR_INVALIDARGS, "malloc_typed: invalid size to allocate");
    } else if (typ == &fsc_typeinfo_Xud_.val) {
	return fsc_alloc_block_full(typ, 0, n, flags);
    } else {
	word nelems = n / typ->virtual_size;
	word rembytes = n % typ->virtual_size;
	return fsc_alloc_block_full(typ, nelems, rembytes, flags);
    }
}

static inline typeinfo_t guess_type_from_tptr(base_t tptr) {
    /* in some case, tptr may be either null or not pointing to typeinfo.
       in those cases, return untyped. */
    assert(!is_cast(tptr));
    if (tptr == 0) {
	return &fsc_typeinfo_Xud_.val;
    } else if (get_header_fast(tptr)->tinfo != &fsc_typeinfo_Xti_.val) {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['t']) {
	    fprintf(stderr, "guess_type_from_tptr: block type guess failed:  %s not typeinfo\n",
		    get_header_fast(tptr)->tinfo->ti_name);
	}
#endif
	return &fsc_typeinfo_Xud_.val;
    } else {
	typeinfo_t tp = (typeinfo_t)tptr;
	if ((tp->kind & TI_KIND_MASK) == TI_POINTER) {
	    if (tp->referee->virtual_size == 0) {
#ifdef FSC_DEBUG_RUNTIME
		if (fsc_debug_flag['t']) {
		    fprintf(stderr, "guess_type_from_tptr: guessed block type %s is abstract\n",
			    tp->referee->ti_name);
		}
#endif
		return &fsc_typeinfo_Xud_.val;
	    }		
#ifdef FSC_DEBUG_RUNTIME
	    if (fsc_debug_flag['t']) {
		fprintf(stderr, "guess_type_from_tptr: block type guessed to %s\n",
			tp->referee->ti_name);
	    }
#endif
	    return tp->referee;
	} else {
#ifdef FSC_DEBUG_RUNTIME
	    if (fsc_debug_flag['t']) {
		fprintf(stderr, "guess_type_from_tptr: block type tp %s is not pointer\n",
			tp->ti_name);
	    }
#endif
	    return &fsc_typeinfo_Xud_.val;
	}
    }
}

ptrvalue FSP_FiPv_Pv_malloc_typed(base_t tptr_b, base_t bn, int n, base_t btc, ofs_t ot) {
    base_t bt;
    typeinfo_t typ;
    
    /* ignore tptr_b: type explicitly given */
    if (ot != 0)
	fsc_raise_error_library(btc, ot, ERR_INVALIDARGS, "malloc_typed");
    bt = base_remove_castflag(btc);
    if (get_header_fast(bt)->tinfo != &fsc_typeinfo_Xti_.val) {
	fsc_raise_error_library(btc, ot, ERR_INVALIDARGS, "malloc_typed");
    }
    typ = (typeinfo_t)bt;

    if (typ->kind & TI_NO_USERALLOC)
	fsc_raise_error_library(btc, ot, ERR_INVALIDARGS, "malloc_typed: invalid type to allocate");
    
    return ptrvalue_of_base_ofs(alloc_by_type_size(typ, n, RFLAG_TYPE_NORMAL), 0);
}

/**
 * @fn void *alloca(size_t n)
 * @brief allocate memory block
 * @param n block size
 * @return pointer to allocated memory block
 *
 * @todo deallocate on return
 *
 * @author Lepidum Co., Ltd., Yutaka OIWA
 */
ptrvalue FSP_Fi_Pv_alloca(base_t tptr_b, base_t bn, int n){
    if(n < 0){
	fsc_raise_error_library(0, n, ERR_INVALIDARGS, "alloca: invalid size to allocate");
    } else {
	base_t b = alloc_by_type_size(guess_type_from_tptr(tptr_b), n, RFLAG_NO_USER_DEALLOC);
	return ptrvalue_of_base_ofs(b, 0);
    }
}

/**
 * @fn void *malloc(size_t n)
 * @author Yutaka Oiwa.
 */
ptrvalue FSP_Fi_Pv_malloc(base_t tptr_b, base_t bn, int n) {
    base_t bt;
    
    if (n < 0) {
	fsc_raise_error_library(0, n, ERR_INVALIDARGS, "malloc: invalid size to allocate");
    } else {
	base_t b = alloc_by_type_size(guess_type_from_tptr(tptr_b), n, RFLAG_TYPE_NORMAL);
	return ptrvalue_of_base_ofs(b, 0);
    }
}

/**
 * @fn void free(void *p)
 * @author Yutaka Oiwa.
 */
void FS_FPv_v_free(base_t bc, ofs_t o) {
    base_t b = base_remove_castflag(bc);
    fsc_header *hdr;

    if (o != 0)
	fsc_raise_error_library(bc, o, ERR_INVALIDARGS, "free");
    if (b == 0)
	return; /* free(NULL) is valid */

    hdr = get_header_fast(b);

    if (hdr->runtime_flags & (RFLAG_NO_USER_DEALLOC | RFLAG_NO_DEALLOC)) {
	fsc_raise_error_library(bc, o, ERR_INVALIDARGS, "free: releasing invalid block");
    }
    if ((hdr->runtime_flags & RFLAG_TYPE_MASK) == RFLAG_TYPE_RELEASED) {
	fsc_raise_error_library(bc, o, ERR_INVALIDARGS, "free: releasing block twice");
    }
    fsc_dealloc_internal(b, "free");
    return;
}

#ifndef min
static inline int min(int x, int y) { return x < y ? x : y; }
#endif

/**
 * @fn void *realloc(void *p, size_t n)
 * @author Yutaka Oiwa.
 */
ptrvalue FSP_FPvi_Pv_realloc(base_t tptr_b, base_t p_b, int p_o, base_t n_b, int n) {
  base_t b = base_remove_castflag(p_b);
  if (p_o)
    fsc_raise_error_library(p_b, p_o, ERR_INVALIDARGS, "realloc: not block top");
  if (n < 0)
    fsc_raise_error_library(n_b, n, ERR_INVALIDARGS, "realloc: invalid size");

  if (n == 0) {
    /* ANSI semantics for realloc(0, p) is as same as free(p) */
    FS_FPv_v_free(p_b, p_o);
    return ptrvalue_of_base_ofs(0, 0);
  } else if (b == 0) {
    /* ANSI semantics for realloc(NULL, s) is as same as malloc(s) */
    return FSP_Fi_Pv_malloc(tptr_b, n_b, n);
  } else {
    ptrvalue new_p;
    base_t new_b;
    size_t copy_size, structured_copy_size;

    fsc_header *hdr = get_header_fast(b);
    typeinfo_t tinfo = hdr->tinfo;

    if (hdr->runtime_flags & (RFLAG_NO_USER_DEALLOC | RFLAG_NO_DEALLOC))
      fsc_raise_error_library(p_b, p_o, ERR_INVALIDARGS, "realloc: resizing invalid block");
    if ((hdr->runtime_flags & RFLAG_TYPE_MASK) != RFLAG_TYPE_NORMAL)
      fsc_raise_error_library(p_b, p_o, ERR_INVALIDARGS, "realloc: resizing deallocated block");
    if ((tinfo->kind & TI_NO_USERALLOC) && (tinfo != &fsc_typeinfo_Xud_.val))
      fsc_raise_error_library(p_b, p_o, ERR_INVALIDARGS, "realloc: resizing invalid block");
    
    if (hdr->total_ofslimit == n)
      return ptrvalue_of_base_ofs(p_b, p_o); /* no change */
    
    if (tinfo == &fsc_typeinfo_Xud_.val) {
	new_p = FSP_Fi_Pv_malloc(tptr_b, n_b, n);
    } else {
	new_p = FSP_FiPv_Pv_malloc_typed(tptr_b, n_b, n, (base_t)hdr->tinfo, 0);
    }
    new_b = base_remove_castflag(base_of_ptrvalue(new_p));

    if (new_b == 0)
      return ptrvalue_of_base_ofs(p_b, p_o); /* allocation failed: no touch: old block is still valid */

    copy_size = min(n, hdr->total_ofslimit);
    structured_copy_size = min(get_header_fast(new_b)->structured_ofslimit, hdr->structured_ofslimit);
    if (hdr->ptr_additional_base)
	structured_copy_size = 0; /* to correctly copy additional bases */

    assert(structured_copy_size <= copy_size);

    memcpy((void *)new_b, (void *)b, structured_copy_size / tinfo->virtual_size * tinfo->real_size);

    if (structured_copy_size < copy_size)
      fsc_copy_slowly(new_b, structured_copy_size, b, structured_copy_size, copy_size - structured_copy_size);

    fsc_dealloc_internal(b, "realloc");

    return ptrvalue_of_base_ofs_flag(new_b, 0, is_cast(p_b));
  }
}

/**
 * @fn void *calloc(size_t num, size_t sz)
 * @author Lepidum Co., Ltd.
 */
ptrvalue FSP_Fii_Pv_calloc(base_t tptr_b, base_t num_b, unsigned num, base_t sz_b, unsigned sz)
{
  unsigned s;
  
  if (num == 0 || sz == 0) {
    return FSP_Fi_Pv_malloc(tptr_b, 0, 0);
  }

  s = num * sz;
  if (s / sz != num) {
    fsc_raise_error_library(0, num, ERR_OUTOFBOUNDS, "calloc: invalid size to allocate");
  }
  return FSP_Fi_Pv_malloc(tptr_b, 0, s);
}
