/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

INLINE fsc_header *get_header_fast(base_t base) {
#ifdef CAUTIONAL_GETHEADER
    fsc_header *r = &((fsc_header *)base_remove_castflag(base))[-1];
    if (r == &((fsc_header *)0)[-1]) {
	if (fsc_debug_flag['S']) {
	    fprintf(stderr, "NULL ptr will be dereferenced: signal will be raised\n");
	}
    } else if (r->magic != FSC_BLOCK_HEADER_MAGIC) {
	fprintf(stderr, "magic %08x\n", r->magic);
	assert(r->magic == FSC_BLOCK_HEADER_MAGIC);
    }
#endif
    return &((fsc_header *)base)[-1];
}

INLINE fsc_header *get_header(base_t base) {
#ifndef CAUTIONAL_GETHEADER
    return &((fsc_header *)base_remove_castflag(base))[-1];
#else
    fsc_header *r = &((fsc_header *)base_remove_castflag(base))[-1];
    if (r == &((fsc_header *)0)[-1]) {
	if (fsc_debug_flag['S']) {
	    fprintf(stderr, "NULL ptr will dereferenced: signal will be raised\n");
	}
    } else if (r->magic != FSC_BLOCK_HEADER_MAGIC) {
	fprintf(stderr, "magic %08x\n", r->magic);
	assert(r->magic == FSC_BLOCK_HEADER_MAGIC);
    }
    return r;
#endif
}

INLINE void dealloc_check_fast(base_t base, ofs_t ofs) {
    if (get_header_fast(base)->runtime_flags & RFLAG_DEALLOCED) {
	fsc_raise_error(base, ofs, ERR_STALEPTR);
    }
}

INLINE void dealloc_check_library(base_t base, ofs_t ofs, const char *libloc) {
    if (get_header(base)->runtime_flags & RFLAG_DEALLOCED) {
	fsc_raise_error_library(base, ofs, ERR_STALEPTR, libloc);
    }
}

INLINE int is_offset_ok(base_t base, ofs_t ofs) {
#ifdef FSC_USE_INLINE_NULLCHECK
    if(base == 0)
	fsc_raise_error(base, ofs, ERR_NULLPTR);
#endif
    if (get_header_fast(base) -> fastaccess_ofslimit <= (unsigned long) ofs) {
	return 0;
    }
    return 1;
}

INLINE boundary_info_t get_boundary(base_t base) {
#ifdef FSC_USE_INLINE_NULLCHECK
    if(base == 0)
	fsc_raise_error(base, ofs, ERR_NULLPTR);
#endif
    return get_header_fast(base) -> fastaccess_ofslimit;
}

INLINE int is_boundary_offset_ok(boundary_info_t bdr, ofs_t ofs) {
    if (bdr <= (unsigned long) ofs) {
	return 0;
    }
    return 1;
}

