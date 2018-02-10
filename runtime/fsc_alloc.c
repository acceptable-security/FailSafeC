/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_debug.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <fsc_alloc_critical.h>

int fsc_alloc_use_sigprocmask = 0;
sigset_t fsc_alloc_blocking_sigset;

base_t fsc_alloc_block_full(typeinfo_t type, size_t nelems, size_t remainder, word flags) {
    size_t virtual_elemsize = type->virtual_size;
    size_t real_elemsize = type->real_size;
    size_t structured_vsize, structured_rsize;
    size_t total_vsize, total_rsize;
    void *raw_block_top;
    
    // calculate block sizes
    structured_vsize = virtual_elemsize * nelems;
    if (structured_vsize / virtual_elemsize != nelems)
	fsc_raise_error_library(0, nelems, ERR_OUTOFBOUNDS, "fsc_alloc_block(SV)");
    structured_rsize = real_elemsize * nelems;
    if (structured_rsize / real_elemsize != nelems)
	fsc_raise_error_library(0, nelems, ERR_OUTOFBOUNDS, "fsc_alloc_block(SR)");
    if (remainder * 2 < remainder) 
	fsc_raise_error_library(0, remainder, ERR_OUTOFBOUNDS, "fsc_alloc_block(R)");
    total_vsize = structured_vsize + remainder;
    if (total_vsize < structured_vsize)
	fsc_raise_error_library(0, remainder, ERR_OUTOFBOUNDS, "fsc_alloc_block(RV)");
    total_rsize = structured_rsize + fsc_round_up_to_wordsize(remainder) + fsc_round_down_to_wordsize(remainder);
    if (total_rsize < structured_rsize)
	fsc_raise_error_library(0, remainder, ERR_OUTOFBOUNDS, "fsc_alloc_block(RR)");
    ENTER_CRITICAL_SECTION
    raw_block_top = GC_malloc(sizeof(fsc_header) + total_rsize + FSC_HEADER_ALIGNSIZE);
    END_CRITICAL_SECTION

    if (raw_block_top == NULL) {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['a'])
	    fprintf (stderr, "fsc_alloc_block: allocation FAILED!!\n");
#endif
	return 0;
    } else {
	void *data;
	fsc_header *header;
	/* round up to word boundary */
	header = (fsc_header *)
	    (((unsigned int)raw_block_top + FSC_HEADER_ALIGNSIZE) & (-FSC_HEADER_ALIGNSIZE));
	data = (char *)(&header[1]);
	
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['a'])
	    fprintf (stderr, "fsc_alloc_block: %s[%d]+%d => real %d, virtual %d, flag %#x @ %p\n",
		    type->ti_name, nelems, remainder, total_rsize, total_vsize, flags, data);
#endif
	header->magic = FSC_BLOCK_HEADER_MAGIC;
	header->tinfo = type;
	header->structured_ofslimit = 
	    header->fastaccess_ofslimit = structured_vsize;
	header->total_ofslimit = total_vsize;
	header->dummy_ofslimit_for_cast = 0;
	header->runtime_flags = flags;
	header->ptr_additional_base = 0;
	/* blocks allocated by GC_malloc are zero-cleared */
	/* memset(data, 0, total_rsize); */
	return (base_t)data;
    }
}

base_t fsc_alloc_block_with_flags(typeinfo_t type, size_t len, word flags) {
    base_t p = fsc_alloc_block_full(type, len, 0, flags);
    if (p == 0) 
	fsc_raise_error_library(0, len, ERR_OUTOFMEMORY, "fsc_alloc_block");
    return p;
}

void fsc_dealloc_internal(base_t base, const char *libloc) {
    fsc_header *header = get_header(base);
    /* RFLAG_DEALLOCED is set for finished varargs blocks: do not check here! */
    if ((header->runtime_flags & RFLAG_TYPE_MASK) == RFLAG_TYPE_RELEASED)
	fsc_raise_error_library(base, 0, ERR_STALEPTR, libloc);
    header->runtime_flags = (header->runtime_flags & RFLAG_TYPE_MASK) | RFLAG_TYPE_RELEASED | RFLAG_DEALLOCED;
    header->fastaccess_ofslimit = 0;
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['a'])
	fprintf (stderr, "fsc_dealloc: %s[] @ %p (flags %#x) deallocated\n",
		 header->tinfo->ti_name, (void *)base, header->runtime_flags);
#endif
}

void fsc_enable_signal_mutex(void) {
  if (fsc_alloc_use_sigprocmask == 0) {
    sigfillset(&fsc_alloc_blocking_sigset);
    fsc_alloc_use_sigprocmask = 1;
  }
}

#define FSC_ALLOC_FORCE_COMPILE_ALL
#define INLINE
#include <fsc_alloc_inline.c>
