/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <stdint.h>
#include <fsc_runtime.h>
#include <fsc_alloc.h>
#include <fsc_debug.h>
#include <primitive_ops.h>
#include <wrapper_helper.h>
#include <copydata.h>
#include <memory.h>
#include <stdio.h>

#ifdef FSC_DEBUG_RUNTIME
static int wrap_count = 0;

void assert_all_buffers_are_released() {
  if (wrap_count)
    fsc_raise_error_library(0, wrap_count, ERR_INTERNAL, "assert_all_buffers_are_released");
}

static void inc_wrap_count(const char *libloc) {
  wrap_count++;
#if 0
  fprintf(stderr, "wrap: %s(%d)\n", libloc, wrap_count);
#endif
}

static void dec_wrap_count() {
  if (!wrap_count)
    fsc_raise_error_library(0, wrap_count, ERR_INTERNAL, "wrap_count underflow");
  wrap_count--;
#if 0
  fprintf(stderr, "release(%d)\n", wrap_count);
#endif
}

#endif

char *wrapper_get_string_zn(base_t base0, ofs_t ofs, void **pointer_to_discard, 
			    size_t limit, const char *libloc) {
    base_t base;
    fsc_header *header;
    char *p;
    ofs_t ofslimit, i;
    
    fsc_ensure_nonnull(base0, ofs, libloc);
    base = base_remove_castflag(base0);
    header = get_header_fast(base);
    ofslimit = header->total_ofslimit;
    dealloc_check_fast(base, ofs);

    if (ofslimit <= ofs)
	fsc_raise_error_library(base, ofs, ERR_OUTOFBOUNDS, libloc);

#ifdef FSC_DEBUG_RUNTIME
    inc_wrap_count(libloc);
#endif
    if (get_header_fast(base)->tinfo->kind & TI_CONTINUOUS) {
	p = get_realoffset_c(base, ofs);
	for (i = 0; i < limit; i++) {
	    if (ofslimit <= ofs + i)
		fsc_raise_error_library(base0, ofs + i, ERR_OUTOFBOUNDS, libloc);
	    if (p[i] == 0)
		break; /* EOS FOUND INSIDE BOUNDARY */
	}
	if (p[i]) {
	    /* non-terminated string */
	    char *new_p = fsc_alloc_raw(limit + 1);
	    memcpy(new_p, p, limit);
	    new_p[limit] = '\0';
	    *pointer_to_discard = p = new_p;
	} else {
	    *pointer_to_discard = NULL;
	}
    } else {
	string_buffer b = INIT_string_buffer;
	char c;

	for(i = 0; i < limit; i++) {
	    if (ofslimit <= ofs + i)
		fsc_raise_error_library(base0, ofs + i, ERR_OUTOFBOUNDS, libloc);
	    put_string_buffer(&b, c = read_byte(base, ofs + i));
	    if (c == 0)
		break;
	}
	p = get_sz_string_buffer(&b);
	*pointer_to_discard = p;
    }
    return p;
}

char *wrapper_get_string_z(base_t base0, ofs_t ofs, void **pointer_to_discard, 
			   const char *libloc) {
    return wrapper_get_string_zn(base0, ofs, pointer_to_discard,
				 SIZE_MAX, libloc);
}

char *wrapper_get_rawimage(base_t base0, ofs_t ofs, void **pointer_to_discard, 
			   size_t nbytes, const char *libloc) {
    base_t base;
    fsc_header *header;
    char *p;

    fsc_assert_accessible(base0, ofs, nbytes, libloc);
    base = base_remove_castflag(base0);
    header = get_header_fast(base);

#ifdef FSC_DEBUG_RUNTIME
    inc_wrap_count(libloc);
#endif
    p = header->tinfo->ti_get_raw_image_addr(base, ofs, nbytes);
    if (p) {
	*pointer_to_discard = NULL;
	return p;
    } else {
	p = fsc_alloc_raw(nbytes);
	fsc_copy_to_raw(p, base, ofs, nbytes);
	*pointer_to_discard = p;
	return p;
    }
}

void *wrapper_get_read_buffer(base_t base0, ofs_t ofs,
			      void **pointer_to_discard,
			      unsigned int count, 
			      const char *libloc) {
    char *p;

    fsc_assert_accessible(base0, ofs, count, libloc);

#ifdef FSC_DEBUG_RUNTIME
    inc_wrap_count(libloc);
#endif
    p = get_header(base0)->tinfo->ti_get_raw_image_addr(base0, ofs, count);
    if (p) {
	*pointer_to_discard = NULL;
	return p;
    } else {
	return (*pointer_to_discard = fsc_alloc_raw(count));
    }
}

void wrapper_writeback_release_tmpbuf(base_t base0, ofs_t ofs,
				      void *tmpbuf0,
				      word count) {
#ifdef FSC_DEBUG_RUNTIME
    dec_wrap_count();
#endif
    if (tmpbuf0 == NULL)
	return;
    else {
	base_t base = base_remove_castflag(base0);
	char *tmpbuf = tmpbuf0;
	fsc_copy_from_raw(base, ofs, tmpbuf, count);
	fsc_release_raw(tmpbuf0);
	return;
    }
}

void wrapper_release_tmpbuf(void *p) {
#ifdef FSC_DEBUG_RUNTIME
    dec_wrap_count();
#endif
    if (p != NULL) {
	fsc_release_raw(p);
    }
}

ptrvalue wrapper_make_new_static_string(const char *s) {
    int len = strlen(s) + 1;
    base_t p = fsc_alloc_static_block(&fsc_typeinfo_c.val, len);
    char *p0 = get_realoffset_c(p, 0);
    strcpy(p0, s);
    return ptrvalue_of_base_ofs(p, 0);
}

/* raw_addr function */

void *get_raw_addr_default(base_t base0, ofs_t ofs, size_t size) {
    return 0;
}

addr_and_length_t get_raw_addr_length_default(base_t base0, ofs_t ofs) {
    return compose_addr_and_length_t(0, 0);
}

void *get_raw_addr_continuous(base_t base0, ofs_t ofs, size_t size) {
    base_t base;
    fsc_header *header;
    ofs_t ofslimit;
    char *p;

    base = base_remove_castflag(base0);
    if (base == 0)
	return 0;

    header = get_header_fast(base);
    if (!(header->tinfo->kind & TI_CONTINUOUS))
	fsc_raise_error_library(base0, ofs, ERR_INTERNAL, "get_raw_addr_continuous");
    if (header->runtime_flags & RFLAG_DEALLOCED)
	return 0;
    if (header->ptr_additional_base)
	return 0;
    ofslimit = header->total_ofslimit;

    if (ofs >= ofslimit)
	return 0;
    if (ofs + size < ofs || ofs + size > ofslimit)
	return 0;

    return (char *)base + ofs;
}

addr_and_length_t get_raw_addr_length_continuous(base_t base0, ofs_t ofs) {
    base_t base;
    fsc_header *header;
    ofs_t ofslimit;
    char *p;

    base = base_remove_castflag(base0);
    if (base == 0)
	return compose_addr_and_length_t(0, 0);

    header = get_header_fast(base);
    if (!(header->tinfo->kind & TI_CONTINUOUS))
	fsc_raise_error_library(base0, ofs, ERR_INTERNAL, "get_raw_addr_continuous");
    if (header->runtime_flags & RFLAG_DEALLOCED)
	return compose_addr_and_length_t(0, 0);
    if (header->ptr_additional_base)
	return compose_addr_and_length_t(0, 0);
    ofslimit = header->total_ofslimit;

    if (ofs >= ofslimit)
	return compose_addr_and_length_t(0, 0);
    return compose_addr_and_length_t((char *)base + ofs, ofslimit - ofs);
}

#define INLINE
#include <wrapper_helper_inline.c>
