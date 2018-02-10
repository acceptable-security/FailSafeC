/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

INLINE int fsc_is_nullpointer(base_t base, ofs_t ofs) {
    /* In fsc_is_nullpointer, invalid pointers are not considered to be NULL. */
    return base_remove_castflag(base) == 0 && ofs == 0;
}

INLINE void fsc_ensure_nonnull(base_t base, ofs_t ofs, const char *libloc) {
    /* In fsc_ensure_nonnull, all invalid pointers (not only NULL pointer) are rejected by design. */
    if (base_remove_castflag(base) == 0)
	fsc_raise_error_library(base, ofs, ERR_NULLPTR, libloc);
}

INLINE void fsc_assert_accessible(base_t base0, register ofs_t ofs, register ofs_t size, const char *libloc) {
    ofs_t ofslimit;
    fsc_header *hdr;

    fsc_ensure_nonnull(base0, ofs, libloc);
    dealloc_check_library(base0, ofs, libloc);

    hdr = get_header(base0);

    if (hdr->tinfo->ti_check_accessible) {
	if (!hdr->tinfo->ti_check_accessible(base0, ofs, size))
	    fsc_raise_error_library(base0, ofs, ERR_OUTOFBOUNDS, libloc);
    } else {
	ofslimit = get_header(base0)->total_ofslimit;

	/* ensure ofs <= ofs + count <= ofslimit */
	if (ofs >= ofslimit)
	    fsc_raise_error_library(base0, ofs, ERR_OUTOFBOUNDS, libloc);
	if (ofs + size < ofs || ofs + size > ofslimit) /* being careful about integer overflow */
	    fsc_raise_error_library(base0, ofslimit, ERR_OUTOFBOUNDS, libloc);
    }
}

char *wrapper_get_string_z(base_t, ofs_t, void **_to_discard, const char *);
char *wrapper_get_string_zn(base_t, ofs_t, void **_to_discard, size_t, const char *);
char *wrapper_get_rawimage(base_t, ofs_t, void **_to_discard, size_t, const char *);
void *wrapper_get_read_buffer(base_t, ofs_t, void **_to_discard, size_t, const char *);
void wrapper_writeback_release_tmpbuf(base_t, ofs_t, void *, word);
void wrapper_release_tmpbuf(void *);
ptrvalue wrapper_make_new_static_string(const char *);
