/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

INLINE void fsc_finish_varargs(base_t b0) {
    if (b0) {
	fsc_header *header = get_header_fast(b0);
	word rf = header->runtime_flags;
	if ((rf & RFLAG_TYPE_MASK) == RFLAG_TYPE_VARARGS) {
#ifdef FSC_OLD_VARARG_CHECK
	    header->runtime_flags = rf = (rf & ~RFLAG_TYPE_MASK) | RFLAG_TYPE_VARARGS_FINISHED_BY_USER | RFLAG_DEALLOCED;
	    header->fastaccess_ofslimit = 0;
#endif
#ifdef FSC_DEBUG_RUNTIME
	    if (fsc_debug_flag['v'])
		fprintf(stderr, "block at %#08x called _va_end\n", b0);
#endif
	} else {
	    fsc_raise_error_library(b0, 0, ERR_INVALIDARGS, "va_end");
	}
    }
}

INLINE void fsc_va_end_base_ofs(base_t b, ofs_t o) {
    base_t b0 = base_remove_castflag(b);
    if (b0) {
	fsc_finish_varargs(b0);
    }
}
