/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_debug.h>
#include <block.h>
#include <lazytype.h>
#include <fsc_atomic.h>
#include <wrapper_helper.h> /* for fsc_assert_accessible */

void fsc_lazyblock_settype(base_t base0, typeinfo_t type) {
    base_t base = base_remove_castflag(base0);
    ofs_t nelems, strlimit;
    fsc_header *header = get_header_fast(base);

    if (header->tinfo != &fsc_typeinfo_Xud_.val)
	return;
    if ((header->runtime_flags & RFLAG_TYPE_MASK) != RFLAG_TYPE_NORMAL)
	fsc_raise_error_library(base0, 0, ERR_STALEPTR, "fsc_lazyblock_settype");
    if (type->kind & TI_NO_USERALLOC)
	fsc_raise_error_library(base0, 0, ERR_INVALIDARGS, "fsc_lazyblock_settype: invalid type");

    nelems = header->total_ofslimit / type->virtual_size;
    strlimit = nelems * type->virtual_size;

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['t']) {
	fprintf(stderr, "fsc_lazyblock_settype: block @ %#x set its type to %s, vsize %d, strsize %d\n",
		base, type->ti_name, header->total_ofslimit, strlimit);
    }
#endif

    header->structured_ofslimit = strlimit;
    header->fastaccess_ofslimit = strlimit;
    if (type->kind & TI_CONTINUOUS) {
	FSC_ATOMIC_SET_OR(header->runtime_flags, RFLAG_INTERNAL_BASEAREA);
    }
    header->tinfo = type;
    if (header->runtime_flags & RFLAG_DEALLOCED)
	header->fastaccess_ofslimit = 0; /* TODO: is this timing correct? */
}

byte read_byte_undecided(base_t base, ofs_t ofs) {
    fsc_assert_accessible(base, ofs, 1, NULL);
    return 0;
}

hword read_hword_undecided(base_t base, ofs_t ofs) {
    fsc_assert_accessible(base, ofs, 2, NULL);
    return 0;
}

value read_word_undecided(base_t base, ofs_t ofs) {
    fsc_assert_accessible(base, ofs, 4, NULL);
    return value_of_int(0);
}

dvalue read_dword_undecided(base_t base, ofs_t ofs) {
    fsc_assert_accessible(base, ofs, 8, NULL);
    return dvalue_of_dword(0);
}

void write_byte_to_undecided(base_t base, ofs_t ofs, byte v, typeinfo_t type) {
    fsc_assert_accessible(base, ofs, 1, NULL);
    if (v == 0) return;
    fsc_lazyblock_settype(base, type ? type : &fsc_typeinfo_c.val);
    write_byte(base, ofs, v, type);
}

void write_hword_to_undecided(base_t base, ofs_t ofs, hword v, typeinfo_t type) {
    fsc_assert_accessible(base, ofs, 2, NULL);
    if (v == 0) return;
    fsc_lazyblock_settype(base, type ? type : &fsc_typeinfo_s.val);
    write_hword(base, ofs, v, type);
}

void write_word_to_undecided(base_t base, ofs_t ofs, value v, typeinfo_t type) {
    fsc_assert_accessible(base, ofs, 4, NULL);
    if (int_of_value(v) == 0 && base_remove_castflag(base_of_value(v)) == 0) return;
    fsc_lazyblock_settype(base, type ? type : &fsc_typeinfo_i.val);
    write_word(base, ofs, v, type);
}

void write_dword_to_undecided(base_t base, ofs_t ofs, dvalue v, typeinfo_t type) {
    fsc_assert_accessible(base, ofs, 8, NULL);
    if (dword_of_dvalue(v) == 0 && base_remove_castflag(base_of_dvalue(v)) == 0) return;
    fsc_lazyblock_settype(base, type ? type : &fsc_typeinfo_q.val); /* TODO */
    write_dword(base, ofs, v, type);
}

struct typeinfo_init fsc_typeinfo_Xud_ = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "__undecided",
    TI_SPECIAL,
    NULL,
    4, 8,
    read_dword_undecided,
    read_word_undecided,
    read_hword_undecided,
    read_byte_undecided,
    write_dword_to_undecided,
    write_word_to_undecided,
    write_hword_to_undecided,
    write_byte_to_undecided,
    FSC_ADDITIONAL_HELPERS_DEFAULT
  }
};

