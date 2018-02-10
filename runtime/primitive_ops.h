/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */
#ifndef PRIMITIVE_OPS_H
#define PRIMITIVE_OPS_H
#include <block.h>
#include <type_repr.h>
#include <typeinfo.h>
#include <fsc_debug.h>

/* generic */
/* read word */
extern value read_word_offseted_word(base_t base, ofs_t ofs);
extern value read_word_compose_byte(base_t base, ofs_t ofs);

/* read short */
extern hword read_hword_compose_byte(base_t base, ofs_t ofs);
extern hword read_hword_by_word(base_t base, ofs_t ofs);
extern hword read_hword_offseted_hword(base_t base, ofs_t ofs);

/* read byte */
extern byte read_byte_by_word(base_t base, ofs_t ofs);
extern byte read_byte_offseted_byte(base_t base, ofs_t ofs);

/* read dword */
extern dvalue read_dword_by_word(base_t base, ofs_t ofs);
extern dvalue read_dword_offseted_dword(base_t base, ofs_t ofs);

/* fat int */
extern value read_word_fat_int(base_t base, ofs_t ofs);
extern byte read_byte_fat_int(base_t base, ofs_t ofs);

/* fat long long */

extern dvalue read_dword_fat_longlong(base_t base0, ofs_t ofs);
extern value read_word_fat_longlong(base_t base0, ofs_t ofs);
extern void write_dword_fat_longlong(base_t base0, ofs_t ofs, dvalue v, typeinfo_t ti);
extern void write_word_fat_longlong(base_t base0, ofs_t ofs, value v, typeinfo_t ti);

extern value partial_value_of_dvalue(dvalue d, int ofs);
extern void write_partial_value_to_dvalue(dvalue *d, value v, int ofs);

/* fat pointer */

extern value read_word_fat_pointer(base_t base, ofs_t ofs);
extern void write_word_fat_pointer(base_t base, ofs_t ofs, value v, typeinfo_t ti);

extern void copyto_generic(base_t, ofs_t);

/* continuous region */

extern dvalue read_dword_continuous(base_t base, ofs_t ofs);
extern value read_word_continuous(base_t base, ofs_t ofs);
extern hword read_hword_continuous(base_t base, ofs_t ofs);
extern byte read_byte_continuous(base_t base, ofs_t ofs);

extern byte read_byte_noaccess(base_t base, ofs_t ofs);
extern hword read_hword_noaccess(base_t base, ofs_t ofs);
extern value read_word_noaccess(base_t base, ofs_t ofs);
extern dvalue read_dword_noaccess(base_t base, ofs_t ofs);

extern dvalue read_dword_remainder(base_t base, ofs_t ofs);
extern value read_word_remainder(base_t base, ofs_t ofs);
extern hword read_hword_remainder(base_t base, ofs_t ofs);
extern byte read_byte_remainder(base_t base, ofs_t ofs);
extern void write_byte_remainder(base_t base, ofs_t ofs, byte val, typeinfo_t ty);
extern void write_hword_remainder(base_t base, ofs_t ofs, hword val, typeinfo_t ty);
extern void write_word_remainder(base_t base, ofs_t ofs, value val, typeinfo_t ty);
extern void write_dword_remainder(base_t base, ofs_t ofs, dvalue val, typeinfo_t ty);

/**************** WRITE ****************/

extern void write_dword_to_word(base_t base, ofs_t ofs, dvalue v, typeinfo_t);
extern void write_word_to_byte(base_t base, ofs_t ofs, value v, typeinfo_t);

extern void write_hword_to_byte(base_t base, ofs_t ofs, hword v, typeinfo_t);
extern void write_hword_to_word(base_t base, ofs_t ofs, hword v, typeinfo_t);
extern void write_byte_to_word(base_t base, ofs_t ofs, byte v, typeinfo_t);

extern void write_hword_offseted_hword(base_t base, ofs_t ofs, hword v, typeinfo_t);
extern void write_word_offseted_word(base_t base, ofs_t ofs, value v, typeinfo_t);
extern void write_dword_offseted_dword(base_t base, ofs_t ofs, dvalue v, typeinfo_t);
extern void write_byte_offseted_byte(base_t base, ofs_t ofs, byte v, typeinfo_t);

extern void write_word_fat_int(base_t base, ofs_t ofs, value v, typeinfo_t);

extern void write_dword_continuous(base_t base, ofs_t ofs, dvalue v, typeinfo_t);
extern void write_word_continuous(base_t base, ofs_t ofs, value v, typeinfo_t);
extern void write_hword_continuous(base_t base, ofs_t ofs, hword v, typeinfo_t);
extern void write_byte_continuous(base_t base, ofs_t ofs, byte v, typeinfo_t);

extern void write_byte_noaccess(base_t base, ofs_t ofs, byte val, typeinfo_t);
extern void write_hword_noaccess(base_t base, ofs_t ofs, hword val, typeinfo_t);
extern void write_word_noaccess(base_t base, ofs_t ofs, value val, typeinfo_t);
extern void write_dword_noaccess(base_t base, ofs_t ofs, dvalue val, typeinfo_t);

/**************** dispatch function ****************/

extern dvalue read_dword(base_t, ofs_t);
extern value read_word(base_t, ofs_t);
extern hword read_hword(base_t, ofs_t);
extern byte read_byte(base_t, ofs_t);

extern void write_byte(base_t base, ofs_t ofs, byte v, typeinfo_t ti);
extern void write_hword(base_t base, ofs_t ofs, hword v, typeinfo_t ti);
extern void write_word(base_t base, ofs_t ofs, value v, typeinfo_t ti);
extern void write_dword(base_t base, ofs_t ofs, dvalue v, typeinfo_t ti);

/* specific functions */
extern byte *get_realoffset_c(base_t base, ofs_t ofs);
extern hword *get_realoffset_s(base_t base, ofs_t ofs);
extern float *get_realoffset_f(base_t base, ofs_t ofs);
extern double *get_realoffset_d(base_t base, ofs_t ofs);
extern value *get_realoffset_i(base_t base, ofs_t ofs);
extern dvalue *get_realoffset_q(base_t base, ofs_t ofs);
extern ptrvalue *get_realoffset_P(base_t base, ofs_t ofs);

extern int is_well_typed_pointer(value v, typeinfo_t target);

/* additional base */
extern base_t *fsc_allocate_additional_base(base_t);
extern value read_merge_additional_base_word(word v, base_t base, ofs_t ofs);
extern dvalue read_merge_additional_base_dword(dword v, base_t base, ofs_t ofs);
extern void write_additional_base_word(base_t base, ofs_t ofs, base_t write_b);
extern void write_additional_base_dword(base_t base, ofs_t ofs, base_t write_b);

extern word word_of_float(float f);
extern float float_of_word(word w);
extern dword dword_of_double(double d);
extern double double_of_dword(dword d);

extern value value_of_float(float f);
extern float float_of_value(value w);
extern dvalue dvalue_of_double(double d);
extern double double_of_dvalue(dvalue d);

/*** varargs blocks ****/

/* varargs blocks are actually block of ints. */

extern void fsc_put_varargs(base_t base, size_t n, value v);
extern void fsc_put_varargs_2(base_t base, size_t n, dvalue v);

extern int inside_structured_area(register fsc_header *hdr, register ofs_t ofs, register ofs_t size);

/* wrapper helper methods in access methods */
extern void *get_raw_addr_default(base_t, ofs_t, size_t);
extern void *get_raw_addr_continuous(base_t, ofs_t, size_t);
extern addr_and_length_t get_raw_addr_length_default(base_t, ofs_t);
extern addr_and_length_t get_raw_addr_length_continuous(base_t, ofs_t);

#ifndef FSC_DEBUG_RUNTIME
#define INLINE extern inline
#include <primitive_ops_inline.c>
#undef INLINE
#endif

#endif
