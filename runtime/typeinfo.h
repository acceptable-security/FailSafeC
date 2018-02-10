/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef TYPEINFO_H
#define TYPEINFO_H
#include <type_repr.h>

typedef struct typeinfo_s *typeinfo_t;

typedef ptrvalue addr_and_length_t;

#define compose_addr_and_length_t(a,l) (ptrvalue_of_base_ofs((base_t)(a),(ofs_t)(l)))
#define addr_of_addr_and_length_t(a) ((void *)base_of_ptrvalue(a))
#define length_of_addr_and_length_t(a) ((size_t)ofs_of_ptrvalue(a))

#include <block.h>

enum typeinfo_flags {
    TI_KIND_MASK = 0xf,
    TI_NO_USERALLOC = 0x10,
    TI_CONTINUOUS = 0x20
};

enum typeinfo_kind {
    TI_PRIMITIVE = 0,
    TI_POINTER = 1,
    TI_FUNCTION = 0x12,
    TI_STRUCT = 0x3,
    TI_SPECIAL = 0x14
};

struct typeinfo_s {
    char *ti_name;
    word kind;
    typeinfo_t referee;
    word virtual_size;
    word real_size;
    
    dvalue (*ti_read_dword)(base_t, ofs_t);
    value (*ti_read_word)(base_t, ofs_t);
    hword (*ti_read_hword)(base_t, ofs_t);
    byte (*ti_read_byte)(base_t, ofs_t);

    void (*ti_write_dword)(base_t, ofs_t, dvalue, typeinfo_t);
    void (*ti_write_word)(base_t, ofs_t, value, typeinfo_t);
    void (*ti_write_hword)(base_t, ofs_t, hword, typeinfo_t);
    void (*ti_write_byte)(base_t, ofs_t, byte, typeinfo_t);

    /* introduced in ABI 2 */
    void *(*ti_get_raw_image_addr)(base_t, ofs_t, size_t);
    /* introduced in ABI 5 */
    int (*ti_check_accessible)(base_t, ofs_t, ofs_t);
    /* introduced in ABI 6 */
    addr_and_length_t (*ti_get_raw_image_addr_and_length)(base_t, ofs_t);
};

struct typeinfo_init {
  struct fsc_header header;
  struct typeinfo_s val;
};

extern struct typeinfo_init fsc_typeinfo_q;
extern struct typeinfo_init fsc_typeinfo_i;
extern struct typeinfo_init fsc_typeinfo_s;
extern struct typeinfo_init fsc_typeinfo_c;
extern struct typeinfo_init fsc_typeinfo_f;
extern struct typeinfo_init fsc_typeinfo_d;
extern struct typeinfo_init fsc_typeinfo_Xti_;
extern struct typeinfo_init fsc_typeinfo_v;

#define FSC_ADDITIONAL_HELPERS_DEFAULT get_raw_addr_default,0,get_raw_addr_length_default
#define FSC_ADDITIONAL_HELPERS_CONTINUOUS get_raw_addr_continuous,0,get_raw_addr_length_continuous

#define EMIT_TYPEINFO_ACCESS_METHODS_TABLE_NOACCESS \
    read_dword_noaccess, read_word_noaccess, \
    read_hword_noaccess, read_byte_noaccess, \
    write_dword_noaccess, write_word_noaccess, \
    write_hword_noaccess, write_byte_noaccess, \
    FSC_ADDITIONAL_HELPERS_DEFAULT

#define EMIT_HEADER_FOR_TYPEINFO EMIT_FSC_HEADER(fsc_typeinfo_Xti_.val,0)
#endif
