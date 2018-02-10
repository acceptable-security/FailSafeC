/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef BLOCK_H
#define BLOCK_H
#include <fsc_debug.h>
#include <type_repr.h>
#include <stdlib.h>
#include <fsc_error.h>
#ifdef CAUTIONAL_GETHEADER
#include <assert.h>
#include <stdio.h>
#endif

#define FSC_HEADER_ALIGNSIZE 8
#define FSC_BLOCK_HEADER_MAGIC 0xb9aaf10f

struct fsc_header {
    word magic __attribute__((aligned (8)));
    struct typeinfo_s *tinfo;
    word runtime_flags;
    word fastaccess_ofslimit;
    word dummy_ofslimit_for_cast;
    word structured_ofslimit;
    word total_ofslimit;
    base_t *ptr_additional_base;
};

enum block_runtime_flag {
    RFLAG_TYPE_MASK = 0xf,
    RFLAG_TYPE_NORMAL = 0,
    RFLAG_TYPE_VARARGS = 1,
    RFLAG_TYPE_VARARGS_FINISHED_BY_USER = 2,
    RFLAG_TYPE_RELEASED = 3,
    RFLAG_NO_USER_DEALLOC = 0x10,
    RFLAG_NO_DEALLOC = 0x20,
    RFLAG_DEALLOCED = 0x40,
    RFLAG_INTERNAL_BASEAREA = 0x80,
    RFLAG_MAPPED_BLOCK = 0x100,
};
#define RFLAG_FOR_STATIC_DATA (RFLAG_NO_DEALLOC | RFLAG_NO_USER_DEALLOC)
#define RFLAG_FOR_STACK_DATA (RFLAG_NO_USER_DEALLOC)

typedef struct fsc_header fsc_header;

extern fsc_header *get_header_fast(base_t base);
extern fsc_header *get_header(base_t base);
extern void dealloc_check_fast(base_t base, ofs_t ofs);
extern void dealloc_check_library(base_t base, ofs_t ofs, const char *libloc);
extern int is_offset_ok(base_t base, ofs_t ofs);
extern boundary_info_t get_boundary(base_t base);
extern int is_boundary_offset_ok(boundary_info_t bdr, ofs_t ofs);

#define fsc_abort(b,err) fsc_raise_error(0,0,(err))

#define EMIT_FSC_HEADER(t,s) { .magic = FSC_BLOCK_HEADER_MAGIC, \
                               .tinfo = &t, .fastaccess_ofslimit = s, \
                               .structured_ofslimit = s, \
                               .dummy_ofslimit_for_cast = 0, .total_ofslimit = s, \
                               .ptr_additional_base = 0, \
                               .runtime_flags = RFLAG_FOR_STATIC_DATA }

#define fsc_round_up_to_wordsize(v) (((ofs_t)(v) + sizeof(void *) - 1) & (-sizeof(void *)))
#define fsc_round_down_to_wordsize(v) (((ofs_t)(v)) & (-sizeof(void *)))

#ifndef FSC_DEBUG_RUNTIME
#define INLINE extern inline
#include <block_inline.c>
#undef INLINE
#endif

#endif
