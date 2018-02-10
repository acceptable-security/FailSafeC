/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef TYPE_REPR_H
#define TYPE_REPR_H
#include <byteorder_defs.h>
#define FATVALUE_MODE_COMPLEX
/*#define FATVALUE_MODE_DWORD*/
#define FATDVALUE_MODE_STRUCT

#define FSC_TAGBIT 2
#define FSC_TAGVALUE (1 << FSC_TAGBIT)
#define fsc_canonify_tag(x) ((x) ? FSC_TAGVALUE : 0)

typedef unsigned long long dword;
typedef unsigned int word;
typedef unsigned short hword;
typedef unsigned char byte;

typedef word base_t, ofs_t, vaddr_t, boundary_info_t;

typedef signed int signed_ofs_t;
typedef signed long long signed_dw_ofs_t;
typedef unsigned long long unsigned_dw_ofs_t;
#define MAX_OFS_T (~(ofs_t)0)

#ifdef FATVALUE_MODE_COMPLEX
typedef unsigned int __complex__ value, ptrvalue;

#define value_of_int(x) ((value)(word)(x))
#define value_of_base_vaddr(b,va) ({ value __x = (va); __imag__ __x = (b); __x; })
/* #define value_of_base_vaddr(b,va) ((value)(((word)(va)) + ((word)(b)) * 1i)) */
#define ptrvalue_of_base_ofs value_of_base_vaddr
#define ptrvalue_of_base_ofs_flag(b,o,f) (value_of_base_vaddr((b)|fsc_canonify_tag(f),(o)))

#define vaddr_of_value(v) ((vaddr_t)__real__ (v))
#define base_of_value(v) ((base_t)__imag__ (v))
#define ofs_of_ptrvalue vaddr_of_value
#define base_of_ptrvalue base_of_value

#define EMIT_INIT_i(b,o) {{ ((word)(b))+(o), (base_t)(b) }}
#define EMIT_INITPTR(b,o,f) {{ (o), ((base_t)(b))+fsc_canonify_tag(f) }}

union fsc_initU_i {
    struct fsc_initS_i {
	word ofs, base;
    } init;
    value cv;
};
union fsc_initUptr {
    struct fsc_initSptr {
	word ofs, base;
    } init;
    value cv;
};

#else
#ifdef FATVALUE_MODE_DWORD
typedef unsigned long long value, ptrvalue; /* base and virtual addr */

#define value_of_int(x) ((dword)(word)(x))
#if !defined(FSC_NO_INLINEASM) && defined(__i386__)
extern value value_of_base_vaddr(base_t b, word va);
#else
#define value_of_base_vaddr(b,va) ((word)(va) | ((dword)(word)(b) << 32))
#endif
#define ptrvalue_of_base_ofs value_of_base_vaddr
#define ptrvalue_of_base_ofs_flag(b,o,f) (value_of_base_vaddr((b)|fsc_canonify_tag(f),(o)))

#define vaddr_of_value(v) ((vaddr_t)((v) & 0xffffffffU))
#define ofs_of_ptrvalue vaddr_of_value
#define base_of_value(v) ((base_t)(((unsigned long long) v) >> 32))
#define base_of_ptrvalue base_of_value

#define EMIT_INIT_i(b,o) {EMIT_INIT_TWO_WORDS(((base_t)(b)),((word)(b))+(o))}
#define EMIT_INITPTR(b,o,f) {EMIT_INIT_TWO_WORDS(((base_t)(b))+fsc_canonify_tag(f),(o))}

union fsc_initU_i {
    struct fsc_initS_i {
      EMIT_DECL_TWO_WORDS (word base, word ofs);
    } init;
    value cv;
};
union fsc_initUptr {
    struct fsc_initSptr {
      EMIT_DECL_TWO_WORDS (word base, word ofs);
    } init;
    value cv;
};
#else

typedef struct { word v_base; word v_vaddr; } value;

#define value_of_int(x) ((value) { 0, x })
#define value_of_base_vaddr(b, va) ((value) { b, va })

#define vaddr_of_value(v) ((v).v_vaddr)
#define base_of_value(v) ((v).v_base)

#define EMIT_INIT_i(b,o) { (b), (b) + (o) }

#endif

#endif

#ifdef FATDVALUE_MODE_COMPLEX
typedef unsigned long long __complex__ dvalue;

#define dvalue_of_dword(x) ((dvalue)(dword)(x))
#define dvalue_of_base_vaddr(b,va) ({ dvalue __x = (va); __imag__ __x = (base_t)(b); __x; })
/* #define dvalue_of_base_vaddr(b,va) ((dvalue)(((dword)(va)) + ((dword)(b)) * 1i))*/

#define vaddr_of_dvalue(v) (__real__ (v))
#define base_of_dvalue(v) ((word)(__imag__ (v)))

#else

typedef struct { word dv_base[2]; dword dv_vaddr; } dvalue;
#define dvalue_of_dword(x) ((dvalue) { {0, 0}, x })
#define dvalue_of_base_vaddr(b, va) ((dvalue) { EMIT_INIT_TWO_WORDS(0,(b)), va })

#define vaddr_of_dvalue(v) ((v).dv_vaddr)
#define base_of_dvalue(v) ((v).dv_base[OFS_SHORT_LOW])

union fsc_initU_q {
  dvalue cv;
};

#define EMIT_INIT_q(b,o) {{ EMIT_INIT_TWO_WORDS(0,(b)), (b) + (o) }}
#endif

#define value_of_word value_of_int
#define int_of_value(v) vaddr_of_value(v)
#define word_of_value int_of_value
#define dword_of_dvalue(v) vaddr_of_dvalue(v)

#define is_cast(b) ((b) & FSC_TAGVALUE)
#define base_put_castflag(b) ((b) | FSC_TAGVALUE)
#define base_remove_castflag(b) ((b) & ~FSC_TAGVALUE)

extern value value_of_base_ofs(base_t b, ofs_t o);
extern ofs_t ofs_of_base_vaddr(base_t b, vaddr_t v);
extern ofs_t ofs_of_value(value v);
extern ofs_t vaddr_of_base_ofs(base_t b, ofs_t o);
extern value value_of_ptrvalue(ptrvalue p);
extern dvalue dvalue_of_value(value v);
extern value value_of_dvalue(dvalue v);

extern base_t set_base_castflag_Pv(base_t base, ofs_t ofs);
extern ptrvalue ptrvalue_of_value_Pv(value v);

#ifndef FSC_DEBUG_RUNTIME
#define INLINE extern inline
#include <type_repr_inline.c>
#undef INLINE
#endif
#endif
