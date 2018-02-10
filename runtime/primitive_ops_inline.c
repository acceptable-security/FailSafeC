/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

/* inline functions for primitive_ops.h */

#if !defined(FSC_DEBUG_RUNTIME)

   /* OPT, FSCC-generated || OPT, library */
   /* better to be macros, to get better location reporting */
#  define read_dword(base, ofs) (get_header(base)->tinfo->ti_read_dword((base), (ofs)))
#  define read_word(base, ofs) (get_header(base)->tinfo->ti_read_word((base), (ofs)))
#  define read_hword(base, ofs) (get_header(base)->tinfo->ti_read_hword((base), (ofs)))
#  define read_byte(base, ofs) (get_header(base)->tinfo->ti_read_byte((base), (ofs)))
#  define write_byte(base, ofs, v, ti) (get_header(base)->tinfo->ti_write_byte((base), (ofs), (v), (ti)))
#  define write_hword(base, ofs, v, ti) (get_header(base)->tinfo->ti_write_hword((base), (ofs), (v), (ti)))
#  define write_word(base, ofs, v, ti) (get_header(base)->tinfo->ti_write_word((base), (ofs), (v), (ti)))
#  define write_dword(base, ofs, v, ti) (get_header(base)->tinfo->ti_write_dword((base), (ofs), (v), (ti)))

#else
#  if !defined(FSC_RUNTIME_LIBRARY)
   /* DEBUG, FSCC-generated */
     extern void fsc_debugmsg_access_methods(base_t base, ofs_t ofs, int size);
     void fsc_debugmsg_access_methods_r1(base_t base, ofs_t ofs, int size0);
     void fsc_debugmsg_access_methods_r2(base_t base, ofs_t ofs, int size0, dvalue val);
     void fsc_debugmsg_access_methods_w(base_t base, ofs_t ofs, int size0, dvalue val);
   
#    define fsc_debug_access_methods_w(b,o,s,v) if (fsc_debug_flag['m']) fsc_debugmsg_access_methods_w((b),(o),(s),(v))
#    define fsc_debug_access_methods_r(b,o,s,t,c,e)			\
       if (fsc_debug_flag['m']) { 					\
         t val;								\
         fsc_debugmsg_access_methods_r1((b),(o),(s));			\
         val = e;							\
         fsc_debugmsg_access_methods_r2((b),(o),(s),c(val));		\
         return val;							\
       } else								\
         return e
#  else
     /* DEBUG, library */
#    define fsc_debug_access_methods_w(b,o,s,v)
#    define fsc_debug_access_methods_r(b,o,s,t,c,e) return e
#  endif

   INLINE dvalue read_dword(base_t base, ofs_t ofs) {
     fsc_debug_access_methods_r(base, ofs, 8, dvalue, (dvalue), get_header(base)->tinfo->ti_read_dword(base, ofs));
   }
   
   INLINE value read_word(base_t base, ofs_t ofs) {
     fsc_debug_access_methods_r(base, ofs, 4, value, dvalue_of_value, get_header(base)->tinfo->ti_read_word(base, ofs));
   }
   
   INLINE hword read_hword(base_t base, ofs_t ofs) {
     fsc_debug_access_methods_r(base, ofs, 2, hword, dvalue_of_dword, get_header(base)->tinfo->ti_read_hword(base, ofs));
   }
   
   INLINE byte read_byte(base_t base, ofs_t ofs) {
     fsc_debug_access_methods_r(base, ofs, 1, byte, dvalue_of_dword, get_header(base)->tinfo->ti_read_byte(base, ofs));
   }
   
   INLINE void write_byte(base_t base, ofs_t ofs, byte v, typeinfo_t ti) {
     fsc_debug_access_methods_w(base, ofs, -1, dvalue_of_dword(v));
     get_header(base)->tinfo->ti_write_byte(base, ofs, v, ti);
   }
   
   INLINE void write_hword(base_t base, ofs_t ofs, hword v, typeinfo_t ti) {
     fsc_debug_access_methods_w(base, ofs, -2, dvalue_of_dword(v));
     get_header(base)->tinfo->ti_write_hword(base, ofs, v, ti);
   }
   
   INLINE void write_word(base_t base, ofs_t ofs, value v, typeinfo_t ti) {
     fsc_debug_access_methods_w(base, ofs, -4, dvalue_of_value(v));
     get_header(base)->tinfo->ti_write_word(base, ofs, v, ti);
   }
   
   INLINE void write_dword(base_t base, ofs_t ofs, dvalue v, typeinfo_t ti) {
     fsc_debug_access_methods_w(base, ofs, -8, v);
     get_header(base)->tinfo->ti_write_dword(base, ofs, v, ti);
   }
#endif

/* specific functions */
INLINE byte * get_realoffset_c(base_t base, ofs_t ofs) {
  return (byte *)(base + ofs);
}

INLINE hword * get_realoffset_s(base_t base, ofs_t ofs) {
  return (hword *)(base + ofs);
}

INLINE float * get_realoffset_f(base_t base, ofs_t ofs) {
  return (float *)(base + ofs);
}

INLINE double * get_realoffset_d(base_t base, ofs_t ofs) {
    return (double *)(base + ofs);
}

INLINE value * get_realoffset_i(base_t base, ofs_t ofs) {
  return (value *)(base + ofs * 2);
}

INLINE dvalue * get_realoffset_q(base_t base, ofs_t ofs) {
  return (dvalue *)(base + ofs * 2);
}

INLINE ptrvalue * get_realoffset_P(base_t base, ofs_t ofs) {
  return (value *)(base + ofs * 2);
}

/* value will be written to the target. */
INLINE int is_well_typed_pointer(value v, typeinfo_t target) {
  base_t b = base_remove_castflag(base_of_value(v));
  if (b == 0) {
    return 0;
  } else {
    ofs_t o = ofs_of_value(v);
    typeinfo_t valuetarget = get_header(b)->tinfo;
    if ((target->kind & TI_KIND_MASK) == TI_POINTER && target->referee == valuetarget
	&& o % valuetarget->virtual_size == 0) {
      return 1;
    } else {
      return 0;
    }
  }
}

/* helpers */
INLINE value read_merge_additional_base_word(word v, base_t base, ofs_t ofs) {
    base_t *abase = get_header(base)->ptr_additional_base;
    if (abase == 0) {
	return value_of_int(v);
    } else {
	assert (ofs % 4 == 0);
	return value_of_base_vaddr(*(base_t *)(ofs + (char *)abase), v);
    }
}

INLINE dvalue read_merge_additional_base_dword(dword v, base_t base, ofs_t ofs) {
    base_t *abase = get_header(base)->ptr_additional_base;
    if (abase == 0) {
	return dvalue_of_dword(v);
    } else {
	assert (ofs % 4 == 0);
	return dvalue_of_base_vaddr(*(base_t *)(ofs + OFS_SHORT_LOW * 4 + (char *)abase), v);
    }
}

extern base_t *fsc_allocate_additional_base(base_t);

INLINE void write_additional_base_word(base_t base, ofs_t ofs, base_t write_b) {
    base_t *abase = get_header(base)->ptr_additional_base;
    if (abase == 0) {
	if (write_b == 0)
	    return;
	else
	    abase = fsc_allocate_additional_base(base);
    }
    assert (ofs % 4 == 0);
    *(base_t *)(ofs + (char *)abase) = write_b;
}

INLINE void write_additional_base_dword(base_t base, ofs_t ofs, base_t write_b) {
    base_t *abase = get_header(base)->ptr_additional_base;
    if (abase == 0) {
	if (write_b == 0)
	    return;
	else
	    abase = fsc_allocate_additional_base(base);
    }
    assert (ofs % 4 == 0);
    *(base_t *)(ofs + OFS_SHORT_LOW * 4 + (char *)abase) = write_b;
    *(base_t *)(ofs + OFS_SHORT_HIGH * 4 + (char *)abase) = 0;
}

INLINE word word_of_float(float f) {
    return *(word *)&f;
}

INLINE float float_of_word(word w) {
    return *(float *)&w;
}

INLINE dword dword_of_double(double d) {
    return *(dword *)&d;
}

INLINE double double_of_dword(dword d) {
    return *(double *)&d;
}

INLINE value value_of_float(float f) { return value_of_int(word_of_float(f)); }
INLINE float float_of_value(value v) { return float_of_word(vaddr_of_value(v)); }
INLINE dvalue dvalue_of_double(double d)  { return dvalue_of_dword(dword_of_double(d)); }
INLINE double double_of_dvalue(dvalue v) { return double_of_dword(vaddr_of_dvalue(v)); }

#ifndef FATDVALUE_MODE_COMPLEX
INLINE value partial_value_of_dvalue(dvalue d, int ofs) {
  word va;
  base_t b;
  assert (ofs == 0 || ofs == sizeof(word));
  if (ofs == OFS_SHORT_HIGH * 4)
    va = (word)(vaddr_of_dvalue(d) >> (8 * sizeof(word)));
  else
    va = (word)(vaddr_of_dvalue(d));
  b = d.dv_base[ofs / sizeof(word)];
  return value_of_base_vaddr(b, va);
}

INLINE void write_partial_value_to_dvalue(dvalue *d, value v, int ofs) {
  assert (ofs == 0 || ofs == sizeof(word));
  d->dv_base[ofs / sizeof(word)] = base_of_value(v);
  ((word *)&(d->dv_vaddr))[ofs / sizeof(word)] = vaddr_of_value(v);
}
#endif

/*** varargs blocks ****/

/* varargs blocks are actually block of ints. */

INLINE void fsc_put_varargs(base_t base, size_t n, value v) {
    /* called by compiler-generated code.
       no need to perform boundary check. */
    /* assert(get_header_fast(base)->tinfo == &fsc_typeinfo_i.val); */
    ((value *)base)[n] = v;
}

INLINE void fsc_put_varargs_2(base_t base, size_t n, dvalue v) {
    /* called by compiler-generated code.
       no need to perform boundary check. */
    /* assert(get_header_fast(base)->tinfo == &fsc_typeinfo_i.val); */
    /*printf("b:%d v:%lld\n", base_of_dvalue(v), vaddr_of_dvalue(v));*/
    write_dword_to_word(base, n * sizeof(word), v, NULL);
}
