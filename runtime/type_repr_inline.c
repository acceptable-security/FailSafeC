/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2005.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */
#define VOIDPTR_USE_ORIG_BASES

#if defined(FATVALUE_MODE_DWORD) && !defined(FSC_NO_INLINEASM) && defined(__i386__)
INLINE value value_of_base_vaddr(base_t b, word va) {
    value p;
    __asm("": "=A" (p): "a" (va), "d" (b));
    return p;
}
#endif
INLINE value value_of_base_ofs(base_t b0, ofs_t o) {
    base_t b = base_remove_castflag(b0);
#ifdef VOIDPTR_USE_ORIG_BASES
    return value_of_base_vaddr(b0, b + o);
#else
    return value_of_base_vaddr(b, b + o);
#endif
}

INLINE ofs_t ofs_of_base_vaddr(base_t b, vaddr_t v) {
  return v - base_remove_castflag(b);
}

INLINE ofs_t ofs_of_value(value v) {
  return ofs_of_base_vaddr(base_remove_castflag(base_of_value(v)), vaddr_of_value(v));
}

INLINE ofs_t vaddr_of_base_ofs(base_t b, ofs_t o) {
  return base_remove_castflag(b) + o;
}

INLINE value value_of_ptrvalue(ptrvalue p) {
#ifdef VOIDPTR_USE_ORIG_BASES
    register base_t b = base_of_ptrvalue(p);
#else
    register base_t b = base_remove_castflag(base_of_ptrvalue(p));
#endif
    register ofs_t o = ofs_of_ptrvalue(p);
    register ofs_t v = vaddr_of_base_ofs(b, o);
    return value_of_base_vaddr(b, v);
}

INLINE dvalue dvalue_of_value(value v) {
    return dvalue_of_base_vaddr(base_of_value(v), vaddr_of_value(v));
}

INLINE value value_of_dvalue(dvalue v) {
    return value_of_base_vaddr(base_of_dvalue(v), (vaddr_t)vaddr_of_dvalue(v));
}

INLINE base_t set_base_castflag_Pv(base_t b, ofs_t o) {
#ifdef VOIDPTR_USE_ORIG_BASES
    return b;
#else
    return base_put_castflag(b);
#endif
}

INLINE ptrvalue ptrvalue_of_value_Pv(value v) {
    return ptrvalue_of_base_ofs(base_of_value(v), ofs_of_value(v));
}

