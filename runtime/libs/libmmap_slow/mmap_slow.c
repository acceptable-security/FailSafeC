/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009. */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_mman.h>

#include <sys/mman.h>
#include <fsc_linkinfo.h>
#include <typeinfo.h>

static struct typeinfo_init fsc_typeinfo_Sn27stdlib_ext_mmap_slow_block_;

static int allocated_amount = 0;
#define ALLOC_RECLAIM_THRESHOLD 1048576

struct struct_Sn27stdlib_ext_mmap_slow_block_ {
    fsc_mapped_block mblk;
};

inline static addr_and_length_t get_addr_length_mmap_slow(base_t base0, ofs_t ofs) {
    base_t base = base_remove_castflag(base0);
    fsc_header *hdr = get_header(base);
    fsc_mapped_block *b = &((struct struct_Sn27stdlib_ext_mmap_slow_block_ *)base)->mblk;

    void *real_addr = (void *)vaddr_of_base_ofs(base, ofs);

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M']) {
	fprintf(stderr, "access request for slow mmap:\n  management block %#x, map address %#x, fsc block at %#x\n",
		(int)b, (int)b->realbase, (int)b->block_base);
	fprintf(stderr, "  request base=%#x, ofs=%#x, real addr %#x (ofs %#x)\n",
		base0, ofs, (int)real_addr, (int)real_addr - (int)b->realbase);
    }
#endif

    if (real_addr >= b->realbase &&
	real_addr < b->reallast) {
	return compose_addr_and_length_t(real_addr, b->reallast - real_addr);
    } else {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['M']) {
	    fprintf(stderr, "  ...access denied\n");
	}
#endif
	return compose_addr_and_length_t(0, 0);
    }
}

inline static void *get_addr_mmap_slow(base_t base0, ofs_t ofs, size_t len) {
    addr_and_length_t al = get_addr_length_mmap_slow(base0, ofs);
    void *p = addr_of_addr_and_length_t(al);
    if (!p)
	fsc_raise_error(base0, ofs, ERR_OUTOFBOUNDS);
    if (len < 0 || len > length_of_addr_and_length_t(al))
	fsc_raise_error(base0, ofs + length_of_addr_and_length_t(al), ERR_OUTOFBOUNDS);
    return p;
}

static byte read_byte_mmap_slow(base_t base0, ofs_t ofs) {
    byte *p = (byte *)get_addr_mmap_slow(base0, ofs, sizeof (byte));
    return *p;
}

static hword read_hword_mmap_slow(base_t base0, ofs_t ofs) {
    hword *p = (hword *)get_addr_mmap_slow(base0, ofs, sizeof (hword));
    return *p;
}

static value read_word_mmap_slow(base_t base0, ofs_t ofs) {
    word *p = (word *)get_addr_mmap_slow(base0, ofs, sizeof (word));
    /* TODO: additional base */
    return value_of_int(*p);
}

static dvalue read_dword_mmap_slow(base_t base0, ofs_t ofs) {
    dword *p = (dword *)get_addr_mmap_slow(base0, ofs, sizeof (dword));
    return dvalue_of_dword(*p);
}

static void write_byte_mmap_slow(base_t base0, ofs_t ofs, byte v, typeinfo_t ty) {
    byte *p = (byte *)get_addr_mmap_slow(base0, ofs, sizeof (byte));
    *p = v;
}

static void write_hword_mmap_slow(base_t base0, ofs_t ofs, hword v, typeinfo_t ty) {
    hword *p = (hword *)get_addr_mmap_slow(base0, ofs, sizeof (hword));
    *p = v;
}

static void write_word_mmap_slow(base_t base0, ofs_t ofs, value v, typeinfo_t ty) {
    word *p = (word *)get_addr_mmap_slow(base0, ofs, sizeof (word));
    *p = vaddr_of_value(v);
}

static void write_dword_mmap_slow(base_t base0, ofs_t ofs, dvalue v, typeinfo_t ty) {
    dword *p = (dword *)get_addr_mmap_slow(base0, ofs, sizeof (dword));
    *p = vaddr_of_dvalue(v);
}

static fsc_mapped_block *create_new_mapping_slow(fsc_mapped_block_handler *handler, 
					  size_t length, int prot, int flags, int fd, off_t offset) {
    if (length >= ALLOC_RECLAIM_THRESHOLD || length + allocated_amount >= ALLOC_RECLAIM_THRESHOLD) {
	GC_gcollect();
	GC_invoke_finalizers();
	allocated_amount = 0;
    } else {
	allocated_amount += length;
    }
    
    void *p = mmap(0, length, prot, flags, fd, offset);
    fsc_header *h;
    if (p == (void *)MAP_FAILED)
	return 0;
    
    struct struct_Sn27stdlib_ext_mmap_slow_block_ *fscblk = 
	(struct struct_Sn27stdlib_ext_mmap_slow_block_ *)
	fsc_alloc_block_library(&fsc_typeinfo_Sn27stdlib_ext_mmap_slow_block_.val, 1);
    h = get_header_fast((base_t)fscblk);
    h->fastaccess_ofslimit = 0;
    h->structured_ofslimit = length;
    h->total_ofslimit = length;
    h->runtime_flags = (h->runtime_flags & RFLAG_TYPE_MASK) | RFLAG_NO_USER_DEALLOC | RFLAG_NO_DEALLOC;
    fscblk->mblk.realbase = p;
    fscblk->mblk.block_base = (base_t)fscblk;
    fscblk->mblk.handler_info = 0;
    fscblk->mblk.active_map = 0;
    return &fscblk->mblk;
}

static void finalizer_for_slow_mmap_block(void *p, void *b0) {
    fsc_mapped_block *b = b0;
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "finalizing block %p.\n", b);
#endif
    fsc_unregister_mapped_block(b);
}

static void all_unmapped_slow(fsc_mapped_block_handler *h, fsc_mapped_block *b) {
    void *gc_block = GC_base((void *)b->block_base);
    GC_finalization_proc ofn;
    void *ocd;
    GC_register_finalizer(gc_block, finalizer_for_slow_mmap_block, (void *)b, &ofn, &ocd);
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_unregister_mapped_block: waiting for finalizing on block %p.\n", b);
#endif
    b->block_base = 0;
    GC_gcollect();
    GC_invoke_finalizers();
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_unregister_mapped_block: garbage collected.\n");
#endif
}

static int is_accessible_mmap_slow(base_t base0, ofs_t ofs, ofs_t length) {
    addr_and_length_t al = get_addr_length_mmap_slow(base0, ofs);
    if (!addr_of_addr_and_length_t(al))
	return 0;
    if (length < 0 || length > length_of_addr_and_length_t(al))
	return 0;
    return 1;
}

static void *get_rawimage_mmap_slow(base_t base0, ofs_t ofs, size_t length) {
    addr_and_length_t al = get_addr_length_mmap_slow(base0, ofs);
    if (!addr_of_addr_and_length_t(al))
	return 0;
    if (length < 0 || length > length_of_addr_and_length_t(al))
	return 0;
    return addr_of_addr_and_length_t(al);
}

static struct typeinfo_init fsc_typeinfo_Sn27stdlib_ext_mmap_slow_block_ = {
    EMIT_HEADER_FOR_TYPEINFO,
    {
	"mmaped block (slow)",
	TI_SPECIAL,
	NULL,
	4,
	sizeof(struct struct_Sn27stdlib_ext_mmap_slow_block_),
	read_dword_mmap_slow,
	read_word_mmap_slow,
	read_hword_mmap_slow,
	read_byte_mmap_slow,
	write_dword_mmap_slow,
	write_word_mmap_slow,
	write_hword_mmap_slow,
	write_byte_mmap_slow,
	get_rawimage_mmap_slow,
	is_accessible_mmap_slow,
	get_addr_length_mmap_slow
    }};

static fsc_mapped_block_handler mmap_slow_handler = {
    "mmap handler (slow)",
    create_new_mapping_slow,
    default_addr_to_fat_pointer,
    all_unmapped_slow,
};

ptrvalue FSP_FPviiiii_Pv_mmap(base_t tptr_b,
			      base_t start_b, ofs_t start_v,
			      base_t length_b, size_t length_v,
			      base_t prot_b, int prot_v,
			      base_t flags_b, int flags_v,
			      base_t fd_b, int fd_v,
			      base_t offset_b, off_t offset_v) {
    fsc_mapped_block *b;

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "calling fsc_create_mmap\n");
#endif

    b = fsc_create_mmap(&mmap_slow_handler, 
			vaddr_of_value(value_of_base_ofs(start_b, start_v)),
			length_v, prot_v, flags_v, fd_v, offset_v);

    if (b == 0)
	return ptrvalue_of_base_ofs(0, (word)MAP_FAILED);
    else {
	ptrvalue pv;
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['M'])
	    fprintf(stderr, "mmap (mmap_slow)_mmap: returned block: management block %#x, map address %#x, fsc block at %#x\n",
		    (int)b, (int)b->realbase, (int)b->block_base);
#endif
	pv = b->handler->addr_to_fat_pointer(b->handler, b, b->realbase);
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['M'])
	    fprintf(stderr, "mmap (mmap_slow) returning: base=%#x, ofs=%#x, flag=%d\n",
		    base_remove_castflag(base_of_ptrvalue(pv)), 
		    ofs_of_ptrvalue(pv),
		    is_cast(base_of_ptrvalue(pv)));
#endif
	return pv;
    }
}

BEGIN_FSC_TYPEINFO
FSC_PROVIDE_VALUE(mmap,"FPviiiii_Pv")
END_FSC_TYPEINFO
