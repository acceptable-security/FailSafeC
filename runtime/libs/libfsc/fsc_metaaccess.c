/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <stdint.h>
#include <fsc_runtime.h>
#include <fsc_alloc.h>
#include <fsc_debug.h>
#include <primitive_ops.h>
#include <wrapper_helper.h>
#include <copydata.h>
#include <memory.h>
#include <fsc_linkinfo.h>

value FS_FPv_i_fsc_ofs_of_pointer(base_t b, ofs_t o) {
    return value_of_int((int)o);
}

ptrvalue FS_FPv_Pv_fsc_base_of_pointer(base_t b, ofs_t o) {
    return ptrvalue_of_base_ofs(b, 0);
}

ptrvalue FS_FPv_Pv_fsc_type_of_block(base_t b0, ofs_t o) {
    base_t base;
    fsc_header *header;

    fsc_ensure_nonnull(b0, o, "fsc_typeof_block");
    base = base_remove_castflag(b0);
    header = get_header_fast(base);

    return ptrvalue_of_base_ofs(base_put_castflag((base_t)(header->tinfo)), 0);
}

value FS_FPv_i_fsc_size_of_block(base_t b0, ofs_t o) {
    base_t base;
    fsc_header *header;

    fsc_ensure_nonnull(b0, o, "fsc_typeof_block");
    base = base_remove_castflag(b0);
    header = get_header_fast(base);

    return value_of_int(header->total_ofslimit);
}

value FS_FPv_i_fsc_flags_of_block(base_t b0, ofs_t o) {
    base_t base;
    fsc_header *header;

    fsc_ensure_nonnull(b0, o, "fsc_typeof_block");
    base = base_remove_castflag(b0);
    header = get_header_fast(base);

    return value_of_int(header->runtime_flags);
}

ptrvalue FS_FPv_Pc_fsc_name_of_type(base_t b0, ofs_t o) {
    base_t base;
    fsc_header *header;

    fsc_ensure_nonnull(b0, o, "fsc_typeof_block");
    base = base_remove_castflag(b0);
    header = get_header_fast(base);
    
    if (header->tinfo != &fsc_typeinfo_Xti_.val || o != 0) {
	fsc_raise_error_library(b0, o, ERR_INVALIDARGS, "name_of_type");
    } else {
	typeinfo_t typ = (typeinfo_t)base;
	return wrapper_make_new_static_string(typ->ti_name);
    }
}

ptrvalue FS_FPv_i_fsc_flags_of_type(base_t b0, ofs_t o) {
    base_t base;
    fsc_header *header;

    fsc_ensure_nonnull(b0, o, "fsc_typeof_block");
    base = base_remove_castflag(b0);
    header = get_header_fast(base);
    
    if (header->tinfo != &fsc_typeinfo_Xti_.val || o != 0) {
	fsc_raise_error_library(b0, o, ERR_INVALIDARGS, "name_of_type");
    } else {
	typeinfo_t typ = (typeinfo_t)base;
	return value_of_int(typ->kind);
    }
}

void FS_F_v_GC_gcollect(void) {
    GC_gcollect();
    GC_invoke_finalizers();
}

BEGIN_FSC_TYPEINFO
FSC_PROVIDE_VALUE(fsc_ofs_of_pointer,"FPv_i")
FSC_PROVIDE_VALUE(fsc_base_of_pointer,"FPv_Pv")
FSC_PROVIDE_VALUE(fsc_type_of_block,"FPv_Pv")
FSC_PROVIDE_VALUE(fsc_size_of_block,"FPv_i")
FSC_PROVIDE_VALUE(fsc_flags_of_block,"FPv_i")
FSC_PROVIDE_VALUE(fsc_name_of_type,"FPv_Pc")
FSC_PROVIDE_VALUE(fsc_flags_of_type,"FPv_i")
FSC_PROVIDE_VALUE(GC_gcollect,"F_v")
END_FSC_TYPEINFO
