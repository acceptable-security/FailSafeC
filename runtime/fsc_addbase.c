/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2005-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>

base_t *fsc_allocate_additional_base(base_t base0) {
    base_t base = base_remove_castflag(base0);
    fsc_header *header = get_header_fast(base);
    base_t *new_abase;


    if (header->runtime_flags & RFLAG_INTERNAL_BASEAREA) {
	new_abase = 
	    (base_t *)(base + fsc_round_up_to_wordsize(header->total_ofslimit));
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['a']) {
	    fprintf(stderr, "fsc_allocate_additional_base: block %#08x allocate additional_base @ %#08x internal\n",
		    base, (base_t)new_abase);
    }
#endif
    } else {
	new_abase = 
	    (base_t *)GC_malloc(fsc_round_down_to_wordsize(header->total_ofslimit));
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['a']) {
	    fprintf(stderr, "fsc_allocate_additional_base: block %#08x allocate additional_base @ %#08x external\n",
		    base, (base_t)new_abase);
    }
#endif
    }
    header->ptr_additional_base = new_abase;
    return new_abase;
}
