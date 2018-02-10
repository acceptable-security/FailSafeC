/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifdef FSC_DEBUG_RUNTIME
#include <fsc_debug.h>
#include <stdio.h>
#include <stdlib.h>
#include <fsc_runtime.h>

char fsc_debug_flag[256] = {0};
void initialize_fsc_debug_flag(void) __attribute__ ((constructor));

void initialize_fsc_debug_flag(void) {
    unsigned char *env = getenv("FSC_DEBUG_RUNTIME");
    if (env) {
	fprintf(stderr, "enabling FSC debugging\n");
	while (*env) {
	    fsc_debug_flag[*env++] = 1;
	}
    }
}
#endif
/* defined debug flags:

   a  typed block allocation
*/

/*** debug message **/
#ifdef FSC_DEBUG_RUNTIME
void fsc_debugmsg_access_methods_r1(base_t base, ofs_t ofs, int size0) {
    register int size = size0 < 0 ? -size0 : size0;
    const char *dir = size0 < 0 ? "write" : "read";
    fprintf(stderr, "%s acc.meth.%d: %#08x+%d", dir, size, base, ofs);
    if (base_remove_castflag(base) != 0) {
	fsc_header *h = get_header(base);
	fprintf(stderr,
		" [%s (VS %d, RS %d), sz %d (FA %d, ST %d), f %#x]\r",
		h->tinfo->ti_name, h->tinfo->virtual_size, h->tinfo->real_size,
		h->total_ofslimit, h->fastaccess_ofslimit, h->structured_ofslimit,
		h->runtime_flags);
    } else {
	fputc('\n', stderr);
    }
}

void fsc_debugmsg_access_methods_r2(base_t base, ofs_t ofs, int size0, dvalue val) {
    register int size = size0 < 0 ? -size0 : size0;
    const char *dir = size0 < 0 ? "write" : "read";
    fprintf(stderr, "%s acc.meth.%d: %#08x+%d", dir, size, base, ofs);
    if (base_remove_castflag(base) != 0) {
	fsc_header *h = get_header(base);
	fprintf(stderr,
		" => %#x+%lld [%s (VS %d, RS %d), sz %d (FA %d, ST %d), f %#x]\n",
		base_of_dvalue(val), vaddr_of_dvalue(val) - base_remove_castflag(base_of_dvalue(val)),
		h->tinfo->ti_name, h->tinfo->virtual_size, h->tinfo->real_size,
		h->total_ofslimit, h->fastaccess_ofslimit, h->structured_ofslimit,
		h->runtime_flags);
    } else {
	fputc('\n', stderr);
    }
}
void fsc_debugmsg_access_methods_w(base_t base, ofs_t ofs, int size0, dvalue val) {
    register int size = size0 < 0 ? -size0 : size0;
    const char *dir = size0 < 0 ? "write" : "read";
    fprintf(stderr, "%s acc.meth.%d: %#08x+%d", dir, size, base, ofs);
    if (base_remove_castflag(base) != 0) {
	fsc_header *h = get_header(base);
	fprintf(stderr,
		" <= %#x+%lld [%s (VS %d, RS %d), sz %d (FA %d, ST %d), f %#x]\n",
		base_of_dvalue(val), vaddr_of_dvalue(val) - base_remove_castflag(base_of_dvalue(val)),
		h->tinfo->ti_name, h->tinfo->virtual_size, h->tinfo->real_size,
		h->total_ofslimit, h->fastaccess_ofslimit, h->structured_ofslimit,
		h->runtime_flags);
    } else {
	fputc('\n', stderr);
    }
}
#endif
