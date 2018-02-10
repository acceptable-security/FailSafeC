/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2003-2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <stdio.h>
#include <fileptr.h>

struct typeinfo_init fsc_typeinfo_Sn10stdio_FILE_ = {
    EMIT_HEADER_FOR_TYPEINFO,
    {
	"stdio_FILE",
	TI_SPECIAL,
	NULL,
	4,
	sizeof (FILE *),
	EMIT_TYPEINFO_ACCESS_METHODS_TABLE_NOACCESS
    }};

struct stdio_FILE_init {
    struct fsc_header header;
    FILE *p;
};

static int std_descs_inited = 0;

static struct stdio_FILE_init std_descs[3] = {
    { EMIT_FSC_HEADER(fsc_typeinfo_Sn10stdio_FILE_.val, 0), 0 },
    { EMIT_FSC_HEADER(fsc_typeinfo_Sn10stdio_FILE_.val, 0), 0 },
    { EMIT_FSC_HEADER(fsc_typeinfo_Sn10stdio_FILE_.val, 0), 0 },
};

extern struct typeinfo_init fsc_typeinfo_PSn10stdio_FILE_;

struct fsc_storage_PSn10stdio_FILE__s GV_PSn10stdio_FILE__stdin = {
    EMIT_FSC_HEADER(fsc_typeinfo_PSn10stdio_FILE_.val, 4),
    EMIT_INITPTR((base_t)&std_descs[0].p, 0, 0)
};
		    
struct fsc_storage_PSn10stdio_FILE__s GV_PSn10stdio_FILE__stdout = {
    EMIT_FSC_HEADER(fsc_typeinfo_PSn10stdio_FILE_.val, 4),
    EMIT_INITPTR((base_t)&std_descs[1].p, 0, 0)
};
		    
struct fsc_storage_PSn10stdio_FILE__s GV_PSn10stdio_FILE__stderr = {
    EMIT_FSC_HEADER(fsc_typeinfo_PSn10stdio_FILE_.val, 4),
    EMIT_INITPTR((base_t)&std_descs[2].p, 0, 0)
};

static void initialize_stddesc() {
    if (!std_descs_inited) {
	std_descs_inited = 1;
	std_descs[0].p = stdin;
	std_descs[1].p = stdout;
	std_descs[2].p = stderr;
    }
}

FILE **get_FILE_pointer_addr(base_t b0, ofs_t o) {
    base_t b;
    fsc_header *h;
    FILE *p;

    initialize_stddesc();
    b = base_remove_castflag(b0);
    if (b == 0)
	fsc_raise_error_library(b0, o, ERR_NULLPTR, "get_FILE_pointer");
    h = get_header_fast(b);
    if (h->tinfo != &fsc_typeinfo_Sn10stdio_FILE_.val)
	fsc_raise_error_library(b0, o, ERR_TYPEMISMATCH, "get_FILE_pointer");
    if (o != 0)
	fsc_raise_error_library(b0, o, ERR_OUTOFBOUNDS, "get_FILE_pointer");
    return (FILE **)b;
}

FILE *get_FILE_pointer(base_t b0, ofs_t o) {
    FILE *p = *get_FILE_pointer_addr(b0, o);
    if (!p)
	fsc_raise_error_library(b0, o, ERR_OUTOFBOUNDS, "get_FILE_pointer: file already closed");
    return p;
}

