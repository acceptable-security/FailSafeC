/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2003-2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
#ifndef FILEPTR_H
#define FILEPTR_H
#include <fsc_runtime.h>
#ifdef FSC_RUNTIME_LIBRARY
struct fsc_storage_PSn10stdio_FILE__s {
    struct fsc_header header;
    union fsc_initUptr val;
};
extern struct typeinfo_init fsc_typeinfo_Sn10stdio_FILE_;
extern struct fsc_storage_PSn10stdio_FILE__s GV_stdin, GV_stderr, GV_stdout;
#include <stdio.h>
FILE **get_FILE_pointer_addr(base_t b0, ofs_t o);
FILE *get_FILE_pointer(base_t b0, ofs_t o);
#endif
#endif
