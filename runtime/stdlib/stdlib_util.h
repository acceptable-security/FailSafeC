/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
#ifndef STDLIB_UTIL_H
#define STDLIB_UTIL_H

char **copy_argv_to_raw(base_t argv_b, ofs_t argv_o, void **pointer_to_discard, int argc, const char *libcloc);
char **copy_argv0_argv_to_raw(base_t argv0_b, ofs_t argv0_o,
                              base_t argv_b, ofs_t argv_o,
                              void **pointer_to_discard, int argc, const char *libcloc);
ptrvalue copy_argv_from_raw(const char * const *argv, int *argc);
void release_argv_tempbuf(void *to_discard);
void writeback_argv_tempbuf(base_t argv_b, ofs_t argv_o, void *to_discard);

#endif
