/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

extern struct safe_buffer_info {
    struct safe_buffer_info *next;
    int busy;
    void *pageaddr, *addr;
    int alloc_size, len;
    base_t b;
    ofs_t o;
} *fsc_safe_buffer_list;

extern void *fsc_allocate_overrun_safe_buffer(base_t b, ofs_t o, size_t len);
extern void fsc_free_overrun_safe_buffer(void *addr);

