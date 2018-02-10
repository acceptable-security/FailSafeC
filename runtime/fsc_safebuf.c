/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_debug.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>

#include <fsc_safebuf.h>

/* implementation of memory blocks which are safe for "sequential buffer overrun". */

struct safe_buffer_info *fsc_safe_buffer_list = NULL;

void *fsc_allocate_overrun_safe_buffer(base_t b, ofs_t o, size_t len) {
    size_t pagesize = getpagesize();
    size_t alloc_size = (len + pagesize - 1) / pagesize * pagesize;
    void *pageaddr, *addr;

    assert ((pagesize & (pagesize - 1)) == 0);

    pageaddr = mmap((void *)0, alloc_size + pagesize, PROT_READ | PROT_WRITE,
		    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (pageaddr == (void *)-1) {
	if (errno == ENOMEM)
	    fsc_raise_error_library(0, 0, ERR_OUTOFMEMORY, "fsc_allocate_overrun_safe_buffer");
	else
	    fsc_raise_error_library(0, 0, ERR_UNKNOWN, "fsc_allocate_overrun_safe_buffer");
    }
    
    if (mprotect((void *)((int)pageaddr + alloc_size), pagesize, PROT_NONE))
	fsc_raise_error_library(0, 0, ERR_UNKNOWN, "fsc_allocate_overrun_safe_buffer");

    addr = (void *)((int)pageaddr + alloc_size - len);

    {
    struct safe_buffer_info *p = malloc(sizeof(struct safe_buffer_info));

    p->busy = 1;
    p->pageaddr = pageaddr;
    p->addr = addr;
    p->alloc_size = alloc_size;
    p->len = len;
    p->b = b;
    p->o = o;

    p->next = fsc_safe_buffer_list;
    fsc_safe_buffer_list = p;
    }
    return addr;
}

void fsc_free_overrun_safe_buffer(void *addr) {
    struct safe_buffer_info *p, *p_prev;

    for(p = p_prev = fsc_safe_buffer_list; p; p_prev = p, p = p->next) {
	if (p->addr == addr)
	    break;
    }

    if (p == NULL) {
	fsc_raise_error_library(0, (ofs_t)addr, ERR_UNKNOWN, "fsc_free_overrun_safe_buffer");
    }

    if (p == p_prev)
	fsc_safe_buffer_list = p->next;
    else
	p_prev->next = p->next;

    munmap(p->pageaddr, p->alloc_size + getpagesize());
}
