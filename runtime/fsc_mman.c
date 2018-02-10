/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2009 AIST.

   This file is written by Yutaka Oiwa in 2009.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <fsc_mman.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>

#ifdef FSC_DEBUG_RUNTIME
#include <stdio.h>
#endif

struct fsc_mapped_block fsc_current_mapped_blocks = {
    /* dummy block */
    &fsc_current_mapped_blocks, &fsc_current_mapped_blocks, 0, 0, 0, 0, 0
};

#define FOR_ALL_BLOCKS(b) for(b = fsc_current_mapped_blocks.next; b != &fsc_current_mapped_blocks; b = b->next)

fsc_mapped_block *realaddr_to_mapped_block(void *p) {
    fsc_mapped_block *b;
    FOR_ALL_BLOCKS(b) {
	if ((word)p >= (word)b->realbase && (word)p < (word)b->reallast) {
	    return b;
	}
    }
    return 0;
}

fsc_mapped_block *fscblock_to_mapped_block(base_t base0) {
    base_t base = base_remove_castflag(base0);
    fsc_mapped_block *b;
    FOR_ALL_BLOCKS(b) {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['M'])
	    fprintf(stderr, "fscblock_to_mapped_block: looking on mapped block %#lx, fscaddr=%#lx\n", (long)b, (long)b->block_base);
#endif
	if (b->block_base == base)
	    return b;
    }
    return 0;
}

ptrvalue realaddr_to_mapped_fat_pointer(void *p) {
    struct fsc_mapped_block *b = realaddr_to_mapped_block(p);

    if (b)
	return b->handler->addr_to_fat_pointer(b->handler, b, p);
    else
	return ptrvalue_of_base_ofs(0, 0);
}

void fsc_register_mapped_block(fsc_mapped_block *b) {
    /* NEED_LOCK */
    b->next = fsc_current_mapped_blocks.next;
    b->prev = &fsc_current_mapped_blocks;
    b->next->prev = b;
    b->prev->next = b;
}

void fsc_unregister_mapped_block(fsc_mapped_block *b) {
    /* called when object has no pointers pointing to it. */
    /* NEED_LOCK */
    int page_size = getpagesize();
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_unregister_mapped_block: unregistering block %p.\n", b);
#endif
    munmap(b->realbase, (word)b->reallast - (word)b->realbase);
    if (b->active_map_allocated_p)
	GC_free(b->active_map);
    b->prev->next = b->next;
    b->next->prev = b->prev;
}

ptrvalue default_addr_to_fat_pointer(struct fsc_mapped_block_handler *self, struct fsc_mapped_block *blk, void *p) {
    return ptrvalue_of_base_ofs_flag(blk->block_base, (word)p - (word)blk->block_base, 1);
}

void fsc_unmap_mmapped_page(fsc_mapped_block *b, void *start, size_t length) {
    /* support partial unmapping */
    /* NEED_LOCK */
    int page_size = getpagesize(), start_page, npages;
    int i;
    if (!b->active)
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "munmap called on inactive region");
    if (b->kind != FSC_MMAP_MMAP) {
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "munmap called on shm region");
    }
    if ((word)start % page_size)
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "munmap called on unaligned start address");
    start_page = ((word)start - (word)b->realbase) / page_size;
    if (start_page < 0 || start_page >= b->npages)
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "munmap called on invalid start address");
    npages = (length + page_size - 1) / page_size;
    if (npages < 0 || start_page + npages - 1 >= b->npages || start_page + npages < start_page) {
#ifdef FSC_DEBUG_RUNTIME
	fprintf(stderr, "npages %d  start_page + npages %d   b->npages %d  start_page %d\n", npages, start_page + npages, b->npages, start_page);
#endif
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "munmap called on invalid length");
    }
    munmap(start, npages * page_size);
    if (start != mmap(start, npages * page_size, PROT_NONE, MAP_FIXED | MAP_NORESERVE | MAP_ANONYMOUS | MAP_PRIVATE, -1, 0))
	fsc_raise_error_library(b->block_base, 0, ERR_INTERNAL, "mmapping dummy page on unmapped pages failed");
    for(i = 0; i < npages; i++) {
	b->active_map[start_page + i] = 0;
    }
    for(i = 0; i < b->npages; i++) {
	if (b->active_map[i])
	    return;
    }
#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_unmap_mmapped_page: block %p is no longer active.\n", b);
#endif
    b->active = 0;
    b->handler->all_unmapped(b->handler, b);
}

int fsc_change_page_protections(fsc_mapped_block *b, void *start, size_t length, int prot) {
    /* NEED_LOCK */
    int page_size = getpagesize(), start_page, npages;
    int i;
    if (!b->active)
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "mprotect called on inactive region");
    if (b->kind != FSC_MMAP_MMAP) {
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "mprotect called on shm region");
    }
    if ((word)start % page_size)
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "mprotect called on unaligned start address");
    start_page = ((word)start - (word)b->realbase) / page_size;
    if (start_page < 0 || start_page >= b->npages)
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "mprotect called on invalid start address");
    npages = (length + page_size - 1) / page_size;
    if (npages < 0 || start_page + npages - 1 >= b->npages || start_page + npages < start_page) {
#ifdef FSC_DEBUG_RUNTIME
	fprintf(stderr, "npages %d  start_page + npages %d   b->npages %d  start_page %d\n", npages, start_page + npages, b->npages, start_page);
#endif
	fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "mprotect called on invalid length");
    }
    for(i = 0; i < npages; i++) {
	if (! b->active_map[start_page + i]) {
	    errno = ENOMEM;
	    return -1;
	}
    }
    return mprotect(start, npages * page_size, prot);
}

fsc_mapped_block *fsc_create_mmap(fsc_mapped_block_handler *handler, size_t start, size_t length, int prot, int flags, int fd, off_t offset) {
    fsc_mapped_block *b;
    size_t pagesize = sysconf(_SC_PAGESIZE);
    int real_flags = 0;
    int npages = (length + pagesize - 1) / pagesize;
    int i;

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_create_mmap: handler=%s, start=%#x, length=%d, prot=%#x, flags=%#x, fd=%d, ofs=%d\n",
		handler->handler_type_name, start, length, prot, flags, fd, (int)offset);
#endif
    if (npages < 0) {
	errno = EINVAL;
	return 0;
    }

    switch (flags & (MAP_SHARED | MAP_PRIVATE)) {
    case MAP_SHARED:
	real_flags |= MAP_SHARED;
	break;
    case MAP_PRIVATE:
	real_flags |= MAP_PRIVATE;
	break;
    default:
	errno = EINVAL;
	return 0;
    }

    if ((flags & MAP_ANONYMOUS) == MAP_ANONYMOUS) {
	real_flags |= MAP_ANONYMOUS;
	fd = -1;
    } else {
	if (fd < 0)
	    fsc_raise_error_library(0, length, ERR_INVALIDARGS, "fsc_map_file: wrong fd given");
    }

    if ((flags & MAP_NORESERVE) == MAP_NORESERVE)
	real_flags |= MAP_NORESERVE;

    if ((flags & MAP_EXECUTABLE) == MAP_EXECUTABLE) {
	errno = EINVAL;
	return 0;
    }
    
    if (offset % pagesize != 0)
	fsc_raise_error_library(0, offset, ERR_INVALIDARGS, "fsc_map_file: invalid offset given");

    if ((flags & MAP_FIXED) == MAP_FIXED) {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['M'])
	    fprintf(stderr, "fsc_create_mmap: requested fixed mapping\n");
#endif
	/* requested fixed mapping */
	if (start % pagesize)
	    fsc_raise_error_library(0, start, ERR_INVALIDARGS, "mmap called on unaligned address");
	if (length % pagesize)
	    fsc_raise_error_library(0, start, ERR_INVALIDARGS, "mmap called on unaligned address");
	b = realaddr_to_mapped_block((void *)start);

	if (b) {
#ifdef FSC_DEBUG_RUNTIME
	    if (fsc_debug_flag['M'])
		fprintf(stderr, "fsc_create_mmap: found matching mapping at %p, length %#x\n", b->realbase, b->npages * pagesize);
#endif
	    if (!b->active)
		fsc_raise_error_library(b->block_base, 0, ERR_INVALIDARGS, "mmap called on inactive region");
	    if (b->kind != FSC_MMAP_MMAP)
		fsc_raise_error_library(0, start, ERR_INVALIDARGS, "mmap called on unsupported memory block");
	    if (b->active &&
		start >= (word)b->realbase &&
		start + length > start &&
		(word)b->realbase + start + length < (word)b->reallast) {
		if (start == (size_t)mmap((void *)start, length, prot, real_flags | MAP_FIXED, fd, offset)) {
		    int start_page = (start - (word)b->realbase) / pagesize;
		    for (i = 0; i < npages; i++) {
			b->active_map[i] = 1;
		    }
		    return b;
		}
	    }
	    if (!errno)
		errno = EINVAL;
	    return 0;
	}
    }

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_create_mmap: calling mmap handler\n");
#endif
    b = handler->create_new_mapping(handler, npages * pagesize, prot, flags, fd, offset);

    if (!b) {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['M'])
	    fprintf(stderr, "fsc_create_mmap: calling mmap handler returned with errno %d\n", errno);
#endif
	if (!errno)
	    errno = EINVAL;
	return 0;
    }

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_create_mmap: returned block: management block %#x, map address %#x, fsc block at %#x\n",
		(int)b, (int)b->realbase, (int)b->block_base);
#endif
    /* handler should have initialized b->realbase, b->block_base, and b->handler_info.
       b->active_map should be either zero or initialized to have npages bytes. */
    b->kind = FSC_MMAP_MMAP;
    get_header(b->block_base)->runtime_flags |= RFLAG_MAPPED_BLOCK;
    b->reallast = (void *)((word)b->realbase + npages * pagesize);
    b->npages = npages;
    b->handler = handler;
    if (!b->active_map) {
	b->active_map = GC_malloc_atomic(npages);
	b->active_map_allocated_p = 1;
    } else {
	b->active_map_allocated_p = 0;
    }
    for(i = 0; i < npages; i++)
	b->active_map[i] = 1;

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_create_mmap: registering management block %#x\n", (int)b);
#endif

    fsc_register_mapped_block(b);
    b->active = 1;

#ifdef FSC_DEBUG_RUNTIME
    if (fsc_debug_flag['M'])
	fprintf(stderr, "fsc_create_mmap: registering management block %#x done\n", (int)b);
#endif
    return b;
}

