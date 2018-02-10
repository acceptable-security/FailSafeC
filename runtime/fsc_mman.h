/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2009.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#include <stdlib.h>
#include <limits.h>
#include <type_repr.h>

enum fsc_mapped_block_kinds { FSC_MMAP_MMAP, FSC_MMAP_SHM };

typedef struct fsc_mapped_block {
    struct fsc_mapped_block *prev, *next;
    enum fsc_mapped_block_kinds kind;
    int active;
    void *realbase, *reallast; /* last not inclusive */
    size_t npages; /* in pages */
    char *active_map;
    int active_map_allocated_p;
    base_t block_base;
    struct fsc_mapped_block_handler *handler;
    void *handler_info;
} fsc_mapped_block;

typedef struct fsc_mapped_block_handler {
    char *handler_type_name;
    fsc_mapped_block *(*create_new_mapping)(struct fsc_mapped_block_handler *handler, 
					    size_t length, int prot, int flags, int fd, off_t offset);
    ptrvalue (*addr_to_fat_pointer)(struct fsc_mapped_block_handler *self, struct fsc_mapped_block *blk, void *p);
    void (*all_unmapped)(struct fsc_mapped_block_handler *self, struct fsc_mapped_block *blk);
} fsc_mapped_block_handler;

extern void fsc_unregister_mapped_block(fsc_mapped_block *b);
extern fsc_mapped_block *realaddr_to_mapped_block(void *p);
extern fsc_mapped_block *fscblock_to_mapped_block(base_t b);
extern ptrvalue realaddr_to_mapped_fat_pointer(void *p);
extern ptrvalue default_addr_to_fat_pointer(struct fsc_mapped_block_handler *self, struct fsc_mapped_block *blk, void *p);
extern void fsc_unmap_mmapped_page(fsc_mapped_block *b, void *start, size_t length);
extern int fsc_change_page_protections(fsc_mapped_block *b, void *start, size_t length, int prot);
extern fsc_mapped_block *fsc_create_mmap(fsc_mapped_block_handler *handler, size_t start, size_t length, int prot, int flags, int fd, off_t offset);
