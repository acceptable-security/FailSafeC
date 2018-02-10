/* Generated file -- do not edit. */
#ifndef _MMAN_H
#define _MMAN_H

#include <sys/types.h>

extern void *mmap(void *, size_t, int, int, int, off_t);
extern int munmap(void *, size_t);
extern int mprotect(void *, size_t, int);

#define PROT_EXEC 4
#define PROT_READ 1
#define PROT_WRITE 2
#define PROT_NONE 0
#define MAP_FIXED 16
#define MAP_SHARED 1
#define MAP_PRIVATE 2
#define MAP_NORESERVE 16384
#define MAP_ANONYMOUS 32
#define MAP_FAILED ((void *)0xffffffff)

#endif
