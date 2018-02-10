/**
 * @file include/alloca.h
 */
#ifndef __ALLOCA_H
#define __ALLOCA_H

#include <sys/__types.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

extern void *alloca(size_t);

#endif
