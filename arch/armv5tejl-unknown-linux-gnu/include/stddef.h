/* Generated file -- do not edit. */
/**
 * @file include/stddef.h
 */
#ifndef __STDDEF_H
#define __STDDEF_H

#include <sys/__types.h>

#ifndef NULL
#define NULL 0
#endif

#define offsetof(t,m) ((size_t)&((t*)0)->m)

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __PTRDIFF_T
#define __PTRDIFF_T
typedef __ptrdiff_t ptrdiff_t;
#endif

/* TODO: wchar_t */

#endif
