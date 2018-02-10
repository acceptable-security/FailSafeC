/* Generated file -- do not edit. */
/**
 * @file include/stdarg.h
 */
#ifndef __STDARG_H
#define __STDARG_H

#include <sys/__types.h>

#ifndef __VA_LIST
#define __VA_LIST
typedef __va_list va_list;
#endif

extern __fsc_attribute__((external)) va_list __builtin_va_start(void);
extern __fsc_attribute__((external)) void __builtin_va_end(va_list);

#define va_start(ap,last) (ap = __builtin_va_start())
#define va_end(ap) ((void)(__builtin_va_end(ap), ap = 0))
#define va_copy(dst, src) (dst = src)

#define va_arg(ap,type) (ap = ap + ((sizeof (type) + sizeof(int) - 1) / sizeof(int)), \
			 (*(type *)(ap - ((sizeof (type) + sizeof(int) - 1) / sizeof(int)))))

#endif

