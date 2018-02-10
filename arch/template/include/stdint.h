/**
 * @file include/stdint.h
 */
#ifndef __STDINT_H
#define __STDINT_H

#include <sys/__types.h>

#ifndef __INT8_T
#define __INT8_T
typedef __int8_t int8_t;
#endif

#ifndef __INT16_T
#define __INT16_T
typedef __int16_t int16_t;
#endif

#ifndef __INT32_T
#define __INT32_T
typedef __int32_t int32_t;
#endif

#ifndef __INT64_T
#define __INT64_T
typedef __int64_t int64_t;
#endif

#ifndef __UINT8_T
#define __UINT8_T
typedef __uint8_t uint8_t;
#endif

#ifndef __UINT16_T
#define __UINT16_T
typedef __uint16_t uint16_t;
#endif

#ifndef __UINT32_T
#define __UINT32_T
typedef __uint32_t uint32_t;
#endif

#ifndef __UINT64_T
#define __UINT64_T
typedef __uint64_t uint64_t;
#endif

#ifndef __INTPTR_T
#define __INTPTR_T
typedef __intptr_t intptr_t;
#endif

#ifndef __UINTPTR_T
#define __UINTPTR_T
typedef __uintptr_t uintptr_t;
#endif

#ifndef __INTMAX_T
#define __INTMAX_T
typedef __intmax_t intmax_t;
#endif

#ifndef __UINTMAX_T
#define __UINTMAX_T
typedef __uintmax_t uintmax_t;
#endif

#endif
