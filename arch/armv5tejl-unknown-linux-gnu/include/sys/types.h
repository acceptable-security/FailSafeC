/* Generated file -- do not edit. */
/**
 * @file include/sys/types.h
 */
#ifndef __SYS_TYPES_H
#define __SYS_TYPES_H

#include <sys/types_internal.h>

#include <sys/__types.h>
#include <stdint.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __SSIZE_T
#define __SSIZE_T
typedef __ssize_t ssize_t;
#endif

#ifndef __ID_T
#define __ID_T
typedef __id_t id_t;
#endif

#ifndef __UID_T
#define __UID_T
typedef __uid_t uid_t;
#endif

#ifndef __GID_T
#define __GID_T
typedef __gid_t gid_t;
#endif

#ifndef __PID_T
#define __PID_T
typedef __pid_t pid_t;
#endif

#ifndef __OFF_T
#define __OFF_T
typedef __off_t off_t;
#endif

#ifndef __MODE_T
#define __MODE_T
typedef __mode_t mode_t;
#endif

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

#ifndef __USECONDS_T
#define __USECONDS_T
typedef __useconds_t useconds_t;
#endif

typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;

typedef __uint8_t  u_int8_t;
typedef __uint16_t u_int16_t;
typedef __uint32_t u_int32_t;
typedef __uint64_t u_int64_t;

typedef char *caddr_t;

#endif
