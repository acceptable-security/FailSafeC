/* Generated file -- do not edit. */
/**
 * @file include/stdio_internal.h
 */
#ifndef __STDIO_INTERNAL_H
#define __STDIO_INTERNAL_H

#define BUFSIZ 8192

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#define L_ctermid 9
#define L_tmpnam 20

typedef struct __fsc_attribute__((named "stdlib_fpos_t")) __fpos_t {
  char __data[12];
} fpos_t;

#endif
