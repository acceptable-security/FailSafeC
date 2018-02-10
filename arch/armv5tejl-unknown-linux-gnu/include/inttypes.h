/* Generated file -- do not edit. */
/**
 * @file include/inttypes.h
 */
#ifndef __INTTYPES_H
#define __INTTYPES_H

#include <stdint.h>

typedef struct __fsc_attribute__((named "stdlib_imaxdiv_t")) _imaxdiv_t {
  intmax_t quot;
  intmax_t rem;
} imaxdiv_t;

intmax_t  imaxabs(intmax_t);
imaxdiv_t imaxdiv(intmax_t, intmax_t);
intmax_t  strtoimax(const char *restrict, char **restrict, int);
uintmax_t strtoumax(const char *restrict, char **restrict, int);

#endif
