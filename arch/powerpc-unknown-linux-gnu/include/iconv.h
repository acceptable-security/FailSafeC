/* Generated file -- do not edit. */
#ifndef __ICONV_H
#define __ICONV_H

/**
 * @file include/iconv.h
 */
#include <stddef.h>
#include <_private_storage.h>

typedef struct _private_storage *iconv_t;

iconv_t iconv_open(const char *, const char *);
size_t iconv(iconv_t, char **, size_t *, char **, size_t *);
int iconv_close(iconv_t);

#endif
