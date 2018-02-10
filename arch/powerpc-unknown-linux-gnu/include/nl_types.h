/* Generated file -- do not edit. */
/**
 * @file include/nl_types.h
 */
#ifndef _NL_TYPES_H
#define _NL_TYPES_H

#include <_private_storage.h>

typedef struct _private_storage *nl_catd;
typedef int nl_item;

#define NL_SETD 1
#define NL_CAT_LOCALE 1

int catclose(nl_catd);
char *catgets(nl_catd, int, int, const char *);
nl_catd catopen(const char *, int);

#endif
