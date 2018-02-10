<%# /* -*- c -*- */
#include <stdio.h>
#%>
/**
 * @file include/stdio_internal.h
 */
#ifndef __STDIO_INTERNAL_H
#define __STDIO_INTERNAL_H

#define BUFSIZ <%=d BUFSIZ %>

#define _IOFBF <%=d _IOFBF %>
#define _IOLBF <%=d _IOLBF %>
#define _IONBF <%=d _IONBF %>

#define L_ctermid <%=d L_ctermid %>
#define L_tmpnam <%=d L_tmpnam %>

typedef struct __fsc_attribute__((named "stdlib_fpos_t")) __fpos_t {
  char __data[<%=d sizeof(fpos_t) %>];
} fpos_t;

#endif
