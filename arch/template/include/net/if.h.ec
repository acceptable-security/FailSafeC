<%#/* -*- C -*- */
#include <net/if.h>
#%>
/**
 * @file include/net/if.h
 */

#ifndef __NET_IF_H
#define __NET_IF_H

#define IF_NAMESIZE <%=d IF_NAMESIZE %>

struct __fsc_attribute__((named "stdlib_if_nameindex")) if_nameindex {
  unsigned if_index;
  char *if_name;
};

extern unsigned if_nametoindex(const char *);
extern char *if_indextoname(unsigned, char *);
extern struct if_nameindex *if_nameindex(void);
extern void if_freenameindex(struct if_nameindex *);

#endif
