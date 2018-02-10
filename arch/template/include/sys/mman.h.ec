<%# /* -*- c -*- */
#include <sys/mman.h>
#%>
#ifndef _MMAN_H
#define _MMAN_H

#include <sys/types.h>

extern void *mmap(void *, size_t, int, int, int, off_t);
extern int munmap(void *, size_t);
extern int mprotect(void *, size_t, int);

#define PROT_EXEC <%=d PROT_EXEC %>
#define PROT_READ <%=d PROT_READ %>
#define PROT_WRITE <%=d PROT_WRITE %>
#define PROT_NONE <%=d PROT_NONE %>
#define MAP_FIXED <%=d MAP_FIXED %>
#define MAP_SHARED <%=d MAP_SHARED %>
#define MAP_PRIVATE <%=d MAP_PRIVATE %>
#define MAP_NORESERVE <%=d MAP_NORESERVE %>
#define MAP_ANONYMOUS <%=d MAP_ANONYMOUS %>
#define MAP_FAILED ((void *)<%=#lx (long)MAP_FAILED %>)

#endif
