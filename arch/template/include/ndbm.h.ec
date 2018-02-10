<%# /* -*- c -*- */
#include "config.h"
#ifdef HAVE_NDBM_H
# include <ndbm.h>
#else
# ifdef HAVE_GDBM_DASH_NDBM_H
#  include <gdbm-ndbm.h>
# else
#  ifdef HAVE_GDBM_SLASH_NDBM_H
#   include <gdbm/ndbm.h>
#  endif
# endif
#endif
#%>
/**
 * @file include/ndbm.h
 */
#ifndef _NDBM_H
#define _NDBM_H

#include <stddef.h>
#include <sys/types.h>
#include <_private_storage.h>

typedef struct __fsc_attribute__((named "stdlib_datum")) _datum {
  void *dptr;
  size_t dsize;
} datum;

typedef struct _private_storage DBM;

#define DBM_INSERT  <%=d DBM_INSERT  %>
#define DBM_REPLACE <%=d DBM_REPLACE %>

extern int dbm_clearerr(DBM *);
extern void dbm_close(DBM *);
extern int dbm_delete(DBM *, datum);
extern int dbm_error(DBM *);
extern datum dbm_fetch(DBM *, datum);
extern datum dbm_firstkey(DBM *);
extern datum dbm_nextkey(DBM *);
extern DBM *dbm_open(const char *, int, mode_t);
extern int dbm_store(DBM *, datum, datum, int);

extern int dbm_pagfno(DBM *);
extern int dbm_dirfno(DBM *);
#endif
