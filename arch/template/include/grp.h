/**
 * @file include/grp.h
 */
#ifndef __GRP_H
#define __GRP_H

#include <sys/__types.h>

#ifndef __GID_T
#define __GID_T
typedef __gid_t gid_t;
#endif

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

struct __fsc_attribute__((named "stdlib_group")) group {
  char *gr_name;
  gid_t gr_gid;
  char **gr_mem;
};

struct group *getgrgid(gid_t);
int getgrgid_r(gid_t, struct group *, char *, size_t, struct group **);

struct group *getgrnam(const char *);
int getgrnam_r(const char *, struct group *, char *, size_t, struct group **);

struct group  *getgrent(void);
void setgrent(void);
void endgrent(void);

#endif
