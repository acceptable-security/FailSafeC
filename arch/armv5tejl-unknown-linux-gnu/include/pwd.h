/* Generated file -- do not edit. */
/**
 * @file include/pwd.h
 */

#ifndef __PWD_H
#define __PWD_H

#include <sys/__types.h>

#ifndef __UID_T
#define __UID_T
typedef __uid_t uid_t;
#endif

#ifndef __GID_T
#define __GID_T
typedef __gid_t gid_t;
#endif

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

struct __fsc_attribute__((named "stdlib_passwd")) passwd {
  char *pw_name;
  char *pw_passwd;
  uid_t pw_uid;
  gid_t pw_gid;
  char *pw_gecos;
  char *pw_dir;
  char *pw_shell;
};

extern struct passwd *getpwnam(const char *);
extern struct passwd *getpwuid(uid_t);

extern int getpwnam_r(const char *, struct passwd *, char *, size_t, struct passwd **);
extern int getpwuid_r(uid_t, struct passwd *, char *, size_t, struct passwd **);

extern void endpwent(void);
extern struct passwd *getpwent(void);
extern void setpwent(void);

#endif
