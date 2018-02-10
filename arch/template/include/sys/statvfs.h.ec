<%# /* -*- c -*- */
#include <sys/statvfs.h>
#%>

/**
 * @file include/sys/statvfs.h
 */
#ifndef __SYS_STATVFS_H
#define __SYS_STATVFS_H

#include <sys/__types.h>

#ifndef __FSBLKCNT_T
#define __FSBLKCNT_T
typedef __fsblkcnt_t fsblkcnt_t;
#endif

#ifndef FSFILCNT_T
#define FSFILCNT_T
typedef __fsfilcnt_t fsfilcnt_t;
#endif

struct __fsc_attribute__((named "stdlib_statvfs")) statvfs
{
  unsigned long f_bsize;
  unsigned long f_frsize;

  fsblkcnt_t f_blocks;
  fsblkcnt_t f_bfree;
  fsblkcnt_t f_bavail;

  fsfilcnt_t f_files;
  fsfilcnt_t f_ffree;
  fsfilcnt_t f_favail;

  unsigned long f_fsid;
  unsigned long f_flag;
  unsigned long f_namemax;
};

#define ST_RDONLY <%=d ST_RDONLY %>
#define ST_NOSUID <%=d ST_NOSUID %>

extern int statvfs(const char *, struct statvfs *);
extern int fstatvfs(int, struct statvfs *);

#endif

