<%# /* -*- c -*- */
#include <sys/stat.h>
#%>
/**
 * @file include/sys/stat.h
 */
#ifndef __SYS_STAT_H
#define __SYS_STAT_H

#include <sys/__types.h>
#include <time.h> /* for struct timespec */

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

#ifndef __DEV_T
#define __DEV_T
typedef __dev_t dev_t;
#endif

#ifndef __MODE_T
#define __MODE_T
typedef __mode_t mode_t;
#endif

#ifndef __INO_T
#define __INO_T
typedef __ino_t ino_t;
#endif

#ifndef __NLINK_T
#define __NLINK_T
typedef __nlink_t nlink_t;
#endif

#ifndef __BLKSIZE_T
#define __BLKSIZE_T
typedef __blksize_t blksize_t;
#endif

#ifndef __BLKCNT_T
#define __BLKCNT_T
typedef __blkcnt_t blkcnt_t;
#endif

#ifndef __OFF_T
#define __OFF_T
typedef __off_t off_t;
#endif

#ifndef __UID_T
#define __UID_T
typedef __uid_t uid_t;
#endif

#ifndef __GID_T
#define __GID_T
typedef __gid_t gid_t;
#endif

#define S_IFMT      <%=d S_IFMT   %>
#define   S_IFBLK   <%=d S_IFBLK  %>
#define   S_IFCHR   <%=d S_IFCHR  %>
#define   S_IFIFO   <%=d S_IFIFO  %>
#define   S_IFREG   <%=d S_IFREG  %>
#define   S_IFDIR   <%=d S_IFDIR  %>
#define   S_IFLNK   <%=d S_IFLNK  %>
#define   S_IFSOCK  <%=d S_IFSOCK %>

#define S_IRWXU     <%=d S_IRWXU  %>
#define   S_IRUSR   <%=d S_IRUSR  %>
#define   S_IWUSR   <%=d S_IWUSR  %>
#define   S_IXUSR   <%=d S_IXUSR  %>

#define S_IRWXG     <%=d S_IRWXG  %>
#define   S_IRGRP   <%=d S_IRGRP  %>
#define   S_IWGRP   <%=d S_IWGRP  %>
#define   S_IXGRP   <%=d S_IXGRP  %>

#define S_IRWXO     <%=d S_IRWXO  %>
#define   S_IROTH   <%=d S_IROTH  %>
#define   S_IWOTH   <%=d S_IWOTH  %>
#define   S_IXOTH   <%=d S_IXOTH  %>

#define S_ISUID     <%=d S_ISUID  %>
#define S_ISGID     <%=d S_ISGID  %>
#define S_ISVTX     <%=d S_ISVTX  %>

#define S_IREAD     <%=d S_IREAD  %>
#define S_IWRITE    <%=d S_IWRITE %>
#define S_IEXEC     <%=d S_IEXEC  %>

<%
#define __S(x) #x
#define __STR(x) __S(x)
%>
#define S_ISBLK(m)   <%=s __STR(S_ISBLK(m))  %>
#define S_ISCHR(m)   <%=s __STR(S_ISCHR(m))  %>
#define S_ISDIR(m)   <%=s __STR(S_ISDIR(m))  %>
#define S_ISFIFO(m)  <%=s __STR(S_ISFIFO(m)) %>
#define S_ISREG(m)   <%=s __STR(S_ISREG(m))  %>
#define S_ISLNK(m)   <%=s __STR(S_ISLNK(m))  %>
#define S_ISSOCK(m)  <%=s __STR(S_ISSOCK(m)) %>

struct __fsc_attribute__((named "stdlib_stat")) stat {
  dev_t     st_dev;
  ino_t     st_ino;
  mode_t    st_mode;
  nlink_t   st_nlink;
  uid_t     st_uid;
  gid_t     st_gid;
  dev_t     st_rdev;
  off_t     st_size;
  time_t    st_atime;
  time_t    st_mtime;
  time_t    st_ctime;
  blksize_t st_blksize;
  blkcnt_t  st_blocks;
};

extern int chmod(const char *, mode_t);
extern int fchmod(int, mode_t);

extern int stat(const char *, struct stat *);
extern int fstat(int, struct stat *);
extern int lstat(const char *, struct stat *);

extern int mkdir(const char *, mode_t);
extern int mkfifo(const char *, mode_t);
extern int mknod(const char *, mode_t, dev_t dev);

extern mode_t umask(mode_t);

#endif
