/* Generated file -- do not edit. */
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

#define S_IFMT      61440
#define   S_IFBLK   24576
#define   S_IFCHR   8192
#define   S_IFIFO   4096
#define   S_IFREG   32768
#define   S_IFDIR   16384
#define   S_IFLNK   40960
#define   S_IFSOCK  49152

#define S_IRWXU     448
#define   S_IRUSR   256
#define   S_IWUSR   128
#define   S_IXUSR   64

#define S_IRWXG     56
#define   S_IRGRP   32
#define   S_IWGRP   16
#define   S_IXGRP   8

#define S_IRWXO     7
#define   S_IROTH   4
#define   S_IWOTH   2
#define   S_IXOTH   1

#define S_ISUID     2048
#define S_ISGID     1024
#define S_ISVTX     512

#define S_IREAD     256
#define S_IWRITE    128
#define S_IEXEC     64


#define S_ISBLK(m)   ((((m)) & 0170000) == (0060000))
#define S_ISCHR(m)   ((((m)) & 0170000) == (0020000))
#define S_ISDIR(m)   ((((m)) & 0170000) == (0040000))
#define S_ISFIFO(m)  ((((m)) & 0170000) == (0010000))
#define S_ISREG(m)   ((((m)) & 0170000) == (0100000))
#define S_ISLNK(m)   ((((m)) & 0170000) == (0120000))
#define S_ISSOCK(m)  ((((m)) & 0170000) == (0140000))

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
