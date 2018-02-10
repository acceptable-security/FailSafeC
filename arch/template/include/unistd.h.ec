<%#/* -*- C -*- */
#include <unistd.h>
#%>
/**
 * @file include/unistd.h
 */
#ifndef __UNISTD_H
#define __UNISTD_H

#include <sys/__types.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __SSIZE_T
#define __SSIZE_T
typedef __ssize_t ssize_t;
#endif

#ifndef __UID_T
#define __UID_T
typedef __uid_t uid_t;
#endif

#ifndef __GID_T
#define __GID_T
typedef __gid_t gid_t;
#endif

#ifndef __OFF_T
#define __OFF_T
typedef __off_t off_t;
#endif

#ifndef __PID_T
#define __PID_T
typedef __pid_t pid_t;
#endif

#ifndef __USECONDS_T
#define __USECONDS_T
typedef __useconds_t useconds_t;
#endif

#ifndef __INTPTR_T
#define __INTPTR_T
typedef __intptr_t intptr_t;
#endif

#define STDIN_FILENO   <%=d STDIN_FILENO  %>
#define STDOUT_FILENO  <%=d STDOUT_FILENO %>
#define STDERR_FILENO  <%=d STDERR_FILENO %>

#define F_OK  <%=d F_OK %>
#define R_OK  <%=d R_OK %>
#define W_OK  <%=d W_OK %>
#define X_OK  <%=d X_OK %>

#define _SC_ARG_MAX <%=d _SC_ARG_MAX %>
#define _SC_OPEN_MAX <%=d _SC_OPEN_MAX %>

#define _CS_PATH <%=d _CS_PATH %>

extern int access(const char *, int);

extern gid_t getgid(void);
extern int getopt(int, char *const [], const char *);
extern char *optarg;
extern int optind, opterr, optopt;

extern uid_t getuid(void);
extern pid_t getpid(void);
extern pid_t getppid(void);
extern uid_t geteuid(void);
extern gid_t getegid(void);
extern pid_t getpgid(pid_t);
extern pid_t getpgrp(void);
extern pid_t getsid(pid_t);

extern int setsid(void);
extern int setuid(uid_t uid);
extern int setgid(gid_t gid);
extern int setegid(gid_t gid);
extern int seteuid(uid_t uid);
extern int setpgid(pid_t, pid_t);
extern pid_t setpgrp(void);
extern int setreuid(uid_t, uid_t);
extern int setregid(gid_t, gid_t);

extern pid_t tcgetpgrp(int);
extern int tcsetpgrp(int, pid_t);

extern useconds_t ualarm(useconds_t, useconds_t);
extern pid_t vfork(void);

extern void sync(void);

extern pid_t fork(void);
extern void _exit(int);

extern int execv(const char *, char *const *);
extern int execve(const char *, char *const *, char *const *);
extern int execvp(const char *, char *const *);
extern int execl(const char *, const char *, ...);
extern int execle(const char *, const char *, ...);
extern int execlp(const char *, const char *, ...);

extern int nice(int);

extern off_t lseek(int, off_t, int);
extern int fcntl(int, int, ...);

extern int close(int);
extern int open(const char *, int, ...);
extern int write(int, const void *, size_t);
extern int read(int, void *, size_t);
extern int pread(int, void  *, size_t, off_t);
extern int pwrite(int, const void  *, size_t, off_t);
extern int unlink(const char *);
extern ssize_t readlink(const char *, char *, size_t);
extern int chown(const char *, uid_t, gid_t);
extern int lchown(const char *, uid_t, gid_t);
extern int fchown(int, uid_t, gid_t);
extern int fchdir(int);
extern int fdatasync(int);
extern long fpathconf(int, int);
extern long pathconf(const char *, int);
extern long gethostid(void);
extern int lockf(int, int, off_t);
extern int pause(void);
extern int chdir(const char *);
extern char *getcwd(char *, size_t);
extern char *getwd(char *);
extern int truncate(const char *, off_t);
extern int fsync(int);

extern int link(const char *, const char *);
extern int symlink(const char *, const char *);

extern int dup(int);
extern int dup2(int, int);
extern int pipe(int[2]);

extern int gethostname(char *, size_t);

extern long sysconf(int name);

extern unsigned int alarm(unsigned int);
extern unsigned int sleep(unsigned int);
extern int usleep(useconds_t);

extern char *crypt(const char *, const char *);

extern int initgroups(const char *, gid_t);
extern int getgroups(int, gid_t *);
extern int setgroups(int, const gid_t *);

extern int chroot(const char *);

extern int getdtablesize(void);

extern int isatty(int);

extern char *ttyname(int);
extern int ttyslot(void);

extern int ftruncate(int, off_t);

extern int rmdir(const char *path);

extern char *getlogin(void);
extern int getlogin_r(char *, size_t);

extern char *getpass(const char *);
extern void swab(const void *, void *, ssize_t);

extern size_t confstr(int, char *, size_t);

extern int getpagesize(void);

#endif
