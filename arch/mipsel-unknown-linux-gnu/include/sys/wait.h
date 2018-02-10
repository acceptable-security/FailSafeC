/* Generated file -- do not edit. */
/**
 * @file include/sys/wait.h
 */
#ifndef __SYS_WAIT_H
#define __SYS_WAIT_H

#include <sys/__types.h>
#include <sys/wait_internal.h>

#ifndef __ID_T
#define __ID_T
typedef __id_t id_t;
#endif

#ifndef __PID_T
#define __PID_T
typedef __pid_t pid_t;
#endif

#include <signal.h>
#include <sys/resource.h>

pid_t wait(int *);
pid_t waitpid(pid_t, int *, int);
int waitid(idtype_t, id_t, siginfo_t *, int);

extern int WIFEXITED(int);
extern int WEXITSTATUS(int);
extern int WIFSIGNALED(int);
extern int WTERMSIG(int);
extern int WIFSTOPPED(int);
extern int WSTOPSIG(int);
extern int WIFCONTINUED(int);

#endif
