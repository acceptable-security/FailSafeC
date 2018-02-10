<%# /* -*- c -*- */
#include <signal.h>
#%>
<%#cflags
-D_XOPEN_SOURCE=500
-D_BSD_SOURCE
#%>
/* <%=s EC_VERSION_MSG %> */
/**
 * @file include/signal.h
 */
#ifndef __SIGNAL_H
#define __SIGNAL_H

#include <sys/__types.h>
#include <time.h>

#ifndef __PID_T
#define __PID_T
typedef __pid_t pid_t;
#endif

#define SIG_DFL  ((void(*)(int))<%=d (int)SIG_DFL  %>)
#define SIG_ERR  ((void(*)(int))<%=d (int)SIG_ERR  %>)
#define SIG_HOLD ((void(*)(int))<%=d (int)SIG_HOLD %>)
#define SIG_IGN  ((void(*)(int))<%=d (int)SIG_IGN  %>)

#define _NSIG <%=d _NSIG %>

#define  SIGHUP    <%=d SIGHUP      %>
#define  SIGINT    <%=d SIGINT      %>
#define  SIGQUIT   <%=d SIGQUIT     %>
#define  SIGILL    <%=d SIGILL      %>
#define  SIGTRAP   <%=d SIGTRAP     %>
#define  SIGABRT   <%=d SIGABRT     %>
#define  SIGIOT    <%=d SIGIOT      %>
#define  SIGFPE    <%=d SIGFPE      %>
#define  SIGKILL   <%=d SIGKILL     %>
#define  SIGBUS    <%=d SIGBUS      %>
#define  SIGSEGV   <%=d SIGSEGV     %>
#define  SIGSYS    <%=d SIGSYS      %>
#define  SIGPIPE   <%=d SIGPIPE     %>
#define  SIGALRM   <%=d SIGALRM     %>
#define  SIGTERM   <%=d SIGTERM     %>
#define  SIGUSR1   <%=d SIGUSR1     %>
#define  SIGUSR2   <%=d SIGUSR2     %>
#define  SIGCHLD   <%=d SIGCHLD     %>
#define  SIGPWR    <%=d SIGPWR      %>
#define  SIGWINCH  <%=d SIGWINCH    %>
#define  SIGURG    <%=d SIGURG      %>
#define  SIGPOLL   <%=d SIGPOLL     %>
#define  SIGSTOP   <%=d SIGSTOP     %>
#define  SIGTSTP   <%=d SIGTSTP     %>
#define  SIGCONT   <%=d SIGCONT     %>
#define  SIGTTIN   <%=d SIGTTIN     %>
#define  SIGTTOU   <%=d SIGTTOU     %>
#define  SIGVTALRM <%=d SIGVTALRM   %>
#define  SIGPROF   <%=d SIGPROF     %>
#define  SIGXCPU   <%=d SIGXCPU     %>
#define  SIGXFSZ   <%=d SIGXFSZ     %>

#define SA_NOCLDSTOP <%=d SA_NOCLDSTOP %>

#define SIG_BLOCK     <%=d SIG_BLOCK   %>
#define SIG_UNBLOCK   <%=d SIG_UNBLOCK %>
#define SIG_SETMASK   <%=d SIG_SETMASK %>

#define SA_ONSTACK   <%=#x SA_ONSTACK   %>
#define SA_RESETHAND <%=#x SA_RESETHAND %>
#define SA_RESTART   <%=#x SA_RESTART   %>
#define SA_SIGINFO   <%=#x SA_SIGINFO   %>
#define SA_NOCLDWAIT <%=#x SA_NOCLDWAIT %>
#define SA_NODEFER   <%=#x SA_NODEFER   %>

#define SS_ONSTACK   <%=#x SS_ONSTACK   %>
#define SS_DISABLE   <%=#x SS_DISABLE   %>

#define SA_ONESHOT   <%=#x SA_ONESHOT   %>

/* TODO */
typedef int sig_atomic_t;

typedef struct __fsc_attribute__((named "stdlib_sigset_t")) __sigset_t {
  char v[<%=d _NSIG %>];
} sigset_t;

typedef struct __fsc_attribute__((named "stdlib_siginfo_t")) __siginfo_t {
  int si_signo;
  int si_errno;
  int si_code;
} siginfo_t;

struct __fsc_attribute__((named "stdlib_sigaction")) sigaction {
  void (*sa_handler)(int);
  void (*sa_sigaction)(int, siginfo_t *, void *);
  int sa_flags;
  sigset_t sa_mask;
};

extern int sigaction(int, const struct sigaction *, struct sigaction *);

extern void (*signal(int, void (*)(int)))(int);

extern int sigprocmask(int, const sigset_t *, sigset_t *);

extern int sigemptyset(sigset_t *);
extern int sigfillset(sigset_t *);
extern int sigaddset(sigset_t *, int);
extern int sigdelset(sigset_t *, int);
extern int sigismember(sigset_t *, int);

extern int raise(int);
extern int kill(pid_t, int);

extern int killpg(pid_t pgrp, int sig);
extern int sighold(int sig);
extern int sigignore(int sig);
extern int sigpause(int sig);
extern int sigrelse(int sig);

extern int siginterrupt(int sig, int flag);

extern int sigsuspend(const sigset_t *sigmask);
extern int sigpending(sigset_t *set);
extern int sigwait(const sigset_t *set, int *sig);

extern void (*sigset(int, void(*)(int)))(int);

#endif
