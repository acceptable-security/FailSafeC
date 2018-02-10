/* -*- c -*- */
/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/

/**
 * @file stdlib/signal_2.sc
 */

#include <signal.h>
#include <stddef.h>

/**
 * @fn int sigemptyset(sigset_t *set)
 * @author Yutaka Oiwa.
 */
int sigemptyset(sigset_t *set) {
  int i;
  for (i = 0; i < _NSIG; i++) {
    set->v[i] = 0;
  }
  return 0;
}

/**
 * @fn int sigfillset(sigset_t *set)
 * @author Yutaka Oiwa.
 */
int sigfillset(sigset_t *set) {
  int i;
  for (i = 0; i < _NSIG; i++) {
    set->v[i] = 1;
  }
  return 0;
}

/**
 * @fn int sigaddset(sigset_t *set, int i)
 * @author Yutaka Oiwa.
 */
int sigaddset(sigset_t *set, int i) {
  set->v[i] = 1;
  return 0;
}

/**
 * @fn int sigdelset(sigset_t *set, int i)
 * @author Yutaka Oiwa.
 */
int sigdelset(sigset_t *set, int i) {
  set->v[i] = 0;
  return 0;
}

/**
 * @fn int sigismember(sigset_t *set, int i)
 * @bug prototype should be int sigismember(const sigset_t *, int)
 *
 * @author Yutaka Oiwa.
 */
int sigismember(sigset_t *set, int i) {
  return set->v[i];
}

/**
 * @bug doxygen cannot recognize this declaration
 *
 * @param sig signal
 * @param handler signal handler
 *
 * @author Yutaka Oiwa.
 */
void (*signal(int sig, void (*handler)(int)))(int) {
  struct sigaction sa;

  sigemptyset(&sa.sa_mask);
  sa.sa_handler = handler;
  sa.sa_flags = 0;

  sigaction(sig, &sa, &sa);

  return (sa.sa_flags & SA_SIGINFO) ? sa.sa_handler : (void (*)(int)) sa.sa_sigaction;
}

/**
 * @bug doxygen cannot recognize this declaration
 *
 * @author Lepidum Co., Ltd.
 */
void (*sigset(int sig, void (*disp)(int)))(int) {
  struct sigaction sa, osa;
  sigset_t set, oset;
  int ret;

  if (disp == SIG_HOLD) {
    ret = sigaction(sig, NULL, &osa);
  } else {
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = disp;
    sa.sa_flags = 0;
    ret = sigaction(sig, &sa, &osa);
  }

  if (ret != 0) {
    return SIG_ERR;
  }

  sigemptyset(&set);
  sigaddset(&set, sig);
  sigprocmask((disp == SIG_HOLD ? SIG_BLOCK : SIG_UNBLOCK), &set, &oset);

  if (sigismember(&oset, sig)) {
    return SIG_HOLD;
  } else {
    return (osa.sa_flags & SA_SIGINFO) ? osa.sa_handler : (void (*)(int)) osa.sa_sigaction;
  }
}
