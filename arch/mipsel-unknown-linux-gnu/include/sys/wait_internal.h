/* Generated file -- do not edit. */
/**
 * @file include/sys/wait_internal.h
 */
#define WNOHANG		1
#define WUNTRACED	2
#define WEXITED     4
#define WSTOPPED    2
#define WCONTINUED  8
#define WNOWAIT     16777216

typedef enum {
  P_ALL  = 0,
  P_PID  = 1,
  P_PGID = 2
} idtype_t;
