/* Generated file -- do not edit. */
/**
 * @file syslog.h
 */
#ifndef __SYSLOG_H
#define __SYSLOG_H

#define LOG_PID    1
#define LOG_CONS   2
#define LOG_NDELAY 8
#define LOG_ODELAY 4
#define LOG_NOWAIT 16

#define LOG_KERN   0
#define LOG_USER   8
#define LOG_MAIL   16
#define LOG_NEWS   56
#define LOG_UUCP   64
#define LOG_DAEMON 24
#define LOG_AUTH   32
#define LOG_CRON   72
#define LOG_LPR    48
#define LOG_LOCAL0 128
#define LOG_LOCAL1 136
#define LOG_LOCAL2 144
#define LOG_LOCAL3 152
#define LOG_LOCAL4 160
#define LOG_LOCAL5 168
#define LOG_LOCAL6 176
#define LOG_LOCAL7 184

#define LOG_EMERG   0
#define LOG_ALERT   1
#define LOG_CRIT    2
#define LOG_ERR     3
#define LOG_WARNING 4
#define LOG_NOTICE  5
#define LOG_INFO    6
#define LOG_DEBUG   7


/* non-standard */
#define LOG_SYSLOG  40


extern void closelog(void);
extern void openlog(const char *, int, int);
extern int setlogmask(int);
extern void syslog(int, const char *, ...);

#endif
