/* Generated file -- do not edit. */
/**
 * @file include/fcntl.h
 */
#ifndef __FCNTL_H
#define __FCNTL_H

#include <sys/stat.h>
#include <unistd.h>

#include <fcntl_internal.h>

struct __fsc_attribute__((named "stdlib_flock")) flock {
  short l_type;
  short l_whence;
  off_t l_start;
  off_t l_len;
  pid_t l_pid;
};

extern int creat(const char *, mode_t);
extern int fcntl(int, int, ...);
extern int open(const char *, int, ...);

extern int posix_fadvise(int, off_t, off_t, int);
extern int posix_fallocate(int, off_t, off_t);

#endif
