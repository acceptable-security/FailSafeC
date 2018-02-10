/* Generated file -- do not edit. */
/**
 * @file include/sys/file.h
 */
#ifndef __SYS_FILES_H
#define __SYS_FILES_H
#include <fcntl.h>

extern int flock(int, int);

#define LOCK_SH 1
#define LOCK_EX 2
#define LOCK_NB 4
#define LOCK_UN 8

#endif
