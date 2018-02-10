/**
 * @file include/dirent.h
 */
#ifndef _DIRENT_H
#define _DIRENT_H

#include <stdio.h>
#include <sys/__types.h>

#ifndef __INO_T
#define __INO_T
typedef __ino_t ino_t;
#endif

struct __fsc_attribute__((named "__stdio_dirent")) dirent {
  ino_t d_ino;
  char d_name[1025]; /* TODO reference NAME_MAX */
};

typedef FILE DIR;

extern int closedir(DIR *);
extern DIR *opendir(const char *);
extern struct dirent *readdir(DIR *);
extern int readdir_r(DIR *, struct dirent *, struct dirent **);
extern void rewinddir(DIR *);
extern void seekdir(DIR *, long);
extern long telldir(DIR *);

#endif
