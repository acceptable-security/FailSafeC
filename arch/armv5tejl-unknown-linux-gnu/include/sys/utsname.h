/* Generated file -- do not edit. */
/**
 * @file include/sys/utsname.h
 */
#ifndef __SYS_UTSNAME_H
#define __SYS_UTSNAME_H

struct __fsc_attribute__((named "stdlib_utsname")) utsname {
  char sysname[1024];
  char nodename[1024];
  char release[1024];
  char version[1024];
  char machine[1024];
};

extern int uname(struct utsname *);

#endif
