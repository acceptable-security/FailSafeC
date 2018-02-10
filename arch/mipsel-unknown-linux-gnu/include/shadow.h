/* Generated file -- do not edit. */
/**
 * @file include/shadow.h
 */

#ifndef __SHADOW_H
#define __SHADOW_H

struct __fsc_attribute__((named "stdlib_spwd")) spwd {
  char *sp_namp;
  char *sp_pwdp;
  long sp_lstchg;
  long sp_max;
  long sp_warn;
  long sp_inact;
  long sp_expire;
  unsigned long sp_flag;
};

struct spwd *getspnam(const char *name);

#endif
