/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#ifndef FSC_ERROR_H
#define FSC_ERROR_H

#include <type_repr.h>

enum fsc_error { 
  ERR_UNKNOWN, ERR_UNALIGNED, ERR_OUTOFBOUNDS, 
  ERR_PTRMINUS, ERR_PTRCOMP, ERR_NOACCESS,
  ERR_NULLPTR, ERR_TYPEMISMATCH, ERR_UNIMPLEMENTED,
  ERR_OUTOFMEMORY, ERR_INVALIDARGS, ERR_STALEPTR,
  ERR_INTERNAL, ERR_SYSERROR, ERR_NOTREACHED
};

void fsc_raise_error(base_t, ofs_t, enum fsc_error)
    __attribute__ ((__noreturn__));

void fsc_halt_program(void)
    __attribute__ ((__noreturn__));

void fsc_raise_error_library(base_t, ofs_t, enum fsc_error, const char *)
    __attribute__ ((__noreturn__));

#define fsc_raise_panic(s) (fsc_raise_error_library(0, 0, ERR_INTERNAL, (s)))
#define fsc_raise_panic_1(a,s) (fsc_raise_error_library(0, (ofs_t)(a), ERR_INTERNAL, (s)))
#define fsc_raise_panic_2(b,o,s) (fsc_raise_error_library((b), (ofs_t)(o), ERR_INTERNAL, (s)))
#define fsc_raise_syserror(s) (fsc_raise_error_library(0, 0, ERR_SYSERROR, (s)))

#endif
