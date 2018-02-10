/**
 * @file include/setjmp.h
 */
#ifndef __SETJMP_H
#define __SETJMP_H

typedef void *jmp_buf[1], *sigjmp_buf[1];

extern __fsc_attribute__((external)) int __builtin_setjmp(jmp_buf);
extern __fsc_attribute__((external)) int __builtin_sigsetjmp(sigjmp_buf, int);
extern void __fsc_attribute__((noreturn)) longjmp(jmp_buf, int);
extern void __fsc_attribute__((noreturn)) siglongjmp(sigjmp_buf, int);

#define setjmp(b) __builtin_setjmp(b)
#define sigsetjmp(b,s) __builtin_sigsetjmp(b,s)

#endif
