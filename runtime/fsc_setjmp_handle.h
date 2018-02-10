#ifndef FSC_SETJMP_HANDLE
#define FSC_SETJMP_HANDLE
#include <type_repr.h>
#include <setjmp.h>
#include <fsc_stackrec.h>

struct fsc_jmpbuf {
    int initialized;
    union {
	jmp_buf jmpbuf;
	sigjmp_buf sigjmpbuf;
    } j;
    int is_sigsetjmp;
    struct fsc_stack_frame *frame;
};

extern value fsc_create_jmpbuf(int);
extern struct fsc_jmpbuf *fsc_get_jmpbuf(base_t, ofs_t, int);

#define FS_FPPv_i___builtin_setjmp(b,o) \
  (write_word(b,o,fsc_create_jmpbuf(0),0), \
   setjmp(fsc_get_jmpbuf(b,o,0)->j.jmpbuf) ? \
     (fsc_raise_error_library(b, o, ERR_INVALIDARGS, "setjmp: race condition"),0) : \
   value_of_int(setjmp(fsc_get_jmpbuf(b,o,1)->j.jmpbuf)))

#define FS_FPPvi_i___builtin_sigsetjmp(b,o,sb,so) \
  (write_word(b,o,fsc_create_jmpbuf(1),0), \
   sigsetjmp(fsc_get_jmpbuf(b,o,0)->j.sigjmpbuf,so) ? \
     (fsc_raise_error_library(b, o, ERR_INVALIDARGS, "sigsetjmp: race condition"),0) : \
   value_of_int(sigsetjmp(fsc_get_jmpbuf(b,o,1)->j.sigjmpbuf,so)))

#endif

