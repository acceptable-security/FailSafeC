/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#include <block.h>
#include <fsc_error.h>
#include <fsc_debug.h>

#include <signal.h>
#include <stdio.h>

#if (! defined (FSC_USE_INLINE_NULLCHECK) && ! defined(FSC_NO_SAFEBUF))

#ifndef FSC_NO_SAFEBUF
#include <fsc_safebuf.h>
#endif
#include <fsc_mman.h>

static void raise_default_action(int sig) {
    struct sigaction act;
    sigset_t set;

    sigemptyset(&act.sa_mask);
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigaction(sig, &act, NULL);

    sigemptyset(&set);
    sigaddset(&set, sig);
    sigprocmask(SIG_UNBLOCK, &set, NULL);

    raise(sig);
    fsc_raise_panic("null-ptr signal handler: unexpected continuation");
}

void fsc_nullptr_handler(int sig, siginfo_t *sinfo, void *p) {
    int addr = (signed int)sinfo->si_addr;

    if (sinfo->si_code == SI_USER
#ifdef SI_TKILL
	|| sinfo->si_code == SI_TKILL
#endif
	) {
#ifdef FSC_DEBUG_RUNTIME
      if (fsc_debug_flag['S'])
	fprintf(stderr, 
		"null-ptr signal handler: detected user-generated sig %d, propagated\n", sig);
#endif
      raise_default_action(sig);
    }

    if (addr >= -(sizeof (fsc_header)) && addr < 0) {
#ifdef FSC_DEBUG_RUNTIME
	if (fsc_debug_flag['S'])
	    fprintf(stderr, 
		    "null-ptr signal handler: detected controlled sigsegv caused by null pointer (%d)\n", addr);
#endif
	fsc_raise_error(0, 0, ERR_NULLPTR);
    }

#ifndef FSC_NO_SAFEBUF
    {
	struct safe_buffer_info *p;

	for (p = fsc_safe_buffer_list; p; p = p->next) {
	    if (addr == (int)p->pageaddr + p->alloc_size) {
#ifdef FSC_DEBUG_RUNTIME
		if (fsc_debug_flag['S'])
		    fprintf(stderr, 
			    "null-ptr signal handler: detected controlled sigsegv caused by safe buffer overrun (%#x)\n", addr);
#endif
		fsc_raise_error(p->b, p->o + p->len, ERR_OUTOFBOUNDS);
	    }
	}
    }
#endif

    {
	fsc_mapped_block *b = realaddr_to_mapped_block((void *)addr);
	if (b) {
#ifdef FSC_DEBUG_RUNTIME
	    if (fsc_debug_flag['S'] || fsc_debug_flag['M'])
		fprintf(stderr, 
			"null-ptr signal handler: detected access fault at %p, inside mapped block at %p..%p ... propagating\n",
			(void *)addr, b->realbase, b->reallast);
#endif
	    raise_default_action(sig);
	}
    }

    fsc_raise_panic_1(addr, "null-ptr signal handler: FATAL: unexpected signal");
}

int install_nullhandler(void) {
    struct sigaction act;
    sigset_t sigset;

    sigemptyset(&act.sa_mask);
    act.sa_sigaction = fsc_nullptr_handler;
    act.sa_flags = SA_SIGINFO;

    if (sigaction(SIGSEGV, &act, NULL)) {
	fsc_raise_syserror("install_nullhandler: cannot set SIGSEGV handler");
    }
    if (sigaction(SIGBUS, &act, NULL)) {
	fsc_raise_syserror("install_nullhandler: cannot set SIGBUS handler");
    }
    return 0;
}

#else

int install_nullhandler(void) {}

#endif

extern void fsc_init_modules(void);

void fsc_init_runtime(void) {
    install_nullhandler();
    fsc_init_modules();
}
