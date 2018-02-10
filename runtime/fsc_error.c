/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#define FSC_RUNTIME_LIBRARY
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <fsc_runtime.h>
#include <fsc_autoconf.h>
#include <signal.h>
#include <errno.h>
#include <fsc_mman.h>

#define USE_GDB_TRACE
#ifdef HAVE_BACKTRACE
#define USE_EXECINFO_TRACE
#endif

#ifdef USE_EXECINFO_TRACE
#include <execinfo.h>
#endif

#ifdef USE_GDB_TRACE
#include <limits.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#endif

static const char *strfscerror(enum fsc_error e) {
    static char buf[160];
    switch (e) {
    case ERR_UNKNOWN:
	return "unknown error";
    case ERR_UNALIGNED:
	return "unaligned access";
    case ERR_OUTOFBOUNDS:
	return "access out of bounds";
    case ERR_PTRMINUS:
	return "pointer subtraction error";
    case ERR_NULLPTR:
	return "accessing null pointer";
    case ERR_PTRCOMP:
	return "pointer comparison error";
    case ERR_NOACCESS:
	return "accessing abstract data structure";
    case ERR_TYPEMISMATCH:
	return "invalid pointer passed";
    case ERR_UNIMPLEMENTED:
	return "invoked unimplemented feature";
    case ERR_OUTOFMEMORY:
	return "out of memory";
    case ERR_INVALIDARGS:
	return "invalid argument to runtime library";
    case ERR_STALEPTR:
	return "using already deallocated block";
    case ERR_INTERNAL:
	return "internal system error";
    case ERR_SYSERROR:
	snprintf(buf, 159, "panic: unacceptable system error (%d: %s)", errno, strerror(errno));
	buf[159] = 0;
	return buf;
    case ERR_NOTREACHED:
	return "returned from a non-returning function";
    default:
	sprintf(buf, "unknown error #%d", (int)e);
	return buf;
    }
}

static const char *strfscrflags(word rflag) {
    static char buf[512];
    word f;
    switch(rflag & RFLAG_TYPE_MASK) {
    case RFLAG_TYPE_NORMAL:
	strcpy(buf, "normal");
	break;
    case RFLAG_TYPE_VARARGS:
	strcpy(buf, "varargs");
	break;
    case RFLAG_TYPE_VARARGS_FINISHED_BY_USER:
	strcpy(buf, "varargs_end");
	break;
    case RFLAG_TYPE_RELEASED:
	strcpy(buf, "dead");
	break;
    default:
	sprintf(buf, "unknown(%d)", rflag & RFLAG_TYPE_MASK);
    }
    rflag &= ~RFLAG_TYPE_MASK;
    f = rflag & ~(rflag & (rflag - 1));
    for (; f != 0; f <<= 1) {
	if (rflag & f) {
	    switch (f) {
	    case RFLAG_NO_USER_DEALLOC:
		strcat(buf, ", no_user_dealloc");
		break;
	    case RFLAG_NO_DEALLOC:
		strcat(buf, ", no_dealloc");
		break;
	    case RFLAG_DEALLOCED:
		strcat(buf, ", deallocated");
		break;
	    case RFLAG_INTERNAL_BASEAREA:
		strcat(buf, ", internal-basearea");
		break;
	    case RFLAG_MAPPED_BLOCK:
		strcat(buf, ", mapped_block");
		break;
	    default:
		sprintf(buf, "unknown(%d)", f);
	    }
	}
    }
    return buf;
}

#ifdef USE_GDB_TRACE
int fsc_backtrace_by_gdb(void) {
    /* use /usr/bin/gdb for trace */
    int target_pid, child_pid, status, r;
    char fname[PATH_MAX + 1], buf_exe[80];

    target_pid = getpid();

    /* fprintf(stderr, "pid = %d\n", target_pid); */

    snprintf(buf_exe, 80, "/proc/%d/exe", target_pid);

    /* fprintf(stderr, "buf_exe = '%s'\n", buf_exe); */

    if (readlink(buf_exe, fname, PATH_MAX) < 0)
	return 0;

    fname[PATH_MAX] = '\0';

    /* fprintf(stderr, "fname = '%s'\n", fname); */

    if (access(fname, R_OK && X_OK) < 0)
	return 0;

    child_pid = fork();

    if (child_pid) {
	/* parent */
	if (child_pid == -1)
	    return 0;
	
	/* fprintf(stderr, "waiting for %d\n", child_pid); */

	while ((r = waitpid(child_pid, &status, 0)) == -1 && errno == EINTR);

	if (r != child_pid)
	    return 0;

	/* fprintf(stderr, "waiting for %d done (%d)\n", child_pid, status); */

	if (! WIFEXITED(status) || WEXITSTATUS(status) != 0)
	    return 0;

	/* fprintf(stderr, "status OK\n"); */

	return 1;
    } else {
	    char buf_cmd[160];

	    char *args[] = { "gdb", "-n", "-batch", 
			     buf_cmd,
			     "--eval-command=set height 0",
			     "--eval-command=bt",
			     "--eval-command=detach",
			     "--eval-command=quit\n" };
	    char *env[] = { NULL };

	    snprintf(buf_cmd, 160, "--eval-command=attach %d", target_pid);

	    if (dup2(2, 1) < 0)
		raise(SIGKILL);

	    close(0);
	    close(2);

	    execve("/usr/bin/gdb", args, env);

	    execve("/bin/gdb", args, env);

	    raise(SIGKILL);
	    return 1; /* NOTREACHED */
    }
}
#endif

void fsc_raise_error_library(base_t b0, ofs_t ofs, enum fsc_error e, const char *libloc) {
    const char *m = strfscerror(e);
    const char *c = is_cast(b0) ? "set" : "not set";
    const base_t b = base_remove_castflag(b0);
    fprintf (stderr, "\n--------------------------------\n");
    fprintf(stderr, 
	    "Fail-Safe C trap: %s%s%s%s\n",
	    (libloc ? "in " : ""), (libloc ? libloc : ""), (libloc ? ": " : ""), m);
    fprintf(stderr,
	    (ofs < 1048576 ?
	     "  Address: %p + %u\n"
	     "  Cast Flag: %s\n" :
	     "  Address: %p + %#x\n"
	     "  Cast Flag: %s\n"),
	    (void *)b, ofs, c);
    if (b != 0) {
	fsc_header *h = get_header_fast(b);
	const char *st = h->tinfo->ti_name;
	fprintf(stderr,
		"  Region's type: %s (VS %d, RS %d)\n"
		"           size: %d (FA %d, ST %d)\n"
		"           block status: %s\n",
		st, h->tinfo->virtual_size, h->tinfo->real_size,
		h->total_ofslimit, h->fastaccess_ofslimit, h->structured_ofslimit,
		strfscrflags(h->runtime_flags)
		);
	if (h->runtime_flags & RFLAG_MAPPED_BLOCK) {
	    fsc_mapped_block *mb = fscblock_to_mapped_block(b);
	    if (mb)
		fprintf(stderr, "  mmapped block: blocktop %p, addr %p, ofs %#lx",
			(void *)mb->realbase, (void *)vaddr_of_base_ofs(b, ofs), 
			(long)vaddr_of_base_ofs(b, ofs) - (long)mb->realbase);
	}
    }

#if defined(USE_GDB_TRACE) || defined(USE_EXECINFO_TRACE)
    fprintf (stderr, "\nbacktrace of instrumented code:\n");
#endif

#ifdef USE_GDB_TRACE
#ifndef USE_EXECINFO_TRACE
    (void)fsc_backtrace_by_gdb();
#else
    if (! fsc_backtrace_by_gdb())
#endif
#endif
#ifdef USE_EXECINFO_TRACE
    {
	void *bt[10];
	size_t size;

	size = backtrace(bt, 10);
	backtrace_symbols_fd (bt, size, 2);
	fprintf (stderr, "(%d entries)\n", size);
    }
    fprintf (stderr, "--------------------------------\n\n");
#endif
    fsc_halt_program();
}

void fsc_halt_program(void) {
  sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, SIGABRT);
  signal(SIGABRT, SIG_DFL);
  sigprocmask(SIG_UNBLOCK, &set, 0);
  abort();
  exit(255);
}  

void fsc_raise_error(base_t b0, ofs_t ofs, enum fsc_error e) {
    fsc_raise_error_library(b0, ofs, e, NULL);
}

