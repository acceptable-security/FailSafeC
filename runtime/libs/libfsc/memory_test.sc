/* -*- c -*- */
/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2003-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#include <libfsc/libfsc.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned int uint;

static unsigned long long seed;

#define ran() (seed = seed * 1234567897LL + 12345LL,seed)

int fsc_test_memory_block(void *ptr, int verbose) {
    uint length_of_block = fsc_size_of_block(ptr);
    uint o = fsc_ofs_of_pointer(ptr);

    if (o >= length_of_block) {
	*(char *)ptr = 0; /* cause error */
	abort();
    }

    length_of_block -= o;

    if (verbose) fprintf(stderr, "testing block %p\n", ptr);

    {	/* write by char, read by char */
	uint i;
	unsigned char *p = ptr;
	uint l = length_of_block;

	if (verbose) fprintf(stderr, "step 1 (%u)\n", l);

	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned char v = ran();
	    p[i] = v;
	    if (verbose > 1) fprintf(stderr, "%02x", v);
	}
	if (verbose > 1) fprintf(stderr, "\n");
	    
	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned char v = ran();
	    if (p[i] != v) {
		fprintf(stderr, "error: offset %d: read %x, expected %x\n", i, p[i], v);
		return 0;
	    }
	    if (verbose > 1) fprintf(stderr, "%02x", v);
	}
	if (verbose> 1) fprintf(stderr, "\n");
    }

    {	/* write by short, read by short */
	uint i;
	unsigned short *p = ptr;
	uint l = length_of_block / 2;

	if (verbose) fprintf(stderr, "step 2 (%u)\n", l);

	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned short v = ran();
	    p[i] = v;
	    if (verbose > 1) fprintf(stderr, "%x ", v);
	}
	if (verbose > 1) fprintf(stderr, "\n");
	    
	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned short v = ran();
	    if (p[i] != v) {
		fprintf(stderr, "error: offset %d: read %x, expected %x\n", i, p[i], v);
		return 0;
	    }
	    if (verbose > 1) fprintf(stderr, "%x ", v);
	}
	if (verbose> 1) fprintf(stderr, "\n");
    }

    {	/* write by int, read by int */
	uint i;
	unsigned int *p = ptr;
	uint l = length_of_block / 4;

	if (verbose) fprintf(stderr, "step 3 (%u)\n", l);

	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned v = ran();
	    p[i] = v;
	    if (verbose > 1) fprintf(stderr, "%x ", v);
	}
	if (verbose > 1) fprintf(stderr, "\n");
	    
	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned v = ran();
	    if (p[i] != v) {
		fprintf(stderr, "error: offset %d: read %x, expected %x\n", i, p[i], v);
		return 0;
	    }
	    if (verbose > 1) fprintf(stderr, "%x ", v);
	}
	if (verbose > 1) fprintf(stderr, "\n");
    }

    {	/* write by long long, read by long long */
	uint i;
	unsigned long long *p = ptr;
	uint l = length_of_block / 8;

	if (verbose) fprintf(stderr, "step 4 (%u)\n", l);

	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned long long v = ran();
	    p[i] = v;
	    if (verbose > 1) fprintf(stderr, "%llx ", v);
	}
	if (verbose > 1) fprintf(stderr, "\n");
	    
	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned long long v = ran();
	    if (p[i] != v) {
		fprintf(stderr, "error: offset %d: read %llx, expected %llx\n", i, p[i], v);
		return 0;
	    }
	    if (verbose > 1) fprintf(stderr, "%llx ", v);
	}
	if (verbose > 1) fprintf(stderr, "\n");
    }

    {
	/* write by byte, read by various */
	uint i, j;
	unsigned char *p = ptr;
	uint l = length_of_block / 8;

	if (verbose) fprintf(stderr, "step 5 (%u)\n", l);

	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned char v = ran();
	    for(j = 0; j < 8; j++) {
		p[i * 8 + j] = v + j;
		if (verbose > 1) fprintf(stderr, "%02x", v + j);
	    }
	    if (verbose > 1) fprintf(stderr, " ", v + j);
	}
	if (verbose > 1) fprintf(stderr, "\n");
	    
	seed = (int)ptr;
	for(i = 0; i < l; i++) {
	    unsigned char v = ran();
	    unsigned char *pp = &p[i * 8];
	    unsigned short *ps = (unsigned short *)pp;
	    unsigned int   *pi = (unsigned int *)pp;
	    unsigned long long *pl = (unsigned long long *)pp;
#ifdef __failsafeC_little_endian
	    unsigned short vs = v * 257 + 256;
	    unsigned int vi = 
		(((v + 3) & 255) << 24) +
		(((v + 2) & 255) << 16) +
		(((v + 1) & 255) << 8) + v;
	    unsigned long long vl = 
		((unsigned long long)((v + 7) & 255) << 56) +
		((unsigned long long)((v + 6) & 255) << 48) +
		((unsigned long long)((v + 5) & 255) << 40) +
		((unsigned long long)((v + 4) & 255) << 32) +
		((unsigned long long)((v + 3) & 255) << 24) +
		((unsigned long long)((v + 2) & 255) << 16) +
		((unsigned long long)((v + 1) & 255) << 8) + v;
#endif
#ifdef __failsafeC_big_endian
	    unsigned short vs = v * 257 + 1;
	    unsigned int vi =
		(( v      & 255) << 24)+
		(((v + 1) & 255) << 16) +
		(((v + 2) & 255) << 8) +
		 ((v + 3) & 255);
	    unsigned long long vl =
		((unsigned long long)( v      & 255) << 56) +
		((unsigned long long)((v + 1) & 255) << 48) +
		((unsigned long long)((v + 2) & 255) << 40) +
		((unsigned long long)((v + 3) & 255) << 32) +
		((unsigned long long)((v + 4) & 255) << 24) +
		((unsigned long long)((v + 5) & 255) << 16) +
		((unsigned long long)((v + 6) & 255) << 8) +
		 (unsigned long long)((v + 7) & 255);
#endif
	    if (*ps != vs)
		fprintf(stderr, "error: offset %d (s): read %x, expected %x\n", i, *ps, vs);
	    if (*pi != vi)
		fprintf(stderr, "error: offset %d (i): read %x, expected %x\n", i, *pi, vi);
	    if (*pl != vl)
		fprintf(stderr, "error: offset %d (q): read %llx, expected %llx\n", i, *pl, vl);

	    if (verbose > 1) fprintf(stderr, "[%016llx,%06x,%04x] ", vl, vi, vs);
	}
	if (verbose > 1) fprintf(stderr, "\n");
    }
	

    fprintf(stderr, "done.\n");
    return 1;
}
