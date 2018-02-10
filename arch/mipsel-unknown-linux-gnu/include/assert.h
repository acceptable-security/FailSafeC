/* Generated file -- do not edit. */
/**
 * @file include/assert.h
 */
#ifndef __ASSERT_H
#define __ASSERT_H

#ifdef NDEBUG
#define assert(x) (void)0
#else
extern void abort(void);
#define assert(x) (((x) || (abort(), 1)), (void) 0)
#endif

#endif
