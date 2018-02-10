/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdio/sscanf.test.c
 */
#include "common.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

/**
 * @testname sscanf_c_1
 * @testfor sscanf
 */
TEST_CASE(sscanf_c_1)
{
  char c[10] = "****";
  sscanf(" cc", "%3c", c);

  TEST(!strcmp(c, " cc*"));
}

/**
 * @testname sscanf_p
 * @testfor sscanf
 */
TEST_CASE(sscanf_p)
{
  void *p = &p, *q;
  char c[64];

  sprintf(c, "%p", p);
  sscanf(c, "%p", &q);
  TEST(p == q);
}

/**
 * @testname sscanf_n
 * @testfor sscanf
 */
TEST_CASE(sscanf_n)
{
  int a, b, c;
  sscanf("12345 6789", "%d %n%d", &a, &b, &c);
  TEST(a == 12345 && b == 6 && c == 6789);
}

/**
 * @testname sscanf_range_1
 * @testfor sscanf
 */
TEST_CASE(sscanf_range_1)
{
  char c[64];
  sscanf("cafebabejava", "%[abcdef]", c);
  TEST(!strcmp(c, "cafebabe"));
}

/**
 * @testname sscanf_range_2
 * @testfor sscanf
 */
TEST_CASE(sscanf_range_2)
{
  char c[64];
  sscanf("cafebabejava", "%[a-f]", c);
  TEST(!strcmp(c, "cafebabe"));
}

/**
 * @testname sscanf_range_3
 * @testfor sscanf
 */
TEST_CASE(sscanf_range_3)
{
  char c[64];
  sscanf("cafebabejava", "%[^j]", c);
  TEST(!strcmp(c, "cafebabe"));
}

/**
 * @testname sscanf_range_4
 * @testfor sscanf
 */
TEST_CASE(sscanf_range_4)
{
  char c[64];
  sscanf("cafe]babejava", "%[^]j]", c);
  TEST(!strcmp(c, "cafe"));
}

/**
 * @testname sscanf_x
 * @testfor sscanf
 */
TEST_CASE(sscanf_x)
{
  unsigned int n;
  sscanf("dEaDbEeF", "%x", &n);
  TEST(n == 0xdeadbeef);
}

static int call_vfscanf(const char *string, const char *fmt, ...)
{
  FILE *file;
  va_list va;
  int ret;
  file = fopen(TEST_FILENAME, "w+");
  fputs(string, file);
  fseek(file, 0, SEEK_SET);
  va_start(va, fmt);
  ret = vfscanf(file, fmt, va);
  va_end(va);
  fclose(file);
  remove(TEST_FILENAME);
  return ret;
}

/**
 * @testname vfscanf_i
 * @testfor vfscanf
 */
TEST_CASE(vfscanf_i)
{
  int d, o, x, X;
  call_vfscanf("42 0744 0xabc01 0Xdeaf98", "%i %i %i %i", &d, &o, &x, &X);
  TEST_FAIL_IF(d != 42);
  TEST_FAIL_IF(o != 0744);
  TEST_FAIL_IF(x != 0xabc01);
  TEST_FAIL_IF(X != 0xdeaf98);
  TEST(1);
}

/**
 * @testname vfscanf_o
 * @testfor vfscanf
 */
TEST_CASE(vfscanf_o)
{
  unsigned int o1, o2;
  call_vfscanf("123 04567", "%o %o", &o1, &o2);
  TEST_FAIL_IF(o1 != 0123);
  TEST_FAIL_IF(o2 != 04567);
  TEST(1);
}

/**
 * @testname vfscanf_h
 * @testfor vfscanf
 */
TEST_CASE(vfscanf_h)
{
  short h[2] = {0, -1};
  char hh[2] = {0, -1};
  call_vfscanf("-12345 67", "%hd %hhd", &h[0], &hh[0]);
  TEST_FAIL_IF(h[0] != -12345);
  TEST_FAIL_IF(h[1] != -1);
  TEST_FAIL_IF(hh[0] != 67);
  TEST_FAIL_IF(hh[1] != (char)-1);
  TEST(1);
}

/**
 * @testname vfscanf_l
 * @testfor vfscanf_l
 */
TEST_CASE(vfscanf_l)
{
  long l;
  long long ll;
  sscanf("65536 81985529216486895", "%ld %Ld", &l, &ll);
  TEST_FAIL_IF(l != 65536L);
  TEST_FAIL_IF(ll != 0x123456789abcdefLL);
  TEST(1);
}

/**
 * @testname vfscanf_f
 * @testfor vfscanf
 */
TEST_CASE(vfscanf_f)
{
  float e, E, f, F, g, G, n[3] = {0.0f, 0.0f, 1.0f};
  call_vfscanf("+1.0 -1.0 0.0 -0.0 INF -inf nan -NaN", "%e %E %f %F %g %G %f %f", &e, &E, &f, &F, &g, &G, &n[0], &n[1]);
  TEST_FAIL_IF(e != 1.0f);
  TEST_FAIL_IF(E != -1.0f);
  TEST_FAIL_IF(f != 0.0f);
  TEST_FAIL_IF(F != -0.0f);
  TEST_FAIL_IF(g != (float)(1.0f/0.0f));
  TEST_FAIL_IF(G != (float)(-1.0f/0.0f));
  TEST_FAIL_IF(n[0] == n[0]);
  TEST_FAIL_IF(n[1] == n[1]);
  TEST_FAIL_IF(n[2] != 1.0f);
  TEST(1);
}

/**
 * @testname vfscanf_lf
 * @testfor vfscanf
 */
TEST_CASE(vfscanf_lf)
{
  double e, E, f, F, g, G, n[3] = {0.0, 0.0, 1.0};
  call_vfscanf("+1.0 -1.0 0.0 -0.0 INF -inf nan -NaN", "%le %lE %lf %lF %lg %lG %lf %lf", &e, &E, &f, &F, &g, &G, &n[0], &n[1]);
  TEST_FAIL_IF(e != 1.0);
  TEST_FAIL_IF(E != -1.0);
  TEST_FAIL_IF(f != 0.0);
  TEST_FAIL_IF(F != -0.0);
  TEST_FAIL_IF(g != 1.0/0.0);
  TEST_FAIL_IF(G != -1.0/0.0);
  TEST_FAIL_IF(n[0] == n[0]);
  TEST_FAIL_IF(n[1] == n[1]);
  TEST_FAIL_IF(n[2] != 1.0);
  TEST(1);
}

/**
 * @testname vfscanf_s
 * @testfor vfscanf
 */
TEST_CASE(vfscanf_s)
{
  char buf[5][16] = {"123456789abcdef", "", "", "", ""};
  call_vfscanf("abcdefg hijklmn\topqrstu\nvwxyz", "%4s%s %s %s %s", buf[0], buf[1], buf[2], buf[3], buf[4]);
  TEST_FAIL_IF(strcmp(buf[0], "abcd") != 0);
  TEST_FAIL_IF(strcmp(buf[1], "efg") != 0);
  TEST_FAIL_IF(strcmp(buf[2], "hijklmn") != 0);
  TEST_FAIL_IF(strcmp(buf[3], "opqrstu") != 0);
  TEST_FAIL_IF(strcmp(buf[4], "vwxyz") != 0);
  TEST(1);
}
