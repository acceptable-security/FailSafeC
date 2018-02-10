/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdio/sprintf.test.c
 */
#include "common.h"
#include <stdio.h>
#include <stdarg.h>

static char buf[1024];

/**
 * @testname printf_percent
 * @testfor printf
 */
TEST_CASE(printf_percent)
{
  sprintf(buf, "O%%o", 1, 2, 3, 4);
  TEST(!strcmp(buf, "O%o"));
}

/**
 * @testname printf_d_1
 * @testfor printf
 */
TEST_CASE(printf_d_1)
{
  sprintf(buf, "%d", 'C');
  TEST(!strcmp(buf, "67"));
}

/**
 * @testname printf_d_2
 * @testfor printf
 */
TEST_CASE(printf_d_2)
{
  sprintf(buf, "%d", 12345);
  TEST(!strcmp(buf, "12345"));
}

/**
 * @testname printf_d_3
 * @testfor printf
 */
TEST_CASE(printf_d_3)
{
  sprintf(buf, "%+d", 12345);
  TEST(!strcmp(buf, "+12345"));
}

/**
 * @testname printf_width_1
 * @testfor printf
 */
TEST_CASE(printf_width_1)
{
  sprintf(buf, "%10d", 12345);
  TEST(!strcmp(buf, "     12345"));
}

/**
 * @testname printf_width_2
 * @testfor printf
 */
TEST_CASE(printf_width_2)
{
  sprintf(buf, "%-10d", 12345);
  TEST(!strcmp(buf, "12345     "));
}

/**
 * @testname printf_width_3
 * @testfor printf
 */
TEST_CASE(printf_width_3)
{
  sprintf(buf, "%010d", 12345);
  TEST(!strcmp(buf, "0000012345"));
}

/**
 * @testname printf_width_4
 * @testfor printf
 */
TEST_CASE(printf_width_4)
{
  sprintf(buf, "%3d", 12345);
  TEST(!strcmp(buf, "12345"));
}

/**
 * @testname printf_width_5
 * @testfor printf
 */
TEST_CASE(printf_width_5)
{
  sprintf(buf, "%-3d", 12345);
  TEST(!strcmp(buf, "12345"));
}

/**
 * @testname printf_width_6
 * @testfor printf
 */
TEST_CASE(printf_width_6)
{
  sprintf(buf, "%d,%*d", 8, 10, 12345);
  TEST(!strcmp(buf, "8,     12345"));
}

/**
 * @testname printf_width_7
 * @testfor printf
 */
TEST_CASE(printf_width_7)
{
  sprintf(buf, "%*3$d %d %d", 12345, 8, 10, 8);
  TEST(!strcmp(buf, "     12345 8 10"));
}

/**
 * @testname printf_c_1
 * @testfor printf
 */
TEST_CASE(printf_c_1)
{
  sprintf(buf, "%c", 'a');
  TEST(!strcmp(buf, "a"));
}

/**
 * @testname printf_c_2
 * @testfor printf
 */
TEST_CASE(printf_c_2)
{
  sprintf(buf, "%c", 0x2000 + 'a');
  TEST(!strcmp(buf, "a"));
}

/**
 * @testname printf_x_1
 * @testfor printf
 */
TEST_CASE(printf_x_1)
{
  sprintf(buf, "%x", 0x12345a);
  TEST(!strcmp(buf, "12345a"));
}

/**
 * @testname printf_x_2
 * @testfor printf
 */
TEST_CASE(printf_x_2)
{
  sprintf(buf, "%#x", 0x12345a);
  TEST(!strcmp(buf, "0x12345a"));
}

/**
 * @testname printf_x_3
 * @testfor printf
 */
TEST_CASE(printf_x_3)
{
  sprintf(buf, "%#x", 0xcafebabe);
  TEST(!strcmp(buf, "0xcafebabe"));
}

/**
 * @testname printf_X_1
 * @testfor printf
 */
TEST_CASE(printf_X_1)
{
  sprintf(buf, "%X", 0x12345a);
  TEST(!strcmp(buf, "12345A"));
}

/**
 * @testname printf_X_2
 * @testfor printf
 */
TEST_CASE(printf_X_2)
{
  sprintf(buf, "%#X", 0x12345a);
  TEST(!strcmp(buf, "0X12345A"));
}

/**
 * @testname printf_X_3
 * @testfor printf
 */
TEST_CASE(printf_X_3)
{
  sprintf(buf, "%#X", 0xdeadbeef);
  TEST(!strcmp(buf, "0XDEADBEEF"));
}

/**
 * @testname printf_o_1
 * @testfor printf
 */
TEST_CASE(printf_o_1)
{
  sprintf(buf, "%o", 031416);
  TEST(!strcmp(buf, "31416"));
}

/**
 * @testname printf_o_2
 * @testfor printf
 */
TEST_CASE(printf_o_2)
{
  sprintf(buf, "%#o", 031416);
  TEST(!strcmp(buf, "031416"));
}

/**
 * @testname printf_n
 * @testfor printf
 */
TEST_CASE(printf_n)
{
  int n;
  sprintf(buf, "12345%n6789", &n);
  TEST_FAIL_IF(n != 5);
  TEST_FAIL_IF(strcmp(buf, "123456789"));
  TEST(1);
}

/**
 * @testname printf_d_zero_minus
 * @testfor printf
 */
TEST_CASE(printf_d_zero_minus)
{
  sprintf(buf, "%-05d", 123);
  TEST_FAIL_IF(strcmp(buf, "123  "));
  sprintf(buf, "%-05d", -123);
  TEST_FAIL_IF(strcmp(buf, "-123 "));
  TEST(1);
}

/**
 * @testname printf_d_precision
 * @testfor printf
 */
TEST_CASE(printf_d_precision)
{
  sprintf(buf, "%.6d", 123);
  TEST_FAIL_IF(strcmp(buf, "000123"));
  sprintf(buf, "%.6d", -123);
  TEST_FAIL_IF(strcmp(buf, "-000123"));
  TEST(1);
}

/**
 * @testname printf_d_zero_precision
 * @testfor printf
 */
TEST_CASE(printf_d_zero_precision)
{
  sprintf(buf, "%08.5d", 123);
  TEST_FAIL_IF(strcmp(buf, "   00123"));
  sprintf(buf, "%08.5d", -123);
  TEST_FAIL_IF(strcmp(buf, "  -00123"));
  TEST(1);
}

/**
 * @testname printf_d_zero_minus_precision
 * @testfor printf
 */
TEST_CASE(printf_d_zero_minus_precision)
{
  sprintf(buf, "%-08.5d", 123);
  TEST_FAIL_IF(strcmp(buf, "00123   "));
  sprintf(buf, "%-08.5d", -123);
  TEST_FAIL_IF(strcmp(buf, "-00123  "));
  TEST(1);
}

/**
 * @testname snprintf_limit_1
 * @testfor snprintf
 */
TEST_CASE(snprintf_limit_1)
{
  snprintf(NULL, 0, "x");
  TEST(1);
}

/**
 * @testname snprintf_limit_2
 * @testfor snprintf
 */
TEST_CASE(snprintf_limit_2)
{
  char x[] = "0123456789ABCDEF";
  int n = snprintf(x, 0, "...");
  TEST_FAIL_IF(n != 3);
  TEST(!strcmp(x, "0123456789ABCDEF"));
}

/**
 * @testname snprintf_limit_3
 * @testfor snprintf
 */
TEST_CASE(snprintf_limit_3)
{
  char x[] = "0123456789ABCDEF";
  int n = snprintf(x, 2, "....");
  TEST_FAIL_IF(n != 4);
  TEST(!memcmp(x, ".\0""23456789ABCDEF", 16));
}

/**
 * @testname snprintf_limit_4
 * @testfor snprintf
 */
TEST_CASE(snprintf_limit_4)
{
  char x[] = "0123456789ABCDEF";
  int n = snprintf(x, 5, "%s", "abcdefghijk");
  TEST_FAIL_IF(n != 11);
  TEST(!memcmp(x, "abcd\0""56789ABCDEF", 16));
}

/**
 * @testname snprintf_limit_5
 * @testfor snprintf
 */
TEST_CASE(snprintf_limit_5)
{
  char x[] = "0123456789ABCDEF";
  int n = snprintf(x, 10, "abcd");
  TEST_FAIL_IF(n != 4);
  TEST(!memcmp(x, "abcd\0""56789ABCDEF", 16));
}

static int call_vsnprintf(char *buf, size_t n, const char *fmt, ...)
{
  va_list va;
  int ret;
  va_start(va, fmt);
  ret = vsnprintf(buf, n, fmt, va);
  va_end(va);
  return ret;
}

/**
 * @testname vsnprintf_1
 * @testfor vsnprintf
 */
TEST_CASE(vsnprintf_1)
{
  call_vsnprintf(buf, sizeof buf, "%d%c%s", 128, 'X', "hello");
  TEST(strcmp(buf, "128Xhello") == 0);
}

/**
 * @testname vsnprintf_limit_1
 * @testfor vsnprintf
 */
TEST_CASE(vsnprintf_limit_1)
{
  call_vsnprintf(NULL, 0, "x");
  TEST(1);
}

/**
 * @testname vsnprintf_limit_2
 * @testfor vsnprintf
 */
TEST_CASE(vsnprintf_limit_2)
{
  char x[] = "0123456789ABCDEF";
  int n = call_vsnprintf(x, 0, "...");
  TEST_FAIL_IF(n != 3);
  TEST(!strcmp(x, "0123456789ABCDEF"));
}

/**
 * @testname vsnprintf_limit_3
 * @testfor vsnprintf
 */
TEST_CASE(vsnprintf_limit_3)
{
  char x[] = "0123456789ABCDEF";
  int n = call_vsnprintf(x, 2, "....");
  TEST_FAIL_IF(n != 4);
  TEST(!memcmp(x, ".\0""23456789ABCDEF", 16));
}

/**
 * @testname vsnprintf_limit_4
 * @testfor vsnprintf
 */
TEST_CASE(vsnprintf_limit_4)
{
  char x[] = "0123456789ABCDEF";
  int n = call_vsnprintf(x, 5, "%s", "abcdefghijk");
  TEST_FAIL_IF(n != 11);
  TEST(!memcmp(x, "abcd\0""56789ABCDEF", 16));
}

/**
 * @testname vsnprintf_limit_5
 * @testfor vsnprintf
 */
TEST_CASE(vsnprintf_limit_5)
{
  char x[] = "0123456789ABCDEF";
  int n = call_vsnprintf(x, 10, "abcd");
  TEST_FAIL_IF(n != 4);
  TEST(!memcmp(x, "abcd\0""56789ABCDEF", 16));
}
