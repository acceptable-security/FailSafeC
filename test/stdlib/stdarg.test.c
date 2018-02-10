#include "common.h"
#include <stdarg.h>
#include <stdio.h>

static int tc(int x, ...) {
  va_list va;
  char c;

  va_start(va, x);
  TEST_FAIL_IF('a' != va_arg(va, int));
  va_end(va);
}

/**
 * @testname va_arg_char
 * @testfor va_arg
 */
TEST_CASE(va_arg_char)
{
  tc(0, (char)'a');
  TEST(1); 
}

static int ts(int x, ...) {
  va_list va;

  va_start(va, x);
  TEST_FAIL_IF(0x1234 != va_arg(va, int));
  va_end(va);
}

/**
 * @testname va_arg_short
 * @testfor va_arg
 */
TEST_CASE(va_arg_short)
{
  ts(0, (short)0x1234);
  TEST(1); 
}

static int ti(int x, ...) {
  va_list va;

  va_start(va, x);
  TEST_FAIL_IF(0x567890ab != va_arg(va, int));
  va_end(va);
}

/**
 * @testname va_arg_int
 * @testfor va_arg
 */
TEST_CASE(va_arg_int)
{
  ti(0, 0x567890ab);
  TEST(1); 
}

static int tll(int x, ...) {
  va_list va;

  va_start(va, x);
  TEST_FAIL_IF(0x1122334455667788LL != va_arg(va, long long));
  va_end(va);
}

/**
 * @testname va_arg_long_long
 * @testfor va_arg
 */
TEST_CASE(va_arg_long_long)
{
  tll(0, 0x1122334455667788LL);
  TEST(1); 
}

const char *str = "hoge-ra";

static int tpc(int x, ...) {
  va_list va;

  va_start(va, x);
  TEST_FAIL_IF(str != va_arg(va, char*));
  va_end(va);
}

/**
 * @testname va_arg_char_star
 * @testfor va_arg
 */
TEST_CASE(va_arg_char_star)
{
  tpc(0, str);
  TEST(1); 
}

static int tf(int x, ...) {
  va_list va;

  va_start(va, x);
  TEST_FAIL_IF(1.23456F != va_arg(va, double));
  va_end(va);
}

/**
 * @testname va_arg_float
 * @testfor va_arg
 */
TEST_CASE(va_arg_float)
{
  tf(0, 1.23456F);
  TEST(1); 
}

static int td(int x, ...) {
  va_list va;

  va_start(va, x);
  TEST_FAIL_IF(1.23456 != va_arg(va, double));
  va_end(va);
}

/**
 * @testname va_arg_double
 * @testfor va_arg
 */
TEST_CASE(va_arg_double)
{
  td(0, 1.23456);
  TEST(1); 
}
