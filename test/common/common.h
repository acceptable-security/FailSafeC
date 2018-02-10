/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <errno.h>
#include <signal.h>
#include <stdlib.h>

#define TEST_CASE(func)      void test_case_##func(void)
#define TEST_CASE_S(func, s) void test_case_##func(void)

extern void TEST_LOG(char *file, int line);
extern void TEST_LOG_IF(int cond, char *file, int line);
extern void check_stack(char *file, int line);

extern int test_succeeded;

#define TEST(cond)  do { \
  int cond__ = (cond); \
  check_stack(__FILE__, __LINE__); \
  TEST_LOG_IF(!cond__, __FILE__, __LINE__); \
  if (!cond__) exit(1); else test_succeeded++; \
} while (0)

#define TEST_FAILED TEST(0)

#define TEST_FAIL_IF(cond) do { if (cond) { TEST_FAILED; } } while (0)
#define TEST_CONT(cond) do { if (!cond) { TEST_FAILED; } } while (0)

#define TEST_OK(cond) do { TEST(cond); exit(0); } while (0)

#define TEST_EFAULT_IF(cond) do { if ((cond) && errno == EFAULT) abort(); } while (0)

#define TEST_FILENAME "test_temp"
#define TEST_DIRNAME  "test_temp_dir"
#define TEST_STDOUT   "test_stdout"

extern char *illegal_char_ptr_1;
extern char *illegal_char_ptr_2;
extern char *illegal_char_ptr_3;
extern char *illegal_char_ptr_4;
extern char *illegal_string;
