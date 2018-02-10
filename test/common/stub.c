/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define TEST_MAGIC 0xABADCAFEUL

#define TEST_CASE(func)      extern void test_case_##func(void);
#define TEST_CASE_S(func, s) extern void test_case_##func(void);
#include <testlist.c>
#undef  TEST_CASE
#undef  TEST_CASE_S

struct test_entry {
  void (*function)(void);
  char *name;
} test_entries[] = {
#define TEST_CASE(func)      { test_case_##func, #func },
#define TEST_CASE_S(func, s) { test_case_##func, #func },
#include <testlist.c>
  { 0, 0 }
};


char *illegal_char_ptr_1;
char *illegal_char_ptr_2;
char *illegal_char_ptr_3;
char *illegal_char_ptr_4;

static char illegal_string_src[5] = "hello";
char *illegal_string = illegal_string_src;

static void init_pointer(void)
{
  char auto_buf[] = "hello";
  char *p;
  illegal_char_ptr_1 = "hello" - 1;
  illegal_char_ptr_2 = "hello" + 6;
  illegal_char_ptr_3 = auto_buf;
  p = malloc(6);
  strcpy(p, "hello");
  free(p);
  illegal_char_ptr_4 = p;
}

static unsigned *stack_magic;

int test_succeeded = 0;

int main(int argc, char **argv)
{
  char *name = argv[1];
  int i;
  for (i = 0; test_entries[i].function; i++) {
    if (strcmp(name, test_entries[i].name) == 0) {
      unsigned magic[] = {TEST_MAGIC, TEST_MAGIC, TEST_MAGIC, TEST_MAGIC, TEST_MAGIC};
      stack_magic = magic;
      init_pointer();
      test_entries[i].function();
      return (test_succeeded ? 0 : 1);
    }
  }
  return 1;
}

void TEST_LOG(char *file, int line)
{
  fprintf(stderr, "  TEST_FAILED: %s(%d)  ", file, line);
}

void TEST_LOG_IF(int cond, char *file, int line)
{
  if (cond) {
    TEST_LOG(file, line);
  }
}

void check_stack(char *file, int line)
{
  int i;
  for (i = 0; i < 5; i++) {
    if (stack_magic[i] != TEST_MAGIC) {
      fprintf(stderr, "BUFFER OVERRUN %s(%d)  ", file, line);
      exit(2);
    }
  }
}
