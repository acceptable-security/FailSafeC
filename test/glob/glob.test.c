#include "common.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glob.h>
#include <sys/stat.h>
#include <sys/types.h>

#define TESTDIR "/tmp/failsafe_c_glob_test_dir"
#define TESTDIR2 "/tmp/failsafe_c_glob_test_dir2"
#define TESTDIR3 "/tmp/failsafe_c_glob_test_dir3"

int setup_dir()
{
  system(TEST_ROOT_DIR "/glob/setup_dir.sh");
}

int cleanup_dir()
{
  system(TEST_ROOT_DIR "/glob/cleanup_dir.sh");
}

/**
 * @testname glob_dir
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_dir)
{
  glob_t buf;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR, 0, NULL, &buf));
  TEST_FAIL_IF(1 != buf.gl_pathc);
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[0], TESTDIR));
  TEST_FAIL_IF(NULL != buf.gl_pathv[1]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_files
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_files)
{
  glob_t buf;
  int foo = 0, bar = 0, baz = 0, hoge = 0, piyo = 0;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR "/*", 0, NULL, &buf));
  TEST_FAIL_IF(5 != buf.gl_pathc);
  for(i = 0; i < 5; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo  = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar  = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz  = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo = 1; }
  }
  TEST_FAIL_IF(!(foo && bar && baz && hoge && piyo));
  TEST_FAIL_IF(NULL != buf.gl_pathv[5]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_files_starts_with_b
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_files_starts_with_b)
{
  glob_t buf;
  int bar = 0, baz = 0;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR "/b*", 0, NULL, &buf));
  TEST_FAIL_IF(2 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar  = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz  = 1; }
  }
  TEST_FAIL_IF(!(bar && baz));
  TEST_FAIL_IF(NULL != buf.gl_pathv[2]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_files_contains_o
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_files_contains_o)
{
  glob_t buf;
  int foo = 0, hoge = 0, piyo = 0;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR "/*o*", 0, NULL, &buf));
  TEST_FAIL_IF(3 != buf.gl_pathc);
  for(i = 0; i < 3; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo  = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge = 1; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo = 1; }
  }
  TEST_FAIL_IF(!(foo && hoge && piyo));
  TEST_FAIL_IF(NULL != buf.gl_pathv[3]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_append
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_append)
{
  glob_t buf;
  int foo = 0, bar = 0, baz = 0, hoge = 0, piyo = 0;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR "/b*",  0, NULL, &buf));
  TEST_FAIL_IF(2 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[2]);

  foo = bar = baz = hoge = piyo = 0;
  TEST_FAIL_IF(0 != glob(TESTDIR "/*o*", GLOB_APPEND, NULL, &buf));
  TEST_FAIL_IF(5 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  for(i = 2; i < 5; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 1 && hoge == 1 && piyo == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[5]);

  foo = bar = baz = hoge = piyo = 0;
  TEST_FAIL_IF(0 != glob(TESTDIR "/*", GLOB_APPEND, NULL, &buf));
  TEST_FAIL_IF(10 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  for(i = 2; i < 5; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 1 && hoge == 1 && piyo == 1));
  for(i = 5; i < 10; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 2 && bar == 2 && baz == 2 && hoge == 2 && piyo == 2));
  TEST_FAIL_IF(NULL != buf.gl_pathv[10]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_dooffs
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_dooffs)
{
  glob_t buf;
  int foo = 0, bar = 0, baz = 0, hoge = 0, piyo = 0;
  int i;

  cleanup_dir();
  setup_dir();

  buf.gl_offs = 5;
  TEST_FAIL_IF(0 != glob(TESTDIR "/b*", GLOB_DOOFFS, NULL, &buf));
  TEST_FAIL_IF(2 != buf.gl_pathc);
  for(i = 0; i < 5; i++){
    TEST_FAIL_IF(buf.gl_pathv[i] != NULL);
  }
  for(i = 5; i < 7; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[7]);

  foo = bar = baz = hoge = piyo = 0;
  TEST_FAIL_IF(0 != glob(TESTDIR "/*o*", GLOB_APPEND | GLOB_DOOFFS, NULL, &buf));
  TEST_FAIL_IF(5 != buf.gl_pathc);
  for(i = 0; i < 5; i++){
    TEST_FAIL_IF(buf.gl_pathv[i] != NULL);
  }
  for(i = 5; i < 7; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  for(i = 7; i < 10; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 1 && hoge == 1 && piyo == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[10]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_mark
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_mark)
{
  glob_t buf;
  int bar = 0, baz = 0;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR "/b*", GLOB_MARK, NULL, &buf));
  TEST_FAIL_IF(2 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[2]);

  bar = baz = 0;
  TEST_FAIL_IF(0 != glob(TESTDIR, GLOB_APPEND | GLOB_MARK, NULL, &buf));
  TEST_FAIL_IF(3 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[2], TESTDIR "/"));
  TEST_FAIL_IF(NULL != buf.gl_pathv[3]);

  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_nocheck
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_nocheck)
{
  glob_t buf;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(GLOB_NOMATCH != glob(TESTDIR "/no-such-file", 0, NULL, &buf));

  TEST_FAIL_IF(0 != glob(TESTDIR "/no-such-file", GLOB_NOCHECK, NULL, &buf));
  TEST_FAIL_IF(1 != buf.gl_pathc);
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[0], TESTDIR "/no-such-file"));
  TEST_FAIL_IF(NULL != buf.gl_pathv[1]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_noescape
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_noescape)
{
  glob_t buf;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(GLOB_NOMATCH != glob(TESTDIR2 "/\\?", 0, NULL, &buf));

  TEST_FAIL_IF(0 != glob(TESTDIR2 "/\\?", GLOB_NOESCAPE, NULL, &buf));
  TEST_FAIL_IF(1 != buf.gl_pathc);
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[0], TESTDIR2 "/\\a"));
  TEST_FAIL_IF(NULL != buf.gl_pathv[1]);
  globfree(&buf);

  TEST_FAIL_IF(0 != glob(TESTDIR2 "/\\a", 0, NULL, &buf));
  TEST_FAIL_IF(1 != buf.gl_pathc);
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[0], TESTDIR2 "/a"));
  TEST_FAIL_IF(NULL != buf.gl_pathv[1]);
  globfree(&buf);

  TEST_FAIL_IF(0 != glob(TESTDIR2 "/\\a", GLOB_NOESCAPE, NULL, &buf));
  TEST_FAIL_IF(1 != buf.gl_pathc);
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[0], TESTDIR2 "/\\a"));
  TEST_FAIL_IF(NULL != buf.gl_pathv[1]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_nosort
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_nosort)
{
  /* this test is same as glob_append */
  glob_t buf;
  int foo = 0, bar = 0, baz = 0, hoge = 0, piyo = 0;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(0 != glob(TESTDIR "/b*",  GLOB_NOSORT, NULL, &buf));
  TEST_FAIL_IF(2 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[2]);

  foo = bar = baz = hoge = piyo = 0;
  TEST_FAIL_IF(0 != glob(TESTDIR "/*o*", GLOB_NOSORT | GLOB_APPEND, NULL, &buf));
  TEST_FAIL_IF(5 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  for(i = 2; i < 5; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 1 && hoge == 1 && piyo == 1));
  TEST_FAIL_IF(NULL != buf.gl_pathv[5]);

  foo = bar = baz = hoge = piyo = 0;
  TEST_FAIL_IF(0 != glob(TESTDIR "/*", GLOB_NOSORT | GLOB_APPEND, NULL, &buf));
  TEST_FAIL_IF(10 != buf.gl_pathc);
  for(i = 0; i < 2; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
  }
  TEST_FAIL_IF(!(bar == 1 && baz == 1));
  for(i = 2; i < 5; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 1 && hoge == 1 && piyo == 1));
  for(i = 5; i < 10; i++){
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/foo")) { foo++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/bar")) { bar++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/baz")) { baz++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/hoge")){ hoge++; }
    if(!strcmp(buf.gl_pathv[i], TESTDIR "/piyo")){ piyo++; }
  }
  TEST_FAIL_IF(!(foo == 2 && bar == 2 && baz == 2 && hoge == 2 && piyo == 2));
  TEST_FAIL_IF(NULL != buf.gl_pathv[10]);
  globfree(&buf);

  cleanup_dir();

  TEST(1);
}

static int handler1_invoked = 0;

static int glob_errfunc_handler1(const char *epath, int eerrno)
{
  handler1_invoked++;
  TEST_FAIL_IF(strcmp(TESTDIR3 "/foo", epath));
  TEST_FAIL_IF(eerrno != EACCES);
  return 0;
}

static int handler2_invoked = 0;

static int glob_errfunc_handler2(const char *epath, int eerrno)
{
  handler2_invoked++;
  TEST_FAIL_IF(strcmp(TESTDIR3 "/bar", epath));
  TEST_FAIL_IF(eerrno != EACCES);
  return 1;
}

static int handler3_invoked = 0;

static int glob_errfunc_handler3(const char *epath, int eerrno)
{
  handler3_invoked++;
  TEST_FAIL_IF(
    strcmp(TESTDIR3 "/foo", epath) && strcmp(TESTDIR3 "/bar", epath)
    );
  TEST_FAIL_IF(eerrno != EACCES);
  return 0;
}

/**
 * @testname glob_errfunc
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_errfunc)
{
  glob_t buf;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(GLOB_NOMATCH != glob(TESTDIR3 "/foo/*", 0, glob_errfunc_handler1, &buf));
  TEST_FAIL_IF(1 != handler1_invoked);

  TEST_FAIL_IF(0 != glob(TESTDIR3 "/foo/*", GLOB_NOCHECK, glob_errfunc_handler1, &buf));
  TEST_FAIL_IF(1 != buf.gl_pathc);
  TEST_FAIL_IF(0 != strcmp(buf.gl_pathv[0], TESTDIR3 "/foo/*"));
  TEST_FAIL_IF(NULL != buf.gl_pathv[1]);
  TEST_FAIL_IF(2 != handler1_invoked);
  globfree(&buf);

  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/bar/*", 0, glob_errfunc_handler2, &buf));
  TEST_FAIL_IF(1 != handler2_invoked);

  TEST_FAIL_IF(GLOB_NOMATCH != glob(TESTDIR3 "/*/*", 0, glob_errfunc_handler3, &buf));
  TEST_FAIL_IF(2 != handler3_invoked);

  cleanup_dir();

  TEST(1);
}

/**
 * @testname glob_err
 * @testfor glob
 * @testfor globfree
 */
TEST_CASE(glob_err)
{
  glob_t buf;
  int i;

  cleanup_dir();
  setup_dir();

  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/foo/*", GLOB_ERR, NULL, &buf));
  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/foo/*", GLOB_NOCHECK | GLOB_ERR, NULL, &buf));
  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/bar/*", GLOB_ERR, NULL, &buf));
  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/*/*", GLOB_ERR, NULL, &buf));

  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/foo/*", GLOB_ERR, glob_errfunc_handler1, &buf));
  TEST_FAIL_IF(1 != handler1_invoked);

  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/foo/*", GLOB_NOCHECK | GLOB_ERR, glob_errfunc_handler1, &buf));
  TEST_FAIL_IF(2 != handler1_invoked);

  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/bar/*", GLOB_ERR, glob_errfunc_handler2, &buf));
  TEST_FAIL_IF(1 != handler2_invoked);

  TEST_FAIL_IF(GLOB_ABORTED != glob(TESTDIR3 "/*/*", GLOB_ERR, glob_errfunc_handler3, &buf));
  TEST_FAIL_IF(1 != handler3_invoked);

  cleanup_dir();

  TEST(1);
}
