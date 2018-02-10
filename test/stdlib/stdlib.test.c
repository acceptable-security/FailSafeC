/**
 * @file stdlib/stdlib.test.c
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <limits.h>
#include <sys/stat.h>
#include <unistd.h>

#include "common.h"

static int div_test(int num, int denom, int quot, int rem)
{
  div_t d;
  d = div(num, denom);
  return d.quot == quot && d.rem == rem;
}


/**
 * @testname div_1
 * @testfor div
 */
TEST_CASE(div_1)
{
  TEST_FAIL_IF(!div_test(0, 1, 0, 0));
  TEST_FAIL_IF(!div_test(1, 1, 1, 0));
  TEST_FAIL_IF(!div_test(10, 10, 1, 0));
  TEST_FAIL_IF(!div_test(19, 10, 1, 9));
  TEST_FAIL_IF(!div_test(997 * 1007 + 512, 997, 1007, 512));
  TEST(1);
}

/**
 * @testname mkstemp_1
 * @testfor mkstemp
 */
TEST_CASE(mkstemp_1)
{
  char t[] = "/tmp/fsc_testXXXXXX";
  int fd = mkstemp(t);
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(close(fd) == -1);
  TEST_FAIL_IF(strlen(t) != strlen("/tmp/fsc_testXXXXXX"));
  TEST_FAIL_IF(unlink(t) == -1);
  TEST(1);
}


/**
 * @testname realpath_1
 * @testfor realpath
 */
TEST_CASE(realpath_1)
{
  char buf[PATH_MAX];
  char *s = realpath("/usr/.././bin/./ls", buf);
  TEST_FAIL_IF(s != buf);
  TEST_FAIL_IF(strcmp(s, "/bin/ls") != 0);
  TEST(1);
}


/**
 * @testname system_1
 * @testfor system
 */
TEST_CASE(system_1)
{
  int r = system("exit 42");
  TEST(WIFEXITED(r) && WEXITSTATUS(r) == 42);
}

/**
 * @testname mkdtemp_1
 * @testfor mkdtemp
 */
TEST_CASE(mkdtemp_1)
{
  char t[] = "/tmp/fsc_testXXXXXX";
  char *p = mkdtemp(t);
  struct stat st;
  TEST_FAIL_IF(p == NULL);
  stat(t, &st);
  TEST_FAIL_IF((st.st_mode & 0777) != 0700);
  TEST_FAIL_IF(strlen(t) != strlen("/tmp/fsc_testXXXXXX"));
  TEST_FAIL_IF(rmdir(t) != 0);
  TEST(1);
}

static int ldiv_test(long num, long denom, long quot, long rem)
{
  ldiv_t d = ldiv(num, denom);
  return d.quot == quot && d.rem == rem;
}

/**
 * @testname ldiv_1
 * @testfor ldiv
 */
TEST_CASE(ldiv_1)
{
  TEST_FAIL_IF(!ldiv_test(0, 1, 0, 0));
  TEST_FAIL_IF(!ldiv_test(1, 1, 1, 0));
  TEST_FAIL_IF(!ldiv_test(1, 2, 0, 1));
  TEST_FAIL_IF(!ldiv_test(-5, 2, -2, -1));
  TEST_FAIL_IF(!ldiv_test(997 * 1007 + 512, 997, 1007, 512));
  TEST(1);
}

static int lldiv_test(long long num, long long denom, long long quot, long long rem)
{
  lldiv_t d = lldiv(num, denom);
  return d.quot == quot && d.rem == rem;
}

/**
 * @testname lldiv_1
 * @testfor lldiv
 */
TEST_CASE(lldiv_1)
{
  TEST_FAIL_IF(!lldiv_test(0, 1, 0, 0));
  TEST_FAIL_IF(!lldiv_test(1, 1, 1, 0));
  TEST_FAIL_IF(!lldiv_test(1, 2, 0, 1));
  TEST_FAIL_IF(!lldiv_test(-5, 2, -2, -1));
  TEST_FAIL_IF(!lldiv_test(997 * 1007 + 512, 997, 1007, 512));
  TEST(1);
}

/**
 * @testname mktemp_1
 * @testfor mktemp
 */
TEST_CASE(mktemp_1)
{
  char t[] = "/tmp/fsc_testXXXXXX";
  char *p = mktemp(t);
  if (p != NULL) {
    struct stat st;
    TEST_FAIL_IF(strlen(p) != strlen("/tmp/fsc_testXXXXXX"));
    TEST_FAIL_IF(stat(p, &st) == 0);
  }
  TEST(1);
}

/**
 * @testname a64l_l64a_1
 * @testfor a64l
 * @testfor l64a
 */
TEST_CASE(a64l_l64a_1)
{
  char *p;

  TEST_FAIL_IF(a64l(".\0/") != 0);

  p = l64a(0);
  TEST_FAIL_IF(strcmp(p, "") != 0);
  TEST_FAIL_IF(a64l(p) != 0);

  p = l64a(1);
  TEST_FAIL_IF(strcmp(p, "/") != 0);
  TEST_FAIL_IF(a64l(p) != 1);

  p = l64a(2);
  TEST_FAIL_IF(strcmp(p, "0") != 0);
  TEST_FAIL_IF(a64l(p) != 2);

  p = l64a(11);
  TEST_FAIL_IF(strcmp(p, "9") != 0);
  TEST_FAIL_IF(a64l(p) != 11);

  p = l64a(12);
  TEST_FAIL_IF(strcmp(p, "A") != 0);
  TEST_FAIL_IF(a64l(p) != 12);

  p = l64a(37);
  TEST_FAIL_IF(strcmp(p, "Z") != 0);
  TEST_FAIL_IF(a64l(p) != 37);

  p = l64a(38);
  TEST_FAIL_IF(strcmp(p, "a") != 0);
  TEST_FAIL_IF(a64l(p) != 38);

  p = l64a(63);
  TEST_FAIL_IF(strcmp(p, "z") != 0);
  TEST_FAIL_IF(a64l(p) != 63);

  p = l64a(64);
  TEST_FAIL_IF(strcmp(p, "./") != 0);
  TEST_FAIL_IF(a64l(p) != 64);

  p = l64a(140);
  TEST_FAIL_IF(strcmp(p, "A0") != 0);
  TEST_FAIL_IF(a64l(p) != 140);

  TEST(1);
}

/**
 * @testname swab_1
 * @testfor swab
 */
TEST_CASE(swab_1)
{
  char *src = "abcdefghijklmn";
  char dst[] = "opqrstuvwxyz.-";
  swab(src, dst, -1);
  TEST_FAIL_IF(strcmp(dst, "opqrstuvwxyz.-") != 0);
  swab(src, dst, 0);
  TEST_FAIL_IF(strcmp(dst, "opqrstuvwxyz.-") != 0);
  swab(src, dst, 8);
  TEST_FAIL_IF(strcmp(dst, "badcfehgwxyz.-") != 0);
  swab(src, dst, 14);
  TEST_FAIL_IF(strcmp(dst, "badcfehgjilknm") != 0);
  TEST(1);
}

/**
 * @testname swab_1s
 * @testfor swab
 */
TEST_CASE_S(swab_1s, FSC_ABRT)
{
  swab("abc", NULL, 2);
  TEST_FAILED;
}

/**
 * @testname swab_2s
 * @testfor swab
 */
TEST_CASE_S(swab_2s, FSC_ABRT)
{
  char buf[16];
  swab(NULL, buf, 2);
  TEST_FAILED;
}
