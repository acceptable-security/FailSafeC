/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file string/str.test.c
 */
/*
 * char *strchr(const char *s, int c);
 *
 */
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <limits.h>
#include "common.h"

static char *s;
static int c;
static char *ret;

/**
 * @testname strchr_1
 * @testfor strchr
 */
TEST_CASE(strchr_1)
{
  s = "AbcdefghabcdefgH";
  c = 'd';
  ret = strchr(s, c);
  TEST(ret == s + 3);
}

/**
 * @testname strchr_2
 * @testfor strchr
 */
TEST_CASE(strchr_2)
{
  s = "AbcdefghabcdefgH";
  c = 'D';
  ret = strchr(s, c);
  TEST(ret == 0);
}

/**
 * @testname strchr_3
 * @testfor strchr
 */
TEST_CASE(strchr_3)
{
  s = "AbcdefghabcdefgH";
  c = 0;
  ret = strchr(s, c);
  TEST(ret == s + 16);
}

/**
 * @testname strchr_4
 * @testfor strchr
 */
TEST_CASE(strchr_4)
{
  s = "AbcdefghabcdefgH";
  c = 'A';
  ret = strchr(s, c);
  TEST(ret == s);
}

/**
 * @testname strchr_5
 * @testfor strchr
 */
TEST_CASE(strchr_5)
{
  s = "AbcdefghabcdefgH";
  c = 'H';
  ret = strchr(s, c);
  TEST(ret == s + 15);
}

/**
 * @testname strrchr_1
 * @testfor strrchr
 */
TEST_CASE(strrchr_1)
{
  s = "AbcdefghabcdefgH";
  c = 'd';
  ret = strrchr(s, c);
  TEST(ret == s + 11);
}

/**
 * @testname strrchr_2
 * @testfor strrchr
 */
TEST_CASE(strrchr_2)
{
  s = "AbcdefghabcdefgH";
  c = 'D';
  ret = strrchr(s, c);
  TEST(ret == 0);
}

/**
 * @testname strrchr_3
 * @testfor strrchr
 */
TEST_CASE(strrchr_3)
{
  s = "AbcdefghabcdefgH";
  c = 0;
  ret = strrchr(s, c);
  TEST(ret == s + 16);
}

/**
 * @testname strrchr_4
 * @testfor strrchr
 */
TEST_CASE(strrchr_4)
{
  s = "AbcdefghabcdefgH";
  c = 'A';
  ret = strrchr(s, c);
  TEST(ret == s);
}

/**
 * @testname strrchr_5
 * @testfor strrchr
 */
TEST_CASE(strrchr_5)
{
  s = "AbcdefghabcdefgH";
  c = 'H';
  ret = strrchr(s, c);
  TEST(ret == s + 15);
}

static int my_memcmp(void *a, void *b, int n)
{
  unsigned char *x = a, *y = b;
  int i;
  for (i = 0; i < n; i++) {
    int e = x[i] - y[i];
    if (e != 0) return e;
  }
  return 0;
}

#define memcmp my_memcmp

static char cpy_buf[16] = "hello";

/**
 * @testname strcpy_1
 * @testfor strcpy
 */
TEST_CASE(strcpy_1)
{
  char r[16] = "foobar";
  TEST_FAIL_IF(strcpy(cpy_buf, "foobar") != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strcpy_2
 * @testfor strcpy
 */
TEST_CASE(strcpy_2)
{
  char r[16] = "bar\0o";
  TEST_FAIL_IF(strcpy(cpy_buf, "bar") != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strcpy_3
 * @testfor strcpy
 */
TEST_CASE(strcpy_3)
{
  char r[16] = "\0ello";
  TEST_FAIL_IF(strcpy(cpy_buf, "") != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strcpy_4
 * @testfor strcpy
 */
TEST_CASE(strcpy_4)
{
  char r[16] = "0123456789ABCDE";
  TEST_FAIL_IF(strcpy(cpy_buf, r) != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strncpy_1
 * @testfor strncpy
 */
TEST_CASE(strncpy_1)
{
  char r[16] = "foobar";
  TEST_FAIL_IF(strncpy(cpy_buf, "foobar", 7) != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strncpy_2
 * @testfor strncpy
 */
TEST_CASE(strncpy_2)
{
  char r[16] = "foolo";
  TEST_FAIL_IF(strncpy(cpy_buf, "foobar", 3) != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strncpy_3
 * @testfor strncpy
 */
TEST_CASE(strncpy_3)
{
  char r[16] = "hello";
  TEST_FAIL_IF(strncpy(cpy_buf, "foobar", 0) != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strncpy_4
 * @testfor strncpy
 */
TEST_CASE(strncpy_4)
{
  char r[16] = "fo\0\0o";
  TEST_FAIL_IF(strncpy(cpy_buf, "fo", 4) != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}

/**
 * @testname strncpy_5
 * @testfor strncpy
 */
TEST_CASE(strncpy_5)
{
  char r[16] = "\0\0\0\0o";
  TEST_FAIL_IF(strncpy(cpy_buf, "", 4) != cpy_buf);
  TEST(memcmp(cpy_buf, r, 16) == 0);
}


/**
 * @testname strerror_1
 * @testfor strerror
 */
TEST_CASE(strerror_1)
{
  TEST_FAIL_IF(strcmp(strerror(0), "Success") != 0);
  TEST_FAIL_IF(strcmp(strerror(EACCES), "Permission denied") != 0);
  TEST_FAIL_IF(strcmp(strerror(EINVAL), "Invalid argument") != 0);
  TEST(1);
}

/**
 * @testname strerror_2
 * @testfor strerror
 */
TEST_CASE(strerror_2)
{
  TEST(strcmp(strerror(-1), "Unknown error 4294967295") == 0);
}


/**
 * @testname strchr_1s
 * @testfor strchr
 */
TEST_CASE_S(strchr_1s, FSC_ABRT)
{
  strchr(illegal_char_ptr_1, 'A');
}

/**
 * @testname strchr_2s
 * @testfor strchr
 */
TEST_CASE_S(strchr_2s, FSC_ABRT)
{
  strchr(illegal_char_ptr_2, 'A');
}

/**
 * @testname strchr_3s
 * @testfor strchr
 */
TEST_CASE_S(strchr_3s, FSC_ABRT)
{
  strchr(illegal_char_ptr_3, 'A');
}

/**
 * @testname strchr_4s
 * @testfor strchr
 */
TEST_CASE_S(strchr_4s, FSC_ABRT)
{
  strchr(illegal_char_ptr_4, 'A');
}

/**
 * @testname strchr_5s
 * @testfor strchr
 */
TEST_CASE_S(strchr_5s, FSC_ABRT)
{
  strchr(illegal_string, 'A');
}

/**
 * @testname strchr_6s
 * @testfor strchr
 */
TEST_CASE_S(strchr_6s, FSC_ABRT)
{
  strchr(0, 'A');
}


/**
 * @testname strrchr_1s
 * @testfor strrchr
 */
TEST_CASE_S(strrchr_1s, FSC_ABRT)
{
  strrchr(illegal_char_ptr_1, 'A');
}

/**
 * @testname strrchr_2s
 * @testfor strrchr
 */
TEST_CASE_S(strrchr_2s, FSC_ABRT)
{
  strrchr(illegal_char_ptr_2, 'A');
}

/**
 * @testname strrchr_3s
 * @testfor strrchr
 */
TEST_CASE_S(strrchr_3s, FSC_ABRT)
{
  strrchr(illegal_char_ptr_3, 'A');
}

/**
 * @testname strrchr_4s
 * @testfor strrchr
 */
TEST_CASE_S(strrchr_4s, FSC_ABRT)
{
  strrchr(illegal_char_ptr_4, 'A');
}

/**
 * @testname strrchr_5s
 * @testfor strrchr
 */
TEST_CASE_S(strrchr_5s, FSC_ABRT)
{
  strrchr(illegal_string, 'A');
}

/**
 * @testname strrchr_6s
 * @testfor strrchr
 */
TEST_CASE_S(strrchr_6s, FSC_ABRT)
{
  strrchr(0, 'A');
}

/**
 * @testname strncpy_1s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_1s, FSC_ABRT)
{
  strncpy(illegal_char_ptr_1, "abc", 3);
}

/**
 * @testname strncpy_2s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_2s, FSC_ABRT)
{
  strncpy(illegal_char_ptr_2, "abc", 3);
}

/**
 * @testname strncpy_3s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_3s, FSC_ABRT)
{
  strncpy(illegal_char_ptr_3, "abc", 3);
}

/**
 * @testname strncpy_4s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_4s, FSC_ABRT)
{
  strncpy(illegal_char_ptr_4, "abc", 3);
}

/**
 * @testname strncpy_5s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_5s, FSC_ABRT)
{
  char a[10];
  strncpy(a, illegal_char_ptr_1, 3);
}

/**
 * @testname strncpy_6s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_6s, FSC_ABRT)
{
  char a[10];
  strncpy(a, illegal_char_ptr_2, 3);
}

/**
 * @testname strncpy_7s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_7s, FSC_ABRT)
{
  char a[10];
  strncpy(a, illegal_char_ptr_3, 3);
}

/**
 * @testname strncpy_8s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_8s, FSC_ABRT)
{
  char a[10];
  strncpy(a, illegal_char_ptr_4, 3);
}

/**
 * @testname strncpy_9s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_9s, FSC_ABRT)
{
  strncpy(NULL, "hello", 3);
}

/**
 * @testname strncpy_10s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_10s, FSC_ABRT)
{
  char a[10];
  strncpy(a, NULL, 3);
}

/**
 * @testname strncpy_11s
 * @testfor strncpy
 */
TEST_CASE_S(strncpy_11s, FSC_ABRT)
{
  char a[10];
  strncpy(a, illegal_string, 6);
}

/**
 * @testname strcasecmp_1
 * @testfor strcasecmp
 */
TEST_CASE(strcasecmp_1)
{
  TEST_FAIL_IF(strcasecmp("abc", "ABC") != 0);
  TEST_FAIL_IF(strcasecmp("abc", "AB") <= 0);
  TEST_FAIL_IF(strcasecmp("ab", "ABC") >= 0);

  TEST_FAIL_IF(strcasecmp("ABC", "abc") != 0);
  TEST_FAIL_IF(strcasecmp("ABC", "ab") <= 0);
  TEST_FAIL_IF(strcasecmp("AB", "abc") >= 0);

  TEST(1);
}

/**
 * @testname strcasecmp_2
 * @testfor strcasecmp
 */
TEST_CASE(strcasecmp_2)
{
  TEST_FAIL_IF(strcasecmp("\x80", "\x81") >= 0);
  TEST_FAIL_IF(strcasecmp("\xc1", "\xc0") <= 0);

  TEST_FAIL_IF(strcasecmp("\xff", "") <= 0);
  TEST_FAIL_IF(strcasecmp("a", "a\xff") >= 0);

  TEST(1);
}

/**
 * @testname strncasecmp_1
 * @testfor strncasecmp
 */
TEST_CASE(strncasecmp_1)
{
  TEST_FAIL_IF(strncasecmp("abc", "ABC", 4) != 0);
  TEST_FAIL_IF(strncasecmp("abc", "AB", 4) <= 0);
  TEST_FAIL_IF(strncasecmp("ab", "ABC", 4) >= 0);

  TEST_FAIL_IF(strncasecmp("ABC", "abc", 4) != 0);
  TEST_FAIL_IF(strncasecmp("ABC", "ab", 4) <= 0);
  TEST_FAIL_IF(strncasecmp("AB", "abc", 4) >= 0);

  TEST_FAIL_IF(strncasecmp("abc", "ABC", 3) != 0);
  TEST_FAIL_IF(strncasecmp("abc", "AB", 2) != 0);
  TEST_FAIL_IF(strncasecmp("ab", "ABC", 2) != 0);

  TEST_FAIL_IF(strncasecmp("ABC", "abc", 3) != 0);
  TEST_FAIL_IF(strncasecmp("ABC", "ab", 2) != 0);
  TEST_FAIL_IF(strncasecmp("AB", "abc", 2) != 0);

  TEST(1);
}

/**
 * @testname strncasecmp_2
 * @testfor strncasecmp
 */
TEST_CASE(strncasecmp_2)
{
  TEST_FAIL_IF(strncasecmp("\x80", "\x81", 1) >= 0);
  TEST_FAIL_IF(strncasecmp("\xc1", "\xc0", 1) <= 0);

  TEST_FAIL_IF(strncasecmp("\xff", "", 1) <= 0);
  TEST_FAIL_IF(strncasecmp("a", "a\xff", 2) >= 0);

  TEST_FAIL_IF(strncasecmp("\x80", "\x81", 0) != 0);
  TEST_FAIL_IF(strncasecmp("\xc1", "\xc0", 0) != 0);

  TEST_FAIL_IF(strncasecmp("\xff", "", 0) != 0);
  TEST_FAIL_IF(strncasecmp("a", "a\xff", 1) != 0);

  TEST(1);
}


/**
 * @testname strncat_1
 * @testfor strncat
 */
TEST_CASE(strncat_1)
{
  char buf[128], *s = "text";
  int i;

  for(i=0; i<128; i++){ buf[i] = '8'; }

  buf[0] = '\0';
  TEST_FAIL_IF(strncat(buf, s, 10) != buf);
  TEST_FAIL_IF(memcmp(buf, "text\0" "88888", 10) != 0);

  TEST_FAIL_IF(strncat(buf, s, 5) != buf);
  TEST_FAIL_IF(memcmp(buf, "texttext\0" "88888", 14) != 0);

  TEST_FAIL_IF(strncat(buf, s, 4) != buf);
  TEST_FAIL_IF(memcmp(buf, "texttexttext\0" "88888", 18) != 0);

  TEST_FAIL_IF(strncat(buf, s, 3) != buf);
  TEST_FAIL_IF(memcmp(buf, "texttexttexttex\0" "88888", 21) != 0);

  TEST_FAIL_IF(strncat(buf, s, 0) != buf);
  TEST_FAIL_IF(memcmp(buf, "texttexttexttex\0" "88888", 21) != 0);

  TEST(1);
}

/**
 * @testname strspn_1
 * @testfor strspn
 */
TEST_CASE(strspn_1)
{
  TEST_FAIL_IF(strspn("", "") != 0);
  TEST_FAIL_IF(strspn("IEEE1394", "") != 0);
  TEST_FAIL_IF(strspn("IEEE1394", "IE") != 4);
  TEST_FAIL_IF(strspn("", "IE") != 0);
  TEST_FAIL_IF(strspn("IEEE1394", "IEEE") != 4);

  TEST(1);
}

/**
 * @testname strcspn_1
 * @testfor strcspn
 */
TEST_CASE(strcspn_1)
{
  TEST_FAIL_IF(strcspn("", "") != 0);
  TEST_FAIL_IF(strcspn("IEEE1394", "") != 8);
  TEST_FAIL_IF(strcspn("IEEE1394", "13") != 4);
  TEST_FAIL_IF(strcspn("", "IE") != 0);
  TEST_FAIL_IF(strcspn("IEEE1394", "333") != 5);

  TEST(1);
}

/**
 * @testname strpbrk_1
 * @testfor strpbrk
 */
TEST_CASE(strpbrk_1)
{
  char *s = "IEEE1394";
  TEST_FAIL_IF(strpbrk("", "") != NULL);
  TEST_FAIL_IF(strpbrk(s, "") != NULL);
  TEST_FAIL_IF(strpbrk(s, "IE") != s);
  TEST_FAIL_IF(strpbrk("", "IE") != NULL);
  TEST_FAIL_IF(strpbrk(s, "931") != s + 4);

  TEST(1);
}


/**
 * @testname strtok_1
 * @testfor strtok
 */
TEST_CASE(strtok_1)
{
  char s[] = "12345+23456 * 34567";
  char *p = strtok(s, "+* ");
  TEST_FAIL_IF(strcmp(p, "12345") != 0);
  p = strtok(NULL, "+* ");
  TEST_FAIL_IF(strcmp(p, "23456") != 0);
  p = strtok(NULL, "+* ");
  TEST_FAIL_IF(strcmp(p, "34567") != 0);
  p = strtok(NULL, "+* ");
  TEST_FAIL_IF(p != NULL);
  p = strtok(NULL, "+* ");
  TEST_FAIL_IF(p != NULL);
  TEST(1);
}

/**
 * @testname strdup_1
 * @testfor strdup
 */
TEST_CASE(strdup_1)
{
  char s[] = "text";
  char *p = strdup(s);
  TEST_FAIL_IF(s == p);
  TEST_FAIL_IF(strcmp(s, p) != 0);
  free(p);
  TEST(1);
}

/**
 * @testname strdup_1s
 * @testfor strdup
 */
TEST_CASE_S(strdup_1s, FSC_ABRT)
{
  char *p = strdup(illegal_char_ptr_1);
}

/**
 * @testname strdup_2s
 * @testfor strdup
 */
TEST_CASE_S(strdup_2s, FSC_ABRT)
{
  char *p = strdup(illegal_char_ptr_2);
}

/**
 * @testname strdup_3s
 * @testfor strdup
 */
TEST_CASE_S(strdup_3s, FSC_ABRT)
{
  char *p = strdup(illegal_char_ptr_3);
}

/**
 * @testname strdup_4s
 * @testfor strdup
 */
TEST_CASE_S(strdup_4s, FSC_ABRT)
{
  char *p = strdup(illegal_char_ptr_4);
}

/**
 * @testname strdup_5s
 * @testfor strdup
 */
TEST_CASE_S(strdup_5s, FSC_ABRT)
{
  char *p = strdup(illegal_string);
}

/**
 * @testname strdup_6s
 * @testfor strdup
 */
TEST_CASE_S(strdup_6s, FSC_ABRT)
{
  char *p = strdup(NULL);
}

/**
 * @testname bcmp_1
 * @testfor bcmp
 */
TEST_CASE(bcmp_1)
{
  char s1[] = "abcdefg";
  char s2[] = "abcdefghijklmn";
  char s3[] = "opqrstu";

  TEST_FAIL_IF(bcmp(s1, s3, 0) != 0);
  TEST_FAIL_IF(bcmp(s1, s2, 3) != 0);
  TEST_FAIL_IF(bcmp(s1, s2, 7) != 0);
  TEST_FAIL_IF(bcmp(s1, s2, 8) == 0);
  TEST(1);
}

/**
 * @testname bcmp_1s
 * @testfor bcmp
 */
TEST_CASE_S(bcmp_1s, FSC_ABRT)
{
  bcmp(illegal_char_ptr_1, "", 1);
  TEST_FAILED;
}

/**
 * @testname bcmp_2s
 * @testfor bcmp
 */
TEST_CASE_S(bcmp_2s, FSC_ABRT)
{
  bcmp(illegal_char_ptr_2, "", 1);
  TEST_FAILED;
}

/**
 * @testname bcmp_3s
 * @testfor bcmp
 */
TEST_CASE_S(bcmp_3s, FSC_ABRT)
{
  bcmp(illegal_char_ptr_3, "", 1);
  TEST_FAILED;
}

/**
 * @testname bcmp_4s
 * @testfor bcmp
 */
TEST_CASE_S(bcmp_4s, FSC_ABRT)
{
  bcmp(illegal_char_ptr_4, "", 1);
  TEST_FAILED;
}

/**
 * @testname bcmp_5s
 * @testfor bcmp
 */
TEST_CASE_S(bcmp_5s, FSC_ABRT)
{
  bcmp(NULL, "", 1);
  TEST_FAILED;
}

/**
 * @testname bcmp_6s
 * @testfor bcmp
 */
TEST_CASE_S(bcmp_6s, FSC_ABRT)
{
  char s1[] = "abc";
  char s2[] = "abc";
  bcmp(s1, s2, 5);
  TEST_FAILED;
}

static int int_bits(void)
{
  int x = ~0;
  int bits = 0;
  while (x & 1) {
    x = (unsigned int)x >> 1;
    ++bits;
  }
  return bits;
}

/**
 * @testname ffs_1
 * @testfor ffs
 */
TEST_CASE(ffs_1)
{
  TEST_FAIL_IF(ffs(0) != 0);
  TEST_FAIL_IF(ffs(1) != 1);
  TEST_FAIL_IF(ffs(2) != 2);
  TEST_FAIL_IF(ffs(3) != 1);
  TEST_FAIL_IF(ffs(0x7f00) != 9);
  TEST_FAIL_IF(ffs(0x7a00) != 10);
  TEST_FAIL_IF(ffs(INT_MIN) != int_bits());
  TEST(1);
}

/**
 * @testname strcoll_1
 * @testfor strcoll
 */
TEST_CASE(strcoll_1)
{
  TEST_FAIL_IF(strcoll("abcd", "efgh") >= 0);
  TEST_FAIL_IF(strcoll("ijkl", "ijkl") != 0);
  TEST_FAIL_IF(strcoll("qrst", "mnop") <= 0);
  TEST(1);
}

/**
 * @testname strcoll_1s
 * @testfor strcoll
 */
TEST_CASE_S(strcoll_1s, FSC_ABRT)
{
  strcoll(illegal_char_ptr_1, "");
  TEST_FAILED;
}

/**
 * @testname strcoll_2s
 * @testfor strcoll
 */
TEST_CASE_S(strcoll_2s, FSC_ABRT)
{
  strcoll(illegal_char_ptr_2, "");
  TEST_FAILED;
}

/**
 * @testname strcoll_3s
 * @testfor strcoll
 */
TEST_CASE_S(strcoll_3s, FSC_ABRT)
{
  strcoll(illegal_char_ptr_3, "");
  TEST_FAILED;
}

/**
 * @testname strcoll_4s
 * @testfor strcoll
 */
TEST_CASE_S(strcoll_4s, FSC_ABRT)
{
  strcoll(illegal_char_ptr_4, "");
  TEST_FAILED;
}

/**
 * @testname strcoll_5s
 * @testfor strcoll
 */
TEST_CASE_S(strcoll_5s, FSC_ABRT)
{
  strcoll(NULL, "");
  TEST_FAILED;
}

static compare_equivalent(int r1, int r2)
{
  return (r1 == 0 && r2 == 0) || (r1 > 0 && r2 > 0) || (r1 < 0 && r2 < 0);
}

/**
 * @testname strxfrm_1
 * @testfor strxfrm
 */
TEST_CASE(strxfrm_1)
{
  char buf1[256] = {0};
  char buf2[256] = {0};
  char *str1 = "abcde";
  char *str2 = "fghij";
  TEST_FAIL_IF(strxfrm(buf1, str1, 255) == 0);
  TEST_FAIL_IF(strxfrm(buf2, str2, 255) == 0);
  TEST_FAIL_IF(!compare_equivalent(strcmp(buf1, buf2), strcoll(str1, str2)));
  TEST(1);
}

/**
 * @testname strxfrm_2
 * @testfor strxfrm
 */
TEST_CASE(strxfrm_2)
{
  char buf[256] = {0};
  char *str = "abcde";
  size_t n;
  TEST_FAIL_IF((n = strxfrm(NULL, str, 0)) == 0);
  TEST_FAIL_IF(strxfrm(buf, str, n + 1) == 0);
  TEST(1);
}

/**
 * @testname strxfrm_1s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_1s, FSC_ABRT)
{
  char buf[256];
  strxfrm(buf, illegal_char_ptr_1, 256);
  TEST_FAILED;
}

/**
 * @testname strxfrm_2s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_2s, FSC_ABRT)
{
  char buf[256];
  strxfrm(buf, illegal_char_ptr_2, 256);
  TEST_FAILED;
}

/**
 * @testname strxfrm_3s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_3s, FSC_ABRT)
{
  char buf[256];
  strxfrm(buf, illegal_char_ptr_3, 256);
  TEST_FAILED;
}

/**
 * @testname strxfrm_4s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_4s, FSC_ABRT)
{
  char buf[256];
  strxfrm(buf, illegal_char_ptr_4, 256);
  TEST_FAILED;
}

/**
 * @testname strxfrm_5s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_5s, FSC_ABRT)
{
  char buf[256];
  strxfrm(buf, illegal_string, 256);
  TEST_FAILED;
}

/**
 * @testname strxfrm_6s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_6s, FSC_ABRT)
{
  char buf[256];
  strxfrm(buf, NULL, 256);
  TEST_FAILED;
}

/**
 * @testname strxfrm_7s
 * @testfor strxfrm
 */
TEST_CASE_S(strxfrm_7s, FSC_ABRT)
{
  char buf[256];
  char *str = "abcde";
  strxfrm(NULL, str, 256);
  TEST_FAILED;
}
