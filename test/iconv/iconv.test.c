#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <iconv.h>

#include "common.h"

/**
 * @testname iconv_1
 * @testfor iconv_open
 */
TEST_CASE(iconv_1)
{
  TEST(iconv_open("no such encoding", "no such encoding") == (iconv_t)-1);
}

static int read_file(char *dst, char *filename, int len)
{
  int fd = open(filename, O_RDONLY);
  int sz;
  if (fd < 0) return -1;
  sz = read(fd, dst, len);
  if (sz >= 0) {
    dst[sz] = 0;
  }
  close(fd);
  return sz;
}

static char euc_file[2048];
static char utf8_file[2048];
static int euc_len;
static int utf8_len;

static int read_text_files()
{
  euc_len = read_file(euc_file,  TEST_ROOT_DIR "/iconv/oeyama_euc.txt",  2048);
  utf8_len = read_file(utf8_file, TEST_ROOT_DIR "/iconv/oeyama_utf8.txt", 2048);
  return euc_len == 684 && utf8_len == 1023;
}

/**
 * @testname iconv_2
 * @testfor iconv
 * @testfor iconv_open
 * @testfor iconv_close
 */
TEST_CASE(iconv_2)
{
  iconv_t cd = iconv_open("utf-8", "euc-jp");
  TEST_FAIL_IF(cd == (iconv_t)-1);
  TEST_FAIL_IF(!read_text_files());
  {
    char *inp = euc_file;
    size_t inp_len = euc_len;
    char outbuf[2048];
    char *outp = outbuf;
    size_t outp_len = 2048;

    while (inp_len > 0) {
      size_t r = iconv(cd, &inp, &inp_len, &outp, &outp_len);
      TEST_FAIL_IF(r == (size_t)-1);
    }
    *outp = 0;
    TEST_FAIL_IF(outp_len + utf8_len != 2048);
    TEST_FAIL_IF(strcmp(outbuf, utf8_file) != 0);
  }
  TEST_FAIL_IF(iconv_close(cd) != 0);
  TEST(1);
}


/**
 * @testname iconv_3
 * @testfor iconv
 * @testfor iconv_open
 * @testfor iconv_close
 */
TEST_CASE(iconv_3)
{
  iconv_t cd = iconv_open("utf-8", "euc-jp");
  TEST_FAIL_IF(cd == (iconv_t)-1);
  TEST_FAIL_IF(!read_text_files());
  {
    char *inp = euc_file;
    size_t inp_len = euc_len;
    char outbuf_large[2048] = {0};

    while (inp_len > 0) {
      char outbuf[33];
      char *outp = outbuf;
      size_t outp_len = 32;
      size_t r = iconv(cd, &inp, &inp_len, &outp, &outp_len);
      TEST_FAIL_IF(outp - outbuf + outp_len != 32);
      *outp = 0;
      strcat(outbuf_large, outbuf);
    }
    TEST_FAIL_IF(strcmp(outbuf_large, utf8_file) != 0);
  }
  TEST_FAIL_IF(iconv_close(cd) != 0);
  TEST(1);
}
