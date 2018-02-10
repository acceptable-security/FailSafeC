/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdio/stdfile.test.c
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "common.h"

static FILE *file;
static char unterminated[2] = "r+";
static char hoge[] = "hoge";
static char *oob_str_1 = &hoge[-1];
static char *oob_str_2 = &hoge[5];

/**
 * @testname fopen_fclose_1
 * @testfor fopen
 * @testfor fclose
 */
TEST_CASE(fopen_fclose_1)
{
  file = fopen(TEST_FILENAME, "w");
  TEST_FAIL_IF(file == 0);
  TEST(fclose(file) == 0);
}

static void create_empty_file(void)
{
  test_case_fopen_fclose_1();
}

/**
 * @testname fopen_fclose_2
 * @testfor fopen
 * @testfor fclose
 */
TEST_CASE(fopen_fclose_2)
{
  file = fopen(TEST_FILENAME, "r");
  TEST(file == 0);
}

/**
 * @testname fopen_fclose_3
 * @testfor fopen
 * @testfor fclose
 */
TEST_CASE(fopen_fclose_3)
{
  create_empty_file();
  file = fopen(TEST_FILENAME, "r");
  TEST_FAIL_IF(file == 0);
  TEST(fclose(file) == 0);
}

/**
 * @testname fread_fwrite_1
 * @testfor fread
 * @testfor fwrite
 */
TEST_CASE(fread_fwrite_1)
{
  char buf[] = "hello, world!";
  file = fopen(TEST_FILENAME, "w");
  TEST_FAIL_IF(file == 0);
  TEST_FAIL_IF(fwrite(buf, 1, sizeof buf, file) != sizeof buf);
  TEST_FAIL_IF(fclose(file) != 0);
  file = fopen(TEST_FILENAME, "r");
  TEST_FAIL_IF(file ==0);
  TEST_FAIL_IF(fread(buf, sizeof buf, 1, file) != 1);
  TEST_FAIL_IF(fclose(file) != 0);
  TEST(strcmp(buf, "hello, world!") == 0);
}

/**
 * @testname fdopen_fileno
 * @testfor fdopen
 * @testfor fileno
 */
TEST_CASE(fdopen_fileno)
{
  int fd = open(TEST_FILENAME, O_WRONLY | O_CREAT, 0644);
  TEST_FAIL_IF(fd < 0);
  file = fdopen(fd, "w");
  TEST_FAIL_IF(file == 0);
  TEST_FAIL_IF(fileno(file) != fd);
  TEST_FAIL_IF(fclose(file) != 0);
  TEST(close(fd) == -1);
}

/**
 * @testname fileno_1
 * @testfor fileno
 */
TEST_CASE(fileno_1)
{
  TEST(fileno(stdin) == 0 && fileno(stdout) == 1 && fileno(stderr) == 2);
}

/**
 * @testname freopen_1
 * @testfor freopen
 */
TEST_CASE(freopen_1)
{
  file = fopen(TEST_FILENAME, "w");
  TEST_FAIL_IF(!file);
  fputc('X', file);
  TEST_FAIL_IF(freopen(TEST_FILENAME "2", "w", file) != file);
  fputc('Y', file);
  TEST_FAIL_IF(freopen(TEST_FILENAME, "r", file) != file);
  TEST_FAIL_IF(fgetc(file) != 'X');
  TEST_FAIL_IF(fgetc(file) != EOF);
  TEST_FAIL_IF(freopen(TEST_FILENAME "2", "r", file) != file);
  TEST_FAIL_IF(fgetc(file) != 'Y');
  TEST_FAIL_IF(fgetc(file) != EOF);
  TEST_FAIL_IF(fclose(file) != 0);
  TEST(1);
}

/**
 * @testname feof_clearerr_1
 * @testfor feof
 * @testfor clearerr
 */
TEST_CASE(feof_clearerr_1)
{
  file = fopen("/dev/null", "r");
  TEST_FAIL_IF(file == 0);
  TEST_FAIL_IF(feof(file) != 0);
  TEST_FAIL_IF(fgetc(file) != EOF);
  TEST_FAIL_IF(feof(file) == 0);
  clearerr(file);
  TEST_FAIL_IF(feof(file));
  TEST(fclose(file) == 0);
}

/**
 * @testname ferror_fflush_clearerr_1
 * @testfor ferror
 * @testfor fflush
 * @testfor clearerr
 */
TEST_CASE(ferror_fflush_clearerr_1)
{
  file = fopen("/dev/full", "w");
  TEST_FAIL_IF(file == 0);
  TEST_FAIL_IF(ferror(file) != 0);
  fputc('a', file);
  TEST_FAIL_IF(fflush(file) != EOF);
  TEST_FAIL_IF(ferror(file) == 0 || errno != ENOSPC);
  clearerr(file);
  TEST_FAIL_IF(ferror(file));
  TEST(fclose(file) == 0);
}

/**
 * @testname fflush_2
 * @testfor fflush
 */
TEST_CASE(fflush_2)
{
  TEST_FAIL_IF(fflush(NULL));
  TEST(1);
}

static void closedfile()
{
  file = fopen(TEST_FILENAME, "a");
  if (file == 0) {
    TEST_FAILED;
  }
  if (fclose(file) != 0) {
    TEST_FAILED;
  }
}

/**
 * @testname fclose_closed
 * @testfor fclose
 */
TEST_CASE_S(fclose_closed, FSC_ABRT)
{
  closedfile();
  fclose(file);
  TEST_FAILED;
}

/**
 * @testname fputs_closed
 * @testfor fputs
 */
TEST_CASE_S(fputs_closed, FSC_ABRT)
{
  closedfile();
  fputs("hoge", file);
  TEST_FAILED;
}

/**
 * @testname fputc_closed
 * @testfor fputc
 */
TEST_CASE_S(fputc_closed, FSC_ABRT)
{
  closedfile();
  fputc('A', file);
  TEST_FAILED;
}

/**
 * @testname fgetc_closed
 * @testfor fgetc
 */
TEST_CASE_S(fgetc_closed, FSC_ABRT)
{
  closedfile();
  fgetc(file);
  TEST_FAILED;
}

/**
 * @testname fgets_closed
 * @testfor fgets
 */
TEST_CASE_S(fgets_closed, FSC_ABRT)
{
  char buf[256];
  closedfile();
  fgets(buf, sizeof buf, file);
  TEST_FAILED;
}

/**
 * @testname ungetc_closed
 * @testfor ungetc
 */
TEST_CASE_S(ungetc_closed, FSC_ABRT)
{
  closedfile();
  ungetc('A', file);
  TEST_FAILED;
}


/**
 * @testname fprintf_closed
 * @testfor fprintf
 */
TEST_CASE_S(fprintf_closed, FSC_ABRT)
{
  closedfile();
  fprintf(file, "hoge");
  TEST_FAILED;
}

/**
 * @testname ferror_closed
 * @testfor ferror
 */
TEST_CASE_S(ferror_closed, FSC_ABRT)
{
  closedfile();
  ferror(file);
  TEST_FAILED;
}

/**
 * @testname fscanf_closed
 * @testfor fscanf
 */
TEST_CASE_S(fscanf_closed, FSC_ABRT)
{
  closedfile();
  fscanf(file, "hoge");
  TEST_FAILED;
}

/**
 * @testname fseek_closed
 * @testfor fseek
 */
TEST_CASE_S(fseek_closed, FSC_ABRT)
{
  closedfile();
  fseek(file, 0 , SEEK_CUR);
  TEST_FAILED;
}
/**
 * @testname ftell_closed
 * @testfor ftell
 */
TEST_CASE_S(ftell_closed, FSC_ABRT)
{
  closedfile();
  ftell(file);
  TEST_FAILED;
}
/**
 * @testname fraed_closed
 * @testfor fraed
 */
TEST_CASE_S(fraed_closed, FSC_ABRT)
{
  char buf[256];
  closedfile();
  fread(buf, 1, 256, file);
  TEST_FAILED;
}
/**
 * @testname fwrite_closed
 * @testfor fwrite
 */
TEST_CASE_S(fwrite_closed, FSC_ABRT)
{
  char buf[256];
  closedfile();
  fwrite(buf, 1, 256, file);
  TEST_FAILED;
}
/**
 * @testname pclose_closed
 * @testfor pclose
 */
TEST_CASE_S(pclose_closed, FSC_ABRT)
{
  closedfile();
  pclose(file);
  TEST_FAILED;
}

/**
 * @testname fopen_1_1s
 * @testfor fopen
 */
TEST_CASE_S(fopen_1_1s, FSC_ABRT)
{
  fopen(illegal_char_ptr_1, "r");
}

/**
 * @testname fopen_1_2s
 * @testfor fopen
 */
TEST_CASE_S(fopen_1_2s, FSC_ABRT)
{
  fopen(illegal_char_ptr_2, "r");
}

/**
 * @testname fopen_1_3s
 * @testfor fopen
 */
TEST_CASE_S(fopen_1_3s, FSC_ABRT)
{
  fopen(illegal_char_ptr_3, "r");
}

/**
 * @testname fopen_1_4s
 * @testfor fopen
 */
TEST_CASE_S(fopen_1_4s, FSC_ABRT)
{
  fopen(illegal_char_ptr_4, "r");
}

/**
 * @testname fopen_1_5s
 * @testfor fopen
 */
TEST_CASE_S(fopen_1_5s, FSC_ABRT)
{
  fopen(illegal_string, "r");
}

/**
 * @testname fopen_1_6s
 * @testfor fopen
 */
TEST_CASE_S(fopen_1_6s, FSC_ABRT)
{
  fopen(0, "r");
}


/**
 * @testname fopen_2_1s
 * @testfor fopen
 */
TEST_CASE_S(fopen_2_1s, FSC_ABRT)
{
  fopen(TEST_FILENAME, illegal_char_ptr_1);
}

/**
 * @testname fopen_2_2s
 * @testfor fopen
 */
TEST_CASE_S(fopen_2_2s, FSC_ABRT)
{
  fopen(TEST_FILENAME, illegal_char_ptr_2);
}

/**
 * @testname fopen_2_3s
 * @testfor fopen
 */
TEST_CASE_S(fopen_2_3s, FSC_ABRT)
{
  fopen(TEST_FILENAME, illegal_char_ptr_3);
}

/**
 * @testname fopen_2_4s
 * @testfor fopen
 */
TEST_CASE_S(fopen_2_4s, FSC_ABRT)
{
  fopen(TEST_FILENAME, illegal_char_ptr_4);
}

/**
 * @testname fopen_2_5s
 * @testfor fopen
 */
TEST_CASE_S(fopen_2_5s, FSC_ABRT)
{
  fopen(TEST_FILENAME, illegal_string);
}

/**
 * @testname fopen_2_6s
 * @testfor fopen
 */
TEST_CASE_S(fopen_2_6s, FSC_ABRT)
{
  fopen(TEST_FILENAME, 0);
}

/**
 * @testname rename_1
 * @testfor rename
 */
TEST_CASE(rename_1)
{
  FILE *fp;

  fp = fopen(TEST_FILENAME, "w");
  TEST_FAIL_IF(fp == NULL);
  fclose(fp);

  TEST_FAIL_IF(
    rename(TEST_FILENAME, TEST_FILENAME ".renamed")
    );

  fp = fopen(TEST_FILENAME, "r");
  TEST_FAIL_IF(fp != NULL);

  fp = fopen(TEST_FILENAME ".renamed", "r");
  TEST_FAIL_IF(fp == NULL);
  fclose(fp);
  
  TEST_FAIL_IF(
    rename(TEST_FILENAME ".renamed", TEST_FILENAME)
    );

  fp = fopen(TEST_FILENAME ".renamed", "r");
  TEST_FAIL_IF(fp != NULL);

  fp = fopen(TEST_FILENAME, "r");
  TEST_FAIL_IF(fp == NULL);
  fclose(fp);

  TEST(1);
}

/**
 * @testname setvbuf_1
 * @testfor setvbuf
 */
TEST_CASE(setvbuf_1)
{
  struct stat s;
  FILE *fp = fopen(TEST_FILENAME, "wb");

  TEST_FAIL_IF(setvbuf(fp, NULL, _IOLBF, 1024));

  fwrite("foo", 3, 1, fp);
  stat(TEST_FILENAME, &s);
  TEST_FAIL_IF(s.st_size != 0);

  fwrite("foo\n", 4, 1, fp);
  stat(TEST_FILENAME, &s);
  TEST_FAIL_IF(s.st_size != 7);

  fclose(fp);
  TEST(1);
}

/**
 * @testname setvbuf_2
 * @testfor setvbuf
 */
TEST_CASE(setvbuf_2)
{
  struct stat s;
  FILE *fp = fopen(TEST_FILENAME, "wb");

  TEST_FAIL_IF(setvbuf(fp, NULL, _IONBF, 1024));

  fwrite("foo", 3, 1, fp);
  stat(TEST_FILENAME, &s);
  TEST_FAIL_IF(s.st_size != 3);

  fwrite("foo\n", 4, 1, fp);
  stat(TEST_FILENAME, &s);
  TEST_FAIL_IF(s.st_size != 7);

  fclose(fp);
  TEST(1);
}

/**
 * @testname setvbuf_3
 * @testfor setvbuf
 */
TEST_CASE(setvbuf_3)
{
  struct stat s;
  FILE *fp = fopen(TEST_FILENAME, "wb");

  TEST_FAIL_IF(setvbuf(fp, NULL, _IOFBF, 1024));

  fwrite("foo", 3, 1, fp);
  stat(TEST_FILENAME, &s);
  TEST_FAIL_IF(s.st_size != 0);

  fwrite("foo\n", 4, 1, fp);
  stat(TEST_FILENAME, &s);
  TEST_FAIL_IF(s.st_size != 0);

  fclose(fp);
  TEST(1);
}

/**
 * @testname fgetpos_fsetpos_1
 * @testfor fgetpos
 * @testfor fsetpos
 */
TEST_CASE(fgetpos_fsetpos_1)
{
  FILE *fp = fopen(TEST_FILENAME, "wb");
  fpos_t pos1;
  fpos_t pos2;
  memset(&pos1, 0, sizeof(fpos_t));
  memset(&pos2, 0, sizeof(fpos_t));
  TEST_FAIL_IF(fgetpos(fp, &pos1) != 0);
  TEST_FAIL_IF(fsetpos(fp, &pos1) != 0);
  TEST_FAIL_IF(fgetpos(fp, &pos2) != 0);
  TEST_FAIL_IF(memcmp(&pos1, &pos2, sizeof(fpos_t)) != 0);
  fseek(fp, 100, SEEK_SET);
  TEST_FAIL_IF(fgetpos(fp, &pos1) != 0);
  TEST_FAIL_IF(memcmp(&pos1, &pos2, sizeof(fpos_t)) == 0);
  TEST_FAIL_IF(fsetpos(fp, &pos2) != 0);
  TEST_FAIL_IF(fgetpos(fp, &pos1) != 0);
  TEST_FAIL_IF(memcmp(&pos1, &pos2, sizeof(fpos_t)) != 0);
  fclose(fp);
  TEST(1);
}

/**
 * @testname rewind_1
 * @testfor rewind
 */
TEST_CASE(rewind_1)
{
  file = fopen(TEST_FILENAME, "wb");
  fseek(file, 100, SEEK_SET);
  TEST_FAIL_IF(ftell(file) != 100);
  rewind(file);
  TEST_FAIL_IF(ftell(file) != 0);
  fclose(file);
  TEST(1);
}


/**
 * @testname ctermid_1
 * @testfor ctermid
 */
TEST_CASE(ctermid_1)
{
  char *s1, *s2, buf[L_ctermid];

  s1 = ctermid(NULL);
  s2 = ctermid(buf);
  TEST_FAIL_IF(!s1 != !s2);
  TEST_FAIL_IF(s1 && strcmp(s1, s2) != 0);
  TEST(1);
}


/**
 * @testname setbuf_1
 * @testfor setbuf
 */
TEST_CASE(setbuf_1)
{
  file = fopen(TEST_FILENAME, "w");
  setbuf(file, NULL);
  TEST(1);
}


/**
 * @testname tmpnam_1
 * @testfor tmpnam
 */
TEST_CASE(tmpnam_1)
{
  char buf[L_tmpnam];
  char *p;
  p = tmpnam(buf);
  TEST_FAIL_IF(p != buf);
  TEST_FAIL_IF(strlen(p) == 0);
  p = tmpnam(NULL);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strlen(p) == 0);
  TEST(1);
}

void file_is_accessible(char *p)
{
  if (p != NULL)
  {
    struct stat s;
    TEST_FAIL_IF(stat(p, &s) == 0);
    file = fopen(p, "w");
    TEST_FAIL_IF(file == NULL);
    TEST_FAIL_IF(fclose(file) != 0);
    remove(p);
  }
}

/**
 * @testname tempnam_1
 * @testfor tempnam
 */
TEST_CASE(tempnam_1)
{
  char *p;
  struct stat s;

  p = tempnam(NULL, NULL);
  file_is_accessible(p);
  free(p);

  p = tempnam(NULL, "prefix");
  TEST_FAIL_IF(strstr(p, "prefi") == NULL);
  file_is_accessible(p);
  free(p);

  p = tempnam(TEST_DIRNAME, NULL);
  TEST_FAIL_IF(strncmp(p, TEST_DIRNAME, strlen(TEST_DIRNAME)) != 0);
  file_is_accessible(p);
  free(p);

  p = tempnam(TEST_DIRNAME, "prefix");
  TEST_FAIL_IF(strstr(p, "prefi") == NULL);
  TEST_FAIL_IF(strncmp(p, TEST_DIRNAME, strlen(TEST_DIRNAME)) != 0);
  file_is_accessible(p);
  free(p);

  TEST(1);
}


/**
 * @testname tmpfile_1
 * @testfor tmpfile
 */
TEST_CASE(tmpfile_1)
{
  file = tmpfile();
  TEST_FAIL_IF(file == NULL);
  TEST_FAIL_IF(fclose(file) != 0);
  TEST(1);
}

/**
 * @testname fseek_ftell_1
 * @testfor fseek
 * @testfor ftell
 */
TEST_CASE(fseek_ftell_1)
{
  FILE *fp;
  int i, r;

  fp = fopen(TEST_FILENAME, "w");
  TEST_FAIL_IF(fp == NULL);

  for(i = 0; i < 100; ++i) {
    r = fputs("01234567", fp);
    TEST_FAIL_IF(r == EOF);
  }

  TEST_FAIL_IF(fseek(fp, -30L, SEEK_END) != 0);
  TEST_FAIL_IF(ftell(fp) != 770);

  TEST_FAIL_IF(fseek(fp, -30L, SEEK_CUR) != 0);
  TEST_FAIL_IF(ftell(fp) != 740);

  TEST_FAIL_IF(fseek(fp, 30L, SEEK_SET) != 0);
  TEST_FAIL_IF(ftell(fp) != 30);

  TEST_FAIL_IF(fseek(fp, -30L, SEEK_SET) == 0);
  TEST(1);
}

/**
 * @testname fseeko_ftello_1
 * @testfor fseeko
 * @testfor ftello
 */
TEST_CASE(fseeko_ftello_1)
{
  FILE *fp;
  int i, r;

  fp = fopen(TEST_FILENAME, "w");
  TEST_FAIL_IF(fp == NULL);

  for(i = 0; i < 100; ++i) {
    r = fputs("01234567", fp);
    TEST_FAIL_IF(r == EOF);
  }

  TEST_FAIL_IF(fseeko(fp, -30, SEEK_END) != 0);
  TEST_FAIL_IF(ftello(fp) != 770);

  TEST_FAIL_IF(fseeko(fp, -30, SEEK_CUR) != 0);
  TEST_FAIL_IF(ftello(fp) != 740);

  TEST_FAIL_IF(fseeko(fp, 30, SEEK_SET) != 0);
  TEST_FAIL_IF(ftello(fp) != 30);

  TEST_FAIL_IF(fseeko(fp, -30, SEEK_SET) == 0);
  TEST(1);
}
