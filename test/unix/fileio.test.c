/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/fileio.test.c
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

#include "common.h"

static int fd;

/**
 * @testname open_close_1
 * @testfor open
 * @testfor close
 */
TEST_CASE(open_close_1)
{
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST_FAIL_IF(fd < 0);
  TEST(close(fd) == 0);
}

/**
 * @testname creat_close_1
 * @testfor creat
 */
TEST_CASE(creat_close_1)
{
  fd = creat(TEST_FILENAME, 0644);
  TEST_FAIL_IF(fd < 0);
  TEST(close(fd) == 0);
}

/**
 * @testname open_1
 * @testfor open
 */
TEST_CASE(open_1)
{
  fd = open(TEST_FILENAME, O_WRONLY);
  TEST(fd == -1 && errno == ENOENT);
}

/**
 * @testname open_2
 * @testfor open
 */
TEST_CASE(open_2)
{
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(close(fd) != 0);
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST(fd == -1 && errno == EEXIST);
}

/**
 * @testname close_1
 * @testfor close
 */
TEST_CASE(close_1)
{
  TEST(close(100) == -1 && errno == EBADF);
}

/**
 * @testname open_write_read_close
 * @testfor open
 * @testfor write
 * @testfor read
 * @testfor close
 */
TEST_CASE(open_write_read_close)
{
  char buf[] = "hello";
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(write(fd, buf, sizeof buf) != sizeof buf);
  TEST_FAIL_IF(close(fd) != 0);
  buf[0] = 'x';
  fd = open(TEST_FILENAME, O_RDONLY);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(read(fd, buf, sizeof buf) != sizeof buf);
  TEST_FAIL_IF(close(fd) != 0);
  TEST(strcmp(buf, "hello") == 0);
}

/**
 * @testname dup_1
 * @testfor dup
 */
TEST_CASE(dup_1)
{
  char buf[] = "hello";
  fd = dup(1);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(write(fd, buf, sizeof buf) != sizeof buf);
  TEST_FAIL_IF(close(fd) != 0);
  buf[0] = 'x';
  fd = open(TEST_STDOUT, O_RDONLY);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(read(fd, buf, sizeof buf) != sizeof buf);
  TEST_FAIL_IF(close(fd) != 0);
  TEST(strcmp(buf, "hello") == 0);
}

/**
 * @testname dup2_1
 * @testfor dup2
 */
TEST_CASE(dup2_1)
{
  char buf[] = "hello";
  fd = dup2(1, 20);
  TEST_FAIL_IF(fd != 20);
  TEST_FAIL_IF(write(fd, buf, sizeof buf) != sizeof buf);
  TEST_FAIL_IF(close(fd) != 0);
  buf[0] = 'x';
  fd = open(TEST_STDOUT, O_RDONLY);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(read(fd, buf, sizeof buf) != sizeof buf);
  TEST_FAIL_IF(close(fd) != 0);
  TEST(strcmp(buf, "hello") == 0);
}

/**
 * @testname pipe_1
 * @testfor pipe
 */
TEST_CASE(pipe_1)
{
  char buf[14] = "hello, world!";
  int p[2];
  int rfd, wfd;
  int ret = pipe(p);
  if (ret < 0) {
    TEST_FAILED;
  }
  if (write(p[1], buf, sizeof buf) != sizeof buf) {
    TEST_FAILED;
  }
  if (read(p[0], buf, sizeof buf) != sizeof buf) {
    TEST_FAILED;
  }
  if (strcmp(buf, "hello, world!") != 0) {
    TEST_FAILED;
  }
  if (close(p[0]) != 0 || close(p[1]) != 0) {
    TEST_FAILED;
  }
  TEST(1);
}

/**
 * @testname pipe_2
 * @testfor pipe
 */
TEST_CASE_S(pipe_2, FSC_ABRT)
{
  pipe(0);
  TEST_FAILED;
}

/**
 * @testname pipe_3
 * @testfor pipe
 */
TEST_CASE_S(pipe_3, FSC_ABRT)
{
  int p;
  pipe(&p);
  TEST_FAILED;
}

/**
 * @testname write_1s
 * @testfor write
 */
TEST_CASE_S(write_1s, FSC_ABRT)
{
  int p[2];
  char a[5] = "hello", *ptr = a;

  TEST_FAIL_IF(pipe(p));
  write(p[1], ptr - 1, 5);
}

/**
 * @testname write_2s
 * @testfor write
 */
TEST_CASE_S(write_2s, FSC_ABRT)
{
  int p[2];
  char a[5] = "hello", *ptr = a;

  TEST_FAIL_IF(pipe(p));
  write(p[1], ptr + 1, 5);
}

/**
 * @testname write_3s
 * @testfor write
 */
TEST_CASE_S(write_3s, FSC_ABRT)
{
  int p[2];

  TEST_FAIL_IF(pipe(p));
  write(p[1], NULL, 5);
}

/**
 * @testname read_1s
 * @testfor read
 */
TEST_CASE_S(read_1s, FSC_ABRT)
{
  int p[2];
  char a[5] = "hello", *ptr = a;
  char buf[5];

  TEST_FAIL_IF(pipe(p));
  TEST_FAIL_IF(write(p[1], ptr, 5) != 5);

  read(p[0], buf - 1, 5);
}

/**
 * @testname read_2s
 * @testfor read
 */
TEST_CASE_S(read_2s, FSC_ABRT)
{
  int p[2];
  char a[5] = "hello", *ptr = a;
  char buf[5];

  TEST_FAIL_IF(pipe(p));
  TEST_FAIL_IF(write(p[1], ptr, 5) != 5);

  read(p[0], buf + 1, 5);
}

/**
 * @testname read_3s
 * @testfor read
 */
TEST_CASE_S(read_3s, FSC_ABRT)
{
  int p[2];
  char a[5] = "hello", *ptr = a;

  TEST_FAIL_IF(pipe(p));
  TEST_FAIL_IF(write(p[1], ptr, 5) != 5);

  read(p[0], NULL, 5);
}

/**
 * @testname fstat_1
 * @testfor fstat
 */
TEST_CASE(fstat_1)
{
  int fd;
  struct stat buf;

  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST(fstat(fd, &buf) == 0);
}

/**
 * @testname fstat_2
 * @testfor fstat
 */
TEST_CASE(fstat_2)
{
  int fd[2];
  struct stat buf;

  TEST_FAIL_IF(pipe(fd));
  TEST_FAIL_IF(fstat(fd[0], &buf));
  TEST_FAIL_IF(fstat(fd[1], &buf));
  TEST(1);
}

/**
 * @testname fstat_1s
 * @testfor fstat
 */
TEST_CASE_S(fstat_1s, FSC_ABRT)
{
  int fd[2];
  struct stat buf;

  TEST_FAIL_IF(pipe(fd));
  TEST_FAIL_IF(fstat(fd[0], NULL));
}

/**
 * @testname fstat_2s
 * @testfor fstat
 */
TEST_CASE_S(fstat_2s, FSC_ABRT)
{
  int fd[2];
  struct stat buf;
  struct stat *p = (struct stat *)(((char*)&buf) - 4);

  TEST_FAIL_IF(pipe(fd));
  TEST_FAIL_IF(fstat(fd[0], p));
}

/**
 * @testname fstat_3s
 * @testfor fstat
 */
TEST_CASE_S(fstat_3s, FSC_ABRT)
{
  int fd[2];
  struct stat buf;
  struct stat *p = (struct stat *)(((char*)&buf) + 4);

  TEST_FAIL_IF(pipe(fd));
  TEST_FAIL_IF(fstat(fd[0], p));
}

/**
 * @testname stat_1
 * @testfor stat
 */
TEST_CASE(stat_1)
{
  struct stat s;

  TEST(stat(TEST_STDOUT, &s) == 0);
}

/**
 * @testname stat_2
 * @testfor stat
 */
TEST_CASE(stat_2)
{
  struct stat s;
  TEST(stat(TEST_FILENAME, &s) == -1);
}

/**
 * @testname stat_3
 * @testfor stat
 */
TEST_CASE(stat_3)
{
  struct stat s;
  FILE *fp;
  int i;

  fp = fopen(TEST_FILENAME, "wb");
  TEST_FAIL_IF(fp == NULL);
  for (i = 0; i < 256; ++i) {
    fprintf(fp, "0123456789abcdef");
  }
  fclose(fp);

  TEST_FAIL_IF(stat(TEST_FILENAME, &s) == -1);
  TEST_FAIL_IF(s.st_size != 4096);
  TEST_FAIL_IF(s.st_uid != geteuid());
  TEST_FAIL_IF(s.st_gid != getegid());
  TEST(1);
}

/**
 * @testname fchown_1
 * @testfor fchown
 */
TEST_CASE(fchown_1)
{
  TEST(fchown(1, -1, -1) == 0);
}

/**
 * @testname fchown_2
 * @testfor fchown
 */
TEST_CASE(fchown_2)
{
  TEST(fchown(1, 0, -1) == -1);
}

/**
 * @testname readlink_1
 * @testfor readlink
 */
TEST_CASE(readlink_1)
{
  char buf[128];
  TEST(readlink("/", buf, sizeof(buf)) == -1);
}

/**
 * @testname symlink_readlink_unlink_1
 * @testfor symlink
 * @testfor readlink
 * @testfor unlink
 */
TEST_CASE(symlink_readlink_unlink_1)
{
  char buf[128] = "AAAAAAAAAAAAAAAAAAAAAA";
  TEST_FAIL_IF(symlink("/bin/sh", TEST_FILENAME) != 0);
  TEST_FAIL_IF(readlink(TEST_FILENAME, buf, sizeof(buf)) != 7);
  TEST_FAIL_IF(buf[7] != 'A');
  buf[7] = 0;
  TEST_FAIL_IF(strcmp("/bin/sh", buf) != 0);
  TEST_FAIL_IF(unlink(TEST_FILENAME) != 0);
  TEST_FAIL_IF(readlink(TEST_FILENAME, buf, sizeof(buf)) != -1);
  TEST(1);
}

/**
 * @testname umask_1
 * @testfor umask
 */
TEST_CASE(umask_1)
{
  umask(S_IRWXG | S_IRWXO);
  TEST_FAIL_IF(umask(S_IWUSR | S_IWGRP | S_IWOTH) != (S_IRWXG | S_IRWXO));
  TEST_FAIL_IF(umask(S_IXUSR | S_IXOTH) != (S_IWUSR | S_IWGRP | S_IWOTH));
  TEST_FAIL_IF(umask(0) != (S_IXUSR | S_IXOTH));

  TEST(1);
}
#include <stdio.h>
#include <errno.h>
/**
 * @testname umask_2
 * @testfor umask
 */
TEST_CASE(umask_2)
{
  struct stat s;
  int fd;

  umask(S_IRWXG | S_IRWXO);
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0777);
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(stat(TEST_FILENAME, &s));
  TEST_FAIL_IF(s.st_mode != (S_IFREG | S_IRWXU));
  close(fd);
  unlink(TEST_FILENAME);

  umask(S_IWGRP | S_IWOTH);
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0777);
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(stat(TEST_FILENAME, &s));
  TEST_FAIL_IF(s.st_mode != (S_IFREG | S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));
  close(fd);
  unlink(TEST_FILENAME);

  umask(S_IXUSR | S_IXOTH);
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0777);
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(stat(TEST_FILENAME, &s));
  TEST_FAIL_IF(s.st_mode != (S_IFREG | S_IRUSR | S_IWUSR | S_IRWXG | S_IROTH | S_IWOTH));
  close(fd);
  unlink(TEST_FILENAME);

  umask(0);
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0777);
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(stat(TEST_FILENAME, &s));
  TEST_FAIL_IF(s.st_mode != (S_IFREG | S_IRWXU | S_IRWXG | S_IRWXO));
  close(fd);
  unlink(TEST_FILENAME);

  TEST(1);
}

/**
 * @testname remove_1
 * @testfor remove
 */
TEST_CASE(remove_1)
{
  TEST_FAIL_IF(close(open(TEST_FILENAME, O_RDWR | O_CREAT, 0644)) != 0);
  TEST_FAIL_IF(remove(TEST_FILENAME) != 0);
  TEST_FAIL_IF(open(TEST_FILENAME, O_RDONLY) >= 0);
  TEST(1);
}

/**
 * @testname remove_2
 * @testfor remove
 */
TEST_CASE(remove_2)
{
  TEST_FAIL_IF(remove(TEST_FILENAME) == 0);
  TEST(1);
}

/**
 * @testname mkdir_remove_1
 * @testfor mkdir
 * @testfor remove
 */
TEST_CASE(mkdir_remove_1)
{
  struct stat st;
  TEST_FAIL_IF(mkdir(TEST_FILENAME, 0755) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) != 0);
  TEST_FAIL_IF(!S_ISDIR(st.st_mode));
  TEST_FAIL_IF(remove(TEST_FILENAME) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) == 0);
  TEST(1);
}

/**
 * @testname mkfifo_remove_1
 * @testfor mkfifo
 * @testfor remove
 */
TEST_CASE(mkfifo_remove_1)
{
  struct stat st;
  TEST_FAIL_IF(mkfifo(TEST_FILENAME, 0644) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) != 0);
  TEST_FAIL_IF(st.st_mode != (S_IFIFO|0644));
  TEST_FAIL_IF(remove(TEST_FILENAME) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) == 0);
  TEST(1);
}

/**
 * @testname mknod_remove_1
 * @testfor mknod
 * @testfor remove
 */
TEST_CASE(mknod_remove_1)
{
  struct stat st;
  TEST_FAIL_IF(mknod(TEST_FILENAME, S_IFIFO|0644, 0) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) != 0);
  TEST_FAIL_IF(st.st_mode != (S_IFIFO|0644));
  TEST_FAIL_IF(remove(TEST_FILENAME) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) == 0);
  TEST(1);
}

/**
 * @testname link_1
 * @testfor link
 */
TEST_CASE(link_1)
{
  struct stat st;
  TEST_FAIL_IF(link(TEST_STDOUT, TEST_FILENAME) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) != 0);
  TEST_FAIL_IF(st.st_nlink != 2);
  TEST(1);
}

/**
 * @testname link_2
 * @testfor link
 */
TEST_CASE(link_2)
{
  TEST(link(".", TEST_FILENAME) != 0);
}

/**
 * @testname fsync_1
 * @testfor fsync
 */
TEST_CASE(fsync_1)
{
  TEST(fsync(1) == 0);
}

/**
 * @testname fsync_2
 * @testfor fsync
 */
TEST_CASE(fsync_2)
{
  TEST(fsync(-1) == -1);
}

/**
 * @testname truncate_1
 * @testfor truncate
 */
TEST_CASE(truncate_1)
{
  struct stat st;
  int fd = open(TEST_FILENAME, O_RDWR | O_CREAT, 0644);
  write(fd, "hogehoge", 8);
  close(fd);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) != 0);
  TEST_FAIL_IF(st.st_size != 8);
  TEST_FAIL_IF(truncate(TEST_FILENAME, 4) != 0);
  TEST_FAIL_IF(stat(TEST_FILENAME, &st) != 0);
  TEST_FAIL_IF(st.st_size != 4);
  TEST(1);
}

/**
 * @testname sync_1
 * @testfor sync
 */
TEST_CASE(sync_1)
{
  sync();
  TEST(1);
}

/**
 * @testname fdatasync_1
 * @testfor fdatasync
 */
TEST_CASE(fdatasync_1)
{
  TEST(fdatasync(1) == 0);
}

/**
 * @testname fdatasync_2
 * @testfor fdatasync
 */
TEST_CASE(fdatasync_2)
{
  TEST(fdatasync(-1) == -1);
}

/**
 * @testname lstat_1
 * @testfor lstat
 */
TEST_CASE(lstat_1)
{
  struct stat s1, s2;
  int r;
  stat("/", &s1);
  symlink("/", TEST_FILENAME);
  r = lstat(TEST_FILENAME, &s2);
  unlink(TEST_FILENAME);
  TEST_FAIL_IF(r == -1);
  TEST(s1.st_ino != s2.st_ino);
}

/**
 * @testname lstat_2
 * @testfor lstat
 */
TEST_CASE(lstat_2)
{
  struct stat s;

  close(open(TEST_FILENAME, O_WRONLY | O_CREAT, 0644));
  TEST(lstat(TEST_FILENAME, &s) == 0);
}

/**
 * @testname lstat_3
 * @testfor lstat
 */
TEST_CASE(lstat_3)
{
  struct stat s;
  TEST(lstat(TEST_FILENAME, &s) == -1);
}

#ifndef MAX_OFF_T
#define MAX_OFF_T_NOTDEFINED
#define MAX_OFF_T (~(((off_t)1) << (sizeof(off_t) * 8 - 1)))
#endif

/**
 * @testname lseek_1
 * @testfor lseek
 */
TEST_CASE(lseek_1)
{
  int fd;
  char buf[64];
  fd = open(TEST_FILENAME, O_RDWR | O_CREAT, 0644);
  write(fd, "hogehogehogehoge", 16);

  TEST_FAIL_IF(lseek(fd, 0, SEEK_CUR) != 16);
  TEST_FAIL_IF(lseek(fd, 0, SEEK_SET) != 0);
  TEST_FAIL_IF(lseek(fd, 0, SEEK_CUR) != 0);
  TEST_FAIL_IF(lseek(fd, 0, SEEK_END) != 16);
  TEST_FAIL_IF(lseek(fd, 0, SEEK_CUR) != 16);

  TEST_FAIL_IF(lseek(fd, 1, SEEK_SET) != 1);
  write(fd, "on", 2);
  TEST_FAIL_IF(lseek(fd, 16, SEEK_SET) != 16);
  TEST_FAIL_IF(lseek(fd, 17, SEEK_SET) != 17);
  TEST_FAIL_IF(lseek(fd, 32, SEEK_SET) != 32);

  TEST_FAIL_IF(lseek(fd, 0, SEEK_CUR) != 32);
  TEST_FAIL_IF(lseek(fd, -16, SEEK_CUR) != 16);
  TEST_FAIL_IF(lseek(fd, -8, SEEK_CUR) != 8);
  write(fd, "piyo", 4);
  TEST_FAIL_IF(lseek(fd, 12, SEEK_CUR) != 24);
  TEST_FAIL_IF(lseek(fd, -24, SEEK_CUR) != 0);
  TEST_FAIL_IF(lseek(fd, -16, SEEK_CUR) != -1);

  TEST_FAIL_IF(lseek(fd, 16, SEEK_END) != 32);
  TEST_FAIL_IF(lseek(fd, -16, SEEK_END) != 0);
  write(fd, "ha", 2);
  TEST_FAIL_IF(lseek(fd, -32, SEEK_END) != -1);

  TEST_FAIL_IF(lseek(fd, 4, SEEK_END) != 20);
  write(fd, "hoge", 4);
  TEST_FAIL_IF(lseek(fd, 0, SEEK_SET) != 0);
  read(fd, buf, 24);
  TEST_FAIL_IF(memcmp(buf, "hanehogepiyohoge\0\0\0\0hoge", 24) != 0);

  TEST_FAIL_IF(lseek(fd, MAX_OFF_T, SEEK_END) != -1);
  TEST_FAIL_IF(lseek(fd, 0, 12345) != -1);

  close(fd);

  TEST(1);
}

#ifdef MAX_OFF_T_NOTDEFINED
#undef MAX_OFF_T_NOTDEFINED
#undef MAX_OFF_T
#endif

/**
 * @testname rmdir_1
 * @testfor rmdir
 */
TEST_CASE(rmdir_1)
{
  mkdir(TEST_FILENAME, 0755);
  TEST_FAIL_IF(rmdir(TEST_FILENAME) != 0);

  TEST(1);
}

/**
 * @testname rmdir_2
 * @testfor rmdir
 */
TEST_CASE(rmdir_2)
{
  char buf[1024];
  int fd;
  TEST_FAIL_IF(rmdir(TEST_FILENAME) == 0);

  fd = open(TEST_FILENAME, O_RDWR | O_CREAT, 0644);
  write(fd, "hoge", 4);
  close(fd);
  TEST_FAIL_IF(rmdir(TEST_FILENAME) == 0);
  remove(TEST_FILENAME);

  mkdir(TEST_FILENAME, 0755);
  strcpy(buf, TEST_FILENAME);
  strcat(buf, "/hoge");
  fd = open(buf, O_RDWR | O_CREAT, 0644);
  write(fd, "hoge", 4);
  close(fd);
  TEST_FAIL_IF(rmdir(TEST_FILENAME) == 0);
  remove(buf);
  TEST_FAIL_IF(rmdir(TEST_FILENAME) != 0);

  TEST(1);
}

