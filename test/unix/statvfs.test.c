/**
 * @file unix/statvfs.test.c
 */
#include <sys/statvfs.h>
#include <fcntl.h>
#include <unistd.h>

#include "common.h"

/**
 * @testname statvfs_fstatvfs_1
 * @testfor statvfs
 * @testfor fstatvfs
 */
TEST_CASE(statvfs_fstatvfs_1)
{
  int fd = open("/bin/ls", O_RDONLY);
  struct statvfs s1, s2;
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(fstatvfs(fd, &s1) != 0);
  TEST_FAIL_IF(statvfs("/bin/echo", &s2) != 0);
  TEST_FAIL_IF(s1.f_bsize != s2.f_bsize);
  TEST_FAIL_IF(s1.f_frsize != s2.f_bsize);
  TEST_FAIL_IF(s1.f_blocks != s2.f_blocks);
  TEST_FAIL_IF(s1.f_bfree != s2.f_bfree);
  TEST_FAIL_IF(s1.f_bavail != s2.f_bavail);
  TEST_FAIL_IF(s1.f_files != s2.f_files);
  TEST_FAIL_IF(s1.f_ffree != s2.f_ffree);
  TEST_FAIL_IF(s1.f_favail != s2.f_favail);
  TEST_FAIL_IF(s1.f_fsid != s2.f_fsid);
  TEST_FAIL_IF(s1.f_flag != s2.f_flag);
  TEST_FAIL_IF(s1.f_namemax != s2.f_namemax);
  close(fd);
  TEST(1);
}
