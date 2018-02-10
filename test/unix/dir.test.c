/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/dir.test.c
 */
#include <dirent.h>

#include "common.h"

/**
 * @testname opendir_closedir_readdir_rewinddir_1
 * @testfor opendir
 * @testfor closedir
 * @testfor readdir
 * @testfor rewinddir
 */
TEST_CASE(opendir_closedir_readdir_rewinddir_1)
{
  DIR *dir = opendir("/");
  struct dirent *dent;
  int found;
  int i;
  TEST_FAIL_IF(dir == NULL);

  for (i = 0; i < 2; i++) {
    found = 0;
    while ((dent = readdir(dir)) != NULL){
      if (strcmp(dent->d_name, "usr") == 0) found++;
      if (strcmp(dent->d_name, "etc") == 0) found++;
      if (strcmp(dent->d_name, "dev") == 0) found++;
      if (strcmp(dent->d_name, "tmp") == 0) found++;
    }
    TEST_FAIL_IF(found != 4);
    rewinddir(dir);
  }
  TEST_FAIL_IF(closedir(dir) != 0);

  TEST(1);
}

/**
 * @testname seekdir_telldir_1
 * @testfor seekdir
 * @testfor telldir
 */
TEST_CASE(seekdir_telldir_1)
{
  DIR *dir = opendir("/");
  struct dirent *dent;
  int found;
  int i;
  long pos;
  
  TEST_FAIL_IF(dir == NULL);

  pos = telldir(dir);
  for (i = 0; i < 2; i++) {
    found = 0;
    while ((dent = readdir(dir)) != NULL){
      if (strcmp(dent->d_name, "usr") == 0) found++;
      if (strcmp(dent->d_name, "etc") == 0) found++;
      if (strcmp(dent->d_name, "dev") == 0) found++;
      if (strcmp(dent->d_name, "tmp") == 0) found++;
    }
    TEST_FAIL_IF(found != 4);
    seekdir(dir, 0);
  }
  TEST_FAIL_IF(closedir(dir) != 0);

  TEST(1);

}

