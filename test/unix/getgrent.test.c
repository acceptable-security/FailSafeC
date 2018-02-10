/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/getgrent.test.c
 */
#include "common.h"
#include <grp.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>


static struct group *dup_grent(struct group *grent)
{
  struct group *grent_new;
  int n_members = 0;
  int i = 0;
  if (grent == NULL)
    return NULL;
  grent_new = (struct group *)malloc(sizeof(struct group));
  grent_new->gr_name=(char*)malloc(sizeof(char) * (strlen(grent->gr_name) + 1));
  strcpy(grent_new->gr_name, grent->gr_name);
  memcpy(&grent_new->gr_gid, &grent->gr_gid, sizeof(gid_t));
  while (grent->gr_mem[n_members])
    ++n_members;
  grent_new->gr_mem = (char **)malloc(sizeof(char *) * (n_members + 1));
  for (i = 0; i < n_members; ++i) {
    grent_new->gr_mem[i] = (char *)malloc(sizeof(char) * (strlen(grent->gr_mem[i]) + 1));
    strcpy(grent_new->gr_mem[i], grent->gr_mem[i]);
  }
  grent_new->gr_mem[i] = NULL;
  return grent_new;
}

static int compare_grents(struct group *grent1, struct group *grent2)
{
  int i = 0;
  if (strcmp(grent1->gr_name, grent2->gr_name))
    return 0;
  if (memcmp(&grent1->gr_gid, &grent2->gr_gid, sizeof(gid_t)))
    return 0;
  for (i = 0; grent1->gr_mem[i] && grent2->gr_mem[i]; ++i) {
    if (strcmp(grent1->gr_mem[i], grent2->gr_mem[i]))
      return 0;
  }
  return grent1->gr_mem[i] == grent2->gr_mem[i];
}

static void free_grent(struct group *grent)
{
  int i = 0;
  free(grent->gr_name);
  for (i = 0; grent->gr_mem[i]; ++i)
    free(grent->gr_mem[i]);
  free(grent->gr_mem);
  free(grent);
}

/**
 * @testname setgrent_getgrent_endgrent_1
 * @testfor setgrent
 * @testfor getgrent
 * @testfor endgrent
 */
TEST_CASE(setgrent_getgrent_endgrent_1)
{
  struct group *pgrents[1024];
  int n_groups = 0;
  int i = 0, j = 0;
  setgrent();
  while (pgrents[n_groups] = dup_grent(getgrent()))
    ++n_groups;
  endgrent();
  TEST_FAIL_IF(n_groups == 0);

  setgrent();
  for (i = 0; i < n_groups; ++i) {
    TEST_FAIL_IF(!compare_grents(pgrents[i], getgrent()));
  }
  endgrent();

  setgrent();
  getgrent();
  setgrent();
  TEST_FAIL_IF(!compare_grents(pgrents[0], getgrent()));
  endgrent();

  for (i = 0; i < n_groups; ++i) {
    for (j = i + 1; j < n_groups; ++j) {
      TEST_FAIL_IF(compare_grents(pgrents[i], pgrents[j]));
    }
    free_grent(pgrents[i]);
  }
  TEST(1);
}

/**
 * @testname getgrgid_1
 * @testfor getgrgid
 */
TEST_CASE(getgrgid_1)
{
  struct group *pg1, *pg2;

  pg1 = getgrgid(getgid());
  TEST_FAIL_IF(pg1 == NULL);
  pg1 = dup_grent(pg1);
  pg2 = getgrgid(getgid());
  TEST_FAIL_IF(!compare_grents(pg1, pg2));

  pg2 = getgrgid(0);
  TEST_FAIL_IF(pg2 == NULL);
  TEST_FAIL_IF(getgid() != 0 && compare_grents(pg1, pg2));

  pg2 = getgrgid(12345);
  TEST_FAIL_IF(pg2 != NULL);
  free_grent(pg1);

  TEST(1);
}

#if 0
/**
 * <at>testname getgrgid_r_1
 * <at>testfor getgrgid_r
 */
 TEST_CASE(getgrgid_r)
{
  struct group g, *pg;
  char buf[1024];
  memset(buf, '\xff', 1024);

  TEST_FAIL_IF(getgrgid_r(getgid(), &g, buf, 0, &pg) == 0);
  TEST_FAIL_IF(buf[0] != '\xff');
  TEST_FAIL_IF(pg == &g);

  TEST_FAIL_IF(getgrgid_r(getgid(), &g, buf, 1, &pg) == 0);
  TEST_FAIL_IF(buf[0] != '\xff');
  TEST_FAIL_IF(pg == &g);

  TEST(1);
}
#endif

#if 0
/**
 * <at>testname getgrgid_r_1s
 * <at>testfor getgrgid_r
 */
 TEST_CASE_S(getgrgid_r_1s, FSC_ABRT)
{
  struct group *pg;
  char buf[1024];
  getgrgid_r(getgid(), NULL, buf, 1024, &pg);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrgid_r_2s
 * <at>testfor getgrgid_r
 */
 TEST_CASE_S(getgrgid_r_2s, FSC_ABRT)
{
  struct group g, *pg;
  getgrgid_r(getgid(), &g, NULL, 1024, &pg);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrgid_r_3s
 * <at>testfor getgrgid_r
 */
 TEST_CASE_S(getgrgid_r_3s, FSC_ABRT)
{
  struct group g, *pg;
  char buf[1];
  getgrgid_r(getgid(), &g, buf, 1024, &pg);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrgid_r_4s
 * <at>testfor getgrgid_r
 */
 TEST_CASE_S(getgrgid_r_4s, FSC_ABRT)
{
  struct group g, *pg;
  char buf[1024];
  getgrgid_r(getgid(), &g, buf, 1024, NULL);
  TEST_FAILED;
}
#endif

char *getgroupname(gid_t gid)
{
  struct group *pg;
  char *str = NULL;
  pg = getgrgid(gid);
  if (pg) {
    str = (char *)malloc(sizeof(char) * (strlen(pg->gr_name) + 1));
    strcpy(str, pg->gr_name);
  }
  return str;
}

/**
 * @testname getgrnam_1
 * @testfor getgrnam
 */
TEST_CASE(getgrnam_1)
{
  struct group *pg1, *pg2;
  char *grname;

  grname = getgroupname(getgid());
  pg1 = getgrnam(grname);
  TEST_FAIL_IF(pg1 == NULL);
  pg1 = dup_grent(pg1);
  pg2 = getgrnam(grname);
  TEST_FAIL_IF(!compare_grents(pg1, pg2));
  free(grname);

  grname = getgroupname(0);
  pg2 = getgrnam(grname);
  TEST_FAIL_IF(pg1 == NULL);
  TEST_FAIL_IF(getgid() != 0 && compare_grents(pg1, pg2));
  free(grname);

  pg2 = getgrnam("");
  TEST_FAIL_IF(pg2 != NULL);
  free_grent(pg1);

  TEST(1);
}

/**
 * @testname getgrnam_1s
 * @testfor getgrnam_1s
 */
TEST_CASE_S(getgrnam_1s, FSC_ABRT)
{
  getgrnam(NULL);
  TEST_FAILED;
}

#if 0
/**
 * <at>testname getgrnam_r_1
 * <at>testfor getgrnam_r
 */
 TEST_CASE(getgrnam_r)
{
  struct group g, *pg;
  char buf[1024];
  char *grname = getgroupname(getgid());
  memset(buf, '\xff', 1024);

  TEST_FAIL_IF(getgrnam_r(grname, &g, buf, 0, &pg) == 0);
  TEST_FAIL_IF(buf[0] != '\xff');
  TEST_FAIL_IF(pg == &g);

  TEST_FAIL_IF(getgrnam_r(grname, &g, buf, 1, &pg) == 0);
  TEST_FAIL_IF(buf[0] != '\xff');
  TEST_FAIL_IF(pg == &g);

  free(grname);
  TEST(1);
}
#endif

#if 0
/**
 * <at>testname getgrnam_r_1s
 * <at>testfor getgrnam_r
 */
 TEST_CASE_S(getgrnam_r_1s, FSC_ABRT)
{
  struct group *pg;
  char buf[1024];
  char *grname = getgroupname(getgid());
  getgrnam_r(grname, NULL, buf, 1024, &pg);
  free(grname);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrnam_r_2s
 * <at>testfor getgrnam_r
 */
 TEST_CASE_S(getgrnam_r_2s, FSC_ABRT)
{
  struct group g, *pg;
  char *grname = getgroupname(getgid());
  getgrnam_r(grname, &g, NULL, 1024, &pg);
  free(grname);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrnam_r_3s
 * <at>testfor getgrnam_r
 */
 TEST_CASE_S(getgrnam_r_3s, FSC_ABRT)
{
  struct group g, *pg;
  char buf[1];
  char *grname = getgroupname(getgid());
  getgrnam_r(grname, &g, buf, 1024, &pg);
  free(grname);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrnam_r_4s
 * <at>testfor getgrnam_r
 */
 TEST_CASE_S(getgrnam_r_4s, FSC_ABRT)
{
  struct group g;
  char buf[1024];
  char *grname = getgroupname(getgid());
  getgrnam_r(grname, &g, buf, 1024, NULL);
  free(grname);
  TEST_FAILED;
}
#endif

#if 0
/**
 * <at>testname getgrnam_r_5s
 * <at>testfor getgrnam_r
 */
 TEST_CASE_S(getgrnam_r_5s, FSC_ABRT)
{
  struct group g, *pg;
  char buf[1024];
  getgrnam_r(NULL, &g, buf, 1024, &pg);
  TEST_FAILED;
}
#endif
