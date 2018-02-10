/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/gethostent.test.c
 */

#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <string.h>
#include "common.h"

static struct hostent *dup_hostent(struct hostent *phent)
{
  struct hostent *phent_new;
  int n_aliases = 0;
  int n_addrs = 0;
  int i = 0;
  phent_new = (struct hostent *)malloc(sizeof(struct hostent));
  phent_new->h_name = (char *)malloc(sizeof(char)*(strlen(phent->h_name)+1));
  strcpy(phent_new->h_name, phent->h_name);
  while (phent->h_aliases[n_aliases]) {
    ++n_aliases;
  }
  phent_new->h_aliases = (char **)malloc(sizeof(char *) * (n_aliases + 1));
  for (i = 0; i < n_aliases; ++i) {
    phent_new->h_aliases[i] = (char *)malloc(sizeof(char) * (strlen(phent->h_aliases[i]) + 1));
    strcpy(phent_new->h_aliases[i], phent->h_aliases[i]);
  }
  phent_new->h_aliases[n_aliases] = NULL;
  phent_new->h_addrtype = phent->h_addrtype;
  phent_new->h_length = phent->h_length;
  while (phent->h_addr_list[n_addrs]) {
    ++n_addrs;
  }
  phent_new->h_addr_list = (char **)malloc(sizeof(char *) * (n_addrs + 1));
  for (i = 0; i < n_addrs; ++i) {
    phent_new->h_addr_list[i] = (char *)malloc(sizeof(char) * phent->h_length);
    memcpy(phent_new->h_addr_list[i], phent->h_addr_list[i], phent->h_length);
  }
  phent_new->h_addr_list[n_addrs] = NULL;
  return phent_new;
}

static void free_hostent(struct hostent * phent)
{
  int i;
  free(phent->h_name);
  for (i = 0; phent->h_aliases[i]; ++i) {
    free(phent->h_aliases[i]);
  }
  free(phent->h_aliases);
  for (i = 0; phent->h_addr_list[i]; ++i) {
    free(phent->h_addr_list[i]);
  }
  free(phent->h_addr_list);
  free(phent);
}

static int compare_hostent(struct hostent *phent1, struct hostent *phent2)
{
  int i;
  if (strcmp(phent1->h_name, phent2->h_name)) {
    return 0;
  }
  for (i = 0; phent1->h_aliases[i] && phent2->h_aliases[i]; ++i) {
    if (strcmp(phent1->h_aliases[i], phent2->h_aliases[i])) {
      return 0;
    }
  }
  if (phent1->h_aliases[i] || phent2->h_aliases[i]) {
    return 0;
  }
  if (phent1->h_addrtype != phent2->h_addrtype) {
    return 0;
  }
  if (phent1->h_length != phent2->h_length) {
    return 0;
  }
  for (i = 0; phent1->h_addr_list[i] && phent2->h_addr_list[i]; ++i) {
    if (memcmp(phent1->h_addr_list[i], phent2->h_addr_list[i], phent1->h_length)) {
      return 0;
    }
  }
  if (phent1->h_addr_list[i] || phent2->h_addr_list[i]) {
    return 0;
  }
  return 1;
}

/**
 * @testname hostent_1
 * @testfor sethostent
 * @testfor gethostent
 * @testfor endhostent
 */
TEST_CASE(hostent_1)
{
  struct hostent *phents[1024], *phent2;
  int i = 0, j = 0;
  sethostent(1);
  while (phents[i] = gethostent()) {
    TEST_FAIL_IF(phents[i]->h_name == NULL);
    TEST_FAIL_IF(phents[i]->h_aliases == NULL);
    TEST_FAIL_IF(phents[i]->h_addr_list == NULL);
    TEST_FAIL_IF(phents[i]->h_addr_list[0] == NULL);
    phents[i] = dup_hostent(phents[i]);
    ++i;
  }
  endhostent();
  for (j = 0; j < i; ++j) {
    phent2 = gethostbyname(phents[j]->h_name);
    TEST_FAIL_IF(phent2 == NULL);
    TEST_FAIL_IF(!compare_hostent(phents[j], phent2));
    free_hostent(phents[j]);
  }
  TEST(1);
}

/**
 * @testname hostent_2
 * @testfor sethostent
 * @testfor gethostent
 * @testfor endhostent
 */
TEST_CASE(hostent_2)
{
  struct hostent *phents[1024], *phent2;
  int i = 0, j = 0;
  sethostent(1);
  while (phents[i] = gethostent()) {
    TEST_FAIL_IF(phents[i]->h_name == NULL);
    TEST_FAIL_IF(phents[i]->h_aliases == NULL);
    TEST_FAIL_IF(phents[i]->h_addr_list == NULL);
    TEST_FAIL_IF(phents[i]->h_addr_list[0] == NULL);
    phents[i] = dup_hostent(phents[i]);
    ++i;
  }
  sethostent(1);
  for (j = 0; j < i && (phent2 = gethostent()); ++j) {
    TEST_FAIL_IF(!compare_hostent(phents[j], phent2));
    free_hostent(phents[j]);
  }
  endhostent();
  TEST(i == j);
}
