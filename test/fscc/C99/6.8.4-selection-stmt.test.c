/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(C99_6_8_4_selection_stmt_1)
{
  int a = 0;
  if (1)
    if (0)
      a = 1;
  else
    a = 2;
  TEST(a == 2);
}

TEST_CASE(C99_6_8_4_selection_stmt_2)
{
  int a = 1;
  switch (1)
    a = 0;
  TEST(a == 1);
}

TEST_CASE(C99_6_8_4_selection_stmt_3)
{
  int a = 1;
  switch (1)
  default: a = 0;
  TEST(a == 0);
}

TEST_CASE(C99_6_8_4_selection_stmt_4)
{
  int a = 1;
  switch (1)
  case 1: a = 0;
  TEST(a == 0);
}

TEST_CASE(C99_6_8_4_selection_stmt_5)
{
  int a = 1;
  switch (0)
    case 0: if (0)
    case 1:   a = 2;
            else
    default:  a = 3;
  TEST(a == 3);
}

TEST_CASE(C99_6_8_4_selection_stmt_6)
{
  int a = 1;
  switch (1)
    case 0: if (0)
    case 1:   a = 2;
            else
    default:  a = 3;
  TEST(a == 2);
}

TEST_CASE(C99_6_8_4_selection_stmt_7)
{
  int a = 1;
  switch (2)
    case 0: if (0)
    case 1:   a = 2;
            else
    default:  a = 3;
  TEST(a == 3);
}

TEST_CASE(C99_6_8_4_selection_stmt_8)
{
  int a = 1;
  switch (1)
    case 0: if (0)
    case 1:   break;
            else
    default:  a = 3;
  TEST(a == 1);
}


