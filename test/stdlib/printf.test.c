/*
   This file is written by Yutaka OIWA.
   Copyright (c) 2005-2007 RCIS, AIST.
 */

#include "common.h"
#include <stdlib.h>

TEST_CASE(sprintf_num_1_1)
{
    char buf[80];
    TEST(sprintf(buf, "%#x %#x", 0, 20) == 6);
    TEST(strcmp(buf, "0 0x14") == 0);
}

TEST_CASE(sprintf_num_1_2)
{
    char buf[80];
    TEST(sprintf(buf, "%#X %#X", 0, 20) == 6);
    TEST(strcmp(buf, "0 0X14") == 0);
}

TEST_CASE(sprintf_num_1_3)
{
    char buf[80];
    TEST(sprintf(buf, "%#o %#o", 0, 20) == 5);
    TEST(strcmp(buf, "0 024") == 0);
}
