/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file manualtest/syslog.test.c
 */
#include <syslog.h>

#include "common.h"

/**
 * @testname openlog_closelog_syslog_1
 * @testfor openlog
 * @testfor closelog
 * @testfor syslog
 */
TEST_CASE(openlog_closelog_syslog_1)
{
  openlog("FSC library test ", LOG_PID, LOG_USER);
  syslog(LOG_INFO, "format test 11600=%d 0xDEADBEAF=%#X hello=%s 2percent=%%%%\n", 11600, 0xDEADBEAF, "hello");
  closelog();
  TEST(1);
}
