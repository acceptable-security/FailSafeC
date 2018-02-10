#include "common.h"
#ifdef __linux
#include <gdbm-ndbm.h>
#else
#include <ndbm.h>
#endif
#include <fcntl.h>
#include <stdio.h>

#define DBM_FILE TEST_DIRNAME "/test-dbm"

static datum key1(){
  datum r;
  r.dptr = "aaaaa";
  r.dsize = 5;
  return r;
}

static datum key2(){
  datum r;
  r.dptr = "eee";
  r.dsize = 3;
  return r;
}

static datum key3(){
  datum r;
  r.dptr = "jklmn";
  r.dsize = 6;
  return r;
}

static datum value1(){
  datum r;
  r.dptr = "bbbcccddd";
  r.dsize = 9;
  return r;
}

static datum value2(){
  datum r;
  r.dptr = "ffffgggghhhh";
  r.dsize = 12;
  return r;
}

static datum value3(){
  datum r;
  r.dptr = "opqrstu";
  r.dsize = 8;
  return r;
}

/**
 * compare datums.
 */
static int datumcmp(datum a, datum b)
{
  if(a.dptr == NULL || b.dptr == NULL){
    return a.dptr != b.dptr;
  }

  if(a.dsize != b.dsize){
    return 1;
  }

  return memcmp(a.dptr, b.dptr, a.dsize);
}

static int setup_db(DBM *dp){
  datum k, v;

  k = key1();
  v = value1();
  TEST_FAIL_IF(0 != dbm_store(dp, k, v, DBM_INSERT));

  k = key2();
  v = value2();
  TEST_FAIL_IF(0 != dbm_store(dp, k, v, DBM_INSERT));

  k = key3();
  v = value3();
  TEST_FAIL_IF(0 != dbm_store(dp, k, v, DBM_INSERT));

  return 0;
}

/**
 * @testname dbm_open_close
 * @testfor dbm_open
 * @testfor dbm_close
 */
TEST_CASE(dbm_open_close)
{
  DBM *dp;
  dp = dbm_open(DBM_FILE, O_RDWR, 0644);
  TEST_FAIL_IF(dp != (DBM*)0);

  dp = dbm_open(DBM_FILE, O_RDWR | O_CREAT, 0644);
  TEST_FAIL_IF(dp == (DBM*)0);

  dbm_close(dp);
  TEST(1);
}

/**
 * @testname dbm_store_fetch
 * @testfor dbm_open
 * @testfor dbm_store
 * @testfor dbm_fetch
 * @testfor dbm_close
 */
TEST_CASE(dbm_store_fetch)
{
  DBM *dp;
  datum k, v;

  dp = dbm_open(DBM_FILE, O_RDWR | O_CREAT, 0644);
  TEST_FAIL_IF(dp == (DBM*)0);

  setup_db(dp);

  k = key1();
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(datumcmp(v, value1()));

  k = key2();
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(datumcmp(v, value2()));

  k = key3();
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(datumcmp(v, value3()));

  k = key3();
  k.dsize--;
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(v.dptr != NULL);

  dbm_close(dp);
  TEST(1);
}

/**
 * @testname dbm_store_delete
 * @testfor dbm_open
 * @testfor dbm_store
 * @testfor dbm_delete
 * @testfor dbm_fetch
 * @testfor dbm_close
 */
TEST_CASE(dbm_store_delete)
{
  DBM *dp;
  datum k, v;

  dp = dbm_open(DBM_FILE, O_RDWR | O_CREAT, 0644);
  TEST_FAIL_IF(dp == (DBM*)0);

  setup_db(dp);

  k = key1();
  TEST_FAIL_IF(0 != dbm_delete(dp, k));
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(v.dptr != NULL);

  k = key2();
  TEST_FAIL_IF(0 != dbm_delete(dp, k));
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(v.dptr != NULL);

  k = key3();
  TEST_FAIL_IF(0 != dbm_delete(dp, k));
  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(v.dptr != NULL);

  k = key3();
  k.dsize--;
  TEST_FAIL_IF(0 == dbm_delete(dp, k));

  dbm_close(dp);
  TEST(1);
}

/**
 * @testname dbm_store_fetch_flag
 * @testfor dbm_open
 * @testfor dbm_store
 * @testfor dbm_fetch
 * @testfor dbm_close
 */
TEST_CASE(dbm_store_fetch_flag)
{
  DBM *dp;
  datum k, v;

  dp = dbm_open(DBM_FILE, O_RDWR | O_CREAT, 0644);
  TEST_FAIL_IF(dp == (DBM*)0);

  setup_db(dp);

  k = key1();

  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(datumcmp(v, value1()));

  v = value2();
  TEST_FAIL_IF(0 == dbm_store(dp, k, v, DBM_INSERT));

  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(datumcmp(v, value1()));

  v = value3();
  TEST_FAIL_IF(0 != dbm_store(dp, k, v, DBM_REPLACE));

  v = dbm_fetch(dp, k);
  TEST_FAIL_IF(datumcmp(v, value3()));

  dbm_close(dp);
  TEST(1);
}

/**
 * @testname dbm_store_enum
 * @testfor dbm_open
 * @testfor dbm_store
 * @testfor dbm_firstkey
 * @testfor dbm_nextkey
 * @testfor dbm_close
 */
TEST_CASE(dbm_store_enum)
{
  DBM *dp;
  datum k, v;
  int f[3] = { 0, 0, 0 };

  dp = dbm_open(DBM_FILE, O_RDWR | O_CREAT, 0644);
  TEST_FAIL_IF(dp == (DBM*)0);

  k = dbm_firstkey(dp);
  TEST_FAIL_IF(k.dptr != NULL);

  setup_db(dp);

  k = dbm_firstkey(dp);
  TEST_FAIL_IF(k.dptr == NULL);
  if(!datumcmp(k, key1())){ f[0]++; }
  if(!datumcmp(k, key2())){ f[1]++; }
  if(!datumcmp(k, key3())){ f[2]++; }

  do{
    k = dbm_nextkey(dp);
    if(!datumcmp(k, key1())){ f[0]++; }
    if(!datumcmp(k, key2())){ f[1]++; }
    if(!datumcmp(k, key3())){ f[2]++; }
  }while(k.dptr != NULL);

  TEST_FAIL_IF(f[0] != 1 || f[1] != 1 || f[2] != 1);

  k = dbm_firstkey(dp);
  TEST_FAIL_IF(k.dptr == NULL);
  if(!datumcmp(k, key1())){ f[0]++; }
  if(!datumcmp(k, key2())){ f[1]++; }
  if(!datumcmp(k, key3())){ f[2]++; }

  do{
    k = dbm_nextkey(dp);
    if(!datumcmp(k, key1())){ f[0]++; }
    if(!datumcmp(k, key2())){ f[1]++; }
    if(!datumcmp(k, key3())){ f[2]++; }
  }while(k.dptr != NULL);

  TEST_FAIL_IF(f[0] != 2 || f[1] != 2 || f[2] != 2);

  dbm_close(dp);
  TEST(1);
}
