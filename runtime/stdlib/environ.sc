/* -*- c -*- */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

extern char **environ;

static int is_valid_envname(const char *name)
{
    /* test getenv_1s: invalid pointer is to raise exception */
  /*  if (!name) return 0; */
  if (name[0] == 0) return 0;
  if (strchr(name, '=')) return 0;
  return 1;
}

static char *getenv_index(const char *name, int *index)
{
  char **p = environ;
  if (p) {
    int i;
    for (i = 0; p[i]; i++) {
      char *v = p[i];
      int j;

      for (j = 0;; j++) {
        if (name[j] == 0 || name[j] == '=') {
          if (v[j] == '=') {
            *index = i;
            return &v[j+1];
          }
          break;
        }
        if (name[j] != v[j])
          break;
      }
    }
  }
  return NULL;
}

/**
 * @fn char *getenv(const char *name)
 * @brief get environment variable
 *
 * @author Lepidum Co., Ltd.
 */
char *getenv(const char *name)
{
  int x;
  if (!is_valid_envname(name)) {
    return NULL;
  }
  return getenv_index(name, &x);
}

static int setenv_index(const char *namevalue, int index)
{
  char **p;
  int size = 0;
  int i;
  
  if (index >= 0) {
    environ[index] = (char *)namevalue;
    return 0;
  }
  if (environ) {
    for (size = 0; environ[size]; size++)
      ;
  }
  p = malloc(sizeof(char *) * (size + 2));
  if (!p) return -1;
  if (environ) {
    for (i = 0; i < size; i++) {
      p[i] = environ[i];
    }
  }
  p[size] = (char *)namevalue;
  p[size+1] = NULL;
  environ = p;
  return 0;
}

/**
 * @fn int setenv(const char *name, const char *val, int overwrite)
 * @brief set environment variable
 *
 * @author Lepidum Co., Ltd.
 */
int setenv(const char *name, const char *val, int overwrite)
{
  int index;
  char *old;
  char *buf;
  int ret;
  size_t name_len, val_len;

  if (!is_valid_envname(name) || !val) {
    errno = EINVAL;
    return -1;
  }
  name_len = strlen(name);
  val_len = strlen(val);
  
  old = getenv_index(name, &index);
  if (old) {
    if (overwrite) {
      if (strlen(old) >= strlen(val)) {
        strcpy(old, val);
        return 0;
      }
    } else {
      return 0;
    }
  } else {
    index = -1;
  }
  buf = malloc(name_len + val_len + 2);
  if (buf == NULL) return -1;
  sprintf(buf, "%s=%s", name, val);
  ret = setenv_index(buf, index);
  if (ret < 0) {
    free(buf);
  }
  return ret;
}

/**
 * @fn int putenv(const char *namevalue)
 * @brief set environment variable
 *
 * @author Lepidum Co., Ltd.
 */
int putenv(const char *namevalue)
{
  int index = -1;
  char *old;

  old = getenv_index(namevalue, &index);
  return setenv_index(namevalue, index);
}

/**
 * @fn int unsetenv(const char *name)
 * @brief remove environment variable
 *
 * @author Lepidum Co., Ltd.
 */
int unsetenv(const char *name)
{
  int index;
  char *val;

  if (!is_valid_envname(name)) {
    errno = EINVAL;
    return -1;
  }
  for (;;) {
    val = getenv_index(name, &index);
    if (!val) return 0;
    do {
      environ[index] = environ[index+1];
      index++;
    } while (environ[index]);
  }
}

