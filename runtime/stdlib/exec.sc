/* -*- C -*- */
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

extern char **environ;

extern int execve(const char *path, char *const *argv, char *const *envp);
extern int __execvpe(const char *path, char *const *argv, char *const *envp);

/**
 * @fn int execv(const char *path, char *const *argv)
 * @brief execute a file.
 * @param path path of executable file.
 * @param argv arguments.
 * @return do not return on success , -1 on error.
 *
 * @crashcase invalid pointer, buffer overflow, non-terminated string
 * @fsctype string(ro), array(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
int execv(const char *path, char *const *argv)
{
  return execve(path, argv, environ);
}

/**
 * @fn int execvp(const char *path, char *const *argv)
 * @brief execute a file.
 * @param path path of executable file.
 * @param argv arguments.
 * @return do not return on success , -1 on error.
 *
 * @crashcase invalid pointer, buffer overflow, non-terminated string
 * @fsctype string(ro), array(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
int execvp(const char *path, char *const *argv)
{
  return __execvpe(path, argv, environ);
}

static int execlep(const char *path, const char *argv0, va_list va1, int flage, int flagp)
{
  int ret;
  va_list va2;
  va_copy(va2, va1);
  {
    size_t argc, i;
    const char **argv;
    char **envp = environ;

    argc = 1;
    if (argv0 != NULL) {
      for (argc = 1; ; argc++) {
        char *v = va_arg(va1, char *);
        if (v == NULL) {
          if (flage) envp = va_arg(va1, char **); 
          break;
        }
      }
    } else {
      if (flage) envp = va_arg(va1, char **);
    }
    argv = malloc((argc + 1) * sizeof(char *));
    argv[0] = argv0;
    for (i = 1; i < argc; i++) {
      char *v = va_arg(va2, char *);
      argv[i] = v;
    }
    argv[i] = NULL;
    if (flagp)
	ret = __execvpe(path, (char *const *)argv, envp);
    else
	ret = execve(path, (char *const *)argv, envp);
    free(argv);
  }
  va_end(va2);
  return ret;
}

/**
 * @fn int execl(const char *path, const char *arg0, ...)
 * @brief execute a file.
 * @param path path of executable file.
 * @param arg0 arguments.
 * @return do not return on success , -1 on error.
 *
 * @crashcase invalid pointer, buffer overflow, non-terminated string
 * @fsctype string(ro), array(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
int execl(const char *path, const char *argv0, ...)
{
  int ret;
  va_list va;
  va_start(va, argv0);
  ret = execlep(path, argv0, va, 0, 0);
  va_end(va);
  return ret;
}

/**
 * @fn int execle(const char *path, const char *arg0, ...)
 * @brief execute a file.
 * @param path path of executable file.
 * @param arg0 arguments.
 * @return do not return on success , -1 on error.
 *
 * @crashcase invalid pointer, buffer overflow, non-terminated string
 * @fsctype string(ro), array(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
int execle(const char *path, const char *argv0, ...)
{
  int ret;
  va_list va;
  va_start(va, argv0);
  ret = execlep(path, argv0, va, 1, 0);
  va_end(va);
  return ret;
}

/**
 * @fn int execlp(const char *path, const char *arg0, ...)
 * @brief execute a file.
 * @param path path of executable file.
 * @param arg0 arguments.
 * @return do not return on success , -1 on error.
 *
 * @crashcase invalid pointer, buffer overflow, non-terminated string
 * @fsctype string(ro), array(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
int execlp(const char *path, const char *argv0, ...)
{
  int ret;
  va_list va;
  va_start(va, argv0);
  ret = execlep(path, argv0, va, 0, 1);
  va_end(va);
  return ret;
}
