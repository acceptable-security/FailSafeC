/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/netdb.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <string.h>

#include <stdlib_util.h>

extern struct typeinfo_init fsc_typeinfo_Pc;

struct argv_data {
  int argc;
  void *data[1];
};

/**
 * @brief copy argv-like string array.
 * @param argv_b base of argv.
 * @param argv_o offset of argv.
 * @param argc argc value if argv is not null-terminated, else -1.
 * @return raw string array is returned.
 *
 * @author Lepidum Co., Ltd.
 */
char **copy_argv_to_raw(base_t argv_b, ofs_t argv_o, void **pointer_to_discard, int argc, const char *libcloc)
{
  struct argv_data *p;
  void **to_discard;
  size_t *len;
  char **argv;
  int count;
  int i;

  if (argc == -1) {
    for (count = 0;; count++) {
      value s = read_word(argv_b, argv_o + 4 * count);
      if (fsc_is_nullpointer(base_of_value(s), ofs_of_value(s))) {
        break;
      }
    }
  } else {
    count = argc;
  }
  /* struct {
   *   int argc;
   *   void *to_discard[argc];
   *   size_t len[argc];
   *   char *argv[argc + 1];
   * };
   */
  p = (struct argv_data *)fsc_alloc_raw(sizeof(int) + sizeof(void *) * (3 * count + 1));
  p->argc = count;
  to_discard = &p->data[0];
  len = (size_t *)&p->data[count];
  argv = (char **)&p->data[2 * count];
  for (i = 0; i < count; i++) {
    value  s     = read_word(argv_b, argv_o + 4 * i);
    base_t str_b = base_of_value(s);
    ofs_t  str_o = ofs_of_value(s);
    if (base_remove_castflag(str_b)) {
      char  *str =  wrapper_get_string_z(str_b, str_o, &to_discard[i], libcloc);
      argv[i] = str;
      len[i] = strlen(str);
    } else {
      to_discard[i] = NULL;
      argv[i] = NULL;
      len[i] = 0;
    }
  }
  argv[i] = NULL;
  *pointer_to_discard = p;
  return argv;
}

/**
 * @brief copy argv-like string array.
 * @param argv0_b base of argv0
 * @param argv0_o offset of argv0
 * @param argv_b base of argv.
 * @param argv_o offset of argv.
 * @param argc argc value if argv is not null-terminated, else -1.
 * @return raw string array is returned.
 *
 * @author Lepidum Co., Ltd.
 */
char **copy_argv0_argv_to_raw(base_t argv0_b, ofs_t argv0_o,
                              base_t argv_b, ofs_t argv_o,
                              void **pointer_to_discard, int argc, const char *libcloc)
{
  struct argv_data *p;
  void **to_discard;
  size_t *len;
  char **argv;
  int count;
  int i;

  if (argc == -1) {
    for (count = 0;; count++) {
      if (count == 0) {
        if (fsc_is_nullpointer(argv0_b, argv0_o)) {
          break;
        }
      } else {
        value s = read_word(argv_b, argv_o + 4 * count - 4);
        if (fsc_is_nullpointer(base_of_value(s), ofs_of_value(s))) {
          break;
        }
      }
    }
  } else {
    count = argc;
  }
  /* struct {
   *   int argc;
   *   void *to_discard[argc];
   *   size_t len[argc];
   *   char *argv[argc + 1];
   * };
   */
  p = (struct argv_data *)fsc_alloc_raw(sizeof(int) + sizeof(void *) * (3 * count + 1));
  p->argc = count;
  to_discard = &p->data[0];
  len = (size_t *)&p->data[count];
  argv = (char **)&p->data[2 * count];
  
  for (i = 0; i < count; i++) {
    base_t str_b;
    ofs_t  str_o;
    if (i == 0) {
      str_b = argv0_b;
      str_o = argv0_o;
    } else {
      value  s     = read_word(argv_b, argv_o + 4 * i - 4);
      str_b = base_of_value(s);
      str_o = ofs_of_value(s);
    }
    if (!fsc_is_nullpointer(str_b, str_o)) {
      char  *str =  wrapper_get_string_z(str_b, str_o, &to_discard[i], libcloc);
      argv[i] = str;
      len[i] = strlen(str);
    } else {
      to_discard[i] = NULL;
      argv[i] = NULL;
      len[i] = 0;
    }
  }
  argv[i] = NULL;
  *pointer_to_discard = p;
  return argv;
}

void release_argv_tempbuf(void *to_discard)
{
  struct argv_data *p = to_discard;
  int i;
  for (i = 0; i < p->argc; i++) {
    wrapper_release_tmpbuf(p->data[i]);
  }
  fsc_release_raw(p);
}

void writeback_argv_tempbuf(base_t argv_b, ofs_t argv_o, void *to_discard)
{
  struct argv_data *p = to_discard;
  int i;
  void **to_discards = &p->data[0];
  size_t *len = (size_t *)&p->data[p->argc];
  
  for (i = 0; i < p->argc; i++) {
    value  s     = read_word(argv_b, argv_o + 4 * i);
    base_t str_b = base_of_value(s);
    ofs_t  str_o = ofs_of_value(s);
    wrapper_writeback_release_tmpbuf(str_b, str_o, to_discards[i], len[i] + 1);
  }
  fsc_release_raw(p);
}

/*ptrvalue copy_argv_from_raw(const char *const *argv, int *argc)
{
  ptrvalue *p;
  int i, count;

  for (count = 0; argv[count]; count++)
    ;
  p = (ptrvalue *)fsc_alloc_static_block(&fsc_typeinfo_Pc.val, count + 1);
  for (i = 0; i < count; i++) {
    p[i] = wrapper_make_new_static_string(argv[i]);
  }
  p[i] = ptrvalue_of_base_ofs(0, 0);
  return p;
}

*/
