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
 * @file stdlib/syslog.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <syslog.h>
#include <errno.h>

/**
 * @fn void closelog(void)
 * @brief close system log
 *
 * @crashcase none
 * @fsctype none
 *
 * @author Lepidum Co., Ltd.
 */
void FS_F_v_closelog(void)
{
  closelog();
}

/**
 * @fn void openlog(const char *ident, int logopt, int facility)
 * @brief open system log
 * @param ident string appeared in each message.
 * @param logopt bit OR of LOG_PID, LOG_CONS, LOG_NDELAY, LOG_ODELAY, LOG_NOWAIT
 * @param facility log facility. (LOG_KERN, LOG_USER, etc...)
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
void FS_FPcii_v_openlog(base_t ident_b, ofs_t ident_o,
                        base_t logopt_b, unsigned int logopt_o,
                        base_t facility_b, unsigned int facility_o)
{
  void *tmp;
  char *ident = wrapper_get_string_z(ident_b, ident_o, &tmp, "openlog");

  openlog(ident, logopt_o, facility_o);

  wrapper_release_tmpbuf(tmp);
  return;
}

/**
 * @fn int setlogmask(int mask)
 * @brief set log priority mask.
 * @param mask new priority mask. if mask is 0, log mask not changed.
 * @return old log priority mask.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fi_i_setlogmask(base_t  mask_b, unsigned int mask_o)
{
  return value_of_int(setlogmask(mask_o));
}

extern value FS_FPcPcV_i_sprintf(base_t, ofs_t, base_t, ofs_t, base_t, ofs_t);

/**
 * @fn void syslog(int priority, const char *message, ...)
 * @brief send log message
 * @param priority log priority.
 * @param message message format string.
 * @param ... format arguments.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype va, string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
void FS_FiPcV_v_syslog(base_t priority_b, unsigned int priority_o,
                       base_t message_b, ofs_t  message_o,
                       base_t va_b, ofs_t va_o)
{
  base_t msg_b;
  char *msg, *p;
  void *tmp;
  int saved_errno;

  /* TODO: errno may changed by fsc_alloc_block_library? */
  saved_errno = errno;

  msg_b = fsc_alloc_block_library(&fsc_typeinfo_c.val, 1024UL);

  /* TODO: %m conversion */
  FS_FPcPcV_i_sprintf(msg_b, 0, message_b, message_o, va_b, va_o);

  msg = wrapper_get_string_z(msg_b, 0, &tmp, "syslog");

  syslog(priority_o, "%s", msg);

  wrapper_release_tmpbuf(tmp);
  fsc_dealloc_internal(msg_b, "syslog");

  return;
}
