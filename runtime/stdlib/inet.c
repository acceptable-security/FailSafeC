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
 * @file stdlib/inet.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <arpa/inet.h>

#if 0
/**
 * @fn uint32_t htonl(uint32_t host)
 * @brief convert host byte order to network byte order.(32bit)
 * @param host number to convert
 * @return number comverted.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fi_i_htonl(base_t host_b, unsigned int host)
{
  return value_of_int(htonl(host));
}

/**
 * @fn uint16_t htons(uint16_t host)
 * @brief convert host byte order to network byte order.(16bit)
 * @param host number to convert
 * @return number comverted.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
unsigned short FS_Fs_s_htons(unsigned short host)
{
  return htons(host);
}

/**
 * @fn uint32_t ntohl(uint32_t net)
 * @brief convert network byte order to host byte order.(32bit)
 * @param net number to convert
 * @return number comverted.
 *
 * @crashcase none
 * @fsctype atomic
 *
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fi_i_ntohl(base_t net_b, unsigned int net)
{
  return value_of_int(ntohl(net));
}


/**
 * @fn uint16_t ntohs(uint16_t net)
 * @brief convert network byte order to host byte order.(16bit)
 * @param net number to convert
 * @return number comverted.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
unsigned short FS_Fs_s_ntohs(unsigned short net)
{
  return ntohs(net);
}

#endif

/**
 * @fn in_addr_t inet_addr(const char *s)
 * @brief convert IPv4 address string to in_addr_t
 * @param s IPv4 address string
 * @retval !=-1 success.
 * @retval -1 error, or 255.255.255.255
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPc_i_inet_addr(base_t s_b, ofs_t s_o)
{
  int ret;
  void *t;
  char *s = wrapper_get_string_z(s_b, s_o, &t, "inet_addr");

  ret = inet_addr(s);

  wrapper_release_tmpbuf(t);
  return value_of_int(ret);
}

struct struct_Sn14stdlib_in_addr_ {
  union fsc_initU_i s_addr;
};
#include <stdio.h>
#include <string.h>
/**
 * @fn char *inet_ntoa(struct in_addr a)
 * @brief convert struct in_addr to IPv4 address string.
 * @param a struct in_addr IPv4 address
 * @return IPv4 address string.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype return_string
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FS_FSn14stdlib_in_addr__Pc_inet_ntoa(struct struct_Sn14stdlib_in_addr_ a)
{
  char *ret;
  struct in_addr in;
  in.s_addr = int_of_value(a.s_addr.cv);

  ret = inet_ntoa(in);

  return wrapper_make_new_static_string(ret);
}
