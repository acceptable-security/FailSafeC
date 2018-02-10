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
 * @file stdlib/socket.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <copydata.h>

#include <sys/socket.h>
#include <sys/select.h>
#include <limits.h>
#include <errno.h>
#include <limits.h> /* for SSIZE_MAX in glibc 2.9 (?) */
#include <string.h>

/**
 * @fn int connect(int sock, const struct sockaddr *addr, socklen_t addr_len);
 * @param sock socket descriptor to connnect.
 * @param addr peer address.
 * @param addr_len length of addr.
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype atomic, struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn15stdlib_sockaddr_i_i_connect(base_t sock_b, unsigned int sock_o,
                                            base_t addr_b, ofs_t addr_o,
                                            base_t len_b, unsigned int len_o)
{
  void *addr, *tmp;
  int ret;

  addr = wrapper_get_rawimage(addr_b, addr_o, &tmp, len_o, "connect");
  ret = connect(sock_o, addr, len_o);
  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn int ssize_t recv(int socket, void *buffer, size_t length, int flags)
 * @brief receive data from socket.
 * @param socket socket descriptor.
 * @param buffer destination buffer.
 * @param length buffer size.
 * @param flags flags. 0 or bit OR of MSG_PEEK, MSG_OOB, MSG_WAITALL.
 * @retval >=0 successed. number of bytes read returned.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPvii_i_recv(base_t socket_b, unsigned int socket,
                       base_t buffer_b, ofs_t buffer_o,
                       base_t length_b, unsigned int length,
                       base_t flags_b,   unsigned int flags)
{
  void *buffer, *tmp;
  int ret;

  if (length == 0) { return value_of_int(0); }

  buffer = wrapper_get_read_buffer(buffer_b, buffer_o, &tmp, length, "recv");
  ret = recv(socket, buffer, length, flags);

  if(ret > 0){
    wrapper_writeback_release_tmpbuf(buffer_b, buffer_o, tmp, ret);
  }else{
    wrapper_release_tmpbuf(tmp);
  }
  return value_of_int(ret);
}

/**
 * @fn int ssize_t send(int socket, const void *buffer, size_t length, int flags)
 * @brief send data on socket.
 * @param socket socket descriptor.
 * @param buffer buffer containing data to send
 * @param length data size to send
 * @param flags flags. 0 or bit OR of MSG_EPR, MSG_OOB.
 * @retval >=0 successed. number of bytes sent returned.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPvii_i_send(base_t socket_b, unsigned int socket,
                       base_t buffer_b, ofs_t buffer_o,
                       base_t length_b, unsigned int length,
                       base_t flags_b,   unsigned int flags)
{
  void *buffer, *tmp;
  int ret;

  if (length == 0) { return value_of_int(0); }

  buffer = wrapper_get_rawimage(buffer_b, buffer_o, &tmp, length, "send");
  ret = send(socket, buffer, length, flags);
  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn int recvfrom(int socket, void *buffer, size_t length, int flags, struct sockaddr *address, socklen_t *address_length)
 * @param socket socket to receive.
 * @param buffer destination byte array.
 * @param length number of bytes.
 * @param flags  type of reception.
 * @param address source address is returned.
 * @param address_length size of address.
 * @retval >=0 number of bytes received.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic), memory(rw)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPviiPSn15stdlib_sockaddr_Pi_i_recvfrom(base_t socket_b, unsigned int socket,
                                                  base_t buffer_b, ofs_t buffer_o,
                                                  base_t length_b, unsigned int length,
                                                  base_t flags_b, unsigned int flags,
                                                  base_t address_b, ofs_t address_o,
                                                  base_t address_length_b, ofs_t address_length_o)
{
  void *buf, *t1;
  void *addr, *t2;
  socklen_t addr_len;
  int ret;

  if (length == 0) return value_of_int(0);

  buf = wrapper_get_read_buffer(buffer_b, buffer_o, &t1, length, "recvfrom");
  if (fsc_is_nullpointer(address_b, address_o)) {
    addr_len = 0;
    addr = t2 = NULL;
  } else {
    addr_len = read_word(address_length_b, address_length_o);
    addr = wrapper_get_read_buffer(address_b, address_o, &t2, addr_len, "recvfrom");
  }
  ret = recvfrom(socket, buf, length, flags, addr, addr ? &addr_len: NULL);

  if(ret >= 0){
    wrapper_writeback_release_tmpbuf(buffer_b, buffer_o, t1, ret);
  }else{
    wrapper_release_tmpbuf(t1);
  }
  if (base_remove_castflag(address_b) != 0) {
    wrapper_writeback_release_tmpbuf(address_b, address_o, t2, addr_len);
    write_word(address_length_b, address_length_o, value_of_int(addr_len), NULL);
  }
  return value_of_int(ret);
}

/**
 * @fn int sendto(int socket, const void *buffer, size_t length, int flags, const struct sockaddr *address, socklen_t address_length)
 * @brief send bytes to socket.
 * @param socket socket to send.
 * @param buffer bytes array to send.
 * @param length number of bytes.
 * @param flags type of send.
 * @param address destination address.
 * @param address_length size of address.
 * @retval >=0 number of bytes sent.
 * @retval -1 error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic), memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPviiPSn15stdlib_sockaddr_i_i_sendto(base_t socket_b, unsigned int socket,
                                               base_t buffer_b, ofs_t  buffer_o,
                                               base_t length_b, unsigned int length,
                                               base_t flags_b, unsigned int flags,
                                               base_t address_b, ofs_t address_o,
                                               base_t address_length_b, unsigned int address_length)
{
  void *buf, *t1;
  void *addr = NULL, *t2 = NULL;
  int ret;

  if (length == 0) return value_of_int(0);

  buf = wrapper_get_rawimage(buffer_b, buffer_o, &t1, length, "sendto");
  if (base_remove_castflag(address_b) != 0) {
    addr = wrapper_get_rawimage(address_b, address_o, &t2, address_length, "sendto");
  }
  ret = sendto(socket, buf, length, flags, addr, address_length);
  wrapper_release_tmpbuf(t1);
  if (addr) {
    wrapper_release_tmpbuf(t2);
  }
  return value_of_int(ret);
}

/**
 * @fn ssize_t recvmsg(int socket, struct msghdr *message, int flags)
 * @brief receive message from socket.
 * @param socket socket descriptor.
 * @param message destination buffer.
 * @param flags flag.
 * @retval >=0 success, number of received bytes
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn13stdlib_msghdr_i_i_recvmsg(base_t socket_b, int socket_o,
                                          base_t msg_b, ofs_t msg_o,
                                          base_t flags_b, int flags_o)
{
  struct msghdr m;
  void *t1 = NULL, *t2 = NULL;
  void *name;
  struct iovec iov[UIO_MAXIOV];
  void *t[UIO_MAXIOV];
  int i;
  int ret;
  int retlen;
  value m_name       = read_word(msg_b, msg_o + 0);
  value m_namelen    = read_word(msg_b, msg_o + 4);
  value m_iov        = read_word(msg_b, msg_o + 8);
  value m_iovlen     = read_word(msg_b, msg_o + 12);
  value m_control    = read_word(msg_b, msg_o + 16);
  value m_controllen = read_word(msg_b, msg_o + 20);
  value m_flags      = read_word(msg_b, msg_o + 24);
  base_t m_iov_b = base_of_value(m_iov);
  ofs_t m_iov_o  = ofs_of_value(m_iov);
  int iov_len = int_of_value(m_iovlen);
  ssize_t total_size = 0;

  if (iov_len <= 0 || iov_len > UIO_MAXIOV) {
    errno = EMSGSIZE;
    return value_of_int(-1);
  }

  for(i = 0; i < iov_len; i++){
    size_t len = int_of_value(read_word(m_iov_b, m_iov_o + i * 8 + 4));

    if(len > SSIZE_MAX || total_size + (ssize_t)len < total_size){
      errno = EINVAL;
      return value_of_int(-1);
    }

    iov[i].iov_len = len;
    total_size += len;
  }

  m.msg_namelen = int_of_value(m_namelen);
  if (base_remove_castflag(base_of_value(m_name))) {
    m.msg_name = wrapper_get_rawimage(base_of_value(m_name), ofs_of_value(m_name), &t1, m.msg_namelen, "recvmsg");
  } else {
    m.msg_name = NULL;
  }

  m.msg_iovlen = iov_len;
  for (i = 0; i < iov_len; i++) {
    value base = read_word(m_iov_b, m_iov_o + i * 8);
    iov[i].iov_base = wrapper_get_read_buffer(base_of_value(base),
                                              ofs_of_value(base),
                                              &t[i],
                                              iov[i].iov_len,
                                              "recvmsg");
  }
  m.msg_iov = iov;

  m.msg_controllen = int_of_value(m_controllen);
  if (base_remove_castflag(base_of_value(m_control))) {
    m.msg_control = wrapper_get_read_buffer(base_of_value(m_control), ofs_of_value(m_control), &t2,
                                            m.msg_controllen, "recvmsg");
  } else {
    m.msg_control = NULL;
  }
  m.msg_flags = int_of_value(m_flags);

  ret = recvmsg(socket_o, &m, flags_o);
  if (m.msg_name) {
    if (ret > 0) {
      wrapper_writeback_release_tmpbuf(base_of_value(m_name), ofs_of_value(m_name), t1, m.msg_namelen);
    } else {
      wrapper_release_tmpbuf(t1);
    }
  }
  if (ret > 0) {
    write_word(msg_b, msg_o + 4, value_of_int(m.msg_namelen), NULL);
  }
  
  retlen = ret;
  for (i = 0; i < m.msg_iovlen; i++) {
    value base = read_word(m_iov_b, m_iov_o + i * 8);
    value len  = read_word(m_iov_b, m_iov_o + i * 8 + 4);
    if (retlen > 0) {
      int x = (int_of_value(len) > retlen) ? retlen : int_of_value(len);
      wrapper_writeback_release_tmpbuf(base_of_value(base), ofs_of_value(base), t[i], x);
      retlen -= x;
    } else {
      wrapper_release_tmpbuf(t[i]);
    }
  }

  if (m.msg_control) {
    if (ret > 0) {
      wrapper_writeback_release_tmpbuf(base_of_value(m_control), ofs_of_value(m_control), t2, m.msg_controllen);
    } else {
      wrapper_release_tmpbuf(t2);
    }
  }
  if (ret > 0) {
    write_word(msg_b, msg_o + 20, value_of_int(m.msg_controllen), NULL);
    write_word(msg_b, msg_o + 24, value_of_int(m.msg_flags), NULL);
  }
  return value_of_int(ret);
}

/**
 * @fn ssize_t sendmsg(int socket, const struct msghdr *message, int flags)
 * @brief send message on socket.
 * @param socket socket descriptor.
 * @param message message to send.
 * @param flags flag.
 * @retval >=0 success, number of sent bytes.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(pointer)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn13stdlib_msghdr_i_i_sendmsg(base_t socket_b, int socket_o,
                                          base_t msg_b, ofs_t msg_o,
                                          base_t flags_b, int flags_o)
{
  struct msghdr m;
  void *t1 = NULL, *t2 = NULL;
  void *name;
  struct iovec iov[UIO_MAXIOV];
  void *t[UIO_MAXIOV];
  int i;
  int ret;
  int retlen;
  value m_name       = read_word(msg_b, msg_o + 0);
  value m_namelen    = read_word(msg_b, msg_o + 4);
  value m_iov        = read_word(msg_b, msg_o + 8);
  value m_iovlen     = read_word(msg_b, msg_o + 12);
  value m_control    = read_word(msg_b, msg_o + 16);
  value m_controllen = read_word(msg_b, msg_o + 20);
  value m_flags      = read_word(msg_b, msg_o + 24);
  base_t m_iov_b = base_of_value(m_iov);
  ofs_t m_iov_o  = ofs_of_value(m_iov);
  int iov_len = int_of_value(m_iovlen);
  ssize_t total_size = 0;

  if (iov_len <= 0 || iov_len > UIO_MAXIOV) {
    errno = EMSGSIZE;
    return value_of_int(-1);
  }

  for(i = 0; i < iov_len; i++){
    size_t len = int_of_value(read_word(m_iov_b, m_iov_o + i * 8 + 4));

    if(len > SSIZE_MAX || total_size + (ssize_t)len < total_size){
      errno = EINVAL;
      return value_of_int(-1);
    }

    iov[i].iov_len = len;
    total_size += len;
  }

  m.msg_namelen = int_of_value(m_namelen);
  if (base_remove_castflag(base_of_value(m_name))) {
    m.msg_name = wrapper_get_rawimage(base_of_value(m_name), ofs_of_value(m_name), &t1, m.msg_namelen, "sendmsg");
  } else {
    m.msg_name = NULL;
  }

  m.msg_iovlen = iov_len;
  for (i = 0; i < iov_len; i++) {
    value base = read_word(m_iov_b, m_iov_o + i * 8);
    iov[i].iov_base = wrapper_get_rawimage(base_of_value(base),
                                           ofs_of_value(base),
                                           &t[i],
                                           iov[i].iov_len,
                                           "sendmsg"
                                           );
  }
  m.msg_iov = iov;

  m.msg_controllen = int_of_value(m_controllen);
  if (base_remove_castflag(base_of_value(m_control))) {
    m.msg_control = wrapper_get_rawimage(base_of_value(m_control), ofs_of_value(m_control), &t2,
                                         m.msg_controllen, "sendmsg");
  } else {
    m.msg_control = NULL;
  }
  m.msg_flags = int_of_value(m_flags);

  ret = sendmsg(socket_o, &m, flags_o);
  if (m.msg_name) {
    wrapper_release_tmpbuf(t1);
  }
  
  for (i = 0; i < m.msg_iovlen; i++) {
    wrapper_release_tmpbuf(t[i]);
  }

  if (m.msg_control) {
    wrapper_release_tmpbuf(t2);
  }
  return value_of_int(ret);
}

#if 0
/**
 * @fn int shutdown(int sock, int how)
 * @brief shutwodn socket
 * @param sock socket descriptor.
 * @param how one of SHUT_RD, SHUT_WR, SHUT_RDWR
 * @retval 0 sccessed.
 * @retval -1 failed.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fii_i_shutdown(base_t sock_b, int sock_o, base_t how_b, int how_o)
{
  return value_of_int(shutdown(sock_o, how_o));
}
#endif

/**
 * @fn int getsockopt(int socket, int level, int option_name, void *option_value, socklen_t *option_len)
 * @brief get socket option.
 * @param socket socket to get option.
 * @param level protocol level.
 * @param option_name option name to get. 
 * @param option_value option value to get. 
 * @param option_len length of option value.
 * @retval 0 success.
 * @retval -1 fail.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(rw)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiiiPvPi_i_getsockopt(base_t sock_b, int sock_o,
                               base_t level_b, int level_o,
                               base_t option_name_b, int option_name_o,
                               base_t option_value_b, ofs_t option_value_o,
                               base_t option_len_b, unsigned int option_len_o
                               )
{
  int ret;
  int option_len = read_word(option_len_b, option_len_o);

  void *tmp;
  void *option_value = wrapper_get_read_buffer(option_value_b, option_value_o, &tmp, option_len, "getsockopt");

  ret = getsockopt(sock_o, level_o, option_name_o, option_value, &option_len);

  wrapper_writeback_release_tmpbuf(option_value_b, option_value_o, tmp, option_len);
  write_word(option_len_b, option_len_o, value_of_int(option_len), NULL);
  return ret;
}

/**
 * @fn int setsockopt(int socket, int level, int option_name, const void *option_value, socklen_t option_len)
 * @brief set socket options.
 * @param socket socket to set option.
 * @param level protocol level.
 * @param option_name option name to set.
 * @param option_value pointer to option value.
 * @param option_len length of option value.
 * @retval 0 success.
 * @retval -1 fail.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiiiPvi_i_setsockopt(base_t sock_b, unsigned int sock,
                              base_t level_b, unsigned int level,
                              base_t option_name_b, unsigned int option_name,
                              base_t option_value_b, ofs_t option_value_o,
                              base_t option_len_b, unsigned int option_len)
{
  int ret;

  if (option_len != 0) {
    void *to_discard;
    char *option_value = wrapper_get_rawimage(option_value_b, option_value_o, &to_discard, option_len, "setsockopt");
    ret = setsockopt(sock, level, option_name, option_value, option_len);
    wrapper_release_tmpbuf(to_discard);
  } else {
    ret = setsockopt(sock, level, option_name, NULL, option_len);
  }

  return value_of_int(ret);
}

/**
 * @fn int accept(int sock, struct sockaddr *addr, socklen_t *len)
 * @param sock socket descriptor
 * @param addr peer address
 * @param len peer address length
 * @retval >=0 success, accepted socket descriptor returned.
 * @retval -1 error occured.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn15stdlib_sockaddr_Pi_i_accept(base_t sock_b, int sock_o,
                                            base_t addr_b, ofs_t addr_o,
                                            base_t len_b, ofs_t len_o
                                            )
{
  int ret;
  if (base_remove_castflag(addr_b)) {
    void *addr, *tmp;
    int len = read_word(len_b, len_o);

    addr = wrapper_get_read_buffer(addr_b, addr_o, &tmp, len, "accept");
    ret = accept(sock_o, addr, &len);
    wrapper_writeback_release_tmpbuf(addr_b, addr_o, tmp, len);
    write_word(len_b, len_o, value_of_int(len), NULL);
  } else {
    ret = accept(sock_o, NULL, NULL);
  }

  return value_of_int(ret);
}

/**
 * @fn int bind(int sock, const struct sockaddr *addr, socklen_t len)
 * @brief bind socket and name.
 * @param sock socket descriptor
 * @param addr address
 * @param len address length
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn15stdlib_sockaddr_i_i_bind(base_t sock_b, int sock_o,
                                         base_t addr_b, ofs_t addr_o,
                                         base_t len_b, unsigned int len_o
                                         )
{
  void *addr, *tmp;
  int ret;

  addr = wrapper_get_rawimage(addr_b, addr_o, &tmp, len_o, "bind");
  ret = bind(sock_o, addr, len_o);
  wrapper_release_tmpbuf(tmp);

  return value_of_int(ret);
}

/**
 * @fn int getsockname(int sock, struct sockaddr *addr, socklen_t *len)
 * @brief get socket address.
 * @param sock socket descriptor
 * @param addr address
 * @param len address length
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn15stdlib_sockaddr_Pi_i_getsockname(base_t sock_b, int sock_o,
                                                 base_t addr_b, ofs_t addr_o,
                                                 base_t len_b, ofs_t len_o
                                                 )
{
  void *addr, *tmp;
  int ret;
  int len = read_word(len_b, len_o);

  addr = wrapper_get_read_buffer(addr_b, addr_o, &tmp, len, "getsockname");
  ret = getsockname(sock_o, addr, &len);
  wrapper_writeback_release_tmpbuf(addr_b, addr_o, tmp, len);
  write_word(len_b, len_o, value_of_int(len), NULL);

  return value_of_int(ret);
}

/**
 * @fn int getpeername(int sock, struct sockaddr *addr, socklen_t *len)
 * @brief get socket peer address.
 * @param sock socket descriptor
 * @param addr address
 * @param len address length
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPSn15stdlib_sockaddr_Pi_i_getpeername(base_t sock_b, int sock_o,
                                                 base_t addr_b, ofs_t addr_o,
                                                 base_t len_b, ofs_t len_o
                                                 )
{
  void *addr, *tmp;
  int ret;
  int len = read_word(len_b, len_o);

  addr = wrapper_get_read_buffer(addr_b, addr_o, &tmp, len, "getpeername");
  ret = getpeername(sock_o, addr, &len);
  wrapper_writeback_release_tmpbuf(addr_b, addr_o, tmp, len);
  write_word(len_b, len_o, value_of_int(len), NULL);

  return value_of_int(ret);
}

#if 0
/**
 * @fn int listen(int sock, int backlog)
 * @brief
 * @param sock socket descriptor
 * @param backlog queue length
 * @retval 0 success.
 * @retval -1 error occured.
 *
 * @crashcase none
 * @fsctype atomic
 *
 * @author Lepidum Co., Ltd.
 */
value FS_Fii_i_listen(base_t sock_b, int sock_o,
                      base_t back_b, int back_o
                      )
{
  return value_of_int(listen(sock_o, back_o));
}
#endif
