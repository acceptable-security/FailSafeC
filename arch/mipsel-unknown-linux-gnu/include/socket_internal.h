/* Generated file -- do not edit. */
/**
 * @file include/socket_internal.h
 */
#define SCM_RIGHTS  1

#define SOCK_DGRAM      1
#define SOCK_RAW        3
#define SOCK_SEQPACKET  5
#define SOCK_STREAM     2

#define SOL_SOCKET  65535

#define SO_ACCEPTCONN  4105
#define SO_BROADCAST   32
#define SO_DEBUG       1
#define SO_DONTROUTE   16
#define SO_ERROR       4103
#define SO_KEEPALIVE   8
#define SO_LINGER      128
#define SO_OOBINLINE   256
#define SO_RCVBUF      4098
#define SO_RCVLOWAT    4100
#define SO_RCVTIMEO    4102
#define SO_REUSEADDR   4
#define SO_SNDBUF      4097
#define SO_SNDLOWAT    4099
#define SO_SNDTIMEO    4101
#define SO_TYPE        4104

#define SOMAXCONN  128

#define MSG_CTRUNC     8
#define MSG_DONTROUTE  4
#define MSG_EOR        128
#define MSG_OOB        1
#define MSG_PEEK       2
#define MSG_TRUNC      32
#define MSG_WAITALL    256

#define AF_INET    2
#define AF_INET6   10
#define AF_UNIX    1
#define AF_UNSPEC  0

/* non-standard */
#define PF_INET    2
#define PF_INET6   10
#define PF_UNIX    1
#define PF_UNSPEC  0

#define SHUT_RD    0
#define SHUT_RDWR  2
#define SHUT_WR    1

/* TODO this should not be here */
#define MAXHOSTNAMELEN  64
