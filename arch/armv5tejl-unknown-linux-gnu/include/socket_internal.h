/* Generated file -- do not edit. */
/**
 * @file include/socket_internal.h
 */
#define SCM_RIGHTS  1

#define SOCK_DGRAM      2
#define SOCK_RAW        3
#define SOCK_SEQPACKET  5
#define SOCK_STREAM     1

#define SOL_SOCKET  1

#define SO_ACCEPTCONN  30
#define SO_BROADCAST   6
#define SO_DEBUG       1
#define SO_DONTROUTE   5
#define SO_ERROR       4
#define SO_KEEPALIVE   9
#define SO_LINGER      13
#define SO_OOBINLINE   10
#define SO_RCVBUF      8
#define SO_RCVLOWAT    18
#define SO_RCVTIMEO    20
#define SO_REUSEADDR   2
#define SO_SNDBUF      7
#define SO_SNDLOWAT    19
#define SO_SNDTIMEO    21
#define SO_TYPE        3

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
