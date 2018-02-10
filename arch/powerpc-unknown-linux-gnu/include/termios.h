/* Generated file -- do not edit. */
/**
 * @file termios.h
 */

#ifndef __TERMIOS_H
#define __TERMIOS_H

#include <sys/__types.h>

#ifndef __CC_T
#define __CC_T
typedef __cc_t cc_t;
#endif

#ifndef __SPEED_T
#define __SPEED_T
typedef __speed_t speed_t;
#endif

#ifndef __TCFLAG_T
#define __TCFLAG_T
typedef __tcflag_t tcflag_t;
#endif

#ifndef __PID_T
#define __PID_T
typedef __pid_t pid_t;
#endif

#define NCCS 32

struct __fsc_attribute__((named "stdlib_termios")) termios {
  tcflag_t c_iflag;
  tcflag_t c_oflag;
  tcflag_t c_cflag;
  tcflag_t c_lflag;
  cc_t c_cc[NCCS];
  cc_t __c_line;  /* linux */
  speed_t __c_ispeed;
  speed_t __c_ospeed;
};

struct __fsc_attribute__((named "stdlib_winsize")) winsize {
  unsigned short ws_row;
  unsigned short ws_col;
  unsigned short ws_xpixel;
  unsigned short ws_ypixel;
};

#define VEOF   4
#define VEOL   6
#define VERASE 2
#define VINTR  0
#define VKILL  3
#define VMIN   5
#define VQUIT  1
#define VSTART 13
#define VSTOP  14
#define VSUSP  12
#define VTIME  7

#define BRKINT 2
#define ICRNL  256
#define IGNBRK 1
#define IGNCR  128
#define IGNPAR 4
#define INLCR  64
#define INPCK  16
#define ISTRIP 32
#define IXANY  2048
#define IXOFF  1024
#define IXON   512
#define PARMRK 8

#define OPOST  1
#define ONLCR  2
#define OCRNL  8
#define ONOCR  16
#define ONLRET 32
#define OFILL  64
#define NLDLY  768
#define   NL0  0
#define   NL1  256
#define CRDLY  12288
#define   CR0  0
#define   CR1  4096
#define   CR2  8192
#define   CR3  12288
#define TABDLY 3072
#define   TAB0 0
#define   TAB1 1024
#define   TAB2 2048
#define   TAB3 3072
#define BSDLY  32768
#define   BS0  0
#define   BS1  32768
#define VTDLY  65536
#define   VT0  0
#define   VT1  65536
#define FFDLY  16384
#define   FF0  0
#define   FF1  16384

#define B0     0
#define B50    1
#define B75    2
#define B110   3
#define B134   4
#define B150   5
#define B200   6
#define B300   7
#define B600   8
#define B1200  9
#define B1800  10
#define B2400  11
#define B4800  12
#define B9600  13
#define B19200 14
#define B38400 15

#define CSIZE  768
#define   CS5  0
#define   CS6  256
#define   CS7  512
#define   CS8  768
#define CSTOPB 1024
#define CREAD  2048
#define PARENB 4096
#define PARODD 8192
#define HUPCL  16384
#define CLOCAL 32768

#define ECHO   8
#define ECHOE  2
#define ECHOK  4
#define ECHONL 16
#define ICANON 256
#define IEXTEN 1024
#define ISIG   128
#define NOFLSH -2147483648
#define TOSTOP 4194304

#define TCSANOW   0
#define TCSADRAIN 1
#define TCSAFLUSH 2

#define TCIOFF 2
#define TCION  3
#define TCOOFF 0
#define TCOON  1

extern speed_t cfgetispeed(const struct termios *);
extern speed_t cfgetospeed(const struct termios *);
extern int cfsetispeed(struct termios *, speed_t);
extern int cfsetospeed(struct termios *, speed_t);
extern int tcdrain(int);
extern int tcflow(int, int);
extern int tcflush(int, int);
extern int tcgetattr(int, struct termios *);
extern pid_t tcgetsid(int);
extern int tcsendbreak(int, int);
extern int tcsetattr(int, int, const struct termios *);

#endif
