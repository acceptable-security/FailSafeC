<%# /* -*- c -*- */
#include <termios.h> 
#%>
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

#define NCCS <%=d NCCS %>

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

#define VEOF   <%=d VEOF   %>
#define VEOL   <%=d VEOL   %>
#define VERASE <%=d VERASE %>
#define VINTR  <%=d VINTR  %>
#define VKILL  <%=d VKILL  %>
#define VMIN   <%=d VMIN   %>
#define VQUIT  <%=d VQUIT  %>
#define VSTART <%=d VSTART %>
#define VSTOP  <%=d VSTOP  %>
#define VSUSP  <%=d VSUSP  %>
#define VTIME  <%=d VTIME  %>

#define BRKINT <%=d BRKINT %>
#define ICRNL  <%=d ICRNL  %>
#define IGNBRK <%=d IGNBRK %>
#define IGNCR  <%=d IGNCR  %>
#define IGNPAR <%=d IGNPAR %>
#define INLCR  <%=d INLCR  %>
#define INPCK  <%=d INPCK  %>
#define ISTRIP <%=d ISTRIP %>
#define IXANY  <%=d IXANY  %>
#define IXOFF  <%=d IXOFF  %>
#define IXON   <%=d IXON   %>
#define PARMRK <%=d PARMRK %>

#define OPOST  <%=d OPOST  %>
#define ONLCR  <%=d ONLCR  %>
#define OCRNL  <%=d OCRNL  %>
#define ONOCR  <%=d ONOCR  %>
#define ONLRET <%=d ONLRET %>
#define OFILL  <%=d OFILL  %>
#define NLDLY  <%=d NLDLY  %>
#define   NL0  <%=d NL0    %>
#define   NL1  <%=d NL1    %>
#define CRDLY  <%=d CRDLY  %>
#define   CR0  <%=d CR0    %>
#define   CR1  <%=d CR1    %>
#define   CR2  <%=d CR2    %>
#define   CR3  <%=d CR3    %>
#define TABDLY <%=d TABDLY %>
#define   TAB0 <%=d TAB0   %>
#define   TAB1 <%=d TAB1   %>
#define   TAB2 <%=d TAB2   %>
#define   TAB3 <%=d TAB3   %>
#define BSDLY  <%=d BSDLY  %>
#define   BS0  <%=d BS0    %>
#define   BS1  <%=d BS1    %>
#define VTDLY  <%=d VTDLY  %>
#define   VT0  <%=d VT0    %>
#define   VT1  <%=d VT1    %>
#define FFDLY  <%=d FFDLY  %>
#define   FF0  <%=d FF0    %>
#define   FF1  <%=d FF1    %>

#define B0     <%=d B0     %>
#define B50    <%=d B50    %>
#define B75    <%=d B75    %>
#define B110   <%=d B110   %>
#define B134   <%=d B134   %>
#define B150   <%=d B150   %>
#define B200   <%=d B200   %>
#define B300   <%=d B300   %>
#define B600   <%=d B600   %>
#define B1200  <%=d B1200  %>
#define B1800  <%=d B1800  %>
#define B2400  <%=d B2400  %>
#define B4800  <%=d B4800  %>
#define B9600  <%=d B9600  %>
#define B19200 <%=d B19200 %>
#define B38400 <%=d B38400 %>

#define CSIZE  <%=d CSIZE  %>
#define   CS5  <%=d CS5    %>
#define   CS6  <%=d CS6    %>
#define   CS7  <%=d CS7    %>
#define   CS8  <%=d CS8    %>
#define CSTOPB <%=d CSTOPB %>
#define CREAD  <%=d CREAD  %>
#define PARENB <%=d PARENB %>
#define PARODD <%=d PARODD %>
#define HUPCL  <%=d HUPCL  %>
#define CLOCAL <%=d CLOCAL %>

#define ECHO   <%=d ECHO   %>
#define ECHOE  <%=d ECHOE  %>
#define ECHOK  <%=d ECHOK  %>
#define ECHONL <%=d ECHONL %>
#define ICANON <%=d ICANON %>
#define IEXTEN <%=d IEXTEN %>
#define ISIG   <%=d ISIG   %>
#define NOFLSH <%=d NOFLSH %>
#define TOSTOP <%=d TOSTOP %>

#define TCSANOW   <%=d TCSANOW   %>
#define TCSADRAIN <%=d TCSADRAIN %>
#define TCSAFLUSH <%=d TCSAFLUSH %>

#define TCIOFF <%=d TCIOFF %>
#define TCION  <%=d TCION  %>
#define TCOOFF <%=d TCOOFF %>
#define TCOON  <%=d TCOON  %>

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
