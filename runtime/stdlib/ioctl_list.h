#define IOC(n) ((n)+0x010C0000)

/* renumber ioctl request */
/*          number         name      is_ptr            type    device */
IOCTL_ENTRY(IOC(0),    FIONREAD,        PTR,            int,  FIFO||SOCK)
IOCTL_ENTRY(IOC(1),  TIOCGWINSZ,        PTR, struct winsize,  TTY)
IOCTL_ENTRY(IOC(2),  TIOCSWINSZ,        PTR, struct winsize,  TTY)
