(foreign-declare "
#include <sys/ioctl.h>
#include <asm/termbits.h>
")

(define (tty-setup fd baud)
  ((foreign-lambda* void ((int fd)
                          (int baud))
                    "
    struct termios2 config = {0};
    if (ioctl(fd, TCGETS2, &config)) {
     fprintf(stderr, \"set-baud!: error in gets2\");
     C_return(0);
    }
    config.c_iflag     = IGNPAR;
    config.c_cflag     = CS8 | BOTHER | CLOCAL;
    config.c_oflag     = 0;
    config.c_lflag     = 0;
    config.c_ispeed    = baud;
    config.c_ospeed    = baud;
    config.c_cc[VMIN]  = 0; // Return as soon as one byte is available
    config.c_cc[VTIME] = 2; // timeout (seconds) per byte
    if (ioctl(fd, TCSETS2, &config)) {
      fprintf(stderr, \"set-baud!: error in sets2\");
      C_return(0);
    }
    usleep(30000); // Allow USB to settle
    ioctl(fd, TCFLSH, TCIOFLUSH);
")
   fd baud))

(define (tty-break fd µs)
  ((foreign-lambda* void ((int fd) (int us)) "
  ioctl(fd, TIOCSBRK, 0);
  usleep(us);
  ioctl(fd, TIOCCBRK, 0);")
   fd µs))

(define (tty-break-set! fd)
  ((foreign-lambda* void ((int fd)) "ioctl(fd, TIOCSBRK, 0);") fd))

(define (tty-break-clear! fd)
  ((foreign-lambda* void ((int fd)) "ioctl(fd, TIOCCBRK, 0);") fd))

#;
(begin
  (import chicken.file.posix)

  (define dw (file-open "/dev/ttyUSB1" open/rdwr))
  (define (baudrate) (quotient 8000000 64))
  (tty-setup dw (baudrate))

  (print "GOT: "
         ((foreign-lambda* int ((int fd))
                           "
  ioctl(fd, TIOCSBRK, 0); usleep(160); ioctl(fd, TIOCCBRK, 0);
  uint8_t r = 1;
  while(1) {
    read(fd, &r, 1);
    if(r != 0x00 && r != 0xff) { 
      return r;
    }
  }
") dw)))
