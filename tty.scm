(foreign-declare "
#include <sys/ioctl.h>
#include <asm/termbits.h>
")

(define (tty-setup fd baud)
  ((foreign-lambda* int ((int fd)
                         (int baud))
                    "
    struct termios2 config = {0};
    if (ioctl(fd, TCGETS2, &config)) {
     fprintf(stderr, \"set-baud!: error in gets2\");
     C_return(0);
    }

    config.c_iflag     = 0;
    config.c_cflag     = CS8 | CSTOPB | PARENB | BOTHER | CLOCAL | CREAD;
    config.c_oflag     = 0;
    config.c_lflag     = 0;
    config.c_ispeed    = baud;
    config.c_ospeed    = baud;
    config.c_cc[VMIN]  = 0;
    config.c_cc[VTIME] = 10; // read with 1s timeout
    if (ioctl(fd, TCSETS2, &config)) {
      fprintf(stderr, \"set-baud!: error in sets2\");
      C_return(0);
    }
    ioctl(fd, TCFLSH, TCIOFLUSH);
    return(1);
")
   fd baud))

(define (tty-break fd µs)
  ((foreign-lambda* void ((int fd) (int us)) "
  ioctl(fd, TIOCSBRK, 0); usleep(us); ioctl(fd, TIOCCBRK, 0);
  usleep(1000 * 100);
  ioctl(fd, TIOCSBRK, 0); usleep(us); ioctl(fd, TIOCCBRK, 0);
")
   fd µs))

(define (tty-break-set! fd)
  ((foreign-lambda* void ((int fd)) "ioctl(fd, TIOCSBRK, 0);") fd))

(define (tty-break-clear! fd)
  ((foreign-lambda* void ((int fd)) "ioctl(fd, TIOCCBRK, 0);") fd))



;; (begin
;;   (import chicken.file.posix)

;;   (define fd (file-open "/dev/ttyUSB0" open/rdwr))
;;   (define (baudrate) 100000)
;;   (tty-setup fd (baudrate))

;;   (file-write fd (list->string (map integer->char '(#x55 #xc3))))

;;   (let loop ((d (file-read fd 1)))
;;     (if (= 1 (second d))
;;       (let ((c (string-ref (first d) 0)))
;;         (printf "~X\n" (char->integer c))
;;             (loop (file-read fd 1)))
;;       (exit 0))))

