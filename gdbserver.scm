(import (only chicken.condition print-error-message)
         chicken.process-context chicken.port)
(include "avr-updi.scm")
(include "gdb.scm")

(foreign-declare "
#include <termios.h>
")

(define config/tty            #f)
(define config/baud           #f)
(define config/gdbserver-port 4444)
(define config/nrepl-port     #f)
(define config/greeting       #t)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: updi-gdbserver [options] <tty> -b <baud>")
      (print " -b <baud>               Set baudrate of UPDI serial port (required)")
      (print " -p <gdbserver-port>     Set TCP listening port for GDB-server (defaults to 4444)")
      (print " -r <repl-port>          Set TCP listening port for Chicken Scheme repl (defaults to off)")
      (print " -g                      Disable initial (SIB) greeting"))))

(let loop ((cla (command-line-arguments)))

  (define (== s) (equal? (car cla) s))

  (define (n) ;; numeric option
    (if (pair? (cdr cla))
        (or (string->number (cadr cla))
            (error "non-numeric argument for " (car cla) (cadr cla)))
        (error "missing argument for " (car cla))))

  (when (pair? cla)
    (cond
     ((== "-b") (set! config/baud           (n)) (loop (cddr cla)))
     ((== "-p") (set! config/gdbserver-port (n)) (loop (cddr cla)))
     ((== "-r") (set! config/nrepl-port     (n)) (loop (cddr cla)))
     ((== "-g") (set! config/greeting        #f) (loop (cdr  cla)))
     (else (set! config/tty (car cla))
           (loop (cdr cla))))))

(unless config/tty
  (print "errro: no serial port set <tty>")
  (usage)
  (exit 1))
(unless config/baud
  (print "errro: no baudrate set -b <baud>")
  (usage)
  (exit 2))

;; (updi-open config/tty config/baud)

(when config/nrepl-port
  (eval `(import chicken.file.posix chicken.string srfi-18
                 chicken.memory.representation
                 chicken.bitwise
                 (only chicken.port with-output-to-string)

                 chicken.tcp chicken.io srfi-18
                 chicken.string
                 chicken.irregex
                 chicken.port
                 chicken.bitwise
                 chicken.file.posix))
  (import nrepl)
  (print)
  (print "Starting repl on port " config/nrepl-port)
  (print "Connect with, for example:")
  (print "  rlwrap nc 127.0.0.1:" config/nrepl-port)
  (print)
  (thread-start! (lambda () (nrepl config/nrepl-port))))

(define socket-listen (tcp-listen config/gdbserver-port))

(print "Starting gdbserver on port " config/gdbserver-port ". Press ctrl-c to quit.")
(print "Connect with, for example:")
(print "  avr-gdb -ex "
       "\"target extended-remote 127.0.0.1:" config/gdbserver-port "\""
       " program.elf")

(begin
  ;;(updi-break)
  (print "using avrdude init sequence (double break)")

  (let ((fd (file-open config/tty (+ open/noctty open/rdwr))))
    ;; (tty-setup fd baudrate)
    (current-updi (%make-updi fd)))
  
  ((foreign-lambda* int ((int fd) (int baud))
                    "
    struct termios termios = {0};
int rc;
    if (tcgetattr(fd, &termios) < 0) {
     fprintf(stderr, \"set-baud!: error in gets2\");
     C_return(0);
    }

    termios.c_iflag     = 0;
    termios.c_cflag     = CS8 | PARENB | CLOCAL | CREAD /*| CSTOPB */ ;
    termios.c_oflag     = 0;
    termios.c_lflag     = 0;
    termios.c_ispeed    = 300;
    termios.c_ospeed    = 300;
    termios.c_cc[VMIN]  = 0;
    termios.c_cc[VTIME] = 10; // read with 1s timeout
  rc = tcsetattr(fd, TCSANOW, &termios);
  if (rc < 0) {
    int ret = -errno;
    fprintf(stderr, \"tcsetattr() failed\\n\");
    return ret;
  }
//    if (ioctl(fd, TCSETS2, &termios)) {
//      fprintf(stderr, \"set-baud!: error in sets2\");
//      C_return(0);
//    }


//    ioctl(fd, TCFLSH, TCIOFLUSH);

    unsigned char reply;

    write(fd, \"\\x00\", 1);
    read(fd, &reply, 1);

    usleep(100*1000);

    write(fd, \"\\x00\", 1);
    read(fd, &reply, 1);

    termios.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL | ISIG | IEXTEN);
#ifdef ECHOCTL
  termios.c_lflag &= ~ECHOCTL;
#endif /* ECHOCTL */
#ifdef ECHOKE
  termios.c_lflag &= ~ECHOKE;
#endif /* ECHOKE */
  termios.c_oflag &= ~(OPOST | ONLCR | OCRNL); 
  termios.c_iflag &= ~(INLCR | IGNCR | ICRNL | IGNBRK);
#ifdef IUCLC
  termios.c_iflag &= ~IUCLC;
#endif /* IUCLC */
#ifdef PARMRK
  termios.c_iflag &= ~PARMRK;
#endif /* PARMRK */

  termios.c_cflag &= ~CSIZE;
    termios.c_cflag |= CS8;

    termios.c_cflag |= CSTOPB;

  termios.c_iflag &= ~(INPCK | ISTRIP);

    termios.c_cflag |= PARENB;

    termios.c_cflag &= ~PARODD;


#ifdef IXANY
  termios.c_iflag &= ~IXANY;
#endif /* IXANY */
  termios.c_iflag &= ~(IXON | IXOFF);

#ifdef CRTSCTS
  termios.c_iflag &= ~CRTSCTS;
#endif /* CRTSCTS */

#ifdef CNEW_RTSCTS
  termios.c_iflag &= ~CNEW_RTSCTS;
#endif /* CRTSCTS */


  rc = tcsetattr(fd, TCSANOW, &termios);
  if (rc < 0) {
    int ret = -errno;
    fprintf(stderr, \"tcsetattr() failed\\n\");
    return ret;
  }

  tcflush(fd, TCIFLUSH); //TCFLSH, TCIOFLUSH);

    return(1);
")
   (current-updi-fd)
   config/baud)
  
  (print)
  ;; updi_link_stcs(pgm, UPDI_CS_CTRLB, 1 << UPDI_CTRLB_CCDETDIS_BIT)
  (file-write (current-updi-fd) "\x55\xC3\x08") ;; (STCS UPDI.CTRLB #b00001000)
  (file-write (current-updi-fd) "\x55\xC2\x80") ;; (STCS UPDI.CTRLA #b10000000) 
  (updi-drain!)
  (print "link ldcs:")
  (print (LDCS UPDI.STATUSA))
  (print "OKEY!"))

(when config/greeting
  (updi-break)
  (print)
  (display "Current device SIB:\n  ")
  (write (SIB))
  (newline))

(let loop ()
  (receive (ip op) (tcp-accept socket-listen)
    (updi-break)
    (stop!) ;; gdb connecting - we _have_ to halt ('?')
    (let loop ()
      (let ((cmd (rsp-read ip)))
        (unless (eof-object? cmd)
          (handle-exceptions
           e (begin
               (print-error-message e)
               ;; TODO: skip is already replied to packet
               (rsp-write "E01" op))
           (rsp-handle cmd ip op))
          (loop)))))
  (loop))
