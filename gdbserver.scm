(import (only chicken.condition print-error-message)
         chicken.process-context chicken.port)
(include "avr-updi.scm")
(include "gdb.scm")

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

(updi-open config/tty config/baud)

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

(when config/greeting
  (print)
  (print "Current device SIB:\n  " (SIB)))

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
