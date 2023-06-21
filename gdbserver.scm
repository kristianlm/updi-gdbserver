(import (only chicken.condition print-error-message)
         chicken.process-context chicken.port)
(include "avr-updi.scm")
(include "gdb.scm")

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: updi-gdbserver <tty> -b <baud>"))))

(let ((cla (command-line-arguments)))
  (if (and (= 3 (length cla))
           (equal? "-b" (cadr cla)))
      (let ((tty (car cla))
            (baud (string->number (caddr cla))))
        (updi-open tty baud))
      (begin
        (usage)
        (exit 0))))

(print "gdbserver on port 4444")

(define socket-listen (tcp-listen 4444))
(begin
  (handle-exceptions
   e (begin)
   (thread-state thread)
   (thread-terminate! thread))
  (define thread
    (thread-start!
     (lambda ()
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
                  (rsp-handle cmd op))
                 (loop)))))
         (loop))))))

;; csi's read (on stdin input) makes the above thread hang
;; periodically. tcp ports don't do that, so I like to access the
;; Chicken repl from nrepl instead of doing (repl).
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
(print "nrepl on port 1234")
(print "try:")
(print " > avr-gdb -ex \"target extended-remote 127.0.0.1:4444\" program.elf # for gdb")
(print " > rlwrap nc 127.0.0.1:1234 # for Scheme repl")
(nrepl 1234)
