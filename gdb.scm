(import chicken.tcp chicken.io srfi-18
        chicken.string
        chicken.irregex
        chicken.port
        chicken.bitwise
        chicken.file.posix)

(define (read-char* ip)
  (parameterize ((tcp-read-timeout #f))
    (read-char ip)))

(define (rsp-expect ip expected)
  (let ((actual (read-char* ip)))
    (unless (equal? expected actual)
      (error (conc "RSP error: expecting " (wrt expected)) actual))))

(define (rsp-read-checksum ip)
  (let ((cs (read-string 2 ip)))
    ;;(print "TODO CHECKSUM: " (wrt cs))
    cs))

;; https://sourceware.org/gdb/onlinedocs/gdb/Overview.html#Overview
;; "$" packet "#" checksum
(define (rsp-read-packet ip)
  (let loop ((packet '()))
    (let ((c (read-char* ip)))
      (cond ((eof-object? c) (error "premature eof " ip))
            ((eq? c #\#)
             (rsp-read-checksum ip)
             (list->string (reverse packet)))
            (else (loop (cons c packet)))))))

(define (packet? cmd) (string? cmd))

(define (rsp-ack op)
  ;;. (file-write 1 "«  ack\n")
  (display "+" op)
  (flush-output op))

(define (rsp-read ip)
  (let ((c (read-char* ip)))
    (cond ((eof-object? c) #!eof)
          ((equal? c #\+) 'ack)
          ((equal? c #\-) 'nack)
          ((equal? c #\$) (rsp-read-packet ip))
          ((equal? c #\x03) 'break)
          (else (error (conc "don't know how to parse " (wrt c)))))))

(define (string-checksum s)
  (let loop ((n 0)
             (sum 0))
    (if (< n (string-length s))
        (loop (+ n 1) (bitwise-and #xFF (+ (char->integer (string-ref s n)) sum)))
        sum)))

;; (pad-left "a" 2 #\0) => "0a"
(define (pad-left s n char)
  (conc (make-string (max 0 (- n (string-length s))) char) s))

;; (with-output-to-string (lambda () (rsp-write " " (current-output-port))))
(define (rsp-write pkg op)
  (let* ((cs* (number->string (string-checksum pkg) 16))
         (cs  (pad-left cs* 2 #\0))
         (frame (conc "$" pkg "#" cs)))
    (file-write 1 (conc "« \x1b[31m " (wrt frame) "\x1b[0m\n"))
    (display frame op)
    (flush-output op)))

;; (hex->string "01024142") => "\x01\x02AB"
(define (hex->string h)
  (let* ((len (string-length h))
         (result (make-string (/ len 2))))
    (let loop ((n 0))
      (when (< n len)
        (string-set!
         result (/ n 2)
         (integer->char
          (bitwise-ior (arithmetic-shift (string->number (string (string-ref h (+ 1 n))) 16) 0)
                       (arithmetic-shift (string->number (string (string-ref h (+ 0 n))) 16) 4))))
        (loop (+ n 2))))
    result))

(define (string->hex s)
  (with-output-to-string
    (lambda ()
      (for-each (lambda (c) (display (pad-left (number->string (char->integer c) 16) 2 #\0)))
                (string->list s)))))

;; (cmd-args "m8000fe,2" ",") => (8388862 2)
;; (cmd-args "P4=64" "=")     => (4 100)
(define (cmd-args cmd separator)
  (map (cut string->number <> 16)
       (irregex-split (if (string? separator) `(,separator) separator)
                      (substring cmd 1))))

(define gdb-ram-start #x800000)
;; (gdb-adr->updi-adr #x8000fe) => #x80fe
;; (gdb-adr->updi-adr #x27) => #x80fe
;; gdb keeps flash from 0x0000 and RAM at #x80_0000.
(define (gdb-adr->updi-adr addr)
  ;;                          UPDI register address
  (cond ((>= addr gdb-ram-start) (+ addr #x-800000))
        ;;            flash space
        (else (+ addr #x8000))))

(define (rsp-handle cmd op)
  (unless (equal? cmd 'ack)
    (file-write 1 (conc " » " (wrt cmd) "\n")))

  (when (packet? cmd) (rsp-ack op)) ;; you're supposed to ack each recieved packet

  (cond
   ((equal? cmd 'ack) #f) ;; make use of this?

   ((equal? cmd 'break)
    (stop!)
    (rsp-write "S00" op))

   ((equal? cmd "c")
    ;; don't rsp-write here! reply at upcoming breakpoint or 'break
    (cont!))
   
   ((equal? cmd "qAttached") ;; attached to existing "process"?
    (rsp-write "1" op))

   ((equal? cmd "?") ;; stop reason
    (rsp-write "S00" op))

   ((equal? cmd "g") ;; send all registers
    (rsp-write (string->hex
                (->bytes
                 (regs) ;; r0 - r31
                 (   u8->bytes (SREG))
                 (u16le->bytes (SP))
                 (u16le->bytes (PC))
                 0 0)) ;; <-- mystery registers (from dwtk / dwire-debug)
               op))

   ;; get memory region
   ;;((substring=? cmd "m"))

   ;; get register value
   ;;((substring=? cmd "p") (error "TODO p (get register)"))

   ;; store register value
   ((and (string? cmd) (substring=? cmd "P"))
    (apply
     (lambda (R value)
       (set! (r R) value)
       (rsp-write "OK" op))
     (cmd-args cmd "=")))

   ((and (string? cmd) (substring=? cmd "m"))
    (apply
     (lambda (addr len)
       (rsp-write (string->hex (memory-read* (gdb-adr->updi-adr addr) len)) op))
     (cmd-args cmd ",")))

   ;; (gdb) monitor reset => "qRcmd,7265736574"
   ;; "OK" => OK, no output
   ;; "xxXXxxXX" hex string => normal output
   ;; "E xx" => error
   ((and (string? cmd) (substring=? cmd "qRcmd,"))    
    (apply
     (lambda (_ #!optional (hex ""))
       (cond ((equal? hex (string->hex "reset"))
              (set! (resetting) #t)
              (set! (resetting) #f)
              (rsp-write "OK" op))
             (else (rsp-write "E00" op))))
     (irregex-split `(",") cmd)))

   ;; TODO: support X for less hex parsing and stuff (must redo packet framing)
   ;;((and (string? cmd) (substring=? cmd "X")))
   
   ;; store to memory (hex)
   ;; eg "M80,8:0000e5cff894ffcf"
   ((and (string? cmd) (substring=? cmd "M"))
    (apply
     (lambda (addr* len* hex)
       (let ((addr (string->number addr* 16))
             (len (string->number len* 16)))
         ;;(print (wrt addr*) " µ " len " = " hex)
         (memory-write* (gdb-adr->updi-adr addr) (hex->string hex) 1)

         ;; TODO: make sure the addresses are aligned. writing a page
         ;; erases it's previous page :-( this needs a lot of cleanup.
         (when (< addr gdb-ram-start) ;; it's flash!
           ;; data is in page buffer
           (STS #x1000 3 1)) ;; issue NVM erase+prog
         
         (rsp-write "OK" op)))
     (irregex-split `(or ":" ",") (substring cmd 1))))
   
   ;; empty packet reply for unknown commands (of which there are
   ;; _a lot_.
   (else (rsp-write "" op))))

;; (rsp-handle "M0,2:00ff" (current-output-port))
;; (rsp-handle "qRcmd,7265736574" (current-output-port))
;; (rsp-handle "qRcmd," (current-output-port))

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
           (let loop ()
             (let ((cmd (rsp-read ip))) 
               (unless (eof-object? cmd)
                 (rsp-handle cmd op)
                 (loop)))))
         (loop))))))
