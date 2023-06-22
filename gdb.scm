(import chicken.tcp chicken.io srfi-18
        chicken.string
        chicken.irregex
        chicken.port
        chicken.bitwise
        chicken.file.posix)

(include "pages.scm")

(define (string-prefix? s prefix) (substring=? s prefix 0 0 (string-length prefix)))

;; gdb-addressing (flash starting at 0x00)
(define cache (make-memory-cache #x1000)) ;; <-- TODO actual target size

(define (bytes-for-each/i s proc)
  (let loop ((n 0))
    (when (< n (string-length s))
      (proc (char->integer (string-ref s n)) n)
      (loop (+ n 1)))))

(unless (equal? (with-output-to-string (lambda () (bytes-for-each/i "ABC" print)))
                "650\n661\n672\n")
  (error "test fail bytes-for-each/i"))

;; addr gdb memory space
(define (memory-write addr bytes)
  (if (< addr gdb-ram-start)
      ;; flash writes go to cache
      (bytes-for-each/i
       bytes
       (lambda (byte index)
         (cache #:set (+ addr index) byte)))
      ;; non-flash written directly
      (memory-write* (gdb-adr->updi-adr addr) bytes 1)))

;; TODO: move this into pages.scm
(define (cache-flush!)
  (let ((pages (memory-pages (cache #:memory) 64)))
    (pages-write pages
                 (lambda (addr len)
                   (list->vector
                    (map char->integer
                         (string->list
                          (memory-read* (gdb-adr->updi-adr addr) len)))))
                 (lambda (addr vec)
                   (print "\x1b[32m" "flash" "\x1b[0m: writing page #x" (number->string addr 16))
                   (let ((bytes
                          (list->string (map integer->char (vector->list vec)))))
                     (memory-write* (gdb-adr->updi-adr addr) bytes 1))
                   ;;          ,-- erase and program
                   (STS #x1000 3 1)))
    (cache #:clear)))

(define (read-char* ip)
  (parameterize ((tcp-read-timeout #f))
    (read-char ip)))

;; TODO: escape these binary characters
(eq? c #\#)
(eq? c #\$)
(eq? c #\})
(eq? c #\*)

(define (rsp-expect ip expected)
  (let ((actual (read-char* ip)))
    (unless (equal? expected actual)
      (error (conc "RSP error: expecting " (wrt expected)) actual))))

(define (rsp-read-checksum ip)
  (let ((cs (read-string 2 ip)))
    ;;(print "TODO CHECKSUM: " (wrt cs))
    cs))

;; TODO: chunk this up into, for example, `(M 1234 "blob") here
;; instead?
;; https://sourceware.org/gdb/onlinedocs/gdb/Overview.html#Overview
;; "$" packet "#" checksum
;; and https://sourceware.org/gdb/onlinedocs/gdb/Overview.html#Binary-Data
(define (rsp-read-packet ip)
  (let loop ((packet '()))
    (let ((c (read-char* ip)))
      (cond ((eof-object? c) (error "premature eof " ip))
            ((eq? c #\#)
             (rsp-read-checksum ip)
             (list->string (reverse packet)))
            ((eq? c #\}) ;; <-- next byte is escaped
             (loop (cons (integer->char (bitwise-xor #x20 (char->integer (read-char* ip))))
                         packet)))
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
    (file-write 1 (conc "« \x1b[30m " (wrt frame) "\x1b[0m\n"))
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

(define gdb-ram-start #x800000)
;; (gdb-adr->updi-adr #x8000fe) => #x80fe
;; (gdb-adr->updi-adr #x27) => #x80fe
;; gdb keeps flash from 0x0000 and RAM at #x80_0000.
(define (gdb-adr->updi-adr addr)
  ;;                          UPDI register address
  (cond ((>= addr gdb-ram-start) (+ addr #x-800000))
        ;;            flash space
        (else (+ addr #x8000))))

(define (rsp-handle cmd ip op)
  (unless (equal? cmd 'ack)
    (file-write 1 (conc " » " (wrt cmd) "\n")))

  (when (packet? cmd) (rsp-ack op)) ;; you're supposed to ack each recieved packet

  (cond
   ((equal? cmd 'ack) #f) ;; make use of this?

   ((equal? cmd 'break)
    (stop!)
    (rsp-write "S00" op))

   ;; from my avr-gdb:
   ;; "qSupported:multiprocess+;swbreak+;hwbreak+;…"
   ((irregex-match `(: "qSupported:" (* any)) cmd)
    => (lambda (m)
         (rsp-write "qXfer:memory-map:read+;" op)))

   ((equal? cmd "s")
    (step!) ;;   ,-- this is a magic, undocumented, number
    (rsp-write "S05" op))

   ((equal? cmd "c")
    (cont!)

    ;; the target doesn't tell us when it's stopped. we have to keep
    ;; asking.
    (let* ((ss '("-" "\\" "|" "/"))
           (s ss)
           (bars (lambda () ;; indicate we're polling for clarity
                   (display (conc "\r" (car s)))
                   (set! s (cdr s))
                   (unless (pair? s) (set! s ss))
                   (flush-output))))

      ;; poll for halted CPU and GDB break
      (let loop ()
        (bars)
        (if (char-ready? ip)
            (let ((packet (rsp-read ip)))
              (if (equal? packet 'break)
                  (begin
                    (file-write 1 (conc " » " 'break "\n"))
                    (stop!)
                    ;; "Program stopped." vs "Program received signal …"
                    (rsp-write "S02" op))
                  (error "unexpected GDB packet during continue" packet)))
            (if (halted?)
                (begin ;; SIGTRAP (breakpoint trap)
                  (rsp-write "S05" op))
                (begin
                  (thread-sleep! 0.05) ;; polling 100 fps
                  (loop)))))))

   ;; TODO: Strictly speaking, we should (cont!). But it's nice to use
   ;; the repl and GDB concurrently, and this is less intrusive.
   ((equal? cmd "D")
    (rsp-write "OK" op))

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
   ;;((string-prefix? cmd "m"))

   ;; get register value
   ;;((string-prefix? cmd "p") (error "TODO p (get register)"))

   ;; TODO: clean this up (and generalize?)
   ;; store register value, eg "P21=0a00"
   ((irregex-match `(: "P" (=> R (* xdigit)) "=" (=> hex (* xdigit))) cmd) =>
    (lambda (m)
      (let* ((R (string->number (irregex-match-substring m 'R) 16))
             (hex (irregex-match-substring m 'hex))
             (blob (hex->string hex))
             (value (if (= 1 (number-of-bytes blob))
                        (bytes->u8 blob)
                        (bytes->u16le blob))))
        (cond ((= R #x22) (set! (PC) value))
              ((= R #x21) (set! (SP) value))
              (else (set! (r R) value)))
        (rsp-write "OK" op))))

   ;; (gdb) monitor reset => "qRcmd,7265736574"
   ;; "OK" => OK, no output
   ;; "xxXXxxXX" hex string => normal output
   ;; "E xx" => error
   ((and (string? cmd) (string-prefix? cmd "qRcmd,"))
    (apply
     (lambda (_ #!optional (hex ""))
       (rsp-write
        (string->hex
         (conc (wrt (eval (with-input-from-string (hex->string hex) read))) "\n"))
        op))
     (irregex-split `(",") cmd)))

   ;; read memory, eg. "m4c,2"
   ((irregex-match `(: "m" (=> addr (* xdigit))
                       "," (=> len (* xdigit))) cmd)
    => (lambda (m)
         (let* ((addr (string->number (irregex-match-substring m 'addr) 16))
                (len  (string->number (irregex-match-substring m 'len) 16)))
           (rsp-write (string->hex (memory-read* (gdb-adr->updi-adr addr) len)) op))))

   ;; TODO: support X for less hex parsing and stuff maybe
   ;;((and (string? cmd) (string-prefix? cmd "X")))

   ;; store to memory (hex)
   ;; eg "M80,8:0000e5cff894ffcf"
   ((irregex-match `(: "M" (=> addr (* xdigit))
                       "," (=> len (* xdigit))
                       ":" (=> hex (* xdigit))) cmd)
    => (lambda (m)
         (let* ((addr (string->number (irregex-match-substring m 'addr) 16))
                (len  (string->number (irregex-match-substring m 'len) 16))
                (blob (hex->string    (irregex-match-substring m 'hex))))
           ;; TODO: I really don't like this. This is doomed to fail at
           ;; some point. If PC is 0 when we load, for example, the
           ;; cache isn't flushed. A better approach is probably to have
           ;; gdb know which region is flash, and somehow tell it the
           ;; flash pagesize.
           (memory-write addr blob)
           (rsp-write "OK" op))))

   ;; set breakpoint. eg. "Z0,4e,2"
   ((irregex-match `(: "Z" (=> type xdigit)
                       "," (=> address (* xdigit))
                       "," (=> kind (* xdigit))) cmd)
    => (lambda (m)
         (let* ((type    (string->number (irregex-match-substring m 'type)    16))
                (address (string->number (irregex-match-substring m 'address) 16))
                (kind    (string->number (irregex-match-substring m 'kind)    16)))
           (unless (= 2 kind) (error "unexpected breakpoint kind: " kind))
           (cond ((= type 0) (rsp-write "" op)) ;; sw bp
                 ((= type 1)                    ;; hw bp
                  (set! (BP 1) (+ address 1))   ;; what the heck!?
                  (rsp-write "OK" op))
                 ;; type 2, 3, 4: watchpoints (read, write, access)
                 (else (rsp-write "" op))))))

   ;; remove breakpoint "z1,4e,2")
   ((irregex-match `(: "z" (=> type xdigit)
                       "," (=> address (* xdigit))
                       "," (=> kind (* xdigit))) cmd)
    => (lambda (m)
         (let* ((type    (string->number (irregex-match-substring m 'type)    16))
                (address (string->number (irregex-match-substring m 'address) 16))
                (kind    (string->number (irregex-match-substring m 'kind)    16)))
           (unless (= 2 kind) (error "unexpected breakpoint kind: " kind))
           (cond ((= type 0) (rsp-write "" op)) ;; sw bp
                 ((= type 1)                    ;; hw bp
                  (set! (BP 1) 0)
                  (rsp-write "OK" op))
                 ;; type 2, 3, 4: watchpoints (read, write, access)
                 (else (rsp-write "" op))))))

   ;; ==================== flash and memory ====================
   ;;
   ;; This is a lot of hassle and we could get by without it: the
   ;; default fixed memory mapping (program/code splits at 0x800000)
   ;; is ok. It has one problem however: after "load file.elf", we get
   ;; a lot of arbitrary write memory, "M…", packets, and we don't
   ;; know when we should cache-flush!. By telling gdb about our flash
   ;; region, we get a documented vFlashDone which is exactly what we
   ;; want. This lets us cache flash pages and save many, probably
   ;; very many, erase-write cycles: I've seen a lot of "skipping
   ;; identical flash page" in my logs. The following packets are what
   ;; seems to be needed just to get our vFlashDone notification.

   ;; request memory map, eg. "qXfer:memory-map:read::0,18a"
   ;;                          ,-- object     ,- annex must be empty
   ((irregex-match `(: "qXfer:memory-map:read:"
                       ":" (=> offset (* xdigit))
                       "," (=> length (* xdigit))) cmd)
    => (lambda (m)
         (let* ((offset (string->number (irregex-match-substring m 'offset) 16))
                (length (string->number (irregex-match-substring m 'length) 16))
                (blob "<?xml version=\"1.0\"?>
<!DOCTYPE memory-map
          PUBLIC \"+//IDN gnu.org//DTD GDB Memory Map V1.0//EN\"
                 \"http://sourceware.org/gdb/gdb-memory-map.dtd\">
<memory-map>
  <memory type=\"ram\"   start=\"0x800000\" length=\"0x800000\"/>
  <memory type=\"flash\" start=\"0x000000\" length=\"0x800000\">
    <property name=\"blocksize\">0x40</property>
  </memory>
</memory-map>")
                (end (min (string-length blob) (+ offset length))))
           (rsp-write (conc (if (>= end (string-length blob)) "l" "m")
                            ;; TODO:    ,-- escape non-hex binary response
                            (substring blob offset end)) op))))

   ;; "vFlashErase:00000000,00000080"
   ((irregex-match `(: "vFlashErase"
                       ":" (=> addr (* xdigit))
                       "," (=> len  (* xdigit))) cmd)
    => (lambda (m)
         (let* ((addr (string->number (irregex-match-substring m 'addr) 16))
                (len  (string->number (irregex-match-substring m 'len) 16)))
           (memory-write addr (make-string len #\xff))
           (rsp-write "OK" op))))

   ;; "vFlashWrite:0:\x19 \x1f\x1e\x1d\x1c\x1b\x1a\x19\x18\x17…"
   ((irregex-match `(: "vFlashWrite"
                       ":" (=> addr (* xdigit))
                       ":" (=> blob (* any))) cmd)
    => (lambda (m)
         (let* ((addr (string->number (irregex-match-substring m 'addr) 16))
                (blob                 (irregex-match-substring m 'blob)))
           (memory-write addr blob)
           (rsp-write "OK" op))))

   ;; We are permitted to keep flash in an "unpredictable" state until
   ;; gdb issues this. It should therefore be the only place that
   ;; needs to flush the flash cache.
   ((irregex-match `(: "vFlashDone") cmd)
    => (lambda (m)
         (let* ()
           (cache-flush!)
           (rsp-write "OK" op))))

   ;; empty packet reply for unknown commands (of which there are
   ;; _a lot_.
   (else (rsp-write "" op))))

;; (rsp-handle "M0,2:00ff" (current-output-port))
;; (rsp-handle "qRcmd,7265736574" (current-output-port))
;; (rsp-handle "qRcmd," (current-output-port))
