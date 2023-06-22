;; register: any data point on the target data address space
;;
;; trick of the month: make registers a special record, and define a
;; printer for that record that reads the register value off the
;; target. something that will let you inspect registers just by
;; typing their name:
;;
;; #;> PORT.OUT
;; (reg #x0404 0010 1100)
;;
;; might be worth the hassle because: I'm tired of (number->string
;; (SP) 16), and the difference between a register address (CPU.SP)
;; and its getter (SP)? Maybe this can work to serve as both.
;;
;; We'll keep proper getters around in case the print-register-value
;; in record-printer is a bad idea.

;; (string-pad "abc"  4 #\space) => " abc"
;; (string-pad "abc" -4 #\space) => "abc "
(define (string-pad s n char)
  (if (>= n 0)
      (string-append   (make-string (max 0 (- (+ n) (string-length s))) char) s)
      (string-append s (make-string (max 0 (- (- n) (string-length s))) char))))

(define (number->hex n padding)
  (string-append "#x" (string-pad (number->string n 16) padding #\0)))

(define (number->bin n padding)
  (string-append "#b" (string-pad (number->string n 2) padding #\0)))

;; getter: typically LDS => number (1 or 2 bytes). sometimes blobs
;; setter: typically number => STS. but also bytes
;; printer: return a pretty descriptive, decoded string, of a value
(define-record-type
    reg (%make-reg name address getter setter printer)
    reg?
    (name    reg-name    reg-name-set!)
    (address reg-address reg-address-set!)
    (getter  reg-getter  reg-getter-set!)
    (setter  reg-setter  reg-setter-set!)
    (printer reg-printer reg-printer-set!))

(define (reg-default-getter r)   (LDS (reg-address r)   1))
(define (reg-default-setter r v) (STS (reg-address r) v 1))

(begin
 (define (make-value-pretty-printer len)
   (lambda (r port)
     (let ((value ((reg-getter r) r)))
       (display "≡" port)
       (display (string-pad (number->string value 16) (* 2 len) #\0) port)
       (display " " port)
       (display (let* ((x (string-pad (number->string value 2) (* len 8) #\0))
                       (x (flatten (intersperse (chop (string->list x) 4) #\_))))
                  (list->string x)) port)
       (display "₂" port))))

 (define reg-default-printer/u8  (make-value-pretty-printer 1))
 (define reg-default-printer/u16 (make-value-pretty-printer 2)))

;; this is where the magic happens:
(set! (record-printer reg)
      (lambda (r port)
        (display "#<register " port)
        (display (reg-name r) port)
        (display " #x" port)
        (display (string-pad (number->string (reg-address r) 16) 4 #\0) port)
        (when (current-updi-fd)
          (display " " port)
          ((reg-printer r) r port))
        (display ">" port)))

;; TODO: review if "reg" is the best term. something that says
;; "anything in the data address space"
(define (register name address #!optional
                  (getter reg-default-getter)
                  (setter reg-default-setter)
                  (printer (lambda (r p) (reg-default-printer/u8 r p))))
  (%make-reg name address getter setter printer))

(define (get r)       ((reg-getter r) r))
(define (set r value) ((reg-setter r) r value))

;; TODO: something clever like *SP maybe?
;; (set-read-syntax! #\* (lambda (port) `(quote ,(read))))
