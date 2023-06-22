(import chicken.blob)
;; a small bytevector api. there's already the srfi-4 and R7RS APIs so
;; I decided to name this just "bytes".

;; (->bytes 0 "U" #xff) => "\x00U\xff"
;; (->bytes 65 "bc" #\d) => "Abcd"
(define (->bytes . bytes)
  (list->string
   (let loop ((bytes bytes)
              (result '())) ;; <- reversed
     (if (pair? bytes)
         (let ((b (car bytes)))
           (loop (cdr bytes)
                 (cond ((char? b)   (cons b result))
                       ((number? b) (cons (integer->char b) result))
                       ((string? b) (append (reverse (string->list b)) result))
                       ((blob? b)   (append (reverse (string->list (blob->string b))) result))
                       (else (error "bytes: expecting number/string")))))
         (reverse result)))))

(define (bytes->u8 bytes)
  (unless (>= (number-of-bytes bytes) 1) (error "bytes->u8: need >=1 bytes " (wrt bytes)))
  (char->integer (string-ref bytes 0)))

(define (u8->bytes value) (->bytes value))

;; (->bytes->u16/be "\x01\x00") => 256
;; (->bytes->u16/le "\x01\x00") => 513
(define (bytes->u16le bytes)
  (unless (>= (number-of-bytes bytes) 2) (error "bytes->u16: need >=2 bytes " (wrt bytes)))
  (+ (arithmetic-shift (char->integer (string-ref bytes 0)) 0)
     (arithmetic-shift (char->integer (string-ref bytes 1)) 8)))

(define (bytes->u16be bytes)
  (unless (>= (number-of-bytes bytes) 2) (error "bytes->u16: need >=2 bytes " (wrt bytes)))
  (+ (arithmetic-shift (char->integer (string-ref bytes 0)) 8)
     (arithmetic-shift (char->integer (string-ref bytes 1)) 0)))

(define (u16be->bytes value)
  (->bytes (bitwise-and (arithmetic-shift value -8) #xff) ;; TODO: aflag
           (bitwise-and (arithmetic-shift value  0) #xff)))

(define (u16le->bytes value)
  (->bytes (bitwise-and (arithmetic-shift value  0) #xff)
           (bitwise-and (arithmetic-shift value -8) #xff)))

(define (bytes->le bytes)
  (let loop ((n (max 0 (- (number-of-bytes bytes) 1)))
             (result 0))
    (if (>= n 0)
        (loop (- n 1)
              (+ (arithmetic-shift result 8)
                 (char->integer (string-ref bytes n))))
        result)))

(unless (= #x030201 (bytes->le "\x01\x02\x03")) (error "internal test failure"))
