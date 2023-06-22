
;; gdb writes to flash ("M") unaligned and cross-flash boundaries :-(
;; so we cache things in memory and write everything later. this could
;; probably be fixed by sending the correct memory regions to gdb
;; somehow.

;; turn a flat vector into a list of (addr . #( ...)) where the
;; vector represents the memory region of the page starting at addr.
(define (memory-pages vec pagesize)
  (let loop ((n 0)
             (result '()))
    (if (< n (vector-length vec))
        (if (vector-ref vec n)
            (let* ((skip (- pagesize (remainder n pagesize)))
                   (pagen (quotient n pagesize))
                   (addr-start (* pagesize (+ 0 pagen)))
                   (addr-end   (* pagesize (+ 1 pagen))))
              ;;(print "found set skip="  skip " page=" page "[" addr-start " " addr-end "]")
              (loop addr-end (cons (cons addr-start (subvector vec addr-start addr-end))
                                   result)))
            (loop (+ n 1) result))
        result)))

(unless (equal? (memory-pages (vector #f #f #f #f  ;; page 0
                                      01 #f #f #f  ;; page 1
                                      #f 02 #f #f  ;; page 2
                                      #f #f 03 #f  ;; page 3
                                      #f #f #f 04  ;; page 4
                                      #f #f #f #f) ;; page 5
                              4)
                '((16 . #(#f #f #f 4)) ;; page 0 and 5 are gone
                  (12 . #(#f #f 3 #f))
                  ( 8 . #(#f 2 #f #f))
                  ( 4 . #(1 #f #f #f))))

  (error "test failure: memory-pages"))

;; overwrite vec1 with non-#f values of vec2
;; (vector-merge (vector 1 2 3 #f #t) (vector 10 #f 30 #f)) => #(10 2 30 #f)
(define (vector-merge vec1 vec2)
  (unless (equal? (vector-length vec1) (vector-length vec2))
    (error "vector-lenght mismatch" (cons vec1 vec2)  'vector-merge))
  (list->vector
   (map
    (lambda (v1 v2) (if v2 v2 v1))
    (vector->list vec1)
    (vector->list vec2))))

;; a page is (addr . (vector …))
(define (make-page addr vec) (cons addr vec))
(define (page-addr   page) (car page))
(define (page-vector page) (cdr page))

;; efficient update of flash memory. page-write must also erase
;; flash if needed. only changed pages are written.
(define (page-write page reader writer)
  (let* ((pagesize (vector-length (page-vector page)))
         (old      (reader (page-addr page) pagesize))
         (_ (unless (vector? old)
              (error "reader did not return valid memory segment (vector)"
                     old reader)))
         (_ (unless (= (vector-length old) pagesize)
              (error "reader returned bad memory segment length" (vector-length old) pagesize)))
         (vec (vector-merge old (page-vector page))))
    (if (equal? vec old)
        (print "\x1b[32m" "flash" "\x1b[0m: skipping page #x" (number->string (page-addr page) 16)
               ;; " " (string->hex (list->string (map integer->char (vector->list vec))))
               )
        (writer (page-addr page) vec))))

(define (pages-write pages reader writer)
  (for-each (cut page-write <> reader writer)
            pages))

(let ((output (with-output-to-string
                (lambda ()
                  (pages-write `((10 . #(1 2 #f 4)))
                               (lambda (addr len)   (print #:reading addr) (vector 10 20 30 40))
                               (lambda (addr bytes) (print #:writing addr bytes)))))))
  (unless (equal? output "reading:10\nwriting:10#(1 2 30 4)\n" )
    (error "pages-write gone bad" output)))

(define (make-memory-cache size-total)

  (let ((mem (make-vector size-total #f)))

    (define (get adr) (vector-ref mem adr))
    (define (set adr value) (vector-set! mem adr value))

    (lambda (action . arguments)
      (case action
        ((#:get) (get (car arguments)))
        ((#:set) (set (car arguments) (cadr arguments)))
        ((#:clear) (vector-fill! mem #f))
        ((#:memory) mem)
        (else (error "unknown cache command, try #:get #:set #:clear #:memory"))))))
