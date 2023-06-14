(import chicken.file.posix chicken.string srfi-18
        chicken.memory.representation
        chicken.bitwise
        (only chicken.port with-output-to-string))

;; ======================================== basic helpers

(include "tty.scm")
(include "bytes.scm")
(define (wrt x) (with-output-to-string (lambda () (write x))))

;; ========================================

(define-record-type
    updi (%make-updi fd)
    updi?
    (fd updi-fd updi-fd-set!))

(define current-updi (make-parameter #f))
(define (current-updi-fd) (updi-fd (current-updi)))

;; (define updi-baudrate
;;   (getter-with-setter
;;    (lambda ()     (error "TODO"))
;;    (lambda (baud) (tty-setup (current-updi-fd) baud))))

;; (updi-open "/dev/ttyUSB1" 100000)
(define (updi-open tty baudrate)  
  (let ((fd (file-open tty open/rdwr)))
    (tty-setup fd baudrate)
    (current-updi (%make-updi fd))
    (current-updi)))

;; ======================================== UPDI<-->UART interface

(define (tty-data-available? fd)
 (receive (rs ws) (file-select (list fd) (list) 0)
   (if (pair? rs) #t #f)))

;; handle partial reads produced by file-read.
;;
;; timeouts are used to avoid "hangs", waiting for reads that will
;; never occur. they should ideally never occur.
(define (file-read-retrying fd len #!key
                            (timeout (lambda (s) (error (conc "updi read timeout (" s "s)")))))
  (let loop ((len len)
             (result '())) ;; <-- reversed list of strings
    (if (> len 0)
        (let* ((reply* (file-read fd len)) ;; <-- can timeout with 0 bytes
               (reply# (cadr reply*))
               (reply  (substring (car  reply*) 0 reply#)))
          (if (= 0 reply#)
              (timeout 1)
              (loop (- len reply#) (cons reply result))))
        (reverse-string-append result))))

(define (updi-break #!optional (ms 25000)) (tty-break (current-updi-fd) ms))

;; send `data` and consume its echo (since TX is physically connected
;; to RX).
(define (updi-cmd data response-len #!optional (fd (current-updi-fd)))

  ;; flush unexpected data
  (let loop ()
    (when (tty-data-available? fd)
      (print "OBS: data available " (file-read-retrying fd 1))
      (loop)))
  
  (file-write fd data)
  (let ((echo (file-read-retrying fd (number-of-bytes data))))
    (unless (equal? echo data)
      (error (conc "bad echo: expecting " (wrt data) " but got " (wrt echo))))
    (file-read-retrying fd response-len)))

(define (updi-expect-ack)
  (let* ((reply (file-read-retrying (current-updi-fd) 1)))
    (unless (equal? reply "\x40")
      (error (conc "expected ACK, got " (wrt reply))))))

;; ======================================== UPDI instruction set

(define (LDCS address) (bytes->u8 (updi-cmd (->bytes "\x55" (+ #x80 address)) 1)))
(define (STCS address v)
  (updi-cmd (->bytes "\x55" (+ #xC0 address) v) 0)
  (begin))

(define (LDS address len)
  (case len
    ((1) (bytes->u8    (updi-cmd (conc "\x55\x04" (u16le->bytes address)) 1)))
    ((2) (bytes->u16le (updi-cmd (conc "\x55\x05" (u16le->bytes address)) 2)))
    (else (error (conc "LDS: len must be 1 or 2 (len = " len ")")))))

(define (STS address value len)
  (case len
    ((1) ;;                 ,-- STS
     ;;                     |,- 2byte adr, 1byte data
     (updi-cmd (conc "\x55\x44" (u16le->bytes address)) 0)
     (updi-expect-ack)
     (updi-cmd (u8->bytes value) 0))
    ((2) ;;                 ,-- STS
     ;;                     |,- 2byte adr, 2byte data
     (updi-cmd (conc "\x55\x45" (u16le->bytes address)) 0)
     (updi-expect-ack)
     (updi-cmd (u16le->bytes value) 0))
    (else (error (conc "STS: len must be 1 or 2 (len = " len ")"))))

  (updi-expect-ack))

(define (LD len)
  (case len
    ((1) (updi-cmd (conc "\x55\x24") 0))
    ((2) (updi-cmd (conc "\x55\x25") 0))
    (else (error "unsupported LD len " len))))

;; store ptr address
(define (ST-ptr address alen)
  (case alen
    ((1) (error "TODO alen 1"))
    ((2) (updi-cmd (conc "\x55\x69" (u16le->bytes address)) 0))
    (else (error "unknown ST alen" alen)))
  (updi-expect-ack))

;; ;; store data into *ptr
;; (define (ST-*ptr data dlen)
;;   (case dlen
;;     ((1) (error "TODO dlen 1"))
;;     ((2) (error "TODO dlen 2"))
;;     (else (error "unknown ST dlen" dlen)))
;;   (updi-expect-ack))

;; store data into *ptr, post-incrementing it
(define (ST-*ptr++ data dlen)
  (case dlen
    ((1) (updi-cmd (->bytes "\x55\x64" data) 0))
    ((2) (updi-cmd (->bytes "\x55\x65" (u16le->bytes data)) 0))
    (else (error "unknown ST dlen" dlen)))
  (updi-expect-ack))

(define (REPEAT count len)
  (case len
    ((1) (updi-cmd (conc "\x55\xa0" (u8->bytes count)) 0))
    (else (error "unsupported len for REPEAT (≠ 1)" len))))

(define (KEY key) (updi-cmd (conc "\x55\xe0" key) 0) (begin))
(define (SIB)     (updi-cmd (conc "\x55\xe6") 32))

(define (memory-read* address len)
  (unless (<=  len #xff) (error "len must be ≤ #xFF" len))
  (ST-ptr address 2)
  (REPEAT (- len 1) 1)
  (LD 1)
  (file-read-retrying (current-updi-fd) len))

;; (memory-write* #x3f10 "hei" 1)
;; (memory-read* #x3f0f 5)
(define (memory-write* address data blocksize)
  (unless (= blocksize 1) (error "TODO: blocksize ≠ 2"))
  (let ((len (number-of-bytes data)))
    (unless (<=  len #xff) (error "data size must be ≤ #xFF" len))
    (ST-ptr address 2)
    (REPEAT (- len 1) 1)
    (let ((chars (string->list data)))
      (ST-*ptr++ (string (car chars)) 1)
      (for-each
       (lambda (c)
         (updi-cmd (string c) 0)
         (updi-expect-ack))
       (cdr chars)))))

;; TODO: implement memory-read  on >255 bytes (though gdb does pagination on its own)
;; TODO: implement memory-write on >255 bytes

;; ======================================== UPDI control/status registers

;; these are LDCS / STCS register offsets
(define UPDI.STATUSA        #x00)
(define UPDI.STATUSB        #x01)
(define UPDI.CTRLA          #x02)
(define UPDI.CTRLB          #x03)
(define UPDI.DEBUG_CTRLA    #x04) ;; unofficial
(define UPDI.DEBUG_STATUSA  #x05) ;; unofficial
(define UPDI.ASI_KEY_STATUS #x07)
(define UPDI.ASI_RESET_REQ  #x08)
(define UPDI.ASI_CTRLA      #x09)
(define UPDI.ASI_SYS_CTRLA  #x0A)
(define UPDI.ASI_SYS_STATUS #x0B)
(define UPDI.ASI_CRC_STATUS #x0C)


;; cycle a reset like this:
;; (set! (resetting) #t)
;; (set! (resetting) #f)
(define resetting
  (getter-with-setter
   (lambda () (LDCS UPDI.ASI_RESET_REQ))
   (lambda (v)
     ;; TODO: poll ASI_SYS_STATUS RSTSYS bit for successful operation
     (if v
         (STCS UPDI.ASI_RESET_REQ #x59)     ;; start reset condition
         (STCS UPDI.ASI_RESET_REQ #x00)))))  ;; clear reset condition


(define (updi-disable) (STCS UPDI.CTRLB (bitwise-ior (LDCS UPDI.CTRLB) #b00000100)))

;; TODO: sometimes there are more undocumented bits set here
(define (updi-lockstatus?) (not (= 0 (bitwise-and #b00000001 (LDCS UPDI.ASI_SYS_STATUS)))))
(define (updi-urowprog?)   (not (= 0 (bitwise-and #b00000100 (LDCS UPDI.ASI_SYS_STATUS)))))
(define (updi-nvmprog?)    (not (= 0 (bitwise-and #b00001000 (LDCS UPDI.ASI_SYS_STATUS)))))
(define (updi-insleep?)    (not (= 0 (bitwise-and #b00010000 (LDCS UPDI.ASI_SYS_STATUS)))))
(define (updi-rstsys?)     (not (= 0 (bitwise-and #b00100000 (LDCS UPDI.ASI_SYS_STATUS)))))

(define updi-clocksel
  (getter-with-setter
   (lambda ()  (LDCS UPDI.ASI_CTRLA))
   (lambda (v) (STCS UPDI.ASI_CTRLA (bitwise-and #b0011 v)))))

(define updi-key-ocd
  (getter-with-setter
   (lambda ()  (= #b0010 (bitwise-and #b0010 (LDCS UPDI.ASI_KEY_STATUS))))
   (lambda (v) (when v (KEY "     DCO") (unless (updi-key-ocd) (error "key ocd failed"))))))

(define updi-key-nvmprog
  (getter-with-setter
   (lambda ()  (not (= 0 (bitwise-and #b00010000 (LDCS UPDI.ASI_KEY_STATUS)))))
   (lambda (v) (when v
                 (KEY " gorPMVN")
                 (unless (updi-key-nvmprog)
                   (error "key nvmprog failed"))))))

(define updi-key-chiperase
  (getter-with-setter
   (lambda ()  (not (= 0 (bitwise-and #b00001000 (LDCS UPDI.ASI_KEY_STATUS)))))
   (lambda (v) (when v
                 (KEY "esarEMVN") ;; are you seeing this?
                 (unless (updi-key-chiperase)
                   (error "key chiperase failed"))))))

;; nvmprog-mode and ocd-mode seem to be mutually exclusive.
(define (updi-nvmprog!)
  (unless (updi-nvmprog?)
    (when (updi-lockstatus?) (error "cannot nvmprog locked target, try (updi-chip-erase!)"))
    (unless (updi-key-nvmprog)
      (set! (updi-key-nvmprog) #t))
    ;; datasheet @ 32.3.8.2 says we have to reset :-(
    ;; how can we get around that when we're in debug mode? simply skipping reset doesn't work
    (set! (resetting) #t)
    (set! (resetting) #f)
    ;; TODO: retry with timeout here
    (unless (updi-nvmprog?) (error "could not put target in nvmprog mode"))))

(define (updi-chip-erase!)
  (set! (updi-key-chiperase) #t)
  (set! (resetting) #t)
  (set! (resetting) #f)

  ;; TODO retry with timeout here
  (if (updi-lockstatus?)
      (error "could not chip-erase (lockstatus still set)")))


;; ======================================== "popular" target commands
         
(define (halted?)
  (let ((CTRLA (LDCS UPDI.DEBUG_STATUSA)))
    (cond ((= CTRLA 0) #f)
          ((= CTRLA 1) #t)
          (else (error "unknown register value in UPDI.DEBUG_STATUSA (#x05)" CTRLA)))))

(define (stop!) ;; naming matches gdb/qemu's stop/cont commands
  (unless (updi-key-ocd)
    (set! (updi-key-ocd) #t))
  (STCS UPDI.DEBUG_CTRLA #b0001)
  (unless (halted?) ;; extra safety check
    (error "OBS: could not halt target")))

(define (cont!)
  ;; skipping ocd key check here (could it be stopped without a key?)
  (STCS UPDI.DEBUG_CTRLA #b0010)
  (when (halted?) ;; extra safety check
    (error "OBS: could not resume target")))

(define (reset!)
  (set! (resetting) #t)
  ;; TODO: poll for (updi-rstsys?) here
  (set! (resetting) #t))

;; ======================================== target registers
;; TODO: get all of these from a pack file or something
;; TODO: review if we want (set! (PC) x) or (PC x) <-- less typing
;;       or something even better (something that could print bits too?)

(define (make-register addr len)
  (getter-with-setter
   (lambda ()  (LDS addr   len))
   (lambda (v) (STS addr v len))))

(define CPU.SP            #x003d) ;; CPU
(define CPU.REGISTER_FILE #x0fa0) ;; SYSCFG
(define CPU.PC            #x0f94) ;; SYSCFG
(define CPU.SREG          #x3f00) ;; ?

(define r
  (getter-with-setter
   (lambda (r)   (LDS (+ CPU.REGISTER_FILE r) 1))
   (lambda (r v) (STS (+ CPU.REGISTER_FILE r) v 1))))

(define PC    (make-register CPU.PC         1))
(define SP    (make-register CPU.SP         1))
(define PORTA (make-register (+ #x0400 4)   1))
(define SREG  (make-register #x3f00 1))

(define (regs)
  (memory-read* CPU.REGISTER_FILE 32))
