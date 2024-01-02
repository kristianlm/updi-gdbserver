(import chicken.file.posix chicken.string srfi-18
        chicken.memory.representation
        chicken.bitwise chicken.blob
        (only chicken.port with-output-to-string))

;; ======================================== logging

(define verbose? (make-parameter #f))
(define (prnt . args)
  (when (verbose?)
    (with-output-to-port (current-error-port)
      (lambda () (apply print args)))))

;; ======================================== basic helpers

(include "tty.scm")
(include "bytes.scm")
(include "registers.scm")
(define (wrt x) (with-output-to-string (lambda () (write x))))

;; ========================================

(define-record-type
    updi (%make-updi fd)
    updi?
    (fd updi-fd updi-fd-set!))

(define current-updi
  (let ((*fd* #f))
    (case-lambda
     (() *fd*)
     ((fd) (set! *fd* fd) *fd*))))

(define (current-updi-fd) (updi-fd (current-updi)))

;; (define updi-baudrate
;;   (getter-with-setter
;;    (lambda ()     (error "TODO"))
;;    (lambda (baud) (tty-setup (current-updi-fd) baud))))

;; (updi-open "/dev/ttyUSB1" 115200)
(define (updi-open tty baudrate)
  (let ((fd (file-open tty open/rdwr)))
    (tty-setup fd baudrate)
    (current-updi (%make-updi fd))
    (current-updi)))

;; ======================================== UPDI<-->UART interface

(define (tty-data-available? fd wait) ;;          ,-- seconds
  (receive (rs ws) (file-select (list fd) (list) wait)
    (if (pair? rs)
        (if (equal? (car rs) fd)
            #t
            (error "unexpected read fd ≠ " (car rs) fd))
        #f)))

(define timeout-procedure
  (make-parameter
   (lambda (fd s) (error (conc "updi read timeout (" s "s)")))))

;; handle partial reads produced by file-read.
;;
;; timeouts are used to avoid "hangs", waiting for reads that will
;; never occur. they should ideally never occur.
(define (file-read-retrying fd len)
  (let loop ((len len)
             (result '())) ;; <-- reversed list of strings
    (if (> len 0)
        (let* ((reply* (file-read fd len)) ;; <-- can timeout with 0 bytes
               (reply# (cadr reply*))
               (reply  (substring (car  reply*) 0 reply#)))
          (if (= 0 reply#)
              ((timeout-procedure) fd 1)
              (loop (- len reply#) (cons reply result))))
        (let ((final (reverse-string-append result)))
          (prnt "  read: " (string->blob final))
          final))))

(define (updi-drain! wait #!optional (fd (current-updi-fd)))
  ;; flush unexpected data
  (let loop ()
    (when (tty-data-available? fd wait)
      (prnt "updi-cmd drain")
      (file-read-retrying fd 1)
      (loop))))

(define (updi-break #!optional (µs 25000))
  (prnt "updi-break: " µs "µs")
  (tty-break (current-updi-fd) µs))

(define (updi-init)
  (updi-break)
  (updi-drain! 1) ;; TODO: make it work with <1s here, (avrdude uses 250ms)

  (define (timeout-give-up fd s)
    (error "unable to establish link with target"))

  ;; these are taken from avrdude.
  (define (updi-init-session-parameters)
    ;;                    ,--- NACKDIS
    ;;                    |,-- CCDETDIS collision/contention detection
    ;;                    ||,- UPDIDIS
    (STCS UPDI.CTRLB #b00001000)
    ;;                 ,------ IBDLY inter-byte delay
    ;;                 |
    ;;                 | ,---- PARD parity disable
    ;;                 | |,--- DTD Disable Time-Out Detection
    ;;                 | ||,-- RSD Response Signature Disable
    ;;                 | |||,,,-- GTVAL[2:0] Guard Time Value (000=128cycles)
    (STCS UPDI.CTRLA #b10000000))

  (updi-init-session-parameters)

  (parameterize ((timeout-procedure
                  (lambda (fd s)
                    (parameterize ((timeout-procedure timeout-give-up))
                      (prnt "updi-init: no response from programmer, resetting UPDI link")
                      (updi-break) ;; double-break signal for extra hope
                      (updi-break)
                      (updi-init-session-parameters)
                      (LDCS UPDI.STATUSA)
                      ;; hand off pretent-response to the LDCS call
                      ;; below so it can do its bytes->u8 conversion.
                      "\x00"))))
    ;; we verify link by reading STATUSA, as avrdude does. an
    ;; error will timeout and trigger the procedure above.
    (LDCS UPDI.STATUSA)))

;; send `data` and consume its echo (since TX is physically connected
;; to RX).
(define (updi-cmd data response-len #!optional (fd (current-updi-fd)))

  (updi-drain! 0)
  (prnt "updi-cmd " (number-of-bytes data) " + " response-len)

  (prnt " write: " (string->blob data))
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

;; The SIB is ascii but has a structure. See (32.3.7 System
;; Information Block). My ATtiny414 looks like this:
;;
;; ,------- family
;; ||||||| ,- reserved
;; ||||||| | ,--- NVM ver
;; ||||||| | ||| ,--- OCD ver
;; ||||||| | ||| ||| ,- reserved
;; ||||||| | ||| ||| | ,- DBG_OSC_FREQ
;; 0123456 7 89a bcd e f  ,,,,,,,,,,,,,,,,,,, unofficial
;; tinyAVR   P:0 D:0 - 3  M2 (01.59B14.0)\x00
(define (SIB)     (updi-cmd (conc "\x55\xe6") 32)) ;; unofficial 32-bit readout

(define (memory-read* address len)
  (unless (<=  len #xff) (error "len must be ≤ #xFF" len))
  (ST-ptr address 2)
  (REPEAT (- len 1) 1)
  (LD 1)
  (file-read-retrying (current-updi-fd) len))

;; (memory-write* #x3f10 "hei" 1)
;; (memory-read* #x3f0f 5)
(define (memory-write* address data blocksize)
  (unless (= blocksize 1) (error "TODO: blocksize must be 1" blocksize))
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

;; probe for success with exponential backoff.
(define (retrying n success)
  (let loop ((n n)
             (duration 1/10))
    (or (success)
        (if (> n 0)
            (begin (thread-sleep! duration)
                   (loop (- n 1)
                         (* duration 2)))
            #f))))

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

    (unless (retrying 4 (complement updi-nvmprog?))
      (error "could not put target in nvmprog mode"))))

(define (updi-chip-erase!)
  (set! (updi-key-chiperase) #t)
  (set! (resetting) #t)
  (set! (resetting) #f)

  ;; TODO retry with timeout here
  (unless (retrying 4 (complement updi-lockstatus?))
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
  (STS #x0f88 #b00000010 1) ;; OCD: continue until breakpoint
  (STCS UPDI.DEBUG_CTRLA #b0010))

;; warning: this number if guessed. Microchip/Atmel Studio has no
;; "single step 1 CPU instruction" button.
(define (step!)
  (STS #x0f88 #b00000100 1) ;; OCD: step single CPU cycle
  (STCS UPDI.DEBUG_CTRLA #b0010))

(define (reset!)
  (set! (resetting) #t)
  (let loop ()
    (unless (updi-rstsys?)
      (thread-sleep! 0.1)
      (loop)))
  (set! (resetting) #f)
  (let loop ()
    (when (updi-rstsys?)
      (thread-sleep! 0.1)
      (loop))))

;; ======================================== target registers

;; TODO: clean this up
(define CPU.REGISTER_FILE #x0fa0) ;; SYSCFG
(define (regs) (memory-read* CPU.REGISTER_FILE 32))
(define (r R)
  (let ((addr (+ CPU.REGISTER_FILE R)))
    (register (string->symbol (conc "r" R))
              addr
              (lambda (r) (LDS addr 1))
              (lambda (r v) (STS addr v 2))
              (lambda (r p) (reg-default-printer/u8 r p)))))

;; definition: the PC points to the instruction which is about to be
;; executed. TODO: review setting PC
(define PC ;; see test-bp.scm for an explanation of the -2
  (let ((addr #x0f94))
    (register 'PC addr
              (lambda (r) (- (LDS addr 2) 2))
              (lambda (r v) (STS addr (+ v 2) 2))
              (lambda (r port)
                (let ((value ((reg-getter r) r)))
                  (display "≡" port)
                  (display (string-pad (number->string value 16) 4 #\0) port))))))

;; reading SP as a word (LDS CPU.SP 2) is not supported by target, it seems.
(define SP
  (let ((addr #x003d))
    (register 'SP addr
              (lambda (r) (bytes->u16le (memory-read* addr 2)))
              (lambda (r v) (memory-write* addr (u16le->bytes v) 1))
              (lambda (r port)
                (let ((value ((reg-getter r) r)))
                  (display "≡" port)
                  (display (string-pad (number->string value 16) 4 #\0) port))))))

;; TODO: turn into register like for (r 24)
(define BP
  (getter-with-setter
   (lambda (bp)
     (cond ((= bp 0) (LDS #x0f80 2))
           ((= bp 1) (LDS #x0f84 2))
           (else (error "unsupported bp (only 0/1 supported) " bp))))
   (lambda (bp location)
     (unless (or (= 1 (remainder location 2)) (= 0 location))
       (warning "breakpoint location on even address" location))
     (cond ((= bp 0) (STS #x0f80 location 2))
           ((= bp 1) (STS #x0f84 location 2))
           (else (error "unsupported bp (only 0/1 supported) " bp))))))

;; this is really target-specific but hopefully won't change.
(define CPU.SREG (register 'CPU.SREG #x003f))
