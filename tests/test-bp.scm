;; Sometimes the PC* and BP locations are offset by a few bytes, and
;; this little test helps with tracking what's what.
;;
;; test-bp.target.c is designed to easily track exactly when the
;; target executes an instruction. by looking at PIN6, we can tell how
;; far PC* has come, regardless of the value of (PC*).
;;
;; Here's the disassembly of our main():
;;
;;  0x00000046 <+0>:    ldi     r24, 0x40      ; 64
;;  0x00000048 <+2>:    sts     0x0400, r24    ;  0x800400
;;  0x0000004c <+6>:    sts     0x0404, r24    ;  0x800404
;;  0x00000050 <+10>:   sts     0x0404, r1     ;  0x800404
;;  0x00000054 <+14>:   rjmp    .-10           ;  0x4c <main+6>
;;
;; This means that:
;; - If we stop at flash address 0x00000050, PA6 should be HIGH
;; - If we stop at flash address 0x0000004c, PA6 should be LOW
(import chicken.blob)

;; tested against (SIB) => "tinyAVR P:0D:0-3M2 (01.59B14.0)\x00" (attiny414)

(define (PA6) (if (= 0 (bitwise-and #b1000000 (LDS #x0408 1))) 'lo 'hi))

(define PC* ;; raw value
  (let ((addr #x0f94))
    (getter-with-setter
     (lambda ()  (LDS addr   2))
     (lambda (v) (STS addr v 2)))))


(define (stop-reasons)
  (let ((mask (LDS #x0f8d 1)))
    (append (if (not (= 0 (bitwise-and mask #b00000001))) '((BP 0)) '())
            (if (not (= 0 (bitwise-and mask #b00000010))) '((BP 1)) '()))))

(define (breakpoint-test flash-address1 flash-address2)
  (reset!)
  (set! (BP 0) (+ 1 flash-address1))
  (set! (BP 1) (+ 1 flash-address2))
  (cont!)
  (let loop ((n 10))
    (print " (PC*)=#x" (number->string (PC*) 16)
           " status(0f8d)=#x" (number->string (LDS #x0f8d 1) 16)
           " " (if (halted?) "halted" "running")
           " *0f80=" (string->blob (memory-read* #x0f80 16)))
    (if (< n 0)
        (error (conc "CPU never reached BP 1 @ #x" (number->string bp 16)) (BP 1))
        (if (halted?)
            (stop-reasons)
            (begin
              (thread-sleep! 0.1)
              (loop (- n 1)))))))

(begin
  (print ";; Should hit breakpoint before PA6 goes high")
  (print "stop-reasons: " (breakpoint-test #x004c #x0000))
  (print "PA6=" (PA6))
  (print "stop-reasons: " (breakpoint-test #x0000 #x004c))
  (print "PA6=" (PA6))

  (print ";; Should hit breakpoint after PA6 goes high")
  (print "stop-reasons: " (breakpoint-test #x0000 #x0050))
  (print "PA6=" (PA6))
  (print "stop-reasons: " (breakpoint-test #x0050 #x0000))
  (print "PA6=" (PA6)))


#| ==========  On my current setup, this yields:
;; Should hit breakpoint before PA6 goes high ;
(PC*)=#x4e status(0f8d)=#x1 halted *0f80=#${4d000000010000000220000004010000} ;
stop-reasons: ((BP 0))
PA6=lo
 (PC*)=#x4e status(0f8d)=#x2 halted *0f80=#${010000004d0000000220000004020000}
stop-reasons: ((BP 1))
PA6=lo
;; Should hit breakpoint after PA6 goes high
 (PC*)=#x52 status(0f8d)=#x2 halted *0f80=#${01000000510000000220000004020000}
stop-reasons: ((BP 1))
PA6=hi
 (PC*)=#x52 status(0f8d)=#x1 halted *0f80=#${51000000010000000220000004010000}
stop-reasons: ((BP 0))
PA6=hi
   ========== |#


;; We set a breackpoint at flash address #x004c + 1. When the target stops,
;; PC* is on #x004e - the instruction _after_ our breakpoint.
;;


;; ============================== step! test ==============================

(begin
  (reset!)
  (set! (BP 0) 0)
  (set! (BP 1) 0)
  (let loop ((n 0))
    (when (< n 20)
      (print n
             " (PC*)=#x" (number->string (PC*) 16)
             " (-(PC*)2)=#x" (number->string (- (PC*) 2) 16)
             " PA6=" (PA6)
             " status(0f8d)=#x" (number->string (LDS #x0f8d 1) 16)
             " " (if (halted?) "halted" "running")
             " *0f80=" (string->blob (memory-read* #x0f80 16)))
      (step!)
      (loop (+ n 1)))))

;; this reports:
;;
;;  0 (PC*)=#x02 (-(PC*)2)=#x00 PA6=lo status(0f8d)=#x0 halted *0f80=#${00000000000000000020000084000000}
;;  1 (PC*)=#x36 (-(PC*)2)=#x34 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  2 (PC*)=#x38 (-(PC*)2)=#x36 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  3 (PC*)=#x3a (-(PC*)2)=#x38 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  4 (PC*)=#x3c (-(PC*)2)=#x3a PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  5 (PC*)=#x3e (-(PC*)2)=#x3c PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  6 (PC*)=#x40 (-(PC*)2)=#x3e PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  7 (PC*)=#x42 (-(PC*)2)=#x40 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  8 (PC*)=#x48 (-(PC*)2)=#x46 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;  9 (PC*)=#x4a (-(PC*)2)=#x48 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 10 (PC*)=#x4e (-(PC*)2)=#x4c PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 11 (PC*)=#x52 (-(PC*)2)=#x50 PA6=hi status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 12 (PC*)=#x56 (-(PC*)2)=#x54 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 13 (PC*)=#x4e (-(PC*)2)=#x4c PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 14 (PC*)=#x52 (-(PC*)2)=#x50 PA6=hi status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 15 (PC*)=#x56 (-(PC*)2)=#x54 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 16 (PC*)=#x4e (-(PC*)2)=#x4c PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 17 (PC*)=#x52 (-(PC*)2)=#x50 PA6=hi status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 18 (PC*)=#x56 (-(PC*)2)=#x54 PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;; 19 (PC*)=#x4e (-(PC*)2)=#x4c PA6=lo status(0f8d)=#x1 halted *0f80=#${00000000000000000420000004010000}
;;
;; Which I believe indicates that:
;;
;; - (step!) works as expected (executes 1 machine instruction), PA6
;;   toggling makes sense and we're seeing PC* decrease after
;;   0x00000054 which is the negative rjmp.
;;
;; - (PC*) after (step!) is ahead by 2 (it's directly 2 after reset! and the flash addresses are off)
;;
;; - stop-reason after (step!) is the same as hitting breakpoint 1 ☹
;;
;; - PA6 is first seen hi on line 11, where PC*-2 == #x50. That should
;;   be directly _after_ the 0x0000004c flash location from
;;   above. Based on the disassembly, at 0x0000004c we should be just
;;   about to drive PA6 hight so that is good.

;; in conclusion:
;;
;; - +1 when setting breakpoints: (set! (BP x) (+ flash-address 1))
;;
;; - when reading PC* after step and breakpoint, subtract 2
;;
;; - So I think that this will work: if any (stop-reason) is nonempty,
;;   we must provide (- (PC*) 2) to gdb


;; ============================== PC* after (stop!) ==============================
(set! (BP 0) 0)
(set! (BP 1) 0)

;; When I eval this repeatedly, I'm seeing only three outcomes of (- (PC*) 2):
;;
;; ((PC*)-2)=#x4c stop-reasons=() halted PA6=lo *0f80=#${00000000000000000220000044000000}
;; ((PC*)-2)=#x50 stop-reasons=() halted PA6=hi *0f80=#${00000000000000000220000044000000}
;; ((PC*)-2)=#x54 stop-reasons=() halted PA6=lo *0f80=#${00000000000000000220000044000000}
;;
;; This is good news, it's in that tight loop. Also, it's reassuring
;; that PA6 is only hi just before 0x00000050. That's right after
;; executing 0x0000004c which should drive it high.

(begin
  (cont!)
  (stop!)
  (print " ((PC*)-2)=#x" (number->string (- (PC*) 2) 16)
         " stop-reasons=" (stop-reasons)
         " " (if (halted?) "halted" "running")
         " PA6=" (PA6)
         " *0f80=" (string->blob (memory-read* #x0f80 16))))


;; In conclusion, I should (define (PC*) (- (LDS #x0f94 2) 2)). But
;; why -2? And should I +2 when I set it?
