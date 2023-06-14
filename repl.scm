(import nrepl)

(include "avr-updi.scm")

(updi-open "/dev/ttyUSB1" 100000)
(updi-break)


(include "gdb.scm")

;; csi's read (on stdin input) makes the above thread
;; hang periodically. tcp ports don't do that.
(print "nrepl on port 1234")
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
(nrepl 1234)


(PC)
(begin
  (set! (PORTA) #b00100000) (set! (PORTA) 0)
  (set! (PORTA) #b00100000) (set! (PORTA) 0)
  (set! (PORTA) #b00100000) (set! (PORTA) 0)
  (set! (PORTA) #b00100000) (set! (PORTA) 0)
  (set! (PORTA) #b00100000) (set! (PORTA) 0))


(cont!)

;; ========================================

(begin
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000)
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000)
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000)
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000)
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000)
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000)
  (updi-write-byte (+ #x400 4) #b00100000) (updi-write-byte (+ #x400 4) #b00000000))

(updi-write-word #x0f94 #x0100)

(updi-)

(begin
  (set! (PC) 1000)
  (PC))

;; These don't do anything it seems ... hmm, strange
'((updi-read-byte #x0f88)
  (updi-write-byte #x0f88 #x02))

