;; for a standalone script, you'll need to do something like:
;;
;; (import avr.updi)
;; (updi-open "/dev/ttyUSB1" 100000)
;; (updi-break)
;; (print "SIB: " (SIB))
;;

;;                        0     1    2     3     4
(set! (VREF.CTRLA) 1) ;; .55V, 1.1V, 2.5V, 4.3V, 1.5V

;;                    ,- OUTEN
;;                    |     ,- ENABLE
(set! (DAC.CTRLA)  #b01000001)

(set! (PORTA.OUT)  #b01000000) ;; PA6 output

(begin ;; quickly output some analog values
  (set! (DAC.DATA) #x80)
  (set! (DAC.DATA) #xFF)
  (set! (DAC.DATA) #x80)
  (set! (DAC.DATA) #x00))
