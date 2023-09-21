
;; ==================== changing r1 to 0x01
;;          ACK   ACK
55 44 a1 0f 40 01 40
55 69 3d 00 40
55 a0 02
55 24 fd 3f 00
55 69 a0 0f 40
;; ,-- REPEAT
;; |,- SizeB = u8
;; || ,,- count
55 a0 1f

;; LD
;; |,- (ptr) Size A/B = u8 >
;; || -- address
55 24 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 3f eb 00 00 00 00 00 db 05 02 3f ff 3f 00 04
