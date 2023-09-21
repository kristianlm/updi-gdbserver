;; from Microship Studio, clicking "Debug" => "Start debugging and break"
;; that compiles, uploads code, and probably sets the CPU into a "debugging active and halted state"


;; STCS 55 c3 08
;; LDCS 55 80 00 00
;; STCS 55 c3 08
;; LDCS 55 80 20
;; LDCS 55 82 00
;; STCS 55 c2 03

;; SIB 55 e6 74 69 6e 79 41 56 52 20 50 3a 30 44 3a 30 2d 33 4d 32 20 28 30 31 2e 35 39 42 31 34 2e 30 29 00
;; LDCS 55 80 20
;; LDCS 55 89 03
;; STCS 55 c9 03
;; LDCS 55 80 20
;; LDCS 55 8c 00
;; LDCS 55 8b a2
;; LDCS 55 8b 82
;; LDCS 55 8b 82
55 e0 20 67 6f 72 50 4d 56 4e
;; LDCS 55 87 12
;; STCS 55 c8 59 #;(reset request)
;; LDCS 55 8b a3
;; STCS 55 c8 00
;; LDCS 55 8b 21
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
55 69 00 11 40
55 a0 02
55 24 1e 92 22
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDSu8 addr
55 04 01 0f 01
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDSu8 addr
55 04 85 12 f7
;; STCS 55 c8 59 #;(reset request)
;; LDCS 55 8b 29
;; STCS 55 c8 00
;; LDCS 55 8b 21
;; LDCS 55 8b 01
;; LDCS 55 83 08
;; STCS 55 c3 0c ff
;; STCS 55 c3 08
;; LDCS 55 80 20
;; LDCS 55 82 00
;; STCS 55 c2 03

;; SIB 55 e6 74 69 6e 79 41 56 52 20 50 3a 30 44 3a 30 2d 33 4d 32 20 28 30 31 2e 35 39 42 31 34 2e 30 29 00
;; LDCS 55 80 20
;; LDCS 55 89 03
;; STCS 55 c9 03
;; LDCS 55 80 20
;; LDCS 55 8c 00
;; LDCS 55 8b 82
;; LDCS 55 8b 82
;; LDCS 55 8b 82

;; KEY nvm-prog
55 e0 20 67 6f 72 50 4d 56 4e

;; LDCS 55 87 10
;; STCS 55 c8 59 #;(reset request)
;; LDCS 55 8b a3
;; STCS 55 c8 00
;; LDCS 55 8b 21
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08

;; ST-ptr    addr   => ack
55 69        00 11  40
;; REPEAT 1 + 2 times
55 a0     02
;; LD bytes
55 24        1e 92 22

;; LDCS 55 8b 08
;; LDCS 55 8b 08

;; LDSu8  addr    => reply
55 04     01 0f   01

;; NVM.STATUS bitlayout: . . . . | . error busy fbusy
;; LDSu8 addr=NVM.STATUS   =>data
55 04    0210              00
;; STS adr     =>ack data =>ack
55 44  0010    40    05   40

;; LDSu8 addr    =>data: busy & fbusy
55 04    02 10   03
;; LDSu8 addr    =>data: 
55 04    02 10   03
;; LDSu8 addr    =>data: yes, fbuse and busy are over
55 04    02 10   00

;; STCS 55 c8 59 #;(reset request)
;; LDCS 55 8b 29
;; STCS 55 c8 00
;; LDCS 55 8b 21
;; LDCS 55 8b 01
;; LDCS 55 83 08
;; STCS 55 c3 0c ff
;; STCS 55 c3 08
;; LDCS 55 80 20
;; LDCS 55 82 00
;; STCS 55 c2 03

;; SIB 55 e6 74 69 6e 79 41 56 52 20 50 3a 30 44 3a 30 2d 33 4d 32 20 28 30 31 2e 35 39 42 31 34 2e 30 29 00
;; LDCS 55 80 20
;; LDCS 55 89 03
;; STCS 55 c9 03
;; LDCS 55 80 20
;; LDCS 55 8c 00
;; LDCS 55 8b 82
;; LDCS 55 8b 82
;; LDCS 55 8b 82

;; KEY nvm-prog
55 e0 20 67 6f 72 50 4d 56 4e

;; LDCS 55 87 10
;; STCS 55 c8 59 #;(reset request)
;; LDCS 55 8b a3
;; STCS 55 c8 00
;; LDCS 55 8b 21
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 01
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDCS 55 8b 08


;; reading 3 bytes from 0x1100
;; ST-ptr
55 69 00 11 40 
;; REPEAT 1 + 2
55 a0 02
;; LD    addr  => data
55 24    1e 92 22

;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDSu8 addr
55 04 01 0f 01
;; LDCS 55 8b 08
;; LDCS 55 8b 08

;; reading flash
;; ST-ptr  adr      => ack
55 69      80 80    40

;; REPEAT 31 + 1 times
55 a0 1f 

;; LDu16
55 25 ffff ffff ffff ffff ffff ffff ffff ffff ffff ffff
      ffff ffff ffff ffff ffff ffff ffff ffff ffff ffff 
      ffff ffff ffff ffff ffff ffff ffff ffff ffff ffff
      ffff ffff

;; LDSu8 addr
55 04    02 10 00

;; ========================================
;;
;; this just writes a byte to a region of flash. it tickles the NVM
;; module to use the addressed page in the upcoming erase command.
;;
;; STS adr    =>ack data =>ack
55 44  00 80  40    01   40


;; ==================== NVM.CTRLA @ #x1000 aka 00 10 (9.5.1)
;; 00 -     No command
;; 01 WP    Write page buffer to memory (NVMCTRL.ADDR selects which memory)
;; 02 ER    Erase page (NVMCTRL.ADDR selects which memory)
;; 03 ERWP  Erase and write page (NVMCTRL.ADDR selects which memory)
;; 04 PBC   Page buffer clear
;; 05 CHER  Chip erase: Erase Flash and EEPROM (unless EESAVE in FUSE.SYSCFG is ‘1’)
;; 06 EEER  EEPROM Erase
;; 07 WFU   Write fuse (only accessible through UPDI)

;; STS NVM.CTRLA   =>ack data=ER=erase page       =>ack
55 44  00 10       40    02                       40

;; LDSu8 addr
55 04 02 10 03
;; LDSu8 addr
55 04 02 10 00
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDSu8 addr
55 04 02 10 00
;; STS NVM.CTRLA ack data ack
55 44 00 10      40 04 40

;; LDSu8 addr
55 04 02 10 00

;; ST-ptr
55 69      00 80    40

;; REPEAT 1 + 31 times
55 a0 1f
;; ST-*ptr++ (blocksize = 2)
55 65   19 c0  40   2b c0  40   2a c0  40   29 c0  40   28 c0  40
        27 c0  40   26 c0  40   25 c0  40   24 c0  40   23 c0  40
        22 c0  40   21 c0  40   20 c0  40   1f c0  40   1e c0  40
        1d c0  40   1c c0  40   1b c0  40   1a c0  40   19 c0  40
        18 c0  40   17 c0  40   16 c0  40   15 c0  40   14 c0  40
        13 c0  40   11 24  40   1f be  40   cf ef  40   cd bf  40
        df e3  40   de bf  40  

;; STS NVM.CTRLA  ack data ack
55 44  00 10      40  01 40
;; LDSu8 addr
55 04 02 10 03
;; LDSu8 addr
55 04 02 10 00
;; LDSu8 addr
55 04 02 10 00
;; STS adr ack data ack
55 44 40 80 40 01 40
;; STS NVM.CTRLA ack data ack
55 44 00 10      40  02 40
;; LDSu8 addr
55 04 02 10 03
;; LDSu8 addr
55 04 02 10 00
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDSu8 addr
55 04 02 10 00
;; STS adr ack data ack
55 44 00 10 40 04 40
;; LDSu8 addr
55 04 02 10 00
55 69 40 80 40
55 a0 1f
;; ST-*ptr++
55 65 1f e3 40 a0 e0 40 bf e3 40 ee eb 40 f0 e0 40 02 c0 40 05 90 40 0d 92 40 a2 30 40 b1 07 40 d9 f7 40 02 d0 40 30 c0 40 d2 cf 40 e0 e0 40 f4 e0 40 80 e2 40 80 83 40 10 82 40 80 83 40 10 82 40 80 83 40 10 82 40 80 83 40 1a c0 40 e0 e0 40 f4 e0 40 84 81 40 80 62 40 84 83 40 2f ef 40 83 ed 40
;; STS adr ack data ack
55 44 00 10 40 01 40
;; LDSu8 addr
55 04 02 10 03
;; LDSu8 addr
55 04 02 10 00
;; LDSu8 addr
55 04 02 10 00
;; STS adr ack data ack
55 44 80 80 40 01 40
;; STS adr ack data ack
55 44 00 10 40 02 40
;; LDSu8 addr
55 04 02 10 03
;; LDSu8 addr
55 04 02 10 00
;; LDCS 55 8b 08
;; LDCS 55 8b 08
;; LDSu8 addr
55 04 02 10 00
;; STS adr ack data ack
55 44 00 10 40 04 40
;; LDSu8 addr
55 04 02 10 00
55 69 80 80 40
55 a0 1f
55 65 90 e3 40 21 50 40 80 40 40 90 40 40 e1 f7 40 00 c0 40 00 00 40 84 81 40 8f 7d 40 84 83 40 2f ef 40 83 ed 40 90 e3 40 21 50 40 80 40 40 90 40 40 e1 f7 40 00 c0 40 00 00 40 80 91 40 00 3f 40 90 91 40 01 3f 40 89 2b 40 01 f7 40 00 00 40 80 e0 40 90 e0 40 08 95 40 f8 94 40 ff cf 40 01 00 40
;; STS adr ack data ack
55 44 00 10 40 01 40
;; LDSu8 addr
55 04 02 10 03
;; LDSu8 addr
55 04 02 10 00

;; done talking to NVM 02 10 is probably polling #x1002 (some nvm status I presume)

;; LDCS 55 8b 08
;; LDCS 55 8b 08
55 e0 20 20 20 20 20 44 43 4f ;; key enter OCD
;; LDCS 55 87 02
#;STCS 55 c8 59 #;(reset request)
#;LDCS 55 8b 29
#;STCS 55 c8 00
#;LDCS 55 8b 21
#;LDCS 55 8b 01
#;LDCS 55 8b 01
#;LDCS 55 85 00
#;LDCS 55 85 00
#;LDCS 55 8b 82
#;LDCS 55 85 01
#;LDCS 55 85 01
;; LDS OCD_STATUSA key _ _ _   _ 1 _ _    W BP1 BP0
55 05  8c 0f       8           4          0 0
;; LDS
55 05 94 0f 02 00
#;LDCS 55 8b 82
#;LDCS 55 8b 82
#;LDCS 55 85 01
#;STCS 55 c8 59 #;(reset request)
#;LDCS 55 8b a3
#;STCS 55 c8 00
#;LDCS 55 8b 21
#;LDCS 55 8b 01
#;LDCS 55 8b 01
#;LDCS 55 85 00
#;LDCS 55 85 00
#;LDCS 55 8b 82
#;LDCS 55 85 01
#;LDCS 55 85 01
#;LDS
55 05 8c 0f 84 00
;; LDS
55 05 94 0f 02 00
;; LDCS 55 8b 82
;; LDCS 55 8b 82
55 69 3d 00 40
55 a0 02
55 24 ff 3f 00
;; LDCS 55 8b 82
;; LDCS 55 8b 82
55 69 a0 0f 40
55 a0 1f
55 24 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;; LDCS 55 8b 82
;; LDCS 55 8b 82
;; STS adr ack data ack
55 45 80 0f 40 5d 00 40
;; STS adr ack data ack
55 45 84 0f 40 00 00 40
;; STS adr ack data ack
55 44 88 0f 40 02 40
;; STCS 55 c4 02
;; LDCS 55 8b 82
;; LDCS 55 85 01
;; LDCS 55 85 01
;; LDS
55 05 8c 0f 04 01
;; LDS
55 05 94 0f 5e 00
;; LDCS 55 8b 82
;; LDCS 55 8b 82
55 69 3d 00 40
55 a0 02
55 24 fd 3f 02
;; LDCS 55 8b 82
;; LDCS 55 8b 82
55 69 a0 0f 40
55 a0 1f
55 24 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 3f 00 00 00 00 00 00 00 00 02 3f ff 3f c0 00 
