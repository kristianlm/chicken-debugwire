
(define (between lower upper)
  (lambda (r)
    (unless (<= lower r upper) (error (conc "out of range [ " lower " " upper "]") r))
    r))

(define (register r) r)
(define (constant k) k)
(define (half a)
  (when (odd? a) (error "odd address operand" a))
  (arithmetic-shift a -1))

(define-instruction add "Add without Carry"
  ((d (register (between 0 31)))
   (r (register (between 0 31))))
  "0000 11rd dddd rrrr")

(define-instruction adiw "Add Immediate to Word" ;; little endian
  ((d (register (between 24 30) (cut - <> 24) half))
   (K (constant (between 0 63))))
  "1001 0110 KKdd KKKK")

(define-instruction andi "Logical AND with Immediate"
  ((d (register (between 16 31) (cut - <> 16)))
   (K (constant (between 0 255))))
  "0111 KKKK dddd KKKK")

(define-instruction break "Break"
  ()
  "1001 0101 1001 1000")

(define-instruction breq "Branch if Equal"
  ((k (constant (between -64 63))))
  "1111 00kk kkkk k001")

(define-instruction brne "Branch if Not Equal"
  ((k (constant (between -64 63))))
  "1111 01kk kkkk k001")

(define-instruction call "Long Call to a Subroutine"
  ((k (half (between 0 #xffff)))) ;; for 16-bit PC counter (128k mem or less)
  "1001 010k kkkk 111k 
   kkkk kkkk kkkk kkkk")

(define-instruction cli "Clear Global Interrupt Enable Bit"
  ()
  "1001 0100 1111 1000")

(define-instruction eor "Exclusive OR"
  ((d (register (between 0 31)))
   (r (register (between 0 31))))
  "0010 01rd dddd rrrr" )

(define-instruction in "Load an I/O Location to Register"
  ((d (register (between 0 31)))
   (A (constant (between 0 63))))
  "1011 0AAd dddd AAAA")

(define-instruction jmp "Jump" ;; byte-address => word address (16bit)
  ((k (address (even (between 0 #x400000)))))
  "1001 010k kkkk 110k kkkk kkkk kkkk kkkk")

(define-instruction ldi "Load Immediate"
  ((d (register (between 16 31) (cut - <> 16)))
   (K (constant (between 0 255))))
  "1110 KKKK dddd KKKK" )

(define-instruction lds "Load Direct from Data Space"
  ((d (register (between 0 65535)))
   (k (constant (between 0 65535))))
  "1001 000d dddd 0000 kkkk kkkk kkkk kkkk")

(define-instruction ldZ "Load Indirect From Data Space to Register using Z"
  ((d (register (between 0 31))))
  "1000 000d dddd 0000")

(define-instruction ldZ- "Load Indirect From Data Space to Register using Z, pre dec"
  ((d (register (between 0 31))))
  "1001 000d dddd 0010")

(define-instruction mov "Copy Register"
  ((d (register (between 0 31)))
   (r (register (between 0 31))))
  "0010 11rd dddd rrrr")

(define-instruction movw  "Copy Register Word"
  ((d (register (between 0 31)))
   (r (register (between 0 31))))
  "0000 0001 dddd rrrr")

(define-instruction nop "No Operation"
  ()
  "0000 0000 0000 0000")

(define-instruction or "Logical OR"
  ((d (register 0 31))
   (r (register 0 31)))
  "0010 10rd dddd rrrr")

(define-instruction out "Store Register to I/O Location"
  ((A (constant (between 0 63)))
   (r (register (between 0 31))))
  "1011 1AAr rrrr AAAA" )

(define-instruction pop "Pop Register from Stack"
  ((d (register (between 0 31))))
  "1001 000d dddd 1111")

(define-instruction push  "Push Register on Stack"
  ((d (register (between 0 31))))
  "1001 001d dddd 1111")

(define-instruction ret "Return from Subroutine"
  ()
  "1001 0101 0000 1000")

(define-instruction rcall "Relative Call to Subroutine"
  ((k (constant (between -2048 2047))))
  "1101 kkkk kkkk kkkk")

(define-instruction rjmp "Relative Jump"
  ((k ((between -2048 2047))))
  "1100 kkkk kkkk kkkk")

(define-instruction ser "Set all Bits in Register"
  ((d (register (between 16 31))))
  "1110 1111 dddd 1111" )

(define-instruction sbci "Subtract Immediate with Carry SBI - Set Bit in I/O Register"
  ((d (register (between 16 31) (cut - <> 16)))
   (K (constant (between 0 255))))
  "0100 KKKK dddd KKKK")

(define-instruction sbrs "Skip if Bit in Register is Set"
  ((r (register  (between 0 31)))
   (b (constant  (between 0 7))))
  "1111 111r rrrr 0bbb")

(define-instruction spm "Store Program Memory"
  ()
  "1001 0101 1110 1000")

(define-instruction sts "Store Direct to Data Space"
  ((k (constant (between 0 65535)))
   (d (register (between 0 31))))
  "1001 001d dddd 0000 kkkk kkkk kkkk kkkk")

(define-instruction stZ "Store Indirect From Register to Data Space using Index Z"
  ((r (register (between 0 31))))
  "1000 001r rrrr 0000")

(define-instruction stZ+q  "Store Indirect From Register to Data Space using Index Z+q"
  ((q (constant (between 0 63)))
   (r (register (between 0 31))))
  "10q0 qq1r rrrr 0qqq")

(define-instruction subi "Subtract Immediate"
  ((d (register (between 16 31) (cut - <> 16)))
   (K (constant (between 0 255))))
  "0101 KKKK dddd KKKK")
