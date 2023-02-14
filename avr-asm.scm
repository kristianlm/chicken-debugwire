(import chicken.bitwise
        chicken.string
        srfi-1)

(define (register lower upper)
  (lambda (r)
    (unless (<= lower r upper) (error (conc "register out of range [ " lower " " upper "]") r))
    r))

(define (constant lower upper)
  (lambda (k)
    (unless (<= lower k upper) (error (conc "constant out of range [" lower " " upper "]") k))
    k))

(define (register-pair lower upper)
  (lambda (r)
    (unless (<= lower r upper) (error (conc "register-pair out of range [ " lower " " upper "]") r))
    (unless (even? r) (error (conc "register-pair cannot be odd") k))
    (quotient r 2)))

(even (register lower upper))

(define (byte->word k)
  (if (even? k)
      (arithmetic-shift k -1)
      (error "operand must be even" k)))

;; (avr-instruction <name> <description> <lets> <bitcoding>)
(define-syntax instruction->procedure
  (er-macro-transformer
   (lambda (x r t)

     (import (only chicken.string conc)
             (only srfi-1 remove)
             (only srfi-4 list->u8vector))
     
     (define (whitespace? c)
       (or (eq? c #\space)
           (eq? c #\tab)
           (eq? c #\newline)))

     ;; Make a body that produces an integer representing encoding of the
     ;; bits in <bitcoding>:
     ;;
     ;; (bitcoding->form '(1 x x 1)) =>
     ;;  `(let ((<< …)) (<< 1) (<<! x) (<<! x) (<< 1) result)
     ;;
     ;; Assumes all variables referenced in <bitcoding> is available in
     ;; lexical scope. The first occurance of a variable represents
     ;; its MSB.
     ;;
     ;; Bits are shifted into an integer result from MSB to LSB. The
     ;; code is hopefully straight-forward and optimization-friendly.
     (define (bitcoding->form bitcoding)
       `(let* ((result 0)              
               (<< (lambda (bit)
                     (set! result
                           (bitwise-ior (arithmetic-shift result -1)
                                        (arithmetic-shift bit ,(- (length bitcoding) 1)))))))
          ;; push each bit into result's MSB we don't need to know the
          ;; size of the variables.
          ,@(map
             (lambda (c)
               (cond ((equal? c '|0|) `(<< 0))
                     ((equal? c '|1|) `(<< 1))
                     (else `(<< (let ((b (bitwise-and ,c 1)))
                                  (set! ,c (arithmetic-shift ,c -1))
                                  b)))))
             (reverse ;; ,-- turn everything to symbols 😟
              (map (o string->symbol conc) bitcoding)))

          result))

     ;; (u8vector->blob (integer->bloble #xAA112233 4)) => #${aa112233}
     (define (integer->bloble n size)
       (let loop ((n n) (size size) (result '()))
         (if (> size 0)
             (loop (arithmetic-shift n -8) (- size 1) (cons (bitwise-and n #xFF) result))
             (list->u8vector result))))

     (let* ((op        (list-ref x 1))
            (lets      (list-ref x 3))
            (bitcoding (list-ref x 4))
            (bitcoding (remove whitespace? (string->list bitcoding)))
            (args      (map car lets)))
       `(lambda ,args
          ,(bitcoding->form bitcoding))))))

(define-syntax define)

(import chicken.pretty-print)
(pp (bitcoding->form '(1 x x 1)))


(define-instruction add "Add without Carry"
  ((d (register 0 31))
   (r (register 0 31)))
  "0000 11rd dddd rrrr")

(define-instruction andi "Logical AND with Immediate"
  ((d (register 16  31))
   (K (constant  0 255)))
  "0111 KKKK dddd KKKK")

(define-instruction breq "Branch if Equal"
  ((k (byte->word (constant (* 2 -63) (* 2 63)))))
  "1111 00kk kkkk k001")

(define-instruction call "Long Call to a Subroutine"
  ((k (arithmetic-shift (constant 0 65535) -1))) ;; for 16-bit PC counter (128k mem or less)
  "1001 010k kkkk 111k 
   kkkk kkkk kkkk kkkk")

(define-instruction cli "Clear Global Interrupt Enable Bit"
  ()
  "1001 0100 1111 1000")

(define-instruction eor "Exclusive OR"
  ((d (register 0 31))
   (r (register 0 31)))
  "0010 01rd dddd rrrr" )

(define-instruction in "Load an I/O Location to Register"
  ((d (register 0 31))
   (A (constant 0 63)))
  "1011 0AAd dddd AAAA")

(define-instruction jmp "Jump" ;; byte-address => word address (16bit)
  ((k (arithmetic-shift (constant 0 4000000) -1)))
  "1001 010k kkkk 110k kkkk kkkk kkkk kkkk")

(define-instruction ldi "Load Immediate"
  ((d (register 16  31))
   (K (constant  0 255)))
  "1110 KKKK dddd KKKK" )

(define-instruction lds "Load Direct from Data Space"
  ((d (register 0 65535))
   (k (constant 0 65535)))
  "1001 000d dddd 0000 kkkk kkkk kkkk kkkk")

(define-instruction ldZ "Load Indirect From Data Space to Register using Z"
  ((d (register 0 31)))
  "1000 000d dddd 0000")

(define-instruction ldZ- "Load Indirect From Data Space to Register using Z, pre dec"
  ((d (register 0 31)))
  "1001 000d dddd 0010")

(define-instruction mov "Copy Register"
  ((d (register 0 31))
   (r (register 0 31)))
  "0010 11rd dddd rrrr")

(define-instruction movw  "Copy Register Word"
  ((d (register-pair 0 31))
   (r (register-pair 0 31)))
  "0000 0001 dddd rrrr")

(define-instruction nop "No Operation"
  ()
  "0000 0000 0000 0000")

(define-instruction or "Logical OR"
  ((d (register 0 31))
   (r (register 0 31)))
  "0010 10rd dddd rrrr")

(define-instruction out "Store Register to I/O Location"
  ((A (constant 0 63))
   (r (register 0 31)))
  "1011 1AAr rrrr AAAA" )

(define-instruction pop "Pop Register from Stack"
  ((d (register 0 31)))
  "1001 000d dddd 1111")

(define-instruction push  "Push Register on Stack"
  ((d (register 0 31)))
  "1001 001d dddd 1111")

(define-instruction ret "Return from Subroutine"
  ()
  "1001 0101 0000 1000")

(define-instruction rjmp "Relative Jump"
  ((k (byte->word (constant -2048 2047)))) ;; these negative numbers, are we just lucky it works?
  "1100 kkkk kkkk kkkk")

(define-instruction ser "Set all Bits in Register"
  ((d (register 16 31)))
  "1110 1111 dddd 1111" )

(define-instruction sbrs "Skip if Bit in Register is Set"
  ((r (register  0 31))
   (b (constant  0 7)))
  "1111 111r rrrr 0bbb")

(define-instruction spm "Store Program Memory"
  ()
  "1001 0101 1110 1000")

(define-instruction sts "Store Direct to Data Space"
  ((k (constant 0 65535))
   (d (register 0 31)))
  "1001 001d dddd 0000 kkkk kkkk kkkk kkkk")

(define-instruction stZ "Store Indirect From Register to Data Space using Index Z"
  ((r (register 0 31)))
  "1000 001r rrrr 0000")

(define-instruction stZ+q  "Store Indirect From Register to Data Space using Index Z+q"
  ((q (constant 0 63))
   (r (register 0 31)))
  "10q0 qq1r rrrr 0qqq")
