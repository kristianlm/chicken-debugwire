(import chicken.bitwise
        chicken.string
        srfi-1)

(define (between lower upper)
  (lambda (r)
    (unless (<= lower r upper) (error (conc "out of range [ " lower " " upper "]") r))
    r))

(define (register r) r)
(define (constant k) k)
(define (flashadr a)
  (when (odd? a) (error "odd address operand" a))
  (arithmetic-shift a -1))

(define (byte->word k)
  (if (even? k)
      (arithmetic-shift k -1)
      (error "operand must be even" k)))


;; (integer->opcode #xAA112233 32) => #${aa112233}
(define (integer->opcode n size)
  (let loop ((n n) (size (quotient size 8)) (result '()))
    (if (> size 0)
        (loop (arithmetic-shift n -8) (- size 1) (cons (bitwise-and n #xFF) result))
        (u8vector->blob/shared (list->u8vector result)))))

;; (avr-instruction <name> <description> <lets> <bitcoding>)
(define-syntax instruction->procedure
  (er-macro-transformer
   (lambda (x r t)

     (import (only chicken.string conc)
             (only srfi-1 remove count)
             (only srfi-4 list->u8vector))
     
     (define (whitespace? c)
       (or (eq? c #\space)
           (eq? c #\tab)
           (eq? c #\newline)))

     ;; Make a body that produces an integer representing encoding of the
     ;; bits in <bitcoding>. All bits are symbols 😟
     ;;
     ;; (bitcoding->integer '(1 x x 1)) =>
     ;;  `(let ((<< …)) (<< 1) (<<! x) (<<! x) (<< 1) result)
     ;;
     ;; Assumes all variables referenced in <bitcoding> is available
     ;; in lexical scope, and will mutate these. The first occurance
     ;; of a variable represents its MSB.
     ;;
     ;; Bits are shifted into the result from MSB to LSB. The code is
     ;; hopefully straight-forward and optimization-friendly.
     (define (bitcoding->integer bits)
       `(let* ((result 0)
               (<< (lambda (bit)
                     (set! result
                           (bitwise-ior (arithmetic-shift result -1)
                                        (arithmetic-shift bit ,(- (length bits) 1)))))))
          ;; push each bit into result's MSB we don't need to know the
          ;; size of the variables.
          ,@(map
             (lambda (bit)
               (cond ((equal? bit '|0|) `(<< 0))
                     ((equal? bit '|1|) `(<< 1))
                     (else `(<< (let ((b (bitwise-and ,bit 1)))
                                  (set! ,bit (arithmetic-shift ,bit -1))
                                  b)))))
             (reverse bits))

          result))

     ;; (x (register (between 0 1))) => (x ((compose (between 0 1) register) x))
     (define (let-reform var spec)
       `(,var ((compose ,@(reverse spec)) ,var)))

     (let* ((op        (list-ref x 1))
            (lets      (list-ref x 3))
            (bitcoding (list-ref x 4))
            (bitcoding (remove whitespace? (string->list bitcoding)))
            (bitcoding (map (o string->symbol conc) bitcoding))
            (args      (map car lets)))
       `(lambda ,args ;; <-- these are the variables that gets mutated
          ;;          ,-- reassign variables for checks and conversions
          (let* (,@(map (cut apply let-reform <>) lets)
                 (result 
                  (integer->opcode
                   ,(bitcoding->integer bitcoding)
                   ,(length bitcoding))))
            ;; check that all variables have no "bits left":
            ,@(map (lambda (x)
                     `(unless (zero? ,x)
                        (error (conc (quote ,op) ": "
                                     ,(count (cut equal? x <>) bitcoding) "-bit "
                                     "operand overflow: " (quote ,x))
                               ,x)))
                   args)
            result))))))

;; (pp (expand '(instruction->procedure _ _ ((d (register (between 0 1)))) "1010 1111 dd")))

(define-syntax define-instruction
  (syntax-rules ()
    ((_ name description lets bitcoding)
     (define name (instruction->procedure name description lets bitcoding) ))))

(include "avr-instructions.scm")

(import test)
(test-group
 "instruction->procedure"
 (test #${af}   ((instruction->procedure _ _ () "1010 1111")))
 (test #${0fff} (add 31 31))
 (test #${0c00} (add  0  0))
 (test #${27ff} (eor 31 31))

 (test-error (add 32 32))

 
 (test
  (list #${e180} #${bb87} #${9598})
  (list (ldi 24 #x10)
        (out #x17 24)
        (break))))

