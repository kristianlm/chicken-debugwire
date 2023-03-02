(import test avr.asm)

(test-group
 "avr-instructions instruction->procedure"
 (test #${af}   ((instruction->procedure _ _ () "1010 1111")))
 (test #${ff 0f} (add 31 31))
 (test #${00 0c} (add  0  0))
 (test #${ff 27} (eor 31 31))

 (test-error (add 32 32))

 (test
  (list #${80 e1} #${87 bf} #${98 95})
  (list (ldi 24 #x10)
        (out #x37 24)
        (break))))

(import avr.debugwire)
(test-group
 "reader-mask"
 (define j
   (reader-mask
    (lambda (s l) (list #:A s l)) 4 3
    (lambda (s l) (list #:B s l))
    cons '()))

 ;;                                       |AAAABBBA|
 ;;                                       |01234567|
 (test '((#:A 0 3)) (j 0 3))           ;; |---     |
 (test '((#:A 3 1)) (j 3 1))           ;; |   -    |
 (test '((#:B 4 1)) (j 4 1))           ;; |    -   |
 (test '((#:B 6 1)) (j 6 1))           ;; |      - |
 (test '((#:B 4 1) (#:A 3 1)) (j 3 2)) ;; |   --   |
 (test '((#:A 7 1) (#:B 6 1)) (j 6 2)) ;; |      --|
 (test '((#:A 7 1)) (j 7 1))           ;; |       -|
 )

