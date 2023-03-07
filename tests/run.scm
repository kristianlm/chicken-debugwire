(import test avr.asm)

(test-group
 "avr.asm"
 (test #${ff 0f} (add 31 31))
 (test #${00 0c} (add  0  0))
 (test #${ff 27} (eor 31 31))

 (test-error (add 32 32))

 (test
  (list #${80 e1} #${87 bf} #${98 95})
  (list (ldi 24 #x10)
        (out #x37 24)
        (break))))

(test-group
 "instruction->procedure"
 (import chicken.bitwise)
 (test #${55af}  ((instruction->procedure _ _ () "1010 1111    0101 0101"))))


;; ========================================

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

(test-group
 "assert-page/single"

 (test (when #f) (assert-page/single 10  8 1))
 (test (when #f) (assert-page/single 10  9 1))
 (test (when #f) (assert-page/single 10 10 1))
 (test (when #f) (assert-page/single 10 11 1))
 (test (when #f) (assert-page/single 10 12 1))


 (test (when #f) (assert-page/single 10  8 2))
 (test-error     (assert-page/single 10  9 2))
 (test (when #f) (assert-page/single 10 10 2))
 (test (when #f) (assert-page/single 10 11 2))
 (test (when #f) (assert-page/single 10 12 2))


 (test (when #f) (assert-page/single 10  0  8))
 (test (when #f) (assert-page/single 10  0  9))
 (test (when #f) (assert-page/single 10  0 10))
 (test-error     (assert-page/single 10  0 11))
 (test-error     (assert-page/single 10  0 12)))

(test-exit)
