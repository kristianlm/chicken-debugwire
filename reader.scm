;;; misc utilities for handling (*-read start len) procedures. many of
;;; them have limmits on length and unsafe regions.

(define (reader-chunkify reader default-chunksize kons initial)
  (lambda (start len #!key (chunksize default-chunksize))
    (let loop ((start start)
               (len len)
               (result initial))
      (if (> len 0)
          (let* ((size (min chunksize len))
                 (next (+ start size)))
            (loop next (- len size)
                  (kons result (reader start size))))
          result))))

;; replace data from reader (taking arguments start len) in range
;; start2 len2 with data from reader2.
(define (reader-mask reader start2 len2 reader2 kons initial)
  (lambda (start len)
    (let loop ((start start)
               (len len)
               (result initial))
      (if (> len 0)
          (if (< start start2)
              ;; before mapped region
              (let ((size (min len (- start2 start))))
                (loop (+ start size)
                      (- len size)
                      (kons (reader start size) result)))
              (if (< start (+ start2 len2))
                  ;; inside mapped region
                  (let ((size (min (- (+ start2 len2) start) len)))
                    (loop (+ start size)
                          (- len   size)
                          (kons (reader2 start size) result)))
                  ;; after mapped region
                  (let ((size len))
                    (loop (+ start size)
                          (- len size)
                          (kons (reader start size) result)))))
          result))))

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

