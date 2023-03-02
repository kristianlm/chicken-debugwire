;;; misc utilities for handling (*-read start len) procedures. many of
;;; them have limmits on length and unsafe regions.

;; how can I simplify/combine these?
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

(define (writer-chunkify writer default-chunksize)
  (lambda (start bytes #!key (chunksize default-chunksize))
    (let loop ((start start)
               (bytes bytes))
      (when (> (number-of-bytes bytes) 0)
        (let* ((size (min chunksize (number-of-bytes bytes)))
               (next (+ start size)))
          (writer start (substring bytes 0 size))
          (loop next (substring bytes size)))))))

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

