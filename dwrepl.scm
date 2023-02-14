(import chicken.io chicken.file.posix
        chicken.blob
        chicken.bitwise
        chicken.string
        chicken.memory.representation
        chicken.port
        chicken.time
        srfi-1 srfi-18)

(load "tty.so")

(define (wrt x) (with-output-to-string (lambda () (write x))))

;; (bytevector 0 "U" #xff) => "\x00U\xff"
;; (bytevector 65 "bc" #\d) => "Abcd"
(define (bytevector . bytes)
  (list->string
   (append-map
    (lambda (b)
      (cond ((char? b)   (list b))
            ((number? b) (list (integer->char b)))
            ((string? b) (string->list b))
            (else (error "bytevector: expecting number/string"))))
    bytes)))

(define (bytes->u8 bytes)
  (unless (>= (number-of-bytes bytes) 1) (error "bytes->u8: need >=1 bytes " (wrt bytes)))
  (char->integer (string-ref bytes 0)))

(define (u8->bytes value) (bytevector value))

;; (bytes->u16/be "\x01\x00") => 256
;; (bytes->u16/le "\x01\x00") => 513
(define (bytes->u16le bytes)
  (unless (>= (number-of-bytes bytes) 2) (error "bytes->u16: need >=2 bytes " (wrt bytes)))
  (+ (arithmetic-shift (char->integer (string-ref bytes 0)) 0)
     (arithmetic-shift (char->integer (string-ref bytes 1)) 8)))

(define (bytes->u16be bytes)
  (unless (>= (number-of-bytes bytes) 2) (error "bytes->u16: need >=2 bytes " (wrt bytes)))
  (+ (arithmetic-shift (char->integer (string-ref bytes 0)) 8)
     (arithmetic-shift (char->integer (string-ref bytes 1)) 0)))

(define (u16be->bytes value)
  (bytevector (bitwise-and (arithmetic-shift value -8) #xff) ;; TODO: aflag
              (bitwise-and (arithmetic-shift value  0) #xff)))

(define (u16le->bytes value)
  (bytevector (bitwise-and (arithmetic-shift value  0) #xff)
              (bitwise-and (arithmetic-shift value -8) #xff)))


(define dw (file-open "/dev/ttyUSB2" open/rdwr))
(define (baudrate) (quotient 8000000 64))
(tty-setup dw (baudrate))

(define (dw-read dw len #!optional
                 (seconds 2)
                 (timeout
                  (lambda (str)
                    (error (conc "dw-read: reached timeout (" seconds "s)") str))))
  (let ((eot (+ (current-seconds) seconds)))
    (let loop ((len len)
               (result ""))
      (if (> len 0)
          (if (> (current-seconds) eot)
              (timeout result)
              (apply (lambda (chunk bytes)
                       (unless (= bytes len)
                         (thread-sleep! 0.1))
                       (loop (- len bytes) (conc result chunk)))
                     (file-read dw len)))
          result))))

(define (dw-break-expect dw)
  (let loop ()
    (let ((c (apply
              (lambda (str bytes)
                (if (equal? 0 bytes)
                    (error "unexpected EOF")
                    (char->integer (string-ref str 0))))
              (file-read dw 1))))
      (print "dw-break-expect: serial read " c)
      (if (or (equal? c 0)
              (equal? c 255))
          (loop)
          c))))

(define (dw-break! dw)
  (tty-break dw (quotient 20000000 (baudrate)))
  (dw-break-expect dw))

;; since RX and TX is shared, all writes are echoed back to us (unless
;; there is a collision).
(define (dw-write dw str)
  (file-write dw str)
  (let ((len (number-of-bytes str)))
    (dw-read dw len)))

;; ==================== flow ====================

(define (dw-reset dw)
  (dw-write dw (bytevector #x07))
  (dw-break-expect dw))

;; (begin (dw-step dw) (PC dw))
(define (dw-step dw)
  (dw-write dw (bytevector #x60 #x31))
  (dw-break-expect dw))

(define (dw-disable! dw) ;; disable debugWirte on target (enables ISP)
  (dw-write dw (bytevector #x06)))

;; ==================== registers ====================

(define (dw-registers-read dw start length)
  (set! (PC dw) start)
  (set! (BP dw) (+ start length))
  (dw-write dw (bytevector #x66 #xC2 #x01 #x20))
  (dw-read dw length))

(define (dw-registers-write dw start regs)
  (set! (PC dw) start)
  (set! (BP dw) (+ start (number-of-bytes regs)))
  (dw-write dw (bytevector #x66 #xC2 #x05 #x20 regs)))

(define (dw-signature dw) ;; don't really know what this is
  (dw-write dw (bytevector #xF3))
  (bytes->u16 (dw-read dw 2)))

;; (PC dw)
;; (set! (PC dw) 33)
;; TODO: addr flag for bigger chips
(define PC ;; program counter (aka instruction pointer)
  (getter-with-setter
   (lambda (dw)
     (dw-write dw (bytevector #xF0))
     (bytes->u16be (dw-read dw 2)))
   (lambda (dw value) (dw-write dw (bytevector #xD0 (u16be->bytes value))))))

(define BP ;; breakpoint (hw) (BP dw)
  (getter-with-setter
   (lambda (dw)
     (dw-write dw (bytevector #xF1))
     (bytes->u16be (dw-read dw 2)))
   (lambda (dw value) (dw-write dw (bytevector #xD1 (u16be->bytes value))))))

;; (set! (IR dw) "ab")
(define IR ;; instruction register (IR dw)
  (getter-with-setter
   (lambda (dw)
     (dw-write dw (bytevector #xF2))
     (dw-read dw 2))
   (lambda (dw value)
     (let ((value (cond ((string? value)
                         (unless (equal? 2 (number-of-bytes value))
                           (error "expecting 2-byte instruction" (wrt value)))value)
                        ((number? value) (u16be->bytes value))
                        (else (error "don't know how to IR this" value)))))
       (dw-write dw (bytevector #xD2 value))))))

(define (dw-exec dw word)
  (set! (IR dw) word)
  (dw-write dw (bytevector #x23)))

;; ==================== 4.5.1 General Purpose Register File

(define r0
  (getter-with-setter
   (lambda (dw)       (dw-registers-read  dw 0 1))
   (lambda (dw value) (dw-registers-write dw 0 (bytevector value)))))

(define r24
  (getter-with-setter
   (lambda (dw)       (dw-registers-read  dw 24 1))
   (lambda (dw value) (dw-registers-write dw 24 (bytevector value)))))

;; OBS: these are little-endian

(define X ;; X-register
  (getter-with-setter
   (lambda (dw) (bytes->u16le (dw-registers-read dw 26 2)))
   (lambda (dw value) (dw-registers-write dw 26 (u16le->bytes value)))))

(define Y ;; Y-register
  (getter-with-setter
   (lambda (dw) (bytes->u16le (dw-registers-read dw 28 2)))
   (lambda (dw value) (dw-registers-write dw 28 (u16le->bytes value)))))

;; (set! (Z dw) 257)
(define Z ;; Z-register
  (getter-with-setter
   (lambda (dw) (bytes->u16le (dw-registers-read dw 30 2)))
   (lambda (dw value) (dw-registers-write dw 30 (u16le->bytes value)))))


;; u8 hi(int w) {return (w>>8)&0xff;}
;; u8 lo(int w) {return (w   )&0xff;}
;; void DwSetPC(u16 pc) {DwSend(Bytes(0xD0, hi(pc)|AddrFlag(), lo(pc)));}
;; void DwSetBP(u16 bp) {DwSend(Bytes(0xD1, hi(bp)|AddrFlag(), lo(bp)));}

(define (dw-sram-read dw start len)
  (unless (<= len 128) (error "dw-sram-read: len must be <=128" len))
  (set! (Z dw) start)
  (set! (PC dw) 0)
  (set! (BP dw) (* 2 len))
  (dw-write dw (bytevector #x66 #xC2 #x00 #x20))
  (dw-read dw len))

(define (dw-sram-write dw start bytes)
  (let ((len (number-of-bytes bytes)))
    (unless (<= len 128) (error "dw-sram-read: len must be <=128" len))
    (set! (Z dw) start)
    (set! (PC dw) 1)
    (set! (BP dw) (+ 1 (* 2 len)))
    (dw-write dw (bytevector #x66 #xC2 #x04 #x20 bytes))))

;; ====================

(define DDRB
  (getter-with-setter
   (lambda (dw) (bytes->u8 (dw-sram-read dw #x37 1)))
   (lambda (dw v) (dw-sram-write dw #x37 (u8->bytes v)))))

(define PORTB
  (getter-with-setter
   (lambda (dw) (bytes->u8 (dw-sram-read dw #x38 1)))
   (lambda (dw v) (dw-sram-write dw #x38 (u8->bytes v)))))


(define SPMCSR
  (getter-with-setter
   (lambda (dw) (bytes->u8 (dw-sram-read dw #x57 1)))
   (lambda (dw v) (dw-sram-write dw #x57 (u8->bytes v)))))


(define (dw-register1 dw reg)
  (set! (PC dw) reg)
  (set! (BP dw) (+ 1 reg))
  (dw-write dw (bytevector #x66
                           ;;#xD0 00 r
                           ;;#xD1 00 r+1
                           #xC2 #x01 #x21)))

(define (dw-register1! dw reg value)
  (set! (PC dw) reg)
  (set! (BP dw) (+ 1 reg))
  (dw-write dw (bytevector #x66
                           ;;#xD0 00 r
                           ;;#xD1 00 r+1
                           #xC2 #x05 #x21 value)))



;; ==================== flash ====================

(define (dw-flash-read dw start len)
  (set! (Z dw) start)
  (set! (PC dw) 0)
  (set! (BP dw) (* 2 len))
  (dw-write dw (bytevector #x66 #xC2 #x02 #x20))
  (dw-read dw len))

(define (dw-flash-page-erase dw start)
  (define PGERS (arithmetic-shift 1 1))
  (define SPMEN (arithmetic-shift 1 0)) ;; self-program memory enable (?)
  (define out37r24 "\xBF\x87")
  (define spm      "\x95\xE8")
  
  (set! (Z dw) start)
  ;;(set! (r24 dw) (bitwise-ior PGERS SPMEN))
  ;;(dw-exec dw out37r24)
  (set! (SPMCSR dw) (bitwise-ior PGERS SPMEN)) ;;<-- You wish!
  (dw-exec dw spm))

;; 19.9.1: If only SPMEN is written, the following SPM instruction
;; will store the value in R1:R0 in the temporary page buffer
;; addressed by the Z-pointer.
(define (dw-flash-write dw start data)
  (define SPMEN (arithmetic-shift 1 0)) ;; self-program memory enable (?)
  ;;(define out37r24 "\xBF\x87")
  (define spm      "\x95\xE8")
  
  (set! (Z dw) start)
  (set! (SPMCSR dw) (bitwise-ior SPMEN))
  ;;(dw-exec dw out37r24)
  (dw-exec dw spm))

(dw-flash-page-erase dw 50)

(string->blob (dw-flash-read dw 50 170))

;; ============================================================


;; 00000000 <foo>:
;;    0:	80 e1       	ldi	r24, 0x10	; 16
;;    2:	87 bb       	out	0x17, r24	; 23
;;    6:	98 95       	break
;;    8:	08 95       	ret

;;      ldi r24,0x10   out 0x17,r24   break
(begin #${80 e1        87 bb           98 95 })


