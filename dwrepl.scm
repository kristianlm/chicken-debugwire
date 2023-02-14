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
            ((blob? b) (string->list (blob->string b)))
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


(define current-dw (make-parameter (file-open "/dev/ttyUSB2" open/rdwr)))
(define (baudrate) (quotient 8000000 64))
(tty-setup (current-dw) (baudrate))

(define (dw-read len #!key
                 (dw (current-dw))
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

(define (dw-break-expect #!optional (dw (current-dw)))
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

(define (dw-break!)
  (tty-break (current-dw) (quotient 20000000 (baudrate)))
  (dw-break-expect))

;; since RX and TX is shared, all writes are echoed back to us (unless
;; there is a collision).
(define (dw-write str #!optional (dw (current-dw)))
  (file-write dw str)
  (let* ((len (number-of-bytes str))
         (read (dw-read len)))
    (unless (equal? read str)
      (error "expected echo " (wrt str) ", got " (wrt read)))))

;; ==================== flow ====================

(define (dw-reset)
  (dw-write (bytevector #x07))
  (dw-break-expect))

;; (begin (dw-step dw) (PC dw))
(define (dw-step)
  (dw-write (bytevector #x60 #x31))
  (dw-break-expect))

;; disable debugWirte on target (enables ISP)
(define (dw-disable!)
  (dw-write (bytevector #x06)))

;; ==================== registers ====================

(define (dw-registers-read start length)
  (set! (PC) start)
  (set! (BP) (+ start length))
  (dw-write (bytevector #x66 #xC2 #x01 #x20))
  (dw-read length))

(define (dw-registers-write start regs)
  (set! (PC) start)
  (set! (BP) (+ start (number-of-bytes regs)))
  (dw-write (bytevector #x66 #xC2 #x05 #x20 regs)))

(define (dw-signature) ;; don't really know what this is
  (dw-write (bytevector #xF3))
  (bytes->u16 (dw-read 2)))

(define PC ;; program counter (aka instruction pointer)
  (getter-with-setter
   (lambda ()
     (dw-write (bytevector #xF0))
     (bytes->u16be (dw-read 2)))
   (lambda (value) (dw-write (bytevector #xD0 (u16be->bytes value))))))

(define BP ;; breakpoint (hw)
  (getter-with-setter
   (lambda ()
     (dw-write (bytevector #xF1))
     (bytes->u16be (dw-read 2)))
   (lambda (value) (dw-write (bytevector #xD1 (u16be->bytes value))))))

;; (set! (IR dw) "ab")
(define IR ;; instruction register (IR dw)
  (getter-with-setter
   (lambda ()
     (dw-write (bytevector #xF2))
     (dw-read 2))
   (lambda (value)
     (let ((value (cond ((string? value)
                         (unless (equal? 2 (number-of-bytes value))
                           (error "IR: expecting 2-byte instruction" (wrt value)))
                         value)
                        ((blob? value)
                         (unless (equal? 2 (number-of-bytes value))
                           (error "IR: expecting 2-byte instruction" (wrt value)))
                         value)
                        ((number? value) (u16be->bytes value))
                        (else (error "don't know how to IR this" value)))))
       (dw-write (bytevector #xD2 value))))))

(define (dw-exec word)
  (set! (IR) word)
  (dw-write (bytevector #x23)))

;; ======================================== common registers

(define r
  (getter-with-setter
   (lambda (register)       (bytes->u8 (dw-registers-read register 1)))
   (lambda (register value) (dw-registers-write register (bytevector value)))))

(define r24
  (getter-with-setter
   (lambda ()       (dw-registers-read 24 1))
   (lambda (value) (dw-registers-write 24 (bytevector value)))))

;; OBS: these are little-endian

(define X ;; X-register
  (getter-with-setter
   (lambda () (bytes->u16le (dw-registers-read 26 2)))
   (lambda (value) (dw-registers-write 26 (u16le->bytes value)))))

(define Y ;; Y-register
  (getter-with-setter
   (lambda () (bytes->u16le (dw-registers-read 28 2)))
   (lambda (value) (dw-registers-write 28 (u16le->bytes value)))))

;; (set! (Z dw) 257)
(define Z ;; Z-register
  (getter-with-setter
   (lambda () (bytes->u16le (dw-registers-read 30 2)))
   (lambda ( value) (dw-registers-write 30 (u16le->bytes value)))))

;; ======================================== SRAM
(define SP
  (getter-with-setter
   (lambda () (bytes->u16le (dw-sram-read #x5D 2)))
   (lambda (v) (dw-sram-write #x5D (u16le->bytes v)))))


;; u8 hi(int w) {return (w>>8)&0xff;}
;; u8 lo(int w) {return (w   )&0xff;}
;; void DwSetPC(u16 pc) {DwSend(Bytes(0xD0, hi(pc)|AddrFlag(), lo(pc)));}
;; void DwSetBP(u16 bp) {DwSend(Bytes(0xD1, hi(bp)|AddrFlag(), lo(bp)));}

(define (dw-sram-read start len)
  (unless (<= len 128) (error "dw-sram-read: len must be <=128" len))
  (set! (Z) start)
  (set! (PC) 0)
  (set! (BP) (* 2 len))
  (dw-write (bytevector #x66 #xC2 #x00 #x20))
  (dw-read len))

(define (dw-sram-write start bytes)
  (let ((len (number-of-bytes bytes)))
    (unless (<= len 128) (error "dw-sram-read: len must be <=128" len))
    (set! (Z) start)
    (set! (PC) 1)
    (set! (BP) (+ 1 (* 2 len)))
    (dw-write (bytevector #x66 #xC2 #x04 #x20 bytes))))

;; ====================

(define DDRB
  (getter-with-setter
   (lambda () (bytes->u8 (dw-sram-read #x37 1)))
   (lambda (v) (dw-sram-write #x37 (u8->bytes v)))))

(define PORTB
  (getter-with-setter
   (lambda () (bytes->u8 (dw-sram-read #x38 1)))
   (lambda (v) (dw-sram-write #x38 (u8->bytes v)))))

;; ==================== flash ====================

(define (dw-flash-read start len)
  (set! (Z) start)
  (set! (PC) 0)
  (set! (BP) (* 2 len))
  (dw-write (bytevector #x66 #xC2 #x02 #x20))
  (dw-read len))

(define (dw-flash-page-erase start)
  (define PGERS (arithmetic-shift 1 1))
  (define SPMEN (arithmetic-shift 1 0)) ;; self-program memory enable (?)
  (define out37r24 "\xBF\x87")
  (define spm      "\x95\xE8")
  
  (set! (Z) start)
  ;;(set! (r24 dw) (bitwise-ior PGERS SPMEN))
  ;;(dw-exec dw out37r24)
  (set! (SPMCSR dw) (bitwise-ior PGERS SPMEN)) ;;<-- You wish!
  (dw-exec dw spm))

;; 19.9.1: If only SPMEN is written, the following SPM instruction
;; will store the value in R1:R0 in the temporary page buffer
;; addressed by the Z-pointer.
(define (dw-flash-write start data)
  (define SPMEN (arithmetic-shift 1 0)) ;; self-program memory enable (?)
  ;; (define out37r24 "\xBF\x87")
  ;; (define spm      "\x95\xE8")
  
  
  (set! (Z) start)
  (set! (SPMCSR) (bitwise-ior SPMEN))
  ;;(dw-exec dw out37r24)
  (dw-exec (spm)))

(begin
  (dw-sram-write 500 "_ELLO")
  (set! (Z) 500)
  (dw-exec (ldi 24 (char->integer #\h)))
  (dw-exec (stZ 24))
  (dw-sram-read 500 5))

(r 1)
(begin
  (print "SP = " (SP))
  (dw-exec (pop 0))
  (dw-exec (pop 1))
  (print "SP = " (SP))
  (print "r01: " (list (r 0) (r 1))))

(dw-exec (eor 24 24))
(begin ;; yey!
  (dw-exec (ldi 24 66))
  (dw-registers-read 24 1))

(string->blob (dw-flash-read 50 170))

