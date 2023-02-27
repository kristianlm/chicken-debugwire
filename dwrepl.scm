(import chicken.io chicken.file.posix
        chicken.blob
        chicken.bitwise
        chicken.string
        chicken.memory.representation
        chicken.port
        chicken.time
        srfi-1 srfi-18)

(load "tty.so")

(define current-dw (make-parameter (file-open "/dev/ttyUSB0" open/rdwr)))
(define (baudrate) (quotient 4000000 64))

;; get the "pong" reply from target which is issued immediately after
;; break signals.
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

;; issue a break (pulling debugWire/RESET low for a few hundred µs)
;; this should suspend the target CPU and give us the famous #x55
;; reply.
(define (dw-break!)
  (tty-break (current-dw) (quotient 20000000 (baudrate)))
  (dw-break-expect))

(tty-setup (current-dw) (baudrate))
(dw-break!)

(include "avr-asm.scm")

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


(define dw-write-buffer "")

(define (dw-read* len dw seconds timeout)
  (let ((eot (+ (current-seconds) seconds)))
    (let loop ((len len)
               (result ""))
      (if (> len 0)
          (if (> (current-seconds) eot)
              (timeout result)
              (apply (lambda (chunk bytes)
                       (let ((chunk (substring chunk 0 bytes)))
                         (unless (= bytes len)
                           (warning (conc "*** waiting"
                                          " (want " len " bytes,"
                                          " " (- eot (current-seconds)) "s)"))
                           (thread-sleep! 0.1))
                         (loop (- len bytes) (conc result chunk))))
                     (file-read dw len)))
          (begin
            (print "<< " (wrt result))
            result)))))

(define (dw-write* #!optional (dw (current-dw)))

  ;; try to recover from unread responses
  (let loop ((total 0))
    (let ((readout (cadr (file-read (current-dw) (if (= 0 total) 1 1024)))))
      (if (zero? readout)
          (unless (= 0 total)
            (print "dw-write* warning: nonempty buffer when writing new command (" total " bytes)"))
          (loop (+ total readout)))))

  (unless (zero? (number-of-bytes dw-write-buffer))
    (let ((str dw-write-buffer))
      (set! dw-write-buffer "")
      (print ">> " (wrt str))
      (file-write dw str)
      (let* ((len (number-of-bytes str))
             (read (dw-read* len dw 2 (lambda (str) (error "dw-write*: timeout")))))
        (unless (equal? read str)
          (error (conc "expected echo " (wrt str) ", got " (wrt read))))))))

(define (dw-write str #!optional (dw current-dw))
  (set! dw-write-buffer (conc dw-write-buffer str)))

;; since RX and TX is shared, all writes are echoed back to us (unless
;; there is a collision).
(define (dw-read len #!key
                 (dw (current-dw))
                 (seconds 2)
                 (timeout
                  (lambda (str)
                    (error (conc "dw-read: reached timeout (" seconds "s)") str))))
  (dw-write* dw) ;; flush out buffer to UART
  (dw-read* len dw seconds timeout))

;; ==================== flow ====================

(define (dw-reset)
  (dw-write (bytevector #x07))
  (dw-break-expect))

;; (begin (dw-step) (PC))
(define (dw-step)
  (dw-write (bytevector
             #xD1 #x00 #x01 ;; BP
             #x60 #x31))
  (dw-break-expect))

(define (dw-continue/ret) (dw-write (bytevector #x63 #x30)) (dw-break-expect) )
(define (dw-continue)     (dw-write (bytevector #x60 #x30)) #;(dw-break-expect) )

;; disable debugWirte on target (enables ISP)
(define (dw-disable!)
  (dw-write (bytevector #x06)))

;; ==================== registers ====================

(define (dw-registers-read start length)
  ;;(set! (PC) start)
  ;;(set! (BP) (+ start length))
  (dw-write (bytevector
             #xD0 (u16be->bytes start) ;; PC
             #xD1 (u16be->bytes (+ start length)) ;; BP
             #x66 #xC2 #x01 #x20))
  (dw-read length))

(define (dw-registers-write start regs)
  ;; (set! (PC) start)
  ;; (set! (BP) (+ start (number-of-bytes regs)))
  (dw-write (bytevector
             #xD0 (u16be->bytes start) ;; PC
             #xD1 (u16be->bytes (+ start (number-of-bytes regs))) ;; BP
             #x66 #xC2 #x05 #x20 regs)))

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

;; on flash, values are stored little-endian. when transferring over
;; debugWire, however, the instruction register seems to be
;; big-endian. hence the noise below.
(define (->ir value)
  (cond ((number? value) (u16be->bytes value))
        ((blob? value) (->ir (blob->string value)))
        ((string? value)
         (if (equal? 2 (number-of-bytes value))
             (->ir (bytes->u16le value)) ;; <--  ,-- yes, really!
             (error "IR: expecting 2-byte instruction" (wrt value))))
        (else (error "don't know how to IR this" value))))

(define IR ;; instruction register (IR dw)
  (getter-with-setter
   (lambda ()
     (dw-write (bytevector #xF2))
     (bytes->u16be (dw-read 2)))
   (lambda (value)
     (let ((value (->ir value)))
       (dw-write (bytevector #xD2 value))))))

(define (dw-exec word)
  ;; (set! (IR) word)
  (dw-write (bytevector #xD2 (->ir word) #x23)))

;; ======================================== common registers

(define r
  (getter-with-setter
   (lambda (register)       (bytes->u8 (dw-registers-read register 1)))
   (lambda (register value) (dw-registers-write register (bytevector value)))))

(define r24
  (getter-with-setter
   (lambda ()      (dw-registers-read  24 1))
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

;; (dw-sram-read SPMCSR 1)

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

;; the hardware only supports erasing 1 page of flash at a time.
(define (dw-flash-erase/page start)
  (define CTPB  (arithmetic-shift 1 4)) ;; Clear Temporary Page Buffer
  (define PGERS (arithmetic-shift 1 1))
  (define SPMEN (arithmetic-shift 1 0)) ;; Store Program Memory Enable
  (define SPMCSR #x57) ;; Store Prgram Memory Control and Status Register

  (set! (r 29) (bitwise-ior PGERS SPMEN))
  (set! (Z) start)
  (dw-exec (out (adr->io SPMCSR) 29))
  (set! (PC) 0)
  (dw-exec (spm)))

;; (dw-flash-page-erase #x140)

;; 19.9.1: If only SPMEN is written, the following SPM instruction
;; will store the value in R1:R0 in the temporary page buffer
;; addressed by the Z-pointer.
;;
;; TODO: check data size against page bounds at start
(define (dw-flash-write/page start data)
  (define CTPB  (arithmetic-shift 1 4)) ;; Clear Temporary Page Buffer
  (define PGWRT (arithmetic-shift 1 2)) ;; Page Write
  (define SPMEN (arithmetic-shift 1 0)) ;; Store Program Memory Enable
  (define SPMCSR #x57) ;; Store Prgram Memory Control and Status Register

  (set! data (if (blob? data)
                 (blob->u8vector/shared data)
                 data))

  (define len (cond ((u8vector? data) (u8vector-length data))
                    (else (number-of-bytes data))))

  ;; must upload data 16-bits at a time. #xFF should not affect the
  ;; flash, so we can use that for the potentially missing byte.
  (define (byte n)
    (cond ((string? data)   (if (< n len) (char->integer (string-ref data n)) #xFF))
          ((u8vector? data) (if (< n len) (u8vector-ref data n) #xFF))
          (else (error "don'w know how to handle blob type " (wrt data)))))

  ;; clear temporary page buffer sequence
  (set! (r 24) (bitwise-ior CTPB SPMEN))
  (set! (Z) start)
  (dw-exec (out (adr->io SPMCSR) 24))
  (set! (PC) 0)
  (dw-exec (spm))

  ;; fill page buffer sequence (1 word / 2 bytes)
  ;; temporary_flash_buffer[Z] = r0:r1
  (set! (r 29) (bitwise-ior SPMEN)) ;; <-- r29 reused repeatedly below
  (set! (Z) start)                  ;; <-- lsb ignored

  (let loop ((n 0))
    ;;                                 ,-----r0-----.  ,-----r1----.
    (dw-registers-write 0 (bytevector (byte (+ n 0))  (byte (+ n 1))))
    (dw-exec (out (adr->io SPMCSR) 29)) ;; next (spm) writes r0:r1 to TPB
    ;; (set! (PC) 0) ;; <-- TODO move PC to RWW section or something
    (dw-exec (spm))
    (dw-exec (adiw 30 2)) ;; Z := Z + 2
    (when (< n len)
      (loop (+ n 2))))

  ;; write temporary_flash_buffer to flash
  (set! (Z) start)
  (set! (r 29) (bitwise-ior PGWRT SPMEN))
  (dw-exec (out (adr->io SPMCSR) 29))
  (set! (PC) 0)
  (dw-exec (spm))
  ;; TODO: wait for SPM done

  (thread-sleep! .2))




(dw-flash-write/page
 #x140 (bytevector (ldi 17 101) ;; e
                   (nop)
                   (nop)
                   (nop)
                   (nop)
                   (nop)
                   (ldi 18 102) ;; f
                   (nop)
                   (nop)
                   (nop)
                   (nop)
                   (nop)
                   (ret)
                   (rjmp -8)
                   (nop)
                   (ldi 17 103) ;; g
                   (ldi 18 104) ;; h
                   (break)))

;; (dw-flash-erase/page #x140)
(string->blob (dw-flash-read #x140 64))

(SP)
(dw-exec (pop 16))
(set! (r 16) -1)
(dw-exec (push 16))
(r 16)


(begin
  (define (status!)
    (let ((pc (PC)))
      (print "status: pc=" pc (list #:r16:19 (wrt (dw-registers-read 16 4))))))

  (dw-registers-write 16 "    ")
  (status!)
  (set! (PC) (/ #x140 2))
  (dw-continue/ret)
  (thread-sleep! 0.5)
  (dw-break!)
  (status!)

  (thread-sleep! 0.5))


;; DO NOT EVAL THIS:
;; ITS VERY STRANGE, THE FUSES SEEM TO CHANGE!
;; the SPI interface becomes active :-(
;; (dw-flash-write (* 64 12) "h")

(begin
  (dw-sram-write 500 "_ELLO")
  (set! (Z) 500)
  (dw-exec (ldi 24 (char->integer #\h)))
  (dw-exec (stZ 24))
  (dw-sram-read 500 5))

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

(string->list (dw-sram-read (SP) 2))
