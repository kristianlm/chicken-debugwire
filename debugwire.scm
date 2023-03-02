(import chicken.io chicken.file.posix
        chicken.blob
        chicken.bitwise
        chicken.string
        chicken.memory.representation
        chicken.port
        chicken.time
        (only srfi-1 append-map)
        srfi-4
        srfi-18
        (only avr.asm adr->io spm out adiw))

(include "tty.scm")
(include "reader.scm")

(define current-dw (make-parameter #f))
(define baudrate   (make-parameter #f))

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

;; issue a break (pulling debugWIRE/RESET low for a few hundred µs)
;; this should suspend the target CPU and give us the famous #x55
;; reply.
(define (dw-break! #!optional (dw (current-dw)))
  (dw-echo-flush! dw)

  (apply
   (lambda (chunk bytes)
     (unless (zero? bytes)
       (warning "lingering bytes in receive buffer:" (wrt (substring chunk 0 bytes)))))
   (file-read dw 1024 ))

  (tty-break dw (quotient 20000000 (baudrate)))
  (dw-break-expect))

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

;;; ============================== UART ==============================
;;;
;;; all UART reads cause significant delays (milliseconds, it seems)
;;; with our current tty settings. that is, in my tests, I'm seeing
;;; that every file-read on dw lasts at least 15ms. I'm guessing
;;; because c_cc[VTIME] > 0 or something. so we minimize number of
;;; file-read calls as much as possible. writing does not seem to
;;; cause delays, so we write immediately.
;;;
;;; since RX and TX is shared, all writes are echoed back to us
;;; (unless there is a collision). we need to read back our echos, but
;;; we don't want to do it immediately after writing because that
;;; increases the number file-reads. instead, we keep track of the
;;; expected echos and read and discard before we do a real UART read.
;;;
;;; This complicates things but makes it substantially faster.
(define dw-echo "")

;; raw read from UART. causes delay as we're waiting to see if more
;; data will arrive on our.
(define (dw-read* len #!key
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
                       (let ((chunk (substring chunk 0 bytes)))
                         (unless (= bytes len)
                           ;; (warning (conc "*** waiting" " (want " len " bytes," " " (- eot (current-seconds)) "s)"))
                           (thread-yield!))
                         (loop (- len bytes) (conc result chunk))))
                     (file-read dw len)))
          result))))

;; read expected echo back. this is a bit slow, see above.
(define (dw-echo-flush! dw)
  (let ((str dw-echo))
    (set! dw-echo "") ;; clear first, in case of error
    (let* ((len (number-of-bytes str))
           (read (dw-read* len #:dw dw #:seconds 2)))
      (unless (equal? read str)
        (error (conc "expected echo " (wrt str) ", got " (wrt read)))))))

(define (dw-read len #!key
                 (dw (current-dw))
                 (seconds 2)
                 (timeout
                  (lambda (str)
                    (error (conc "dw-read: reached timeout (" seconds "s)") str))))
  (dw-echo-flush! dw)
  (dw-read* len #:dw dw #:seconds seconds  #:timeout timeout))

(define (dw-write str #!optional (dw (current-dw)))
  (let ((len (number-of-bytes str)))
    (set! dw-echo (conc dw-echo str))
    (unless (= len (file-write dw str))
      (error (conc "dw-write: could not write " len " bytes")))))

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
(define (dw-continue #!optional (dw (current-dw)))
  (dw-write (bytevector #x60 #x30)) ;; resume normal execution
  (dw-echo-flush! dw))

(define (dw-continue/bp bp #!key (dw (current-dw)))
  (set! (BP) (/ bp 2))
  (dw-write (bytevector #x61 #x30))
  (dw-echo-flush! dw)
  (dw-break-expect dw))

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

;; https://github.com/dcwbrown/dwire-debug/blob/084c3332522fc781dd3e2316335d8539e9064338/src/dwire/Connection.c#L48
(define (dw-signature)
  (dw-write (bytevector #xF3))
  (bytes->u16be (dw-read 2)))

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
;; debugWIRE, however, the instruction register seems to be
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
   (lambda (value) (dw-registers-write 30 (u16le->bytes value)))))

;; ======================================== SRAM


;; u8 hi(int w) {return (w>>8)&0xff;}
;; u8 lo(int w) {return (w   )&0xff;}
;; void DwSetPC(u16 pc) {DwSend(Bytes(0xD0, hi(pc)|AddrFlag(), lo(pc)));}
;; void DwSetBP(u16 bp) {DwSend(Bytes(0xD1, hi(bp)|AddrFlag(), lo(bp)));}

;; unsafe. any read across DWDR, the internal debugwire register,
;; yields in no response. make sure to avoid doing this.
(define (dw-sram-read* start len)
  ;;(unless (<= len 128) (error "dw-sram-read: len must be <=128" len))
  (set! (Z) start)
  (set! (PC) 0)
  (set! (BP) (* 2 len))
  (dw-write (bytevector #x66 #xC2 #x00 #x20))
  (dw-read len))

(define DWDR #x42) ;; TODO: for attiny85 only
(define dw-sram-read
  (reader-mask dw-sram-read* DWDR 1 (lambda (s l) "\x00") (flip conc) ""))

;; having masked away the unsafe DWDR, we can now:
;; (dw-sram-read 0 512)

(define (dw-sram-write* start bytes)
  (let ((len (number-of-bytes bytes)))
    ;;(unless (<= len 128) (error "dw-sram-read: len must be <=128" len))
    (set! (Z) start)
    (set! (PC) 1)
    (set! (BP) (+ 1 (* 2 len)))
    (dw-write (bytevector #x66 #xC2 #x04 #x20 bytes))))

(define dw-sram-write (writer-chunkify dw-sram-write* 128))
(define SP
  (getter-with-setter
   (lambda () (bytes->u16le (dw-sram-read #x5D 2)))
   (lambda (v) (dw-sram-write #x5D (u16le->bytes v)))))

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

;; unsafe: keep len <= 64 (for some devices it seems)
(define (dw-flash-read* start len)
  (set! (Z) start)
  (set! (PC) 0)
  (set! (BP) (* 2 len))
  (dw-write (bytevector #x66 #xC2 #x02 #x20))
  (dw-read len))

;; read flash chunks, because
;; https://github.com/dcwbrown/dwire-debug/blob/9b98597ce53fa49637909a16d1c63b8f314c1ce1/src/commands/NonVolatile.c#L101. I
;; don't think this applies to non-bootsector chips, but I haven't
;; been able to read more than #x800 bytes of flash from my attiny85.
;;
;; (print (string->blob (dw-flash-read 0 #x2000)))
(define dw-flash-read
  (reader-chunkify dw-flash-read* 64 (flip conc) ""))

;; The hardware only supports erasing 1 page of flash at a time.
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

(define (dw-open! path baudrate*)
  (baudrate baudrate*)
  (current-dw (file-open path open/rdwr))
  (tty-setup (current-dw)))
