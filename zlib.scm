;;; Guile-zlib --- GNU Guile bindings of zlib
;;; Copyright © 2016, 2017, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; This file is part of Guile-zlib.
;;;
;;; Guile-zlib is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-zlib is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-zlib.  If not, see <http://www.gnu.org/licenses/>.

(define-module (zlib)
  #:use-module (zlib config)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:export (make-gzip-input-port
            make-gzip-output-port
            call-with-gzip-input-port
            call-with-gzip-output-port
            %default-buffer-size
            %default-compression-level

            compress
            uncompress
            adler32
            crc32

            make-zlib-input-port
            make-zlib-output-port
            call-with-zlib-input-port
            call-with-zlib-output-port))

;;; Commentary:
;;;
;;; This file is extracted from Guix and originally written by Ludovic Courtès.
;;; Bindings to the gzip-related part of zlib's API.  The main limitation of
;;; this API is that it requires a file descriptor as the source or sink.
;;;
;;; Code:

(define %zlib
  (delay (dynamic-link %libz)))

(define (zlib-procedure ret name parameters)
  "Return a procedure corresponding to C function NAME in libz, or #f if
either zlib or the function could not be found."
  (match (false-if-exception (dynamic-func name (force %zlib)))
    ((? pointer? ptr)
     (pointer->procedure ret ptr parameters))
    (#f
     #f)))

(define-wrapped-pointer-type <gzip-file>
  ;; Scheme counterpart of the 'gzFile' opaque type.
  gzip-file?
  pointer->gzip-file
  gzip-file->pointer
  (lambda (obj port)
    (format port "#<gzip-file ~a>"
            (number->string (object-address obj) 16))))

(define gzerror
  (let ((proc (zlib-procedure '* "gzerror" '(* *))))
    (lambda (gzfile)
      (let* ((errnum* (make-bytevector (sizeof int)))
             (ptr     (proc (gzip-file->pointer gzfile)
                            (bytevector->pointer errnum*))))
        (values (bytevector-sint-ref errnum* 0
                                     (native-endianness) (sizeof int))
                (pointer->string ptr))))))

(define gzdopen
  (let ((proc (zlib-procedure '* "gzdopen" (list int '*))))
    (lambda (fd mode)
      "Open file descriptor FD as a gzip stream with the given MODE.  MODE must
be a string denoting the how FD is to be opened, such as \"r\" for reading or
\"w9\" for writing data compressed at level 9 to FD.  Calling 'gzclose' also
closes FD."
      (let ((result (proc fd (string->pointer mode))))
        (if (null-pointer? result)
            (throw 'zlib-error 'gzdopen)
            (pointer->gzip-file result))))))

(define gzread!
  (let ((proc (zlib-procedure int "gzread" (list '* '* unsigned-int))))
    (lambda* (gzfile bv #:optional (start 0) (count (bytevector-length bv)))
      "Read up to COUNT bytes from GZFILE into BV at offset START.  Return the
number of uncompressed bytes actually read; it is zero if COUNT is zero or if
the end-of-stream has been reached."
      (let ((ret (proc (gzip-file->pointer gzfile)
                       (bytevector->pointer bv start)
                       count)))
        (if (< ret 0)
            (throw 'zlib-error 'gzread! ret)
            ret)))))

(define gzwrite
  (let ((proc (zlib-procedure int "gzwrite" (list '* '* unsigned-int))))
    (lambda* (gzfile bv #:optional (start 0) (count (bytevector-length bv)))
      "Write up to COUNT bytes from BV at offset START into GZFILE.  Return
the number of uncompressed bytes written, a strictly positive integer."
      (let ((ret (proc (gzip-file->pointer gzfile)
                       (bytevector->pointer bv start)
                       count)))
        (if (<= ret 0)
            (throw 'zlib-error 'gzwrite ret)
            ret)))))

(define gzbuffer!
  (let ((proc (zlib-procedure int "gzbuffer" (list '* unsigned-int))))
    (lambda (gzfile size)
      "Change the internal buffer size of GZFILE to SIZE bytes."
      (let ((ret (proc (gzip-file->pointer gzfile) size)))
        (unless (zero? ret)
          (throw 'zlib-error 'gzbuffer! ret))))))

(define gzeof?
  (let ((proc (zlib-procedure int "gzeof" '(*))))
    (lambda (gzfile)
      "Return true if the end-of-file has been reached on GZFILE."
      (not (zero? (proc (gzip-file->pointer gzfile)))))))

(define gzclose
  (let ((proc (zlib-procedure int "gzclose" '(*))))
    (lambda (gzfile)
      "Close GZFILE."
      (let ((ret (proc (gzip-file->pointer gzfile))))
        (unless (zero? ret)
          (throw 'zlib-error 'gzclose ret (gzerror gzfile)))))))



;;;
;;; Port interface.
;;;

(define %default-buffer-size
  ;; Default buffer size, as documented in <zlib.h>.
  8192)

(define %default-compression-level
  ;; Z_DEFAULT_COMPRESSION.
  -1)

(define* (make-gzip-input-port port #:key (buffer-size %default-buffer-size))
  "Return an input port that decompresses data read from PORT, a file port.
PORT is automatically closed when the resulting port is closed.  BUFFER-SIZE
is the size in bytes of the internal buffer, 8 KiB by default; using a larger
buffer increases decompression speed.  An error is thrown if PORT contains
buffered input, which would be lost (and is lost anyway)."
  (define gzfile
    (match (drain-input port)
      (""                                         ;PORT's buffer is empty
       ;; 'gzclose' will eventually close the file descriptor beneath PORT.
       ;; 'close-port' on PORT would get EBADF if 'gzclose' already closed it,
       ;; so that's no good; revealed ports are no good either because they
       ;; leak (see <https://bugs.gnu.org/28784>); calling 'close-port' after
       ;; 'gzclose' doesn't work either because it leads to a race condition
       ;; (see <https://bugs.gnu.org/29335>).  So we dup and close PORT right
       ;; away.
       (gzdopen (dup (fileno port)) "r"))
      (_
       ;; This is unrecoverable but it's better than having the buffered input
       ;; be lost, leading to unclear end-of-file or corrupt-data errors down
       ;; the path.
       (throw 'zlib-error 'make-gzip-input-port
              "port contains buffered input" port))))

  (define (read! bv start count)
    (gzread! gzfile bv start count))

  (unless (= buffer-size %default-buffer-size)
    (gzbuffer! gzfile buffer-size))

  (close-port port)                               ;we no longer need it
  (make-custom-binary-input-port "gzip-input" read! #f #f
                                 (lambda ()
                                   (gzclose gzfile))))

(define* (make-gzip-output-port port
                                #:key
                                (level %default-compression-level)
                                (buffer-size %default-buffer-size))
  "Return an output port that compresses data at the given LEVEL, using PORT,
a file port, as its sink.  PORT must be a file port; it is automatically
closed when the resulting port is closed."
  (define gzfile
    (begin
      (force-output port)                         ;empty PORT's buffer
      (gzdopen (dup (fileno port))
               (string-append "w" (number->string level)))))

  (define (write! bv start count)
    (gzwrite gzfile bv start count))

  (unless (= buffer-size %default-buffer-size)
    (gzbuffer! gzfile buffer-size))

  (close-port port)
  (make-custom-binary-output-port "gzip-output" write! #f #f
                                  (lambda ()
                                    (gzclose gzfile))))

(define* (call-with-gzip-input-port port proc
                                    #:key (buffer-size %default-buffer-size))
  "Call PROC with a port that wraps PORT and decompresses data read from it.
PORT must be a file port; it is closed upon completion.  The gzip internal
buffer size is set to BUFFER-SIZE bytes.

See 'call-with-zlib-input-port' for a slightly slower variant that does not
require PORT to be a file port."
  (let ((gzip (make-gzip-input-port port #:buffer-size buffer-size)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc gzip))
      (lambda ()
        (close-port gzip)))))

(define* (call-with-gzip-output-port port proc
                                     #:key
                                     (level %default-compression-level)
                                     (buffer-size %default-buffer-size))
  "Call PROC with an output port that wraps PORT and compresses data.  PORT
must be a file port; it is closed upon completion.  The gzip internal buffer
size is set to BUFFER-SIZE bytes.

See 'call-with-zlib-output-port' for a slightly slower variant that does not
require PORT to be a file port."
  (let ((gzip (make-gzip-output-port port
                                     #:level level
                                     #:buffer-size buffer-size)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc gzip))
      (lambda ()
        (close-port gzip)))))


;;;
;;; Raw operations, originally from davexunit's guile-zlib
;;; https://github.com/davexunit/guile-zlib
;;; fd28b7515efc4af6faf55854993cb0c8bed1f8c5
;;;

;;
;; ZEXTERN int ZEXPORT uncompress OF((Bytef *dest, uLongf *destLen,
;;                                    const Bytef *source, uLong sourceLen));
;;
;; Decompresses the source buffer into the destination
;; buffer. sourceLen is the byte length of the source buffer. Upon
;; entry, destLen is the total size of the destination buffer, which
;; must be large enough to hold the entire uncompressed data. (The
;; size of the uncompressed data must have been saved previously by
;; the compressor and transmitted to the decompressor by some
;; mechanism outside the scope of this compression library.) Upon
;; exit, destLen is the actual size of the compressed buffer.
;;
;; uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
;; enough memory, Z_BUF_ERROR if there was not enough room in the
;; output buffer, or Z_DATA_ERROR if the input data was corrupted or
;; incomplete. In the case where there is not enough room,
;; uncompress() will fill the output buffer with the uncompressed data
;; up to that point.
(define %uncompress
  (zlib-procedure int "uncompress" (list '* '* '* unsigned-long)))

;;
;; ZEXTERN int ZEXPORT compress OF((Bytef *dest, uLongf *destLen,
;;                                  const Bytef *source, uLong sourceLen));
;;
;; Compresses the source buffer into the destination buffer. sourceLen
;; is the byte length of the source buffer. Upon entry, destLen is the
;; total size of the destination buffer, which must be at least the
;; value returned by compressBound(sourceLen). Upon exit, destLen is
;; the actual size of the compressed buffer.
;;
;; compress returns Z_OK if success, Z_MEM_ERROR if there was not
;; enough memory, Z_BUF_ERROR if there was not enough room in the
;; output buffer.
(define %compress
  (zlib-procedure int "compress" (list '* '* '* unsigned-long)))

;;
;; ZEXTERN uLong ZEXPORT compressBound OF((uLong sourceLen));
;;
;; compressBound() returns an upper bound on the compressed size after
;; compress() or compress2() on sourceLen bytes. It would be used
;; before a compress() or compress2() call to allocate the destination
;; buffer.
(define %compress-bound
  (zlib-procedure unsigned-long "compressBound" (list unsigned-long)))

;; Update a running Adler-32 checksum with the bytes buf[0..len-1] and
;; return the updated checksum.  If buf is Z_NULL, this function returns the
;; required initial value for the checksum.
;;
;;   An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
;; much faster.
;;
;; Usage example:
;;
;;   uLong adler = adler32(0L, Z_NULL, 0);
;;
;;   while (read_buffer(buffer, length) != EOF) {
;;     adler = adler32(adler, buffer, length);
;;   }
;;   if (adler != original_adler) error();
(define %adler32
  (zlib-procedure unsigned-long "adler32" (list unsigned-long '* unsigned-int)))

;; Update a running CRC-32 with the bytes buf[0..len-1] and return the
;; updated CRC-32.  If buf is Z_NULL, this function returns the required
;; initial value for the crc.  Pre- and post-conditioning (one's complement) is
;; performed within this function so it shouldn't be done by the application.
;;
;; Usage example:
;;
;;   uLong crc = crc32(0L, Z_NULL, 0);
;;
;;   while (read_buffer(buffer, length) != EOF) {
;;     crc = crc32(crc, buffer, length);
;;   }
;;   if (crc != original_crc) error();
(define %crc32
  (zlib-procedure unsigned-long "crc32" (list unsigned-long '* unsigned-int)))

;; There is a bit of guesswork involved when creating the bytevectors
;; to store compressed/uncompressed data in. This procedure provides a
;; convenient way to copy the portion of a bytevector that was
;; actually used.
(define (bytevector-copy-region bv start end)
  (let* ((length (- end start))
         (new-bv (make-bytevector length)))
    (bytevector-copy! bv start new-bv 0 length)
    new-bv))

;; uncompress/compress take a bytevector that zlib writes the size of
;; the returned data to. This procedure saves me a few keystrokes when
;; fetching that value.
(define (buffer-length bv)
  (bytevector-u64-native-ref bv 0))

(define (uncompress bv)
  "Uncompresses bytevector and returns a bytevector containing
the uncompressed data."
  (define (try-uncompress length)
    (let* ((dest (make-bytevector (* (sizeof uint8) length)))
           (dest-length (make-bytevector (sizeof unsigned-long))))
      (bytevector-u64-native-set! dest-length 0 length)
      (values (%uncompress (bytevector->pointer dest)
                   (bytevector->pointer dest-length)
                   (bytevector->pointer bv)
                   length)
              (bytevector-copy-region dest 0 (buffer-length dest-length)))))

  ;; We don't know how much space we need to store the uncompressed
  ;; data. So, we make an initial guess and keep increasing buffer
  ;; size until it works.
  (define (step-buffer-length length)
    (inexact->exact (round (* length 1.5))))

  (let try-again ((tries 1)
                  (length (step-buffer-length (bytevector-length bv))))
    ;; Bail after so many failed attempts. This shouldn't happen, but
    ;; I don't like the idea of a potentially unbounded loop that
    ;; keeps allocating larger and larger chunks of memory.
    (if (> tries 10)
        (throw 'zlib-uncompress-error)
        (receive (ret-code uncompressed-data)
            (try-uncompress length)
          ;; return code -5 means that destination buffer was too small.
          ;; return code  0 means everything went OK.
          (cond ((= ret-code -5)
                 (try-again (1+ tries) (step-buffer-length length)))
                ((= ret-code 0)
                 uncompressed-data)
                (else
                 (throw 'zlib-uncompress-error)))))))

(define (compress bv)
  "Compresses bytevector and returns a bytevector containing the compressed data."
  (let* ((bv-length      (bytevector-length bv))
         (dest-length    (%compress-bound bv-length))
         (dest-bv        (make-bytevector dest-length))
         (dest-length-bv (make-bytevector (sizeof unsigned-long)))
         (ret-code       0))
    (bytevector-u64-native-set! dest-length-bv 0 dest-length)
    (set! ret-code
          (%compress (bytevector->pointer dest-bv)
                     (bytevector->pointer dest-length-bv)
                     (bytevector->pointer bv)
                     bv-length))
    (if (= ret-code 0)
        (bytevector-copy-region dest-bv 0
                                (buffer-length dest-length-bv))
        (throw 'zlib-compress-error))))

(define %default-adler32 (%adler32 0 %null-pointer 0))
(define %default-crc32   (%crc32   0 %null-pointer 0))

(define* (adler32 bv #:optional (value %default-adler32))
  "Computes adler32 checksum with optional starting value."
  (%adler32 value (bytevector->pointer bv) (bytevector-length bv)))

(define* (crc32 bv #:optional (value %default-crc32))
  "Computes crc32 checksum with optional starting value."
  (%crc32 value (bytevector->pointer bv) (bytevector-length bv)))


;;;
;;; Low-level zlib stream API.
;;;

(define %zlib-version
  ;; Library version that we're targeting.
  "1.2.11")

;; struct zstream
(define %stream-struct
  (list '*                                        ;next_in
        unsigned-int                              ;avail_in
        unsigned-long                             ;total_in

        '*                                        ;next_out
        unsigned-int                              ;avail_out
        unsigned-long                             ;total_out

        '*                                        ;msg
        '*                                        ;state

        '*                                        ;zalloc
        '*                                        ;zfree
        '*                                        ;opaque

        int                                       ;data_type

        unsigned-long                             ;adler
        unsigned-long))                           ;reserved

(define (offset-of types n)
  "Return the offset of the Nth field among TYPES, the list of types of a
struct's fields."
  (if (zero? n)
      0
      (let* ((base  (sizeof (take types n)))
             (align (alignof (list-ref types n)))
             (mod   (modulo base align)))
        (if (zero? mod)
            base
            (+ base (- align mod))))))

(define-syntax-rule (define-stream-getter name index)
  "Define NAME as a procedure accessing the INDEXth field of %STREAM-STRUCT."
  (define name
    (let* ((offset (offset-of %stream-struct index))
           (type   (list-ref %stream-struct index))
           (size   (sizeof type)))
      (lambda (stream)
        (bytevector-uint-ref stream offset (native-endianness)
                             size)))))

(define-syntax-rule (define-stream-setter name index)
  "Define NAME as a procedure setting the INDEXth field of %STREAM-STRUCT."
  (define name
    (let* ((offset (offset-of %stream-struct index))
           (type   (list-ref %stream-struct index))
           (size   (sizeof type)))
      (lambda (stream value)
        (bytevector-uint-set! stream offset value
                              (native-endianness) size)))))

(define-stream-getter stream-avail-in 1)
(define-stream-getter stream-avail-out 4)
(define-stream-getter stream-error-message 6)
(define-stream-setter set-stream-next-in! 0)
(define-stream-setter set-stream-avail-in! 1)
(define-stream-setter set-stream-next-out! 3)
(define-stream-setter set-stream-avail-out! 4)

(define (stream-error-message* stream)
  "Return the error message associated with STREAM or #f."
  (match (stream-error-message stream)
    ((? zero?) #f)
    (address   (pointer->string (make-pointer address)))))

(define inflate!
  (let ((proc (zlib-procedure int "inflate" `(* ,int))))
    (lambda (stream flush)
      (proc stream flush))))

(define deflate!
  (let ((proc (zlib-procedure int "deflate" `(* ,int))))
    (lambda (stream flush)
      (proc stream flush))))

(define (window-bits-for-format format)
  ;; Search for "windowBits" in <zlib.h>.
  (define MAX_WBITS 15)                           ;<zconf.h>
  (match format
    ('deflate (- MAX_WBITS))                      ;raw deflate
    ('zlib    MAX_WBITS)                          ;zlib header
    ('gzip    (+ MAX_WBITS 16))))                 ;gzip header

(define inflate-init!
  (let ((proc (zlib-procedure int "inflateInit2_" `(* ,int * ,int))))
    (lambda (stream window-bits)
      (let ((ret (proc stream window-bits
                       (string->pointer %zlib-version)
                       (sizeof %stream-struct))))
        (unless (zero? ret)
          (throw 'zlib-error 'inflate-init! ret))))))

(define deflate-init!
  (let ((proc (zlib-procedure int "deflateInit2_" `(* ,int ,int ,int ,int
                                                      ,int * ,int))))
    (lambda* (stream level
                     #:key
                     (window-bits (window-bits-for-format 'zlib))
                     (memory-level 8)
                     (strategy Z_DEFAULT_STRATEGY))
      (let ((ret (proc stream level Z_DEFLATED
                       window-bits memory-level strategy
                       (string->pointer %zlib-version)
                       (sizeof %stream-struct))))
        (unless (zero? ret)
          (throw 'zlib-error 'deflate-init! ret))))))

(define inflate-end!
  (let ((proc (zlib-procedure int "inflateEnd" '(*))))
    (lambda (stream)
      (let ((ret (proc stream)))
        (unless (zero? ret)
          (throw 'zlib-error 'inflate-end! ret))))))

(define deflate-end!
  (let ((proc (zlib-procedure int "deflateEnd" '(*))))
    (lambda (stream)
      (let ((ret (proc stream)))
        (unless (zero? ret)
          (throw 'zlib-error 'deflate-end! ret))))))

;; Error codes.
(define Z_OK 0)
(define Z_STREAM_END 1)
(define Z_NEED_DICT 2)
(define Z_ERRNO -1)
(define Z_STREAM_ERROR -2)
(define Z_DATA_ERROR -3)
(define Z_MEM_ERROR -4)
(define Z_BUF_ERROR -5)

;; Flush flags.
(define Z_NO_FLUSH 0)
(define Z_PARTIAL_FLUSH 1)
(define Z_SYNC_FLUSH 2)
(define Z_FULL_FLUSH 3)
(define Z_FINISH 4)

;; 'deflate-init!' flags.
(define Z_DEFLATED 8)
(define Z_DEFAULT_STRATEGY 0)

(define* (make-zlib-input-port port
                               #:key
                               (format 'zlib)
                               (buffer-size %default-buffer-size)
                               (close? #t))
  "Return an input port that decompresses data read from PORT.  FORMAT is a
symbol denoting the header format; it must be one of 'deflate (RFC 1950),
'zlib (RFC 1951), or 'gzip (RFC 1952).

When CLOSE? is true, PORT is automatically closed when the resulting port is
closed."
  (define input-buffer (make-bytevector buffer-size))

  ;; Instead of writing uncompressed data directly to the user-provided
  ;; buffer, keep a large-enough buffer.  That way, we know we cannot stumble
  ;; into Z_BUF_ERROR because of insufficient output space.
  (define output-buffer (make-bytevector %default-buffer-size))
  (define buffered 0)
  (define offset 0)

  (define eof? #f)

  (define stream (make-bytevector (sizeof %stream-struct)))
  (define pointer
    (let ((ptr (bytevector->pointer stream)))
      (lambda (bv)
        (if (eq? bv stream)
            ptr
            (bytevector->pointer bv)))))

  (define (read! bv start count)
    (cond ((> buffered 0)
           (let ((n (min count buffered)))
             (bytevector-copy! output-buffer offset bv start n)
             (set! buffered (- buffered n))
             (set! offset (+ offset n))
             n))
          (eof? 0)
          (else
           (set! offset 0)
           (set-stream-next-out! stream
                                 (pointer-address
                                  (bytevector->pointer output-buffer)))
           (set-stream-avail-out! stream (bytevector-length output-buffer))

           (let loop ((ret Z_OK)
                      (flush? #f))
             (if (and (not flush?)
                      (or (zero? (stream-avail-in stream))
                          (= Z_BUF_ERROR ret)))
                 (let ((n (get-bytevector-n! port input-buffer
                                             0 buffer-size)))
                   (if (eof-object? n)
                       (loop ret #t)
                       (begin
                         (set-stream-next-in! stream
                                              (pointer-address
                                               (bytevector->pointer input-buffer)))
                         (set-stream-avail-in! stream n)
                         (loop ret flush?))))

                 (let ((ret (inflate! (pointer stream)
                                      (if flush? Z_SYNC_FLUSH 0))))
                   (set! buffered (- (bytevector-length output-buffer)
                                     (stream-avail-out stream)))
                   (cond ((= ret Z_OK)
                          (read! bv start count))
                         ((= ret Z_STREAM_END)
                          (set! eof? #t)
                          (read! bv start count))
                         ((and (not flush?) (= Z_BUF_ERROR ret))
                          (loop ret flush?))
                         (else
                          (throw 'zlib-error ret
                                 (stream-error-message* stream))))))))))

  (define result
    (make-custom-binary-input-port "zlib-input" read! #f #f
                                   (lambda ()
                                     (inflate-end! (pointer stream))
                                     (when close?
                                       (close-port port)))))

  ;; No need for extra buffering.
  (setvbuf result 'none)

  (inflate-init! (pointer stream)
                 (window-bits-for-format format))
  (set-stream-avail-in! stream 0)
  result)

(define* (make-zlib-output-port port
                                #:key
                                (format 'zlib)
                                (buffer-size %default-buffer-size)
                                (level %default-compression-level)
                                (close? #t))
  "Return an output port that compresses data at the given LEVEL, using PORT
as its sink.  FORMAT is a symbol denoting the header format; it must be one
of 'deflate (RFC 1950), 'zlib (RFC 1951), or 'gzip (RFC 1952).

When FORMAT is 'gzip, the gzip header takes default values, and in particular
no modification time and no file name.

When CLOSE? is true, PORT is automatically closed when the resulting port is
closed."
  (define output-buffer (make-bytevector buffer-size))
  (define stream (make-bytevector (sizeof %stream-struct)))

  (define pointer
    (let ((ptr (bytevector->pointer stream)))
      (lambda (bv)
        (if (eq? bv stream)
            ptr
            (bytevector->pointer bv)))))

  (define (output-compressed-data! stream)
    (put-bytevector port output-buffer 0
                    (- buffer-size (stream-avail-out stream)))
    (set-stream-avail-out! stream buffer-size)
    (set-stream-next-out! stream
                          (pointer-address
                           (bytevector->pointer output-buffer))))

  (define* (write! bv start count #:optional flush?)
    (set-stream-next-in! stream (+ start (pointer-address
                                          (bytevector->pointer bv))))
    (set-stream-avail-in! stream count)

    (let loop ()
      (if (zero? (stream-avail-out stream))
          (begin
            (output-compressed-data! stream)
            (loop))
          (let ((ret (deflate! (pointer stream)
                       (if flush? Z_FINISH Z_NO_FLUSH))))
            (cond ((= ret Z_BUF_ERROR)
                   (loop))
                  ((= ret Z_OK)
                   (match (- count (stream-avail-in stream))
                     (0 (loop))                   ;zero would mean EOF
                     (n n)))
                  ((and flush? (= ret Z_STREAM_END))
                   (- count (stream-avail-in stream)))
                  (else
                   (throw 'zlib-error 'deflate! ret
                          (stream-error-message* stream))))))))

  (define (flush)
    (write! #vu8() 0 0 #t)
    (output-compressed-data! stream))

  (define (close)
    (flush)
    (deflate-end! (pointer stream))
    (when close?
      (close-port port)))

  (deflate-init! (pointer stream) level
    #:window-bits (window-bits-for-format format))

  (set-stream-avail-out! stream buffer-size)
  (set-stream-next-out! stream
                        (pointer-address (bytevector->pointer output-buffer)))

  (make-custom-binary-output-port "zlib-output" write! #f #f close))

(define* (call-with-zlib-input-port port proc
                                    #:key
                                    (format 'zlib)
                                    (buffer-size %default-buffer-size))
  "Call PROC with a port that wraps PORT and decompresses data read from it.
PORT is closed upon completion.  The zlib internal buffer size is set to
BUFFER-SIZE bytes."
  (let ((zlib (make-zlib-input-port port
                                    #:format format
                                    #:buffer-size buffer-size
                                    #:close? #t)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc zlib))
      (lambda ()
        (close-port zlib)))))

(define* (call-with-zlib-output-port port proc
                                     #:key
                                     (format 'zlib)
                                     (level %default-compression-level)
                                     (buffer-size %default-buffer-size))
  "Call PROC with an output port that wraps PORT and compresses data in the
given FORMAT, with the given LEVEL.  PORT is closed upon completion.  The
zlib internal buffer size is set to BUFFER-SIZE bytes."
  (let ((zlib (make-zlib-output-port port
                                     #:format format
                                     #:level level
                                     #:buffer-size buffer-size
                                     #:close? #t)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc zlib))
      (lambda ()
        (close-port zlib)))))

;;; zlib.scm ends here
