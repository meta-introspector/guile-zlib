;;; Guile-zlib --- Functional package management for GNU
;;; Copyright © 2016, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-zlib)
  #:use-module (zlib)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

(test-begin "zlib")

(define (random-seed)
  (logxor (getpid) (car (gettimeofday))))

(define %seed
  (let ((seed (random-seed)))
    (format (current-error-port) "random seed for tests: ~a~%"
            seed)
    (seed->random-state seed)))

(define (random-bytevector n)
  "Return a random bytevector of N bytes."
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256 %seed))
            (loop (1+ i)))
          bv))))

(test-assert "compression/decompression pipe"
  (let ((data (random-bytevector (+ (random 10000)
                                    (* 20 1024)))))
    (match (pipe)
      ((parent . child)
       (match (primitive-fork)
         (0                                       ;compress
          (dynamic-wind
            (const #t)
            (lambda ()
              (close-port parent)
              (call-with-gzip-output-port child
                (lambda (port)
                  (put-bytevector port data))))
            (lambda ()
              (primitive-exit 0))))
         (pid                                     ;decompress
          (begin
            (close-port child)
            (let ((received (call-with-gzip-input-port parent
                              (lambda (port)
                                (get-bytevector-all port))
                              #:buffer-size (* 64 1024))))
              (match (waitpid pid)
                ((_ . status)
                 (and (zero? status)
                      (port-closed? parent)
                      (bytevector=? received data))))))))))))

(test-assert "raw compress/decompress"
  (let* ((data (random-bytevector (+ (random 10000) (* 20 1024))))
         (cdata (compress data))
         (ucdata (uncompress cdata)))
    (equal? data ucdata)))

(test-end)
