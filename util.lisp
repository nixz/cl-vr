;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; util.lisp --- A set of utility functions
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================

(in-package: #:vr)

;;; ---------------------------------------------------------------------------
(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
  (if hostname
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
      nil))

;;; ---------------------------------------------------------------------------
(defun hostname ()
  "Gets the host-name using the (machine-instance) command"
  (machine-instance))

;;; ---------------------------------------------------------------------------
(defun repl (stream)
  "We are defining a basic repl here which works on a stream. It basically
liks the standard input and output to this steram and initiates a repl on this
stream"
  (unwind-protect
       (progn
         (setq *standard-input* stream
               *standard-output* stream)
         (loop (print (handler-case (eval (read))                     
                        (error (condition) (list 'error condition))))))
    (cl-user::quit)))

;;; ---------------------------------------------------------------------------
(defun repl-server (&key (port 9999))
  "The funtion starts a repl-server on a socket. The server is essentially a
connection server which brings up the repl on a new process. Once created one
can send commands over the socket"
  (let (socket (count 0))
    (unwind-protect
         (progn
           (let ((socket (usocket:socket-listen (nslookup (hostname)) 9999
                                                :reuseaddress t)))
             (loop
                (let (cstream pid)
                  (setq cstream (usocket:socket-accept socket :element-type 'character))
                  (setq pid (sb-posix:fork))
                  (cond
                    ((zerop pid) (progn
                                   (usocket:socket-close socket)
                                   (repl cstream)))
                    ((plusp pid) (progn
                                   (close cstream)
                                   (setf count (+ count 1))
                                   (format t "~&Count = ~a ~%" count)))
                    (t           (error "Something went wrong while forking."))))))
           (quit))))