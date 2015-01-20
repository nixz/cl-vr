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

(in-package #:cl-vr)

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
(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&")
       (finish-output))
     (condition (err) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err)
       (finish-output))))

;;; ---------------------------------------------------------------------------
(defun start-rp-listener () 
  "Start the server."
  (let ((sock (socket-listen +address+ +port+ 
			     :backlog +backlog+ :reuseaddress t)))
    (let ((cstream (socket-stream (socket-accept sock))))
      (loop for line = (read-line cstream nil)
	 while line do (progn (format cstream "~A~%" (eval (read-from-string line)))
			      (force-output cstream)))
      (close cstream))))

;;; ---------------------------------------------------------------------------
(defun repl (stream)
  (setq *standard-input* stream
           *standard-output* stream
           *error-output* stream)
  (loop for line = (read-line stream nil)
       while line do
       (handling-errors
        (setf +++ ++   ++ +   + -   - (read-from-string line))
        (format stream "~A~%" -)
        (when (member - `((quit) (exit) (continue)) :test (function equal)) 
          (return-from repl)
          )
        (format stream "~A~%" "skipped quit")
        (setf /// //   // /   / (multiple-value-list (eval -)))
        (format stream "~A~%" /)
        (setf *** **   ** *   * (first /))
        (format stream "~A~%" *)
        (force-output stream))))

  ;; (do ((+eof+ (gensym)))
  ;;     (nil)
  ;;   (handling-errors
  ;;    (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
  ;;    (when (or (eq - +eof+)
  ;;              (member - '((quit)(exit)(continue)) :test (function equal)))
  ;;      (return-from repl))
  ;;    (setf /// //   // /   / (multiple-value-list (eval -)))
  ;;    (setf *** **   ** *   * (first /))
  ;;    (format t "~&~{~S~^ ;~%     ~}~%" /)
  ;;    (finish-output))))

;;; ---------------------------------------------------------------------------
;; (defun repl (stream)
;;   "We are defining a basic repl here which works on a stream. It basically
;; liks the standard input and output to this steram and initiates a repl on this
;; stream"
;;   (unwind-protect
;;        (progn
;;          (setq *standard-input* stream
;;                *standard-output* stream)
;;          (loop (print (handler-case (eval (read))                     
;;                         (error (condition) (list 'error condition))))))
;;     (cl-user::quit)))

;;; ---------------------------------------------------------------------------
(defun repl-server (&key (port 9999))
  "The funtion starts a repl-server on a socket. The server is essentially a
connection server which brings up the repl on a new process. Once created one
can send commands over the socket"
  (let (socket)
    (unwind-protect
         (progn
           (let ((socket (usocket:socket-listen (nslookup (hostname)) port
                                                :reuseaddress t)))
             (loop
                (let (cstream pid)
                  (setq cstream (usocket:socket-stream 
                                 (usocket:socket-accept socket 
                                                        :element-type 'character)))
                  (setq pid (sb-posix:fork))
                  (cond
                    ((zerop pid) (progn
                                   (usocket:socket-close socket)
                                   (repl cstream)))
                    ((plusp pid) (progn
                                   (add-child pid)
                                   (close cstream)
                                   (format t "~&Count = ~a ~%" (total-children))
                                   (wait-for-children)))
                    (t           (error "Something went wrong while forking."))))))
           (quit)))))

;;; ---------------------------------------------------------------------------
(defun run-daemon () 
  "This functions runs the daemon as a seperate thread in the background if
  run in swank else it runs it in the main thread"
  (repl-server))

;;; ---------------------------------------------------------------------------
(defun make-daemon (&key (path (user-homedir-pathname)))
  "Makes the vr-daemon in the specified path. If no path is specified then the
  user-home-directory is used as the default path"
  (let ((file-name
         (namestring (cl-fad:merge-pathnames-as-file (pathname path)
                                                     #P"cl-vr-daemon"))))
    #+(and :swank :sbcl)
    (trivial-dump-core::sbcl-save-slime-and-die file-name #'repl-server)
    #-swank
    (trivial-dump-core:save-executable file-name #'repl-server)))


;;; ---------------------------------------------------------------------------
(defun connection (&key host (port 9999))
  "This function makes a connection to the client and returns a connection
  object"
  (usocket:socket-connect host port))
