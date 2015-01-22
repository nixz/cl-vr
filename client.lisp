;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; client.lisp --- The client code to manage connections
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

(in-package :cl-vr)


(defclass server ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :allocation :instance
         :documentation "the name of the host")
   (port :initarg :port
         :initform (error ":port must be specified")
         :allocation :instance
         :documentation "the port number where the server is listening"))
  (:documentation "holds the server address. Host + port"))

(defun new-server (&key host (port 9999))
  "Creates a new server host"
  (make-instance 'server :name host :port port))

;;; ---------------------------------------------------------------------------
(defclass link ()
  ((socket :initarg :socket
         :initform (error ":socket must be specified")
         :allocation :instance
         :documentation "a network socket"))
  (:documentation "Makes a link between any two entities. Right now the two
  entities are a socket but the link may be a more abstract notion spaning not
  only across the network but also within"))

;;; ---------------------------------------------------------------------------
(defgeneric >> (obj str)
   (:Documentation "a generic function for writing a string into the object"))

;;; ---------------------------------------------------------------------------
(defgeneric << (obj)
   (:Documentation "a generic function for reading a string from the object"))

;;; ---------------------------------------------------------------------------
(defmethod new-link ((srv server))
  "reurns an instance of the link object"
  (with-slots (name port) srv
    (make-instance 'link :socket (usocket:socket-connect name port))))

;;; ---------------------------------------------------------------------------
(defmethod >> ((obj link) str)
  "writes string into the link"
  (with-slots (socket) obj
    (write-line string (usocket:socket-stream socket))
    (force-output (usocket:socket-stream socket))))

;;; ---------------------------------------------------------------------------
(defmethod << ((obj link))
  "returns a string which is read from the link"
  (with-slots (socket) obj
    (read-line (usocket:socket-stream socket))))
