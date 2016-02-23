;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; cluster.lisp --- This is the cluster code
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

(lfarm-server:start-server "127.0.0.1" 11111 :background t)
(lfarm-server:start-server "127.0.0.1" 22222 :background t)



(setf lfarm:*kernel* (lfarm:make-kernel '(("127.0.0.1" 11111)
                                          ("127.0.0.1" 22222))))

(defparameter *channel* (lfarm:make-channel))
(defmacro >> (fun-name &rest body)
  `(lfarm:submit-task cl-vr::*channel* #',fun-name ,@body))

(defun << () (lfarm:receive-result cl-vr::*channel*))

(<<  (>> another :x 7 :y 7)) 

(lfarm:deftask add (x y) (+ x y))
(lfarm:deftask another (&key x y) (+ x y))
(lfarm:submit-task cl-vr::*channel* #'cl-vr::another :x 4 :y 6)
(lfarm:submit-task cl-vr::*channel* #'cl-vr::another :x 4 :y 6)

(lfarm:deftask opengl-window (&key width height)
  (defparameter *window* (glop:create-window "done" width height)))

(lfarm:force (lfarm:future (ql:quickload :glop)))
(lfarm:force (lfarm:future (opengl-window :width 500 :height 500)))

(lfarm:broadcast-task (lambda () (ql:quickload :glop)))
(lfarm:broadcast-task (lambda () (opengl-window :width 500 :height 500)))

