;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; window-HMD.lisp --- This is where HMD window and corresponding events are
;;;; defined (IO)
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

;;; ----------------------------------------------------------------------------
(defclass window-HMD (glop:window)
  ((hmd :reader hmd :initarg :hmd)
   (world-vao :accessor world-vao)
   (world-vao-background :accessor world-vao)
   (count :accessor world-count)
   (count-background :accessor world-count)
   (hud-vbo :accessor hud-vbo :initform nil)
   (hud-vao :accessor hud-vao :initform nil)
   (hud-count :accessor hud-count)
   (hud-texture :accessor hud-texture)
   (font :accessor font)))

;;; ----------------------------------------------------------------------------
(defmethod swap (obj window-HMD)
  "swap background and foreground objects"
  (with-slots (world-vao count world-vao-background count-background) obj
    (let ((temp-vao (world-vao))
          (temp-count (count)))
      (setf world-vao world-vao-background)
      (setf count count-background)
      (setf world-vao-background temp-vao)
      (setf count-background temp-count))))

;;; ----------------------------------------------------------------------------
(defparameter *tex-size* 256)
(defparameter *FRONT-BACK* 0.0)
(defparameter *LEFT-RIGHT* 0.0)
;;; ----------------------------------------------------------------------------
(defmethod glop:on-event ((window window-HMD) (event glop:key-event))
  ;; exit on ESC key
  (when (glop:pressed event)
    (case (glop:keysym event)
      (:escape
       (glop:push-close-event window))
      (:left  (setf *LEFT-RIGHT* (+ *LEFT-RIGHT* 1)))
      (:right (setf *LEFT-RIGHT* (- *LEFT-RIGHT* 1)))
      (:up    (setf *FRONT-BACK* (+ *FRONT-BACK* 5)) )
      (:down  (setf *FRONT-BACK* (- *FRONT-BACK* 5)))
      (:space
       (format t "latency = ~{~,3,3f ~,3,3f ~,3,3f ~,3,3f ~,3,3f~}~%"
               (%ovr::get-float-array (hmd window) :dk2-latency 5))))))

;;; ----------------------------------------------------------------------------
(defmethod glop:on-event ((window window-HMD) event)
  ;; ignore any other events
  (declare (ignore window event)))
