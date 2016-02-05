;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; HUD.lisp --- THe code for a heads up display.
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

(defun hud-text (win hmd)
  (declare (ignorable win))
  (format nil "fps: ~s~%~
latency = ~{m2p:~,3,3f ren:~,3,3f tWrp:~,3,3f~%~
          PostPresent: ~,3,3f Err: ~,3,3f~}"
          "??"
          (%ovr::get-float-array
           hmd :dk2-latency 5)))

(defun init-hud (win)
  (let ((vbo (gl:gen-buffer))
        (vao (hud-vao win)))
    (setf (hud-vbo win) vbo)
    (setf (hud-count win) 0)
    (let ((stride (* 4 4))) ;; x,y,u,v * float
      (gl:bind-buffer :array-buffer vbo)
      (%gl:buffer-data :array-buffer (* 0 stride) (cffi:null-pointer)
                       :static-draw)
      (gl:bind-vertex-array vao)
      (gl:enable-client-state :vertex-array)
      (%gl:vertex-pointer 2 :float stride (cffi:null-pointer))
      (gl:enable-client-state :texture-coord-array)
      (%gl:tex-coord-pointer 2 :float stride (* 2 4)))))

(defun update-hud (win string atl)
  (let* ((strings (split-sequence:split-sequence #\newline string))
         (count (reduce '+ strings :key 'length))
        (stride (* (+ 2 2) 6)) ;; x,y,u,v * 2 tris
        (i 0)
        (scale 0.01))
    (gl:bind-buffer :array-buffer (hud-vbo win))
    (%gl:buffer-data :array-buffer (* count stride 4) (cffi:null-pointer)
                     :static-draw)
    (let ((p (%gl:map-buffer :array-buffer :write-only)))
      (unwind-protect
           (loop for line in strings
                 for baseline from 0 by (* 30 scale)
                 when line
                   do (flet ((c (x y u v)
                               (let ((x (* x scale))
                                     (y (+ baseline (* y scale))))
                                 (setf (cffi:mem-aref p :float (+ 0 (* i 4))) x
                                       (cffi:mem-aref p :float (+ 1 (* i 4))) (- y)
                                       (cffi:mem-aref p :float (+ 2 (* i 4))) v
                                       (cffi:mem-aref p :float (+ 3 (* i 4))) u)
                                 (incf i))))
                        (texatl.cl:do-texatl-string (line
                                                     x0 y0 x1 y1
                                                     u0 v0 u1 v1
                                                     :tex-width *tex-size*
                                                     :tex-height *tex-size*)
                                                    atl
                          (c x0 y0 u0 v0)
                          (c x0 y1 u0 v1)
                          (c x1 y1 u1 v1)

                          (c x0 y0 u0 v0)
                          (c x1 y1 u1 v1)
                          (c x1 y0 u1 v0)))
                 finally (setf (hud-count win) i))
        (%gl:unmap-buffer :array-buffer)))))
