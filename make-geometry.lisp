;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; make-geometry.lisp --- The code in this file makes vertex and index
;;;; buffers with the geometry that is specified into global buffers
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

(defparameter *buf* (make-array '(1024) 
                                :element-type 'single-float
                                :fill-pointer 0
                                :adjustable t))
(defparameter *count* 0)
;; (defparameter *color*  (vector 0 0 0 1)) 
;; (defparameter *normal* (vector 1 0 0))

;; (defun color (r g b &optional (a 1))
;;   (setf *color* (vector r g b a)))
;;; ----------------------------------------------------------------------------
(defun norm (a b c)
  (let* ((v1 (sb-cga:vec- a b))
         (v2 (sb-cga:vec- b c)))
    (sb-cga:normalize (sb-cga:cross-product v1 v2))))

;;; ----------------------------------------------------------------------------
(defun vertex (x y z &key (w 1)
                       (color (vector 0 0 0 1)) 
                       (normal (vector 1 0 0)))  
  (loop for i in (list x y z w)
   do (vector-push-extend (float i 0.0) *buf*))
  (loop for i across color
     do (vector-push-extend (float i 0.0) *buf*))
  (loop for i across normal
     do (vector-push-extend (float i 0.0) *buf*))
  (incf *count*))

;;; ----------------------------------------------------------------------------
(defun face (v1 v2 v3 &key (tx (sb-cga:identity-matrix)) 
                        (normal (vector 1.0 .0 .0) normal-provided-p)  
                        (color (vector .0 .0 .0 1.0)))
  (let* ((normal (if normal-provided-p normal (norm v1 v2 v3)))
         (v1 (sb-cga:transform-point v1 tx))
         (v2 (sb-cga:transform-point v2 tx))
         (v3 (sb-cga:transform-point v3 tx))
        (v1.x (aref v1 0)) 
        (v1.y (aref v1 1))
        (v1.z (aref v1 2))
        (v2.x (aref v2 0))
        (v2.y (aref v2 1))
        (v2.z (aref v2 2))
        (v3.x (aref v3 0))
        (v3.y (aref v3 1))
        (v3.z (aref v3 2)))
    (vertex v1.x v1.y v1.z :normal normal :color color)
    (vertex v2.x v2.y v2.z :normal normal :color color)
    (vertex v3.x v3.y v3.z :normal normal :color color)))
  
;;; ----------------------------------------------------------------------------
(defun cube (x y z r &key (color (vector 0 0 0 1)))
  (let* ((x (coerce x 'single-float))
         (y (coerce y 'single-float))
         (z (coerce z 'single-float))
         (r (coerce r 'single-float))
         (a (sb-cga:vec (- r) (- r) (- r)))
         (b (sb-cga:vec (- r) (+ r) (- r)))
         (c (sb-cga:vec (+ r) (+ r) (- r)))
         (d (sb-cga:vec (+ r) (- r) (- r)))
         (fpi (coerce pi 'single-float))
         (normal nil))
    (loop for m in (list (sb-cga:rotate* 0.0 0.0 0.0)
                         (sb-cga:rotate* 0.0 (* fpi 1/2) 0.0)
                         (sb-cga:rotate* 0.0 (* fpi 2/2) 0.0)
                         (sb-cga:rotate* 0.0 (* fpi 3/2) 0.0)
                         (sb-cga:rotate* (* fpi 1/2) 0.0 0.0)
                         (sb-cga:rotate* (* fpi 3/2) 0.0 0.0))
       do (let* ((n (sb-cga:transform-point
                    (sb-cga:vec 0.0 0.0 1.0) m)))
            (setf normal (vector (aref n 0) (aref n 1) (aref n 2))))
        (flet ((v (v)
                  (let ((v (sb-cga:transform-point v m)))
                    (vertex (+ x (aref v 0))
                            (+ y (aref v 1))
                            (+ z (aref v 2)) 
                            :normal normal
                            :color color))))
           (v a)
           (v b)
           (v c)
           (v a)
           (v c)
           (v d)))))

;; (defun skew-symmetric-cross-product (v)
;;   (let ((v1 (aref v 0))
;;         (v2 (aref v 1))
;;         (v3 (aref v 3)))
;;     (sb-cga:matrix    .0  (- v3)    v2
;;                       v3     .0  (- v1)
;;                    (- v2)    v1    .0)))

;; (defun get-rotation (src-vec dest-vec)
;;   (let* ((a (sb-cga:normalize src-vec))
;;          (b (sb-cga:normalize dest-vec))
;;          (v (sb-cga:cross-product a b))
;;          (s (sb-cga:vec-length v))
;;          (c (sb-cga:dot-product a b)))
    
;;     ))
;;; ----------------------------------------------------------------------------
(defun cylinder (x1 y1 z1 x2 y2 z2 r &key (color (vector .28 .18 .14 1)))
  (let* ((x1 (coerce x1 'single-float))
         (y1 (coerce y1 'single-float))
         (z1 (coerce z1 'single-float))
         (x2 (coerce x2 'single-float))
         (y2 (coerce y2 'single-float))
         (z2 (coerce z2 'single-float))
         (x (/ (+ x1 x2) 2))
         (y (/ (+ y1 y2) 2))
         (z (/ (+ z1 z2) 2))
         (v (sb-cga:vec- (sb-cga:vec x2 y2 z2)
                         (sb-cga:vec x1 y1 z1)))
         (v-dest (sb-cga:normalize v))
         (v-src (sb-cga:vec .0 1.0 .0))
         ;; (norm (sb-cga:cross-product v-src v-dest))
         ;; (angle (acos 
         ;;         (/ (sb-cga:dot-product v-src v-dest) 
         ;;            (* (sb-cga:vec-length v-src) 
         ;;               (sb-cga:vec-length v-dest)))))
         (l (sb-cga:vec-length v))
         (vdata  *cylinder-vertices*)
         (tindices *cylinder-indices*)
         (r  (coerce r 'single-float)) 
         (m (sb-cga:matrix* 
                            (sb-cga:translate* x y z)
                            (sb-cga:reorient v-src v-dest)
                            (sb-cga:scale* r l r)
                            ))
         )
    (loop for i from 0 below (length tindices)
       do (face (elt vdata (elt (elt tindices i) 0))
                  (elt vdata (elt (elt tindices i) 1))    
                  (elt vdata (elt (elt tindices i) 2))
                  :tx m :color color))))


;;; ----------------------------------------------------------------------------
(defun sphere (x y z r &key (color (vector 1.0 .0 .0 1.0)))
  (let* ((x (coerce x 'single-float))
         (y (coerce y 'single-float))
         (z (coerce z 'single-float))
         (r (coerce r 'single-float))
         (vdata  *sphere-vertices*)
         (tindices *sphere-indices*)
         (m (sb-cga:matrix* (sb-cga:translate* x y z) (sb-cga:scale* r r r))))
    (loop for i from 0 below (length tindices)
       do (face (elt vdata (elt (elt tindices i) 0))
                  (elt vdata (elt (elt tindices i) 1))    
                  (elt vdata (elt (elt tindices i) 2))
                  :tx m :color color))))

;;; ----------------------------------------------------------------------------
(defun mesh (&key (color (vector 1.0 .5 .0 .5)))
  (let* ()
    (loop for i from 0 below (length *indices*)
       do (flet ((v (v)
                   (let* ((a (aref v 0))
                          (b (aref v 1))
                          (c (aref v 2))
                          )
                     (vertex a b c                             
                             :normal (vector 1 0 0)
                             :color color))))
            (let ((v1 (elt *vertices* (elt (elt *indices* i) 0)))
                  (v2 (elt *vertices* (elt (elt *indices* i) 1)))
                  (v3 (elt *vertices* (elt (elt *indices* i) 2))))
              (face v1 v2 v3 :color color))))))
           ;; (v v1)
           ;; (v v2)
           ;; (v v3)))))

;; (defun sphere (x y z rad slices stacks &key (color (vector 0 0 0 1)))
;;   (let ((x (coerce x 'single-float))
;;         (y (coerce y 'single-float))
;;         (z (coerce z 'single-float))
;;         (r (coerce r 'single-float))
;;         (drho (/ PI stacks))
;;         (dtheta (* 2.0 (/ PI slices)))
;;         (ds (/ 1 slices))
;;         (dt (/ 1 stacks))
;;         (t 1.0)
;;         (s 0.0))
;;     (loop for i from 0 below stacks 
;;           do (let* ((rho (* i drho))
;;                     (srho (sin rho))
;;                     (crho (cos rho))
;;                     (srhodrho (sin (+ rho drho)))
;;                     (crhodrho (cos (+ rho drho)))
;;                     (s 0.0))
;;                (loop for j from 0 to slices
;;                   do (let* ((theta (if (eql j slices) 0.0 (* j dtheta)))
;;                             (stheta (- (sin theta)))
;;                             (ctheta (cos theta))
;;                             (x (* stheta srho))
;;                             (y (* ctheta rho))
;;                             (z crho)


;;                            ;; (y (sin (- (* PI r R-1)
;;                            ;;            (/ PI 2)
;;                            ;;            )))
;;                            ;; (x (* (cos (* 2 PI s S-1))
;;                            ;;        (sin (* PI r R-1))))
;;                            ;; (z (* (sin (* 2 PI s S-1))
;;                            ;;       (sin (* PI r R-1))))))).))))
