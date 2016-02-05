;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; global-parameters.lisp --- This file contains all the global parameters
;;;; used by other programs
;;;;
;;;; Copyright (c) 2016, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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
;;; Eventually all data needs to be loaded from a clouchdb database
;;; backend. This will ensure that things can be persistent across different
;;; computers
;;; ----------------------------------------------------------------------------
(defparameter *mesh* (classimp:import-into-lisp "data/mesh.ply"))
(defparameter *vertices* (classimp:vertices (elt (classimp:meshes *mesh*) 0))) 
(defparameter *indices* (classimp:faces (elt (classimp:meshes *mesh*) 0))) 

;;; ----------------------------------------------------------------------------
(defparameter *sphere-mesh* (classimp:import-into-lisp "data/sphere.ply"))
(defparameter *sphere-vertices* 
  (classimp:vertices (elt (classimp:meshes *sphere-mesh*) 0))) 
(defparameter *sphere-indices* 
  (classimp:faces (elt (classimp:meshes *sphere-mesh*) 0))) 

;;; ----------------------------------------------------------------------------
(defparameter *cylinder-mesh* (classimp:import-into-lisp "data/cylinder.ply"))
(defparameter *cylinder-vertices* 
  (classimp:vertices (elt (classimp:meshes *cylinder-mesh*) 0))) 
(defparameter *cylinder-indices* 
  (classimp:faces (elt (classimp:meshes *cylinder-mesh*) 0))) 
