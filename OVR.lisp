;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; OVR.lisp --- The oculus rift classes
;;;;
;;;; Copyright (c) 2015, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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
(defclass OVR-HMD ()
  ((handle :initarg :handle
         :initform nil
         ;; :accessor handle
         :documentation "the handle which gets created")
   (property :initarg :property
         :documentation "The propertis of the HMD"))
  (:documentation "the class which does oculus rift"))

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance :after ((hmd OVR-HMD) &key index)
  ""
    (with-slots (handle property) hmd
    ;; Check if the hmd is detected
      (setf is-detected (%ovrhmd::detect))
          ;; Create a new hmd handle
    (when (open hmd index)
      (setf property (make-instance 'OVR-Properties :hmd hmd)))))

;;; ---------------------------------------------------------------------------
(defmethod open ((hmd OVR-HMD) &key index)
  "Creates a OVR hmd"
  (setf (slot-value hmd 'handle) (%ovrhmd::create index))
    (unless handle
      (progn
        (format t "~& Sorry couldnt create object ~%")
        (return nil))))

;;; ---------------------------------------------------------------------------
(defmethod close (hmd OVR-HMD)
  "destroys the HMD"
  (%ovrhmd::destroy (slot-value hmd 'handle)))

;;; ---------------------------------------------------------------------------
(defclass OVR-Properties ()
  ((handle :initarg :handle
         ;; :accessor handle
         :documentation "about-slot")
   (type :initarg :type
         ;; :accessor type
         :documentation "about-slot")
   (product-name :initarg :product-name
         ;; :accessor product-name
         :documentation "about-slot")
   (manufacturer :initarg :manufacturer
         ;; :accessor manufacturer
         :documentation "about-slot")
   (vendor-id :initarg :vend
         ;; :accessor vend
         :documentation "about-slot")
   (product-id :initarg :product-id
         ;; :accessor product-id
         :documentation "about-slot")
   (serial-number :initarg :serial-number
         ;; :accessor serial-number
         :documentation "about-slot")
   (firmware-major :initarg :firmware-major
         ;; :accessor firmware-major
         :documentation "about-slot")
   (firmware-minor :initarg :firmware-minor
         ;; :accessor firmware-minor
         :documentation "about-slot")
   (camera-frustum-hfov-in-radians :initarg :camera-frustum-hfov-in-radians
         ;; :accessor camera-frustum-hfov-in-radians
         :documentation "about-slot")
   (camera-frustum-vfov-in-radians :initarg :camera-frustum-vfov-in-radians
         ;; :accessor camera-frustum-vfov-in-radians
         :documentation "about-slot")
   (camera-frustum-near-zin-meters :initarg :camera-frustum-near-zin-meters
         ;; :accessor camera-frustum-near-zin-meters
         :documentation "about-slot")
   (camera-frustum-far-zin-meters :initarg :camera-frustum-far-zin-meters
         ;; :accessor camera-frustum-far-zin-meters
         :documentation "about-slot")
   (hmd-caps :initarg :hmd-caps
         ;; :accessor hmd-caps
         :documentation "about-slot")
   (tracking-caps :initarg :tracking-caps
         ;; :accessor tracking-caps
         :documentation "about-slot")
   (distortion-caps :initarg :distortion-caps
         :documentation "about-slot")
   (default-eye-fov :initarg :default-eye-fov
         ;; :accessor default-eye-fov
         :documentation "about-slot")
   (max-eye-fov :initarg :max-eye-fov
         ;; :accessor max-eye-fov
         :documentation "about-slot")
   (eye-render-order :initarg :eye-render-order
         ;; :accessor eye-render-order
         :documentation "about-slot")
   (eye-render-order-symbol :initarg :eye-render-order-symbol
         ;; :accessor eye-render-order-symbol
         :documentation "about-slot")
   (resolution :initarg :resolution
         ;; :accessor resolution
         :documentation "about-slot")
   (window-pos :initarg :window-pos
         ;; :accessor window-pos
         :documentation "about-slot")
   (display-device-name :initarg :display-device-name
         ;; :accessor display-device-name
         :documentation "about-slot")
   (display-id :initarg :display-id
         ;; :accessor display-id
               :documentation "about-slot"))
  (:documentation "This is the class which stores the HMD properties"))

(defmethod initialize-instance :after ((properties OVR-Properties ) &key (hmd OVR-HMD))
  "Initialize the object"
    ;; Get all the properties and fill up the class
    (let ((props (%ovr::dump-hmd-to-plist (slot-value hmd 'handle))))
      (setf (slot-value properties handle)                         (getf props :handle))
      (setf (slot-value properties type)                           (getf props :type))
      (setf (slot-value properties product-name)                   (getf props :product-name))
      (setf (slot-value properties manufacturer)                   (getf props :manufacturer))
      (setf (slot-value properties vendor-id)                      (getf props :vendor-id))
      (setf (slot-value properties product-id)                     (getf props :product-id))
      (setf (slot-value properties serial-number)                  (getf props :serial-number))
      (setf (slot-value properties firmware-major)                 (getf props :firmware-major))
      (setf (slot-value properties firmware-minor)                 (getf props :firmware-minor))
      (setf (slot-value properties camera-frustum-hfov-in-radians) (getf props :camera-frustum-hfov-in-radians))
      (setf (slot-value properties camera-frustum-vfov-in-radians) (getf props :camera-frustum-vfov-in-radians))
      (setf (slot-value properties camera-frustum-near-zin-meters) (getf props :camera-frustum-near-zin-meters))
      (setf (slot-value properties camera-frustum-far-zin-meters)  (getf props :camera-frustum-far-zin-meters))
      (setf (slot-value properties hmd-caps)                       (getf props :hmd-caps))
      (setf (slot-value properties tracking-caps)                  (getf props :tracking-caps))
      (setf (slot-value properties distortion-caps)                (getf props :distortion-caps))
      (setf (slot-value properties default-eye-fov)                (getf props :default-eye-fov))
      (setf (slot-value properties max-eye-fov)                    (getf props :max-eye-fov))
      (setf (slot-value properties eye-render-order)               (getf props :eye-render-order))
      (setf (slot-value properties eye-render-order-symbol)        (getf props :eye-render-order-symbol))
      (setf (slot-value properties resolution)                     (getf props :resolution))
      (setf (slot-value properties window-pos)                     (getf props :window-pos))
      (setf (slot-value properties display-device-name)            (getf props :display-device-name))
      (setf (slot-value properties display-id)                     (getf props :display-id))))

;;; ---------------------------------------------------------------------------
(defmethod print-object ((prop OVR-Properties) out)
  "prints the libovr object"
  (with-slots (is-detected
               handle
               type
               product-name
               manufacturer
               vendor-id
               product-id
               serial-number
               firmware-major
               firmware-minor
               camera-frustum-hfov-in-radians
               camera-frustum-vfov-in-radians
               camera-frustum-near-zin-meters
               camera-frustum-far-zin-meters
               hmd-caps
               tracking-caps
               distortion-caps
               default-eye-fov
               max-eye-fov
               eye-render-order
               eye-render-order-symbol
               resolution
               window-pos
               display-device-name
               display-id) prop
    (format out "~$(OVR-Properties~%")
    (format out "    :is-detected                    ~a ~%"                    is-detected )
    (format out "    :handle                         ~a ~%"                         handle )
    (format out "    :type                           ~a ~%"                           type )
    (format out "    :product-name                   ~a ~%"                   product-name )
    (format out "    :manufacturer                   ~a ~%"                   manufacturer )
    (format out "    :vendor-id                      ~a ~%"                      vendor-id )
    (format out "    :product-id                     ~a ~%"                     product-id )
    (format out "    :serial-number                  ~a ~%"                  serial-number )
    (format out "    :firmware-major                 ~a ~%"                 firmware-major )
    (format out "    :firmware-minor                 ~a ~%"                 firmware-minor )
    (format out "    :camera-frustum-hfov-in-radians ~a ~%" camera-frustum-hfov-in-radians )
    (format out "    :camera-frustum-vfov-in-radians ~a ~%" camera-frustum-vfov-in-radians )
    (format out "    :camera-frustum-near-zin-meters ~a ~%" camera-frustum-near-zin-meters )
    (format out "    :camera-frustum-far-zin-meters  ~a ~%"  camera-frustum-far-zin-meters )
    (format out "    :hmd-caps                       ~a ~%"                       hmd-caps )
    (format out "    :tracking-caps                  ~a ~%"                  tracking-caps )
    (format out "    :distortion-caps                ~a ~%"                distortion-caps )
    (format out "    :default-eye-fov                ~a ~%"                default-eye-fov )
    (format out "    :max-eye-fov                    ~a ~%"                    max-eye-fov )
    (format out "    :eye-render-order               ~a ~%"               eye-render-order )
    (format out "    :eye-render-order-symbol        ~a ~%"        eye-render-order-symbol )
    (format out "    :resolution                     ~a ~%"                     resolution )
    (format out "    :window-pos                     ~a ~%"                     window-pos )
    (format out "    :display-device-name            ~a ~%"            display-device-name )
    (format out "    :display-id                     ~a ~%"                     display-id )
    (format out "~$)~%")
    ))
    

;;; ---------------------------------------------------------------------------
(defclass OVR-library ()
  ((version :initarg :version
         :documentation "libovr version")
   (time-in-seconds :initarg :time
         :documentation "Dont know what this really does but. Time when we initialized perhaps")
   (total-hmds-detected :initarg :total-hmds-detected
         ;; :accessor total-hmds-detected
                        :documentation "The total number of hmd's detected on the system")
   (hmds :initarg :hmds
         :accessor hmd
         :type 'OVR-HMD
         :documentation "about-slot"))
  (:documentation "initializing the libovr library"))

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance :after ((library OVR-library) &key)
  "Initialized the lib ovr and fill out a few variables"
  (if (open library)
    (with-slots (version time-in-seconds total-hmds-detected) library
      (setf version (%ovr::get-version-string))
      (setf time-in-seconds (%ovr::get-time-in-seconds))
      (setf total-hmds-detected (%ovrhmd::detect)))
    nil))

;;; ---------------------------------------------------------------------------
(defmethod open ((library OVR-library))
  "Wrapper to the original create method"
  (if (%ovr::initialize :debug nil :timeout-ms 500)
      t
      nil))

;;; ---------------------------------------------------------------------------
(defmethod close ((library OVR-library))
  "Wrapper to shutdown the OVR library context"
  (%ovr::shutdown))

;;; ---------------------------------------------------------------------------
(defmethod print-object ((obj OVR-library) out)
  "prints the libovr object"
  (with-slots (version time-in-seconds total-hmds-detected) obj
    (format t "~&(OVR-library~%")
    (format out "    :version:            ~s   ~%" version)
    (format out "    :time-in-seconds     ~,3f ~%" time-in-seconds)
    (format out "    :total-hmds-detected ~a   ~%" total-hmds-detected)
    (format t "~&)~%")))
  
