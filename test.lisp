;; minimal example using glop
(in-package #:cl-vr)

;;; ---------------------------------------------------------------------------
(defun build-world (vao)
  (let ((vbo (gl:gen-buffer))
        (color (vector 0 0 0 1)))
   (labels ((color (r g b &optional (a 1))
              (setf color (vector r g b a))))
     ;; checkerboard ground
     (loop for i from -8 below 8
           do (loop for j from -8 below 8
                    for p = (oddp (+ i j))
                    do (if p
                           (color 0.0 0.9 0.9 1.0)
                           (color 0.1 0.1 0.1 1.0))
                       (vertex i -0.66 j :color color)
                       (vertex (1+ i) -0.66 j :color color)
                       (vertex (1+ i) -0.66 (1+ j) :color color)
                       (vertex i -0.66 j :color color)
                       (vertex (1+ i) -0.66 (1+ j) :color color)
                   (vertex i -0.66 (1+ j) :color color)))
     (load "data/xyz200-1.lisp")
     ;; (load "/home/nshetty/Downloads/mesh.lisp")
     (mesh)
     ;; and some random cubes
     ;; (let ((*random-state* (make-random-state *random-state*))
     ;;       (r 1000.0))
     ;;   (flet ((r () (- (random r) (/ r 2))))
     ;;     (loop for i below 5000
     ;;           do (color (random 1.0) (+ 0.5 (random 0.5)) (random 1.0) 1.0)
     ;;              (cube (+ 0.0 (r)) (- (r)) (+ 1.5 (r)) (+ 0.05 (random 0.10)))
     ;;              (sphere (+ 0.0 (r)) (- (r)) (+ 1.5 (r)) (+ 0.05 (random 0.10))))))
     (let ((stride (* 11 4)))
       (gl:bind-buffer :array-buffer vbo)
       (%gl:buffer-data :array-buffer (* *count* stride) (cffi:null-pointer)
                        :static-draw)
       (gl:bind-vertex-array vao)
       (gl:enable-client-state :vertex-array)
       (%gl:vertex-pointer 4 :float stride (cffi:null-pointer))
       (gl:enable-client-state :normal-array)
       (%gl:normal-pointer :float stride (* 8 4))
       (gl:enable-client-state :color-array)
       (%gl:color-pointer 4 :float stride (* 4 4)))
     (let ((p (%gl:map-buffer :array-buffer :write-only)))
       (unwind-protect
            (loop for i below (fill-pointer *buf*)
                  do (setf (cffi:mem-aref p :float i)
                           (aref *buf* i)))
         (%gl:unmap-buffer :array-buffer)))
     (gl:bind-vertex-array 0)
     (gl:delete-buffers (list vbo))
     *count*)))

;;; ---------------------------------------------------------------------------
(defparameter *w* nil)
(defparameter *move* nil)

;;; ---------------------------------------------------------------------------
(defun draw-world (win)
  (setf *w* win)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :framebuffer-srgb
             :line-smooth :blend :point-smooth :depth-test
             :lighting :light0 :color-material)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  (gl:light :light0 :position '(.0 .0 .0 0.0))
  (when (world-count win)
    (gl:disable :texture-2d)
    (gl:bind-vertex-array (world-vao win))
    (%gl:draw-arrays :triangles 0 (world-count win)))
  (gl:point-size 10)
  (gl:with-pushed-matrix* (:modelview)
    ;(gl:load-identity)
    (gl:translate -2 0.2 -2.5)
    ;; (gl:translate (aref *move* 0) 
    ;;               (aref *move* 1)
    ;;               (aref *move* 2))
    (when (and (hud-count win) (plusp (hud-count win)))
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d (hud-texture win))
      (gl:bind-vertex-array (hud-vao win))
      (%gl:draw-arrays :triangles 0 (hud-count win)))
    )
    (gl:bind-vertex-array 0))


;;; ---------------------------------------------------------------------------
(defun draw-frame (hmd &key eye-render-desc fbo eye-textures win)
  (assert (and eye-render-desc fbo eye-textures))
  (let* ((timing (%ovrhmd::begin-frame hmd
                                       ;; don't need to pass index
                                       ;; unless we use
                                       ;; get-frame-timing
                                       0))
         ;;(props (%ovr::dump-hmd-to-plist hmd))
         ;; get current hmd position/orientation
         ;;(state (%ovrhmd::get-tracking-state hmd))
         ;;(pose (getf state :head-pose))
         ;;(pos (getf (getf pose :the-pose) :position))
         ;;(or (getf (getf pose :the-pose) :orientation))
         ;;(lac (getf pose :linear-acceleration))
         ;;(lv (getf pose :linear-velocity))
         ;;(cam (getf state :camera-pose))
         ;;(cam-pos (getf cam :position))
         ;; set camera orientation from rift
         #++(camera ))
    (declare (ignorable timing))
    ;; get position of eyes
    (multiple-value-bind (head-pose tracking-state)
        (%ovr::get-eye-poses hmd
                             (mapcar (lambda (a)
                                           (getf a :hmd-to-eye-view-offset))
                                         eye-render-desc))

      (let ((status (getf tracking-state :status-flags)))
        ;; change clear color depending on tracking state
        ;; red = no tracking
        ;; blue = orientation only
        ;; green = good
;        (print status)
        (cond
          ((and (member :orientation-tracked status)
                (member :position-tracked status))
           (gl:clear-color 0.1 0.5 0.2 1))
          ((and (member :orientation-tracked status))
           (gl:clear-color 0.1 0.2 0.5 1))
          (t
           (gl:clear-color 0.5 0.1 0.1 1))))
      ;; draw view from each eye
      (gl:bind-framebuffer :framebuffer fbo)
      (loop
        for index below 2
        ;; sdk specifies preferred drawing order, so it can predict
        ;; timing better in case one eye will be displayed before
        ;; other
        for eye = index ;(elt (getf props :eye-render-order) index)
        ;; get position/orientation for specified eye
        for pose = (elt head-pose eye)
        for orientation = (getf pose :orientation)
        for position = (getf pose :position)
        ;; get projection matrix from sdk
        for projection = (%ovr::matrix4f-projection
                          (getf (elt eye-render-desc eye)
                                :fov)
                          0.1 10000.0 ;; near/far
                          ;; request GL style matrix
                          '(:right-handed :clip-range-open-gl))
        ;; draw scene to fbo for 1 eye
        do (flet ((viewport (x)
                    ;; set viewport and scissor from texture config we
                    ;; will pass to sdk so rendering matches
                    (destructuring-bind (&key pos size) x
                      (gl:viewport (elt pos 0) (elt pos 1)
                                   (getf size :w) (getf size :h))
                      (gl:scissor (elt pos 0) (elt pos 1)
                                  (getf size :w)
                                  (getf size :h)))))
             (viewport (getf (elt eye-textures index) :render-viewport)))
           (gl:enable :scissor-test)
           ;; (setf (aref position 2) (+ (aref position 2) *FRONT-BACK*))
           ;; (setf *FRONT-BACK* 0)
           ;; configure matrices
           (let* ((quat (kit.math::quaternion (aref orientation 3)
                                              (aref orientation 0)
                                              (aref orientation 1)
                                              (aref orientation 2))))
             (setf *move* 
                   (kit.math::quat-rotate-vector quat 
                                                 (kit.math::vec *LEFT-RIGHT* 0.0 *FRONT-BACK*)))
           (gl:with-pushed-matrix* (:projection)
             (gl:load-transpose-matrix projection)
             (gl:with-pushed-matrix* (:modelview)
               (gl:load-identity)
               (gl:mult-transpose-matrix
                (kit.math::quat-rotate-matrix
                 ;; kit.math quaternions are w,x,y,z but libovr quats
                 ;; are x,y,z,w
                 quat))
               
               (gl:translate (- (aref position 0))
                             (- (aref position 1))
                             (- (aref position 2)))
               
               (gl:translate -100 -100 -100)
               (gl:translate (aref *move* 0) 
                             (aref *move* 1)
                             (aref *move* 2))
               (draw-world win)
               ))  
             )
           )
      (gl:bind-framebuffer :framebuffer 0)
      ;; pass textures to SDK for distortion, display and vsync
      (%ovr::end-frame hmd head-pose eye-textures))))

(defclass OVR-library ()
  ((version :initarg :version
         :initform (error ":version must be specified")
         :accessor version
         :allocation :instance
         :documentation "libovr version")
   (time-in-seconds :initarg :time
         :initform (error ":time must be specified")
         :accessor time-in-seconds
         :allocation :instance
         :documentation "Dont know what this really does but. Time when we initialized perhaps"))
  (:documentation "initializing the libovr library"))


(defmethod initialize-instance :after ((library OVR-library) &key)
  "Initialized the lib ovr and fill out a few variables"
  (let ((init (%ovr::initialize :debug nil :timeout-ms 500)))
    (with-slots (version time-in-seconds) library
      (setf version (%ovr::get-version-string))
      (setf time-in-seconds (%ovr::get-time-in-seconds)))
    (format t "version: ~s~%" (%ovr::get-version-string))
    (format t "time = ~,3f~%" (%ovr::get-time-in-seconds))
    (format t "detect: ~s HMDs available~%" (%ovrhmd::detect))
    ))

(defmethod print-object ((obj OVR-library) out)
  "prints the libovr object")


(defclass OVR-HMD ()
  ((is-detected :initarg :available
         :initform (error ":available must be specified")
         :accessor is-detected
         :allocation :instance
         :documentation "Check if HMD is available")
   (props :initarg :props
         :initform (error ":props must be specified")
         :accessor props
         :allocation :instance
         :documentation "hmd properties")
   (handle :initarg :handle
         :initform (error ":handle must be specified")
         ;; :accessor handle
         :documentation "about-slot")
   (type :initarg :type
         :initform (error ":type must be specified")
         ;; :accessor type
         :documentation "about-slot")
   (product-name :initarg :product-name
         :initform (error ":product-name must be specified")
         ;; :accessor product-name
         :documentation "about-slot")
   (manufacturer :initarg :manufacturer
         :initform (error ":manufacturer must be specified")
         ;; :accessor manufacturer
         :documentation "about-slot")
   (vendor-id :initarg :vend
         :initform (error ":vend must be specified")
         ;; :accessor vend
         :documentation "about-slot")
   (product-id :initarg :product-id
         :initform (error ":product-id must be specified")
         ;; :accessor product-id
         :documentation "about-slot")
   (serial-number :initarg :serial-number
         :initform (error ":serial-number must be specified")
         ;; :accessor serial-number
         :documentation "about-slot")
   (firmware-major :initarg :firmware-major
         :initform (error ":firmware-major must be specified")
         ;; :accessor firmware-major
         :documentation "about-slot")
   (firmware-minor :initarg :firmware-minor
         :initform (error ":firmware-minor must be specified")
         ;; :accessor firmware-minor
         :documentation "about-slot")
   (camera-frustum-hfov-in-radians :initarg :camera-frustum-hfov-in-radians
         :initform (error ":camera-frustum-hfov-in-radians must be specified")
         ;; :accessor camera-frustum-hfov-in-radians
         :documentation "about-slot")
   (camera-frustum-vfov-in-radians :initarg :camera-frustum-vfov-in-radians
         :initform (error ":camera-frustum-vfov-in-radians must be specified")
         ;; :accessor camera-frustum-vfov-in-radians
         :documentation "about-slot")
   (camera-frustum-near-zin-meters :initarg :camera-frustum-near-zin-meters
         :initform (error ":camera-frustum-near-zin-meters must be specified")
         ;; :accessor camera-frustum-near-zin-meters
         :documentation "about-slot")
   (camera-frustum-far-zin-meters :initarg :camera-frustum-far-zin-meters
         :initform (error ":camera-frustum-far-zin-meters must be specified")
         ;; :accessor camera-frustum-far-zin-meters
         :documentation "about-slot")
   (hmd-caps :initarg :hmd-caps
         :initform (error ":hmd-caps must be specified")
         ;; :accessor hmd-caps
         :documentation "about-slot")
   (tracking-caps :initarg :tracking-caps
         :initform (error ":tracking-caps must be specified")
         ;; :accessor tracking-caps
         :allocation :class :instance
         :documentation "about-slot")
   (distortion-caps :initarg :distortion-caps
         :initform (error ":distortion-caps must be specified")
         :documentation "about-slot")
   (default-eye-fov :initarg :default-eye-fov
         :initform (error ":default-eye-fov must be specified")
         ;; :accessor default-eye-fov
         :documentation "about-slot")
   (max-eye-fov :initarg :max-eye-fov
         :initform (error ":max-eye-fov must be specified")
         ;; :accessor max-eye-fov
         :documentation "about-slot")
   (eye-render-order :initarg :eye-render-order
         :initform (error ":eye-render-order must be specified")
         ;; :accessor eye-render-order
         :documentation "about-slot")
   (eye-render-order-symbol :initarg :eye-render-order-symbol
         :initform (error ":eye-render-order-symbol must be specified")
         ;; :accessor eye-render-order-symbol
         :documentation "about-slot")
   (resolution :initarg :resolution
         :initform (error ":resolution must be specified")
         ;; :accessor resolution
         :documentation "about-slot")
   (window-pos :initarg :window-pos
         :initform (error ":window-pos must be specified")
         ;; :accessor window-pos
         :documentation "about-slot")
   (display-device-name :initarg :display-device-name
         :initform (error ":display-device-name must be specified")
         ;; :accessor display-device-name
         :documentation "about-slot")
   (display-id :initarg :display-id
         :initform (error ":display-id must be specified")
         ;; :accessor display-id
         :documentation "about-slot"))
  (:documentation "the class which does oculus rift"))

(defmethod initialize-instance :after ((hmd OVR-HMD ) &key
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
                                                        display-id))
  ""
  (with-slots (is-detected) hmd
    (setf is-detected (%ovrhmd::detect))
    (setf (slot-value hmd handle) handle)
    (setf (slot-value hmd type) type)
    (setf (slot-value hmd product-name) product-name)
    (setf (slot-value hmd manufacturer) manufacturer)
    (setf (slot-value hmd vendor-id) vendor-id)
    (setf (slot-value hmd product-id) product-id)
    (setf (slot-value hmd serial-number) serial-number)
    (setf (slot-value hmd firmware-major) firmware-major)
    (setf (slot-value hmd firmware-minor) firmware-minor)
    (setf (slot-value hmd camera-frustum-hfov-in-radians) camera-frustum-hfov-in-radians)
    (setf (slot-value hmd camera-frustum-vfov-in-radians) camera-frustum-vfov-in-radians)
    (setf (slot-value hmd camera-frustum-near-zin-meters) camera-frustum-near-zin-meters)
    (setf (slot-value hmd camera-frustum-far-zin-meters) camera-frustum-far-zin-meters)
    (setf (slot-value hmd hmd-caps) hmd-caps)
    (setf (slot-value hmd tracking-caps) tracking-caps)
    (setf (slot-value hmd distortion-caps) distortion-caps)
    (setf (slot-value hmd default-eye-fov) default-eye-fov)
    (setf (slot-value hmd max-eye-fov) max-eye-fov)
    (setf (slot-value hmd eye-render-order) eye-render-order)
    (setf (slot-value hmd eye-render-order-symbol) eye-render-order-symbol)
    (setf (slot-value hmd resolution) resolution)
    (setf (slot-value hmd window-pos) window-pos)
    (setf (slot-value hmd display-device-name) display-device-name)
    (setf (slot-value hmd display-id) display-id))
  (format t "detect: ~s HMDs available~%" (%ovrhmd::detect))

;;; ---------------------------------------------------------------------------
(defun test-3bovr ()
  (unwind-protect
       (%ovr::with-ovr ok (:debug nil :timeout-ms 500)
         (unless ok
           (format t "couldn't initialize libovr~%")
           (return-from test-3bovr nil))
         ;; print out some info
         (format t "version: ~s~%" (%ovr::get-version-string))
         (format t "time = ~,3f~%" (%ovr::get-time-in-seconds))
         (format t "detect: ~s HMDs available~%" (%ovrhmd::detect))
         ;; try to open an HMD
         (%ovr::with-hmd (hmd)
           (unless hmd
             (format t "couldn't open hmd 0~%")
             (format t "error = ~s~%"(%ovrhmd::get-last-error (cffi:null-pointer)))
             (return-from test-3bovr nil))
           ;; print out info about the HMD
           (let ((props (%ovr::dump-hmd-to-plist hmd)) ;; decode the HMD struct
                 w h x y)
             (format t "got hmd ~{~s ~s~^~%        ~}~%" props)
             (format t "enabled caps = ~s~%" (%ovrhmd::get-enabled-caps hmd))
             (%ovrhmd::set-enabled-caps hmd '(:low-persistence
                                              :dynamic-prediction))
             (format t "             -> ~s~%" (%ovrhmd::get-enabled-caps hmd))
             ;; turn on the tracking
             (%ovrhmd::configure-tracking hmd
                                          ;; desired tracking capabilities
                                          '(:orientation :mag-yaw-correction
                                            :position)
                                          ;; required tracking capabilities
                                          nil)
             ;; figure out where to put the window
             (setf w (getf (getf props :resolution) :w))
             (setf h (getf (getf props :resolution) :h))
             (setf x (aref (getf props :window-pos) 0))
             (setf y (aref (getf props :window-pos) 1))
             #+linux
             (when (eq (getf props :type) :dk2)
               ;; sdk is reporting resolution as 1920x1080 when screen is
               ;; set to 1080x1920 in twinview?
               (format t "overriding resolution from ~sx~s to ~sx~s~%"
                       w h 1080 1920)
               (setf w 1080 h 1920))
             ;; create window
             (format t "opening ~sx~s window at ~s,~s~%" w h x y)
             (glop:with-window (win
                                "3bovr test window"
                                w h
                                :x x :y y
                                :win-class 'window-HMD
                                :fullscreen t
                                :depth-size 16)
               (setf (slot-value win 'hmd) hmd)
               ;; configure rendering and save eye render params
               ;; todo: linux/mac versions
               (%ovr::with-configure-rendering eye-render-desc
                   (hmd
                    ;; specify window size since defaults don't match on
                    ;; linux sdk with non-rotated dk2
                    :back-buffer-size (list :w w :h h)
                    ;; optional: specify which window/DC to draw into
                    ;;#+linux :linux-display
                    ;;#+linux(glop:x11-window-display win)
                    ;;#+windows :win-window
                    ;;#+windows(glop::win32-window-id win)
                    ;;#+windows :win-dc
                    ;;#+windows (glop::win32-window-dc win)
                    :distortion-caps
                    '(:time-warp :vignette
                      :srgb :overdrive :hq-distortion
                      #+linux :linux-dev-fullscreen))
                 ;; attach libovr runtime to window
                 #+windows
                 (%ovrhmd::attach-to-window hmd
                                            (glop::win32-window-id win)
                                            (cffi:null-pointer) (cffi:null-pointer))
                 ;; configure FBO for offscreen rendering of the eye views
                 (let* ((vaos (gl:gen-vertex-arrays 2))
                        (fbo (gl:gen-framebuffer))
                        (textures (gl:gen-textures 2))
                        (renderbuffer (gl:gen-renderbuffer))
                        ;; get recommended sizes of eye textures
                        (ls (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-left+
                                                           ;; use default fov
                                                           (getf (elt eye-render-desc
                                                                      %ovr::+eye-left+)
                                                                 :fov)
                                                           ;; and no scaling
                                                           1.0))
                        (rs (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-right+
                                                           (getf (elt eye-render-desc
                                                                      %ovr::+eye-right+)
                                                                 :fov)
                                                           1.0))
                        ;; put space between eyes to avoid interference
                        (padding 16)
                        ;; storing both eyes in 1 texture, so figure out combined size
                        (fbo-w (+ (getf ls :w) (getf rs :w) (* 3 padding)))
                        (fbo-h (+ (* 2 padding)
                                  (max (getf ls :h) (getf rs :h))))
                        ;; describe the texture configuration for libovr
                        (eye-textures
                          (loop for v in (list (list :pos (vector padding
                                                                  padding)
                                                     :size ls)
                                               (list :pos (vector
                                                           (+ (* 2 padding)
                                                              (getf ls :w))
                                                           padding)
                                                     :size rs))
                                collect
                                `(:texture ,(first textures)
                                  :render-viewport ,v
                                  :texture-size (:w ,fbo-w :h ,fbo-h)
                                  :api :opengl)))
                        (font (car
                               (conspack:decode-file
                                (asdf:system-relative-pathname '3b-ovr
                                                               "font.met")))))
                   ;; configure the fbo/texture
                   (format t "left eye tex size = ~s, right = ~s~% total =~sx~a~%"
                           ls rs fbo-w fbo-h)
                   (gl:bind-texture :texture-2d (first textures))
                   (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
                   (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
                   (gl:tex-parameter :texture-2d :texture-min-filter :linear)
                   (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
                   (gl:tex-image-2d :texture-2d 0 :srgb8-alpha8 fbo-w fbo-h
                                    0 :rgba :unsigned-int (cffi:null-pointer))
                   (gl:bind-framebuffer :framebuffer fbo)
                   (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                                              :texture-2d (first textures) 0)
                   (gl:bind-renderbuffer :renderbuffer renderbuffer)
                   (gl:renderbuffer-storage :renderbuffer :depth-component24
                                            fbo-w fbo-h)
                   (gl:framebuffer-renderbuffer :framebuffer :depth-attachment
                                                :renderbuffer renderbuffer)
                   (format t "created renderbuffer status = ~s~%"
                           (gl:check-framebuffer-status :framebuffer))
                   (gl:bind-framebuffer :framebuffer 0)

                   ;; load font texture
                   (gl:bind-texture :texture-2d (second textures))
                   (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
                   (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
                   (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
                   (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
                   (let ((png (png-read:read-png-file
                               (asdf:system-relative-pathname '3b-ovr
                                                              "font.png"))))
                     (gl:tex-image-2d :texture-2d 0 :rgb
                                      (png-read:width png) (png-read:height png)
                                      0 :rgb :unsigned-byte
                                      (make-array (* 3
                                                     (png-read:width png)
                                                     (png-read:height png))
                                                  :element-type
                                                  '(unsigned-byte 8)
                                                  :displaced-to
                                                  (png-read:image-data png)))
                     (gl:generate-mipmap :texture-2d)
                     (gl:bind-texture :texture-2d 0))
                   (setf (hud-texture win) (second textures))

                   ;; set up a vao containing a simple 'world' geometry,
                   ;; and hud geometry
                   (setf (world-vao win) (first vaos)
                         (world-count win) (build-world (first vaos))
                         (hud-vao win) (second vaos))
                   (init-hud win)

                   ;; main loop
                   (loop while (glop:dispatch-events win :blocking nil
                                                         :on-foo nil)
                         when font
                         do (update-hud win (hud-text win hmd)
                                          font)
                         do (draw-frame hmd :eye-render-desc eye-render-desc
                                            :fbo fbo
                                            :eye-textures eye-textures
                                            :win win))
                   ;; clean up
                   (gl:delete-vertex-arrays vaos)
                   (gl:delete-framebuffers (list fbo))
                   (gl:delete-textures textures)
                   (gl:delete-renderbuffers (list renderbuffer))
                   (format t "done~%")
                   (sleep 1))))))
         (progn
           (format t "done2~%")))))



#++
(asdf:load-systems '3b-ovr-sample)

#++
(test-3bovr)
;; (defun run ()
;;   (unwind-protect (test-3bovr) (clx:open-display :display )))

;; (defmacro with-display (host (display screen root-window) &body body)
;;   `(let* ((,display (xlib:open-display ,host))
;;           (,screen (first (xlib:display-roots ,display)))
;;           (,root-window (xlib:screen-root ,screen)))
;;      (unwind-protect (progn ,@body)
;;        (xlib:close-display ,display))))


#++
(let ((*default-pathname-defaults* (asdf:system-relative-pathname '3b-ovr "./")))
  (texatl:make-font-atlas-files "font.png" "font.met" 256 256
                                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                                16
                                :dpi 128
                                :padding 4
                                :string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[]= "))
