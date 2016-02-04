;; minimal example using glop
(in-package #:cl-vr)

;;; ----------------------------------------------------------------------------
(defclass window-HMD (glop:window)
  ((hmd :reader hmd :initarg :hmd)
   (world-vao :accessor world-vao)
   (count :accessor world-count)
   (hud-vbo :accessor hud-vbo :initform nil)
   (hud-vao :accessor hud-vao :initform nil)
   (hud-count :accessor hud-count)
   (hud-texture :accessor hud-texture)
   (font :accessor font)))

;;; ----------------------------------------------------------------------------
(defparameter *tex-size* 256)
(defparameter *FRONT-BACK* 0.0)
(defparameter *LEFT-RIGHT* 0.0)
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

(defun hud-text (win hmd)
  (declare (ignorable win))
  (format nil "fps: ~s~%~
latency = ~{m2p:~,3,3f ren:~,3,3f tWrp:~,3,3f~%~
          PostPresent: ~,3,3f Err: ~,3,3f~}"
          "??"
          (%ovr::get-float-array
           hmd :dk2-latency 5)))

(defmethod glop:on-event ((window window-HMD) event)
  ;; ignore any other events
  (declare (ignore window event)))

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
     (load "xyz200-1.lisp")
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

(defparameter *w* nil)
(defparameter *move* nil)
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

(defparameter *once* nil)

(defun test-3bovr ()
  (when *once*
    ;; running it twice at once breaks things, so try to avoid that...
    (format t "already running?~%")
    (return-from test-3bovr nil))
  ;; initialize library
  (setf *once* t)
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
           (format t "done2~%")
           (setf *once* nil)
           (format t "done3 ~s~%" *once*)))))



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
