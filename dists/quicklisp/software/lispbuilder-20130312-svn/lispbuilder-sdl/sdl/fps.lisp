
(in-package #:lispbuilder-sdl)


(defgeneric (setf target-frame-rate) (rate fpsmngr)
  (:documentation "Set the target frame rate for the game loop.
RATE > 0 will lock the game loop to the specified frame rate, and
calculate the average frame rate over a number of frames.
RATE = 0 will unlock the frame rate, and calculate the average
frame rate over a number of frames.
RATE < 0 will unlock the frame rate. The average frane rate is 
not calculated"))

(defgeneric process-timestep (fpsmngr fn)
  (:documentation "Manages the timestep. Called once per game loop."))

(defclass fps-manager ()
  ((world :accessor world :initform 1000)
   (index :accessor index :initform 0)
   (window :accessor average-window :initform nil)
   (not-through-p :accessor not-through-p :initform nil)
   ;;(calculated :accessor average-fps :initform 0)
   (current-ticks :accessor current-ticks :initform (sdl-cffi::sdl-get-ticks))
   (last-ticks :accessor last-ticks :initform (sdl-cffi::sdl-get-ticks))
   (delta-ticks :accessor delta-ticks :initform 0))
  (:default-initargs
   :window 60))

(defclass fps-fixed (fps-manager)
  ((target-frame-rate :reader target-frame-rate)
   (frame-count :accessor frame-count :initform 0 :initarg :frame-count)
   (rate-ticks :accessor rate-ticks)
   (delay-ticks :accessor delay-ticks :initform (sdl-cffi::sdl-get-ticks))
   (upper-limit :accessor upper-limit :initform 1000 :initarg :upper-limit)
   (lower-limit :accessor lower-limit :initform 1 :initarg :lower-limit)
   (max-dt :initform 500 :initarg :max-dt))
  (:default-initargs
   :target-frame-rate 30))

(defclass fps-timestep (fps-manager)
  ((fps-ticks
    :accessor fps-ticks
    :type integer
    :initform 0
    :initarg :time)
   (dt
    :reader _dt_
    :initform 10
    :initarg :dt)
   (max-dt
    :initform 100  
    :initarg :max-dt)
   (accumulator
    :accessor accumulator
    :type integer
    :initform 0
    :initarg :accumulator)))

(defclass fps-mixed (fps-timestep fps-fixed)
  ((fps-ticks
    :accessor fps-ticks
    :type integer
    :initform 0
    :initarg :time)
   (dt
    :reader _dt_
    :initform 10
    :initarg :dt)
   (max-dt
    :initform 100  
    :initarg :max-dt)
   (accumulator
    :accessor accumulator
    :type integer
    :initform 0
    :initarg :accumulator)))

(defclass fps-unlocked (fps-timestep)
  ((physics-hook-function
    :accessor ps-fn
    :initform #'(lambda (fps-time dt)
                  (declare (ignorable fps-time dt))
                  nil)
    :initarg :ps-fn)))

(defmethod initialize-instance :after ((self fps-manager)
                                       &key
                                       window
                                       &allow-other-keys)
  (when window
    (setf (average-window self) (make-array window :initial-element 0))))

(defmethod initialize-instance :after ((self fps-fixed)
                                       &key
                                       target-frame-rate
                                       &allow-other-keys)
  (when target-frame-rate
    (setf (target-frame-rate self) target-frame-rate)))

(defgeneric _average-fps_ (fps-manager))
(defmethod _average-fps_ ((self fps-manager))
  (with-slots (not-through-p) self
    (if not-through-p
      not-through-p
      (let ((window-length (1- (length (average-window self)))))
        (loop :repeat window-length
              :with window = (average-window self)
              :with index = (index self)
              :with i-start = (if (< index window-length) (1+ index) 0)
              :with j-start = (if (< i-start window-length) (1+ i-start) 0)
              :for i = i-start :then (if (< i window-length) (1+ i) 0)
              :for j = j-start :then (if (< j window-length) (1+ j) 0)
              :summing (- (svref window j)
                          (svref window i)) :into total
              :finally (return (/ 1000 (/ total window-length))))))))

(defun calculate-time-scale (fps-manager delta-ticks)
  (/ delta-ticks (world fps-manager)))

(defmethod _dt_ ((self fps-fixed))
  (calculate-time-scale self (delta-ticks self)))

(defmethod (setf target-frame-rate) :around (rate (self fps-fixed))
  (with-slots (target-frame-rate) self
    (if (and (numberp rate) (zerop rate))
      ;; Zero is the same as NIL
      (setf target-frame-rate nil)
      (setf target-frame-rate rate))
    (setf (not-through-p self) target-frame-rate)
    (call-next-method)
    target-frame-rate))

(defmethod (setf target-frame-rate) (rate (self fps-fixed))
  (declare (ignorable rate))
  (with-slots (target-frame-rate) self
    (if (numberp target-frame-rate)
      (when (and (>= target-frame-rate (lower-limit self))
                 (<= target-frame-rate (upper-limit self)))
        (setf (frame-count self) 0
              (rate-ticks self) (truncate (/ 1000 target-frame-rate)))))))

(defmethod (setf target-frame-rate) (rate (self fps-timestep))
  (declare (ignorable rate self))
  nil)

(defmethod (setf target-frame-rate) (rate (self fps-mixed))
  (declare (ignorable rate))
  (with-slots (target-frame-rate) self
    (if (numberp target-frame-rate)
      (when (and (>= target-frame-rate (lower-limit self))
                 (<= target-frame-rate (upper-limit self)))
        (setf (frame-count self) 0
              (rate-ticks self) (truncate (/ 1000 target-frame-rate)))))))

(defmethod process-timestep :around ((self fps-manager) fn)
  (declare (ignorable fn))
  (with-slots (current-ticks delta-ticks last-ticks index window not-through-p)
      self
    (setf current-ticks (sdl-cffi::sdl-get-ticks)
          delta-ticks (- current-ticks last-ticks))
    (setf (svref window index) current-ticks)
    
    (when not-through-p
      (when (>= index (1- (length window)))
        (setf not-through-p nil)))
  
    (call-next-method)

    (unless (< (incf index) (length window))
      (setf index 0))
  
    (setf last-ticks current-ticks)))

(defmethod process-timestep ((self fps-manager) fn)
  (when fn (funcall fn)))

;;;; --------------------------
;;;; Lock the game loop to a specified rate
;;;; This is a reimplementation of the algorithm for SDL_gfx
;;;; From http://www.ferzkopp.net/joomla/content/view/19/14/

(defmethod process-timestep :after ((self fps-fixed) fn)
  (declare (ignorable fn))
  (with-slots (target-frame-rate frame-count rate-ticks current-ticks delay-ticks
                                 max-dt) self
    ;; Delay game loop, if necessary
    (when target-frame-rate
      (incf frame-count)
      (let ((delta (truncate (- (+ delay-ticks (* frame-count rate-ticks))
                                current-ticks))))
        (if (> delta 0)
          (sdl-cffi::sdl-delay (if (> delta max-dt) max-dt delta))
          (setf frame-count 0
                delay-ticks current-ticks))))))

;;;; --------------------------
;;;; Lock timestep to Specified Rate
;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/
(defmethod process-timestep :before ((self fps-unlocked) fn)
  (declare (ignorable fn))
  (with-slots (fps-ticks delta-ticks dt max-dt accumulator physics-hook-function)
      self
    (incf accumulator (if (> delta-ticks max-dt) max-dt delta-ticks))
    (loop until (< accumulator dt) do
          (progn
            (when physics-hook-function
              (funcall physics-hook-function fps-ticks dt))
            (incf fps-ticks dt)
            (decf accumulator dt)))))

(defun (setf frame-rate) (rate &optional (fpsmanager *default-fpsmanager*))
  (setf (target-frame-rate fpsmanager) rate))

;;;; (defun (setf frame-rate) (rate &optional fpsmanager)
;;;;   (setf (target-frame-rate *default-fpsmanager*) rate))

(defun frame-rate (&optional (fpsmanager *default-fpsmanager*))
    "Manage the target frame rate for the game loop.
`RATE` > `0` will lock the game loop to the specified frame rate, and
calculate the average frame rate over a number of frames.
`RATE` = `0` will unlock the frame rate, and calculate the average
frame rate over a number of frames.
`RATE` < `0` will unlock the frame rate. The average frane rate is 
not calculated.

See [WITH-EVENTS](#with-events), and [AVERAGE-FPS](#average-fps)."
  (target-frame-rate fpsmanager))

(defun time-scale (&optional (fpsmanager *default-fpsmanager*))
  (calculate-time-scale fpsmanager (delta-ticks fpsmanager)))
(defun frame-time (&optional (fpsmanager *default-fpsmanager*))
  "Returns how long current frame time is"
  (calculate-time-scale fpsmanager (delta-ticks fpsmanager)))

(defun average-fps (&optional (fpsmanager *default-fpsmanager*))
  "Returns the average frame rate of the event loop calculated over a sliding window
of 'n' frames."
  (_average-fps_ fpsmanager))

(defun dt (&optional (fpsmanager *default-fpsmanager*))
  (_dt_ fpsmanager))

(defun max-dt (&optional (fpsmanager *default-fpsmanager*))
  (slot-value fpsmanager 'max-dt))

(defun ticks (&optional (fpsmanager *default-fpsmanager*))
  (fps-ticks fpsmanager))

(defun physics-hook-p (&optional (fpsmanager *default-fpsmanager*))
  (ps-fn fpsmanager))
(defun set-physics-hook (fn &optional (fpsmanager *default-fpsmanager*))
  (setf (ps-fn fpsmanager) fn))
(defsetf physics-hook-p set-physics-hook)

(defun system-ticks ()
  (sdl-cffi::sdl-get-ticks))


;;;; --------------------------
;;;; Lock timestep to Specified Rate
;;;; From http://www.gaffer.org/game-physics/fix-your-timestep/
(defmacro with-timestep (&body body)
  `(progn
     (incf (accumulator *default-fpsmanager*) (if (> (delta-ticks *default-fpsmanager*)
                                                     (max-dt *default-fpsmanager*))
                                                (max-dt *default-fpsmanager*)
                                                (delta-ticks *default-fpsmanager*)))
     (loop until (< (accumulator *default-fpsmanager*) (dt *default-fpsmanager*)) do
           (progn
             ,@body
             (incf (fps-ticks *default-fpsmanager*) (dt *default-fpsmanager*))
             (decf (accumulator *default-fpsmanager*) (dt *default-fpsmanager*))))))

(defmacro with-frame-rate (&body body)
  ;;
  ;; around
  (let ((self  (gensym "self-"))
        (delta (gensym "delta-")))
    `(let ((,self *default-fpsmanager*)
           (,delta nil))
       
       (setf (current-ticks ,self) (sdl-cffi::sdl-get-ticks)
             (delta-ticks ,self) (- (current-ticks ,self) (last-ticks ,self)))
       (setf (svref (average-window ,self) (index ,self)) (current-ticks ,self))
       
       (when (not-through-p ,self)
         (when (>= (index ,self) (1- (length (average-window ,self))))
           (setf (not-through-p ,self) nil)))

       ;; call-next-method
       ,@body

       ;;
       ;; after
       (when (target-frame-rate ,self)
         (incf (frame-count ,self))
         (let ((,delta (truncate (- (+ (delay-ticks ,self) (* (frame-count ,self)
                                                             (rate-ticks ,self)))
                                   (current-ticks ,self)))))
           (if (> ,delta 0)
             (sdl-cffi::sdl-delay (if (> ,delta (max-dt ,self))
                                    (max-dt ,self)
                                    ,delta))
             (setf (frame-count ,self) 0
                   (delay-ticks ,self) (current-ticks ,self)))))

       ;;
       ;; around
       (unless (< (incf (index ,self)) (length (average-window ,self)))
         (setf (index ,self) 0))
       (setf (last-ticks ,self) (current-ticks ,self)))))
  
