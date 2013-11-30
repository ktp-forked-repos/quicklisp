
(in-package :clem-test)


(defmacro with-benchmark (&body body)
  (let ((start-var (gensym))
        (end-var (gensym)))
    `(let ((,start-var (get-internal-run-time)))
       (values
        (multiple-value-list
         (progn ,@body))
        (let ((,end-var (get-internal-run-time)))
          (- ,end-var ,start-var))))))

(defun matrix-bench-1 ()
  (let ((x (random-matrix 256 256 :matrix-class 'double-float-matrix :limit 255d0)))
    (print-range x 0 3 0 3)
    (let ((xb1 (time (copy-to-ub8-matrix (gaussian-blur x :k 1 :sigma 1))))
	  (xb2 (gaussian-blur x :k 1 :sigma 1)))
      (time (dotimes (i 10) (setf xb2 (gaussian-blur xb2 :k 1 :sigma 1))))
      (print-range xb1 0 3 0 3)
      (print-range xb2 0 3 0 3)
      (print (cons (class-of xb1) (class-of xb2)))
      (let ((ub2 (copy-to-ub8-matrix (clem::subset-matrix xb2 10 267 10 267))))
	(print-range ub2 0 3 0 3)
	(print (dim xb1))
	(print (dim ub2))
	(let ((q (time (mat-subtr xb1 ub2 :matrix-class 'sb16-matrix))))
	  (print q)
	  (print (sum q)))))))

(defun matrix-bench-2 ()
  (let* ((r 64) (c 64)
	 (x (copy-to-ub8-matrix (normalize (random-matrix r c)))))
    (time
     (dotimes (i r)
       (dotimes (j c)
	 (val x i j))))))

(defun matrix-bench-3 ()
  (let* ((x (random-double-float-matrix 512 512 :max 255d0))
	 (y (random-double-float-matrix 5 5 :max 255d0)))
    (time
     (let ((conv (discrete-convolve x y :truncate t)))))))

(defun matrix-bench-4 ()
  (let* ((x (random-ub8-matrix 512 512 :max 255))
	 (y (random-ub8-matrix 5 5 :max 255)))
    (time
     (let ((conv (discrete-convolve x y
                                    :matrix-class 'ub32-matrix
                                    :truncate t
                                    :norm-v nil)))))))

(defun matrix-bench-5 ()
  (let* ((x (random-double-float-matrix 512 512 :max 1.0d0))
	 (y (random-double-float-matrix 5 5 :max 1.0d0)))
    (time
     (let ((conv (discrete-convolve x y
                                    :matrix-class 'double-float-matrix
                                    :truncate t
                                    :norm-v nil)))))))


(defun matrix-bench-6 ()
  (let ((x (random-matrix 512 512
                          :matrix-class 'double-float-matrix
                          :limit 255d0)))
    (print-range x  0 5 0 5)
    (let ((xb1 (time (gaussian-blur x :k 3 :sigma 1))))
      (print-range xb1 0 5 0 5)
      (print (cons (dim x) (dim xb1))))))

(defun matrix-bench-7 ()
  (let ((x (make-instance 'double-float-matrix
                          :rows 512 :cols 512 :initial-element 2d0))
        (y (make-instance 'double-float-matrix
                          :rows 512 :cols 512 :initial-element 3d0))
        (z (make-instance 'double-float-matrix
                          :rows 512 :cols 512 :initial-element 0d0)))
    (print-range x  0 5 0 5)
    (let ((xb1 (time (gaussian-blur x :k 3 :sigma 1))))
      (print-range xb1 0 5 0 5)
      (print (cons (dim x) (dim xb1))))))

(defun matrix-bench-8 ()
  (let ((x (make-instance 'double-float-matrix
                          :rows 1024 :cols 1024 :initial-element 2d0))
        (y (make-instance 'double-float-matrix
                          :rows 1024 :cols 1024 :initial-element 3d0))
        (z (make-instance 'double-float-matrix
                          :rows 1024 :cols 1024 :initial-element 0d0)))
    (print-range x 0 5 0 5)
    (let ((xb1 (time (clem::mat-mult3 x y z))))
      (print-range xb1 0 5 0 5)
      (print (cons (dim x) (dim xb1))))))

(defun matrix-bench-9 ()
  (let ((x (make-instance 'double-float-matrix
                          :rows 8 :cols 16 :initial-element 2d0))
        (y (make-instance 'double-float-matrix
                          :rows 16 :cols 8 :initial-element 3d0))
        (z (make-instance 'double-float-matrix
                          :rows 8 :cols 8 :initial-element 0d0)))
    (print-range x 0 5 0 5)
    (let ((xb1 (time (clem::mat-mult3 x y z))))
      (print-range xb1 0 5 0 5)
      (print (cons (dim x) (dim xb1))))))

(defun matrix-bench-10 ()
  (let ((x (make-instance 'double-float-matrix
                          :rows 1024 :cols 1024 :initial-element 2d0))
        (y (make-instance 'double-float-matrix
                          :rows 1024 :cols 1024 :initial-element 3d0))
        (z (make-instance 'double-float-matrix
                          :rows 1024 :cols 1024 :initial-element 0d0)))
    (print-range x 0 5 0 5)
    (let ((xb1 (time (clem::mat-mult-3-block x y z))))
      (print-range xb1 0 5 0 5)
      (print (cons (dim x) (dim xb1))))))

(defun matrix-bench-13 ()
  (declare (optimize (speed 3)
                     (space 0)
                     (safety 0)))
  (let ((x (make-instance 'single-float-matrix
                          :rows 1024 :cols 1024 :initial-element 1f0)))
    (declare (type single-float-matrix x))
    (let ((acc 0f0)
          (rows (rows x))
          (cols (cols x))
          (v (clem::matrix-vals x)))
      (declare (type (simple-array single-float (* *)) v)
               (type single-float acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the single-float (aref v i j))))))
      (print acc))))

(defun matrix-bench/ub8-matrix-add-mref ()
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (declare (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the (unsigned-byte 8) (clem::mref x i j))))))
      (print acc))))

(defun matrix-bench/ub8-matrix-add-aref ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((x (make-instance 'ub8-matrix :rows 2048 :cols 2048 :initial-element 1)))
    (declare (type ub8-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x))
          (v (clem::matrix-vals x)))
      (declare (type (simple-array (unsigned-byte 8) (* *)) v)
               (type (unsigned-byte 32) acc)
               (type fixnum rows cols))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the (unsigned-byte 8) (aref v i j))))))
      (print acc))))

(defun matrix-bench-16 ()
  (let ((x (make-instance 'ub32-matrix :rows 1024 :cols 1024 :initial-element 1)))
    (declare (type ub32-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf acc (+ acc (the (unsigned-byte 32) (clem::mref x i j))))))
      (print acc))))

(defun matrix-bench-18 ()
  (let ((x (make-instance 'bit-matrix :rows 1024 :cols 1024 :initial-element 1)))
    (declare (type bit-matrix x))
    (let ((acc 0)
          (rows (rows x))
          (cols (cols x)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf acc (+ acc (the (unsigned-byte 32) (clem::mref x i j))))))
      (print acc))))

(defun matrix-bench-19 (x)
  (let ((acc 0)
        (rows (rows x))
        (cols (cols x)))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf acc (+ acc (clem::mref x i j)))))
    (print acc)))

(defun matrix-bench-23/aref ()
  (declare (optimize (speed 3)
                     (space 0)
                     (safety 0)))
  (let ((x (make-instance 'double-float-matrix :rows 2048 :cols 2048 :initial-element 2d0)))
    (declare (type double-float-matrix x))
    (let ((acc 0d0)
          (rows (rows x))
          (cols (cols x))
          (v (clem::matrix-vals x)))
      (declare (type double-float acc)
               (type fixnum rows cols)
               (type (simple-array double-float (* *)) v))
      (dotimes (i rows)
        (declare (type fixnum i))
        (dotimes (j cols)
          (declare (type fixnum j))
          (setf acc (+ acc (the double-float (* (aref v i j)
                                                (aref v i j)))))))
      (print acc))))

(defun matrix-bench-24 (x)
  (let ((y (clem:mat-copy-proto x)))
    (declare (type double-float-matrix x))
    (let ((rows (rows x))
          (cols (cols x)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (clem::mref y i j) (clem::mref x i j)))))
    y))

(defun run-bench ()
  ;;  (matrix-bench-3)
  (matrix-bench-4)
  ;;  (matrix-bench-5)
  t)


