
(in-package :clem-benchmark)

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0))
      (n (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 2d0)))
  (with-matrix-benchmark (:add-1024-1024/double-float)
    (m+ m n)))

(let ((m (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 1d0))
      (n (make-instance 'double-float-matrix :dimensions '(1024 1024) :initial-element 2d0)))
  (with-matrix-benchmark (:add-in-place-1024-1024/double-float)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0))
      (n (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 2s0)))
  (with-matrix-benchmark (:add-1024-1024/single-float)
    (m+ m n)))

(let ((m (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 1s0))
      (n (make-instance 'single-float-matrix :dimensions '(1024 1024) :initial-element 2s0)))
  (with-matrix-benchmark (:add-in-place-1024-1024/single-float)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/sb8)
    (m+ m n)))

(let ((m (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-in-place-1024-1024/sb8)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'sb16-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb16-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/sb16)
    (m+ m n)))

(let ((m (make-instance 'sb16-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb16-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-in-place-1024-1024/sb16)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'sb32-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb32-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/sb32)
    (m+ m n)))

(let ((m (make-instance 'sb32-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'sb32-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-in-place-1024-1024/sb32)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/ub8)
    (m+ m n)))

(let ((m (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub8-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-in-place-1024-1024/ub8)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'ub16-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub16-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/ub16)
    (m+ m n)))

(let ((m (make-instance 'ub16-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub16-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-in-place-1024-1024/ub16)
    (mat-add m n :in-place t)))

(let ((m (make-instance 'ub32-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub32-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-1024-1024/ub32)
    (m+ m n)))

(let ((m (make-instance 'ub32-matrix :dimensions '(1024 1024) :initial-element 1))
      (n (make-instance 'ub32-matrix :dimensions '(1024 1024) :initial-element 2)))
  (with-matrix-benchmark (:add-in-place-1024-1024/ub32)
    (mat-add m n :in-place t)))

