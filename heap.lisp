
(defstruct(binary-heap (:include vector-implementation)))

(declaim (inline hleft hright hparent href (setf href)))
(defun hparent(i) (declare (fixnum i)) ((the fixnum) (/ i 2)))
(defun hleft(i) (declare (fixnum i)) ((the fixnum) (* i 2)))
(defun hright(i) (declare (fixnum i)) ((the fixnum) (1+ (* i 2))))
(defun href(array i) (declare (vector array) (fixnum i)) (aref array (1- i)))

(defun (setf href)(value array i)
  (declare (vector array) (fixnum i))
  (setf (aref array (1- i)) value))

(defun heapify(A i predicate &key (heap-size (length A)) (key #'identity))
  (declare (vector A) (fixnum i))
  (let ((l (left i))
        (r (right i))
        (largest i))
    (declare (fixnum l r largest))
    (flet((test(a b)
            (declare (fixnum a b))
            (funcall
             predicate (funcall key (href A a)) (funcall key (href A b)))))
      (when (and (<= l heap-size) (test l i)) (setf largest l))
      (when (and (<= r heap-size) (test r largest)) (setf largest r))
      (unless (= largest i)
        (rotatef (href A i) (href A largest)))
      (heapify A largest))))

(defun build-heap(A predicate &key (key #'identity))
  (declare (vector A) (function predicate key))
  (do((i (/ (length A) 2) (1- i)))
     ((< i 1))
    (declare (fixnum i))
    (heapify A i predicate :key key)))

(defun heapsort(A predicate &key (key #'identity))
  (build-max-heap A predicate :key key)
  (do((i (length A) (1- i)))
      ((< i 2))
    (rotatef (href A 1) (href A i))
    (heapify A 1 predicate :heap-size (1- i) :key key)))

(defun heap-maximum(A) (href A 1))

(defun heap-extract(A predicate &key (key #'identity))
  (when (< (length A) 1)
    (error 'underflow :structure A))
  (let ((max (href A 1)))
    (setf (href A 1) (href A (length A)))
    (decf (fill-pointer A))
    (heapify A 1 predicate :key key)
    max))

(defun heap-key-changed(A i predicate &key (key #'identity))
  "Note this only work if key is increased in a minimum heap or decreased in a maximum heap"
    (do((i i p)
        (p (hparent i) (hparent i)))
       ((or (= i 1) (funcall predicate (funcall key (href A p)) (funcall key (href A i)))))
      (rotatef (href A i) (href A p))))

(defun heap-insert(A value predicate &key (key identity) (extension +standard-heap-extend-size+))
  (vector-push-extend value A extension)
  (heap-change-key A (length A) value))

(defun heap-delete(A i predicate &key (key #'identity) (extension +standard-heap-extend-size+))
  (setf (href A i) (href A (length A)))
  (decf (fill-pointer A))
  (when (and extension (> (- (array-dimension A 0) (length A)) extension))
    (adjust-array A (fill-pointer A)))
  (heapify A i predicate &key key))

|#
