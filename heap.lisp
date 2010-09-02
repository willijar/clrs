(in-package :clrs)

(declaim (inline hleft hright hparent))
(defun hparent(i) (floor i 2))
(defun hleft(i) (* i 2))
(defun hright(i) (1+ (* i 2)))
(defmacro href(A i) `(aref ,A (1- ,i)))

(defun heapify(A i predicate &key (heap-size (length A)) (key #'identity))
  (labels((test(i j)
            (funcall
             predicate (funcall key (href A i)) (funcall key (href A j))))
          (doheap(i)
            (let ((l (hleft i))
                  (r (hright i))
                  (largest i))
              (when (and (<= l heap-size) (test l i)) (setf largest l))
              (when (and (<= r heap-size) (test r largest)) (setf largest r))
              (unless (= largest i)
                (rotatef (href A i) (href A largest))
                (doheap largest)))))
    (doheap i)))

(defun build-heap(A predicate &key (key #'identity))
  (do((i (floor (length A) 2) (1- i)))
     ((= i 0))
    (heapify A i predicate :key key)))

(defun heapsort(A predicate &key (key #'identity))
  (build-heap A predicate :key key)
  (do((i (length A) (1- i)))
     ((= i 1))
    (rotatef (href A 1) (href A i))
    (heapify A 1 predicate :heap-size (1- i) :key key)))

(defun heap-maximum(A) (href A 1))

(defun heap-extract(A predicate &key (key #'identity))
  (when (< (length A) 1)
    (underflow A))
  (let ((max (href A 1)))
    (setf (href A 1) (href A (length A)))
    (decf (fill-pointer A))
    (heapify A 1 predicate :key key)
    max))

#|
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

(defstruct(binary-heap (:include vector-implementation))
  (key-fn #'identity :type function :read-only t)
  (comp-fn #'< :type function :read-only t))


|#