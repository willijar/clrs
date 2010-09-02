(in-package :clrs)

(declaim (inline hleft hright hparent))
(defun hparent(i) (floor i 2))
(defun hleft(i) (* i 2))
(defun hright(i) (1+ (* i 2)))
(defmacro href(A i) `(aref ,A (1- ,i)))

(defun heapify(A i predicate key &optional (heap-size (length A)) )
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

(defun build-heap(A predicate key)
  (do((i (floor (length A) 2) (1- i)))
     ((= i 0))
    (heapify A i predicate key)))

(defun heapsort(A predicate key)
  (build-heap A predicate key)
  (do((i (length A) (1- i)))
     ((= i 1))
    (rotatef (href A 1) (href A i))
    (heapify A 1 predicate key (1- i))))

(defun heap-maximum(A) (href A 1))

(defun heap-extract(A predicate key)
  (when (< (length A) 1)
    (underflow A))
  (let ((max (href A 1)))
    (setf (href A 1) (href A (length A)))
    (decf (fill-pointer A))
    (heapify A 1 predicate key)
    max))

(defun heap-key-changed(A i predicate key)
   "Note this only work if key is increased in a minimum heap or decreased in a maximum heap"
     (do((i i p)
         (p (hparent i) (hparent i)))
        ((or (= i 1) (funcall predicate (funcall key (href A p)) (funcall key (href A i)))))
       (rotatef (href A i) (href A p))))

(defun heap-insert(A value predicate key)
  (vector-push value A)
  (heap-key-changed A (length A) predicate key))

(defun heap-delete(A i predicate key)
  (setf (href A i) (href A (length A)))
  (decf (fill-pointer A))
  (heapify A i predicate key))

(defstruct(binary-heap (:include vector-implementation))
  (key-fn #'identity :type function :read-only t)
  (comp-fn #'< :type function :read-only t))

(defmethod length((h binary-heap)) (length (binary-heap-vector h)))
(defmethod empty-p((h binary-heap)) (zerop (length (binary-heap-vector h))))

(defmethod enqueue(x (h binary-heap))
  (let ((v (binary-heap-vector h)))
    (when (= (length v) (array-dimension v 0))
      (restart-case
          (error 'overflow :structure h)
        (extend(&optional (extension +standard-heap-extend-size+))
          :report "Extend binary heap"
          :interactive (lambda() (format t "Enter entension: ") (list (read)))
          (adjust-array v (+ (length v) extension)))))
    (heap-insert v x (binary-heap-comp-fn h) (binary-heap-key-fn h))))

(defmethod insert(x (h binary-heap)) (enqueue x h))

(defmethod dequeue((h binary-heap))
  (heap-extract (binary-heap-vector h)
                (binary-heap-comp-fn h)
                (binary-heap-key-fn h)))

(defmethod search(k (h binary-heap))
  (let ((p (find k (binary-heap-vector h) :key (binary-heap-key-fn h) :test (binary-key-eql-fn h))))
    (when p (aref (binary-heap-vector h) p))))


