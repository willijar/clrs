(in-package :clrs)

(declaim (inline hleft hright hparent))
(defun hparent(i) (floor i 2))
(defun hleft(i) (* i 2))
(defun hright(i) (1+ (* i 2)))
(defmacro href(A i) `(aref ,A (1- ,i)))

(defun heapify(A i predicate key &optional (heap-size (length A)) index-fn)
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
                (when index-fn
                  (funcall index-fn (href A i) i)
                  (funcall index-fn (href A largest) largest))
                (doheap largest)))))
    (doheap i)))

(defun build-heap(A predicate key &optional index-fn)
  (do((i (floor (length A) 2) (1- i)))
     ((= i 0))
    (heapify A i predicate key (length A) index-fn)))

(defun heapsort(A predicate key)
  (build-heap A predicate key)
  (do((i (length A) (1- i)))
     ((= i 1))
    (rotatef (href A 1) (href A i))
    (heapify A 1 predicate key (1- i))))

(defun heap-maximum(A) (href A 1))

(defun heap-extract(A predicate key &optional index-fn)
  (when (< (length A) 1)
    (underflow A))
  (let ((max (href A 1)))
    (setf (href A 1) (href A (length A)))
    (when index-fn
      (funcall index-fn (href A 1) 1)
      (funcall index-fn max -1))
    (decf (fill-pointer A))
    (heapify A 1 predicate key (length A) index-fn)
    max))

(defun heap-key-changed(A i predicate key-fn &optional index-fn)
   "Note this only work if key is increased in a minimum heap or
decreased in a maximum heap"
     (do((i i p)
         (p (hparent i) (hparent i)))
        ((or (= i 1)
             (funcall predicate
                      (funcall key-fn (href A p))
                      (funcall key-fn (href A i)))) i)
       (rotatef (href A i) (href A p))
       (when index-fn
         (funcall index-fn (href A i) i)
         (funcall index-fn (href A p) p))))

(defun heap-insert(A value predicate key &optional index-fn)
  (funcall index-fn value (1+ (vector-push value A)))
  (heap-key-changed A (length A) predicate key index-fn))

(defun heap-delete(A i predicate key &optional index-fn)
  (when index-fn
    (funcall index-fn (href A i) -1)
    (funcall index-fn (href A (length A)) i))
  (setf (href A i) (href A (length A)))
  (decf (fill-pointer A))
  (heapify A i predicate key (length A) index-fn))

(defstruct(binary-heap
            (:include vector-implementation)
            (:constructor %make-binary-heap))
  (key-fn #'identity :type function :read-only t)
  (comp-fn #'< :type function :read-only t)
  (index-fn nil :type (or function null) :read-only t))

(defun make-binary-heap(&key (initial-size  +standard-heap-allocation-size+)
                           (adjustable t)
                           (element-type t)
                           (key-fn #'identity)
                           (comp-fn #'<)
                           (index nil))
  (%make-binary-heap
   :vector (make-array initial-size
                       :element-type element-type
                       :fill-pointer 0
                       :adjustable adjustable)
   :key-fn key-fn
   :comp-fn comp-fn
   :index-fn
   (etypecase index
     (null nil)
     (function index)
     (vector
      (lambda(a &optional b)
        (if b (setf (aref index a) b) (aref index a))))
     (hash-table
      (lambda(a &optional b)
        (if b (if (< b 0) (remhash a index) (setf (gethash a index) b))
            (gethash a index))))
     (symbol
      (ecase index
        (vector
         (let ((index (make-array initial-size :element-type 'fixnum)))
           (lambda(a &optional b)
             (if b (setf (aref index a) b) (aref index a)))))
        (hash-table
         (let ((index (make-hash-table)))
           (lambda(a &optional b)
             (if b (if (< b 0) (remhash a index) (setf (gethash a index) b))
                 (gethash a index))))))))))

(defmethod size((h binary-heap)) (length (binary-heap-vector h)))
(defmethod empty-p((h binary-heap)) (zerop (length (binary-heap-vector h))))

(defmethod enqueue(x (h binary-heap))
  (let ((v (binary-heap-vector h)))
    (when (= (fill-pointer v) (array-dimension v 0))
      (restart-case
          (error 'overflow :structure h)
        (extend(&optional (extension +standard-heap-allocation-size+))
          :report "Extend binary heap"
          :interactive (lambda() (format t "Enter entension: ") (list (read)))
          (setf (binary-heap-vector h)
                (setf v (adjust-array v (+ (length v) extension)))))))
    (heap-insert v x (binary-heap-comp-fn h) (binary-heap-key-fn h)
                 (binary-heap-index-fn h))))

(defmethod insert(x (h binary-heap)) (enqueue x h))

(defmethod dequeue((h binary-heap))
  (heap-extract (binary-heap-vector h)
                (binary-heap-comp-fn h)
                (binary-heap-key-fn h)
                (binary-heap-index-fn h)))

(defmethod peek((h binary-heap))
  (let ((A (binary-heap-vector h)))
    (unless (< (length A) 1)
      (underflow A))
    (href A 1)))

(defmethod key-changed(x (h binary-heap))
  (let ((f (binary-heap-index-fn h)))
    (unless f (error "key-changed not implemented for unindexed heap"))
    (when (eql (binary-heap-key-fn h) #'identity)
      (error "cannot use key-changed if object key is identity - use
      delete and insert instead"))
    (let ((i (funcall f x)))
      (unless (>= i 0) (error "~A not found in ~A" x h))
      (heap-key-changed (binary-heap-vector h)
                        i
                        (binary-heap-comp-fn h)
                        (binary-heap-key-fn h)
                        f))))

(defmethod delete(x (h binary-heap))
  (let ((f (binary-heap-index-fn h)))
    (unless f (error "delete not implemented for unindexed heap"))
    (let ((i (funcall f x)))
      (when (>= i 0)
        (heap-delete (binary-heap-vector h)
                     i
                     (binary-heap-comp-fn h)
                     (binary-heap-key-fn h)
                     f)))))

(let ((fib (make-array 2 :element-type '(integer 0)
                         :adjustable t
                         :initial-contents '(1 2))))
(defun fibonacci(i)
  (when (>= i (length fib))
    (setf fib (adjust-array fib (1+ i) :initial-element 0)))
  (let ((v (aref fib i)))
    (if (zerop v)
        (setf (aref fib i) (+ (fibonacci (- i 2)) (fibonacci (1- i))))
        v))))