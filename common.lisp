;;; Common definitions for CLRS
;;; J.A.R. Williams 2010

(in-package :clrs)

(defgeneric insert(x s)
  (:documentation "Augment set s with element x. It is assumed that
  attributes of element x needed by the set implementation have
  already been initialised."))

(defgeneric search(k s)
  (:documentation "Given a set s and a key value k,
  return the lowest rank element x in S such that x.key = k, or nil if no such
  element belongs to x"))

(defgeneric delete(x s)
  (:documentation "Given an element x in set s, removes x from s. Note
  this operations takes the element x, not a key value. Return true if
  object found and deleted."))

(defgeneric rank(x s)
  (:documentation "Given an ordered set s return the 0 based rank of
  element x in that set or nil if x not present"))

(defgeneric rank-lookup(r s)
  (:documentation "Return the element x in sequence s with rank r."))

(defgeneric successor(x s)
  (:documentation "Given an element x whose key is from a totally
  ordered set s, return the next larger element in s or nil of x is
  the maximum element"))

(defgeneric predecessor(x s)
  (:documentation "Given an element x whose key is from a totally
  ordered set s, return the next smaller element in s or nil of x is
  the minimum element"))

(defgeneric size(s)
  (:documentation "Return the number of elements stored in a data structure"))

(defgeneric empty-p(s)
  (:documentation "Return true if a data structure is empty"))

(defgeneric traverse(f s)
  (:documentation "For each entry in a data structure call the
  designated function with the value. Entries will be
  visited in order if applicable"))

(defgeneric push(x s)
  (:documentation "Augment stack s with element x"))

(defgeneric pop(s)
  (:documentation "pop last element from stack s"))

(defgeneric peek(s)
  (:documentation "Return value of next element on set s without
  removing it."))

(defgeneric enqueue(x q)
  (:documentation "Add element x to end of queue q"))

(defgeneric dequeue(q)
  (:documentation "Return next element x from queue q"))

(defgeneric key-changed(x h)
  (:documentation "Notify set h that key value of element x has changed"))

(defparameter +standard-heap-allocation-size+ 16
  "The standard allocation block size for the underlying arrays.")

(defparameter +standard-heap-extend-size+ 16
  "The standard size by which the underlying arrays are augmented.")

(defstruct (vector-implementation (:conc-name implementation-))
  (vector
   (make-array +standard-heap-allocation-size+ :fill-pointer 0 :adjustable t)
   :type vector))

(defstruct (list-implementation (:conc-name implementation-))
  (head nil :type list))

(define-condition clrm-error(error)
  ((structure :reader error-structure :initarg :structure))
  (:report (lambda(c strm)
             (format strm "CLRM Error in ~A" (error-structure c))))
  (:documentation "Base for all data structure errors"))

(define-condition overflow(clrm-error)
  ()
  (:documentation "Data structure Overflow")
  (:report (lambda(c strm) (format strm "Overflow: ~A" (error-structure c)))))

(define-condition underflow(clrm-error)
  ()
  (:documentation "Data structure Underflow")
  (:report (lambda(c strm) (format strm "Underflow: ~A" (error-structure c)))))

(defgeneric underflow(s)
  (:documentation "Called when a data structre underflows")
  (:method(s)
    (restart-case
        (error 'underflow :structure s)
      (use-value(v)
        :report "Use a value"
      :interactive (lambda()(format t "Enter a new value: ")
                          (multiple-value-list (eval (read))))
      v))))

(defgeneric overflow(s)
  (:documentation "Called when a structure overflows"))

(defun invalid-index-error(datum structure &key (min 0) (max (size structure)))
  (error 'simple-type-error :datum datum :expected-type `(INTEGER ,min ,max)
         :format-control "invalid index ~D in ~A"
         :format-arguments (list datum structure)))

;; derivative implementations
(defmethod successor(x s)
  (let ((r (rank x s)))
    (when (and r (< r (- (size s) 2)))
      (rank-lookup (1+ r) s))))

(defmethod predecessor(x s)
  (let ((r (rank x s)))
    (when (and r (> r 0))
      (rank-lookup (1- r) s))))


(defmethod empty-p(s)
  (zerop (size s)))

(defmethod size((s list-implementation)) (length (implementation-head s)))

(defmethod size((s vector-implementation)) (length (implementation-vector s)))

(defmethod empty-p((s list-implementation)) (null (implementation-head s)))

(defmethod rank(x (s list-implementation))
  (position x (implementation-head s)))

(defmethod rank-lookup(r (q list-implementation))
  (let ((l (when (>= r 0) (nthcdr r (list-queue-head q)))))
    (if l
      (car l)
      (invalid-index-error r q))))

(defmethod predecessor(x (q list-implementation))
  (mapl
   #'(lambda(l) (when (eql x (second l)) (return-from predecessor (first l))))
   (implementation-head q))
  nil)

(defmethod successor(x (q list-implementation))
  (mapl
   #'(lambda(l) (when (eql x (first l)) (return-from successor (second l))))
   (implementation-head q))
  nil)

(defmethod traverse(f (q list-implementation))
  (cl:map 'nil f (implementation-head q)))

(defmethod peek(s)
  (if (empty-p s)
      (underflow s)
      (rank-lookup 0 s)))
