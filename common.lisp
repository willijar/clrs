;;; Common definitions for CLRS
;;; J.A.R. Williams 2010

(in-package :clrs)

(defgeneric search(k s)
  (:documentation "Given a set s and a key value k,
  return the element x in S such that x.key = k, or nil if no such
  element belongs to x"))

(defgeneric insert(x s)
  (:documentation "Augment set s with element x. It is assumed that
  attributes of element x needed by the set implementation have
  already been initialised."))

(defgeneric delete(x s)
  (:documentation "Given an element x in set s, removes x from s. Note
  this operations takes the element x, not a key value. Return true if object found and deleted."))

(defgeneric minimum(s)
  (:documentation "Given a totally ordered set s, return the element of s with the smallest key."))

(defgeneric maximum(s)
  (:documentation "Given a totally ordered set s, return the element of s with the largest key."))

(defgeneric successor(x s)
  (:documentation "Given an element x whose key is from a totally
  ordered set s, return the next larger element in s or nil of x is
  the maximum element"))

(defgeneric predecessor(x s)
  (:documentation "Given an element x whose key is from a totally
  ordered set s, return the next smaller element in s or nil of x is
  the minimum element"))

(defstruct (vector-implementation (:conc-name implementation-))
  (vector
   (make-array +standard-heap-allocation-size+ :fill-pointer 0 :adjustable t)
   :type vector :read-only t))

(defstruct (list-implementation (:conc-name implementation-))
  (head nil :type list))

(defgeneric length(s)
  (:documentation "Return the number of elements stored in a data structure")
  (:method((s sequence)) (cl:length s))
  (:method((s vector-implementation)) (cl:length (implementation-vector s)))
  (:method((s list-implementation)) (cl:length (implementation-head s))))

(defgeneric empty-p(s)
  (:documentation "Return true if a data structure is empty")
  (:method(s) (zerop (length s))))

(defgeneric map(function structure &rest args)
  (:documentation "For each entry in a data structure call the
  designated function with the value and the args. Entries will be
  visited in order if applicable"))

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

(defparameter +standard-heap-allocation-size+ 16
  "The standard allocation block size for the underlying arrays.")

(defparameter +standard-heap-extend-size+ 16
  "The standard size by which the underlying arrays are augmented.")



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


(defmethod delete(x (s vector))
  (let ((start (position x s)))
    (when start
      (do((i start (1+ i)))
         ((= i (1- (length s))))
        (setf (aref s i) (aref s (1+ i))))
      (decf (fill-pointer s)))))

(defmethod map(f (s sequence) &rest args)
  (cl:map 'nil (if args #'(lambda(v) (apply f (cons v args))) f) s))
