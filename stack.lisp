(in-package :clrs)

(defstruct (vector-stack (:include vector-implementation)))
(defstruct (list-stack(:include list-implementation)))

(defmethod empty-p((s vector-stack))
  (zerop (length (vector-stack-vector s))))

(defun make-stack(&key
                  (initial-size +standard-heap-allocation-size+)
                  (element-type t)
                  (adjustable t)
                  (implementation (if initial-size 'vector 'list)))
  (ecase implementation
    (vector
     (make-vector-stack
                    :vector
                    (make-array initial-size
                                :element-type element-type
                                :fill-pointer 0
                                :adjustable adjustable)))
    (list
     (make-list-stack))))

(defmethod push(x (s list-stack)) (cl:push x (list-stack-head s)))

(defmethod overflow((s vector-stack))
  (restart-case
      (error 'overflow :structure s)
    (extend(&optional (extension +standard-heap-extend-size+))
      :report "Extend stack"
      :interactive (lambda() (format t "Enter entension: ") (list (read)))
      (let ((v (implementation-vector s)))
        (adjust-array v (+ (length v) extension))))))

(defmethod push(x (s vector-stack))
  (unless (vector-push x (vector-stack-vector s))
    (overflow s)
    (vector-push x (vector-stack-vector s)))
  x)

(defmethod pop((s vector-stack))
  (let ((v (vector-stack-vector s)))
    (if (zerop (length v))
        (underflow s)
        (vector-pop v))))

(defmethod pop((s list-stack))
  (if (null (list-stack-head s))
      (underflow s)
      (cl:pop (list-stack-head s))))

;; general interface

(defmethod insert(x (s list-stack)) (push x s))

(defmethod insert(x (s vector-stack)) (push x s))

(defmethod delete(x (s list-stack))
  (setf (implementation-head s) (cl:delete x (implementation-head s))))

(defmethod delete(x (s vector-stack))
  (setf (implementation-vector s) (cl:delete x (implementation-vector s))))

(defmethod rank(x (s vector-stack))
  (let* ((v (implementation-vector s))
         (p (position x v)))
    (when p (- (length v) p 1))))

(defmethod rank-lookup((k integer) (s vector-stack))
  (let* ((v (implementation-vector s))
         (n (length v)))
    (if (< -1 k n)
        (aref v (- n k 1))
        (invalid-index-error k s))))

(defmethod traverse(f (s vector-stack))
  (let* ((v (implementation-vector s))
         (n (length v)))
    (dotimes(i n)
      (funcall f (aref v (- n i 1))))))