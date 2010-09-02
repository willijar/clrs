
(in-package :clrs)

(defgeneric push(x s)
  (:documentation "Augment stack s with element x"))

(defgeneric pop(s)
  (:documentation "pop element x from stack s"))

(defstruct (vector-stack (:include vector-implementation)))
(defstruct (list-stack(:include list-implementation)))

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
        (adjust-array v (+ (cl:length v) extension))))))

(defmethod push(x (s vector-stack))
  (unless (vector-push x (vector-stack-vector s))
    (overflow s)
    (vector-push x (vector-stack-vector s)))
  x)

(defmethod pop((s vector-stack))
  (let ((v (vector-stack-vector s)))
    (if (zerop (cl:length v))
        (underflow s)
        (vector-pop v))))

(defmethod pop((s list-stack))
  (if (null (list-stack-head s))
      (underflow s)
      (cl:pop (list-stack-head s))))

;; geneal interface


(defmethod insert(x (s list-stack)) (push x s))

(defmethod insert(x (s vector-stack)) (push x s))

(defmethod delete(x (s list-stack))
  (setf (implementation-head s) (cl:delete x (implementation-head s))))

(defmethod delete(x (s vector-stack))
  (delete x (implementation-vector s)))

(defmethod length((s list-stack)) (cl:length (implementation-head s)))
(defmethod length((s vector-stack)) (cl:length (implementation-vector s)))
(defmethod empty-p((s list-stack)) (null (implementation-head s)))

(defun invalid-index-error(datum structure &key (min 0) (max (length structure)))
  (error 'simple-type-error :datum datum :expected-type `(INTEGER ,min ,max)
         :format-control "invalid index ~D in ~A"
         :format-arguments (list datum structure)))

(defmethod search((k integer) (s vector-stack))
  (let* ((v (implementation-vector s))
         (n (cl:length v)))
    (if (< -1 k n)
        (aref v (- n k 1))
        (invalid-index-error k s))))

(defmethod search((k integer) (s list-stack))
  (let* ((l (implementation-head s))
         (n (cl:length l)))
    (if (< -1 k n)
        (nth (- n k 1) l)
        (invalid-index-error k s))))

(defmethod minimum((s vector-stack))
  (let* ((v (implementation-vector s))
         (n (cl:length v)))
    (if (zerop n)
        (underflow s)
        (aref v (1- n)))))

(defmethod minimum((s list-stack))
  (let ((l (implementation-head s)))
    (if l (first l) (underflow s))))

(defmethod maximum((s vector-stack))
  (let* ((v (implementation-vector s))
         (n (cl:length v)))
    (if (zerop n)
        (underflow s)
        (aref v 0))))

(defmethod maximum((s list-stack))
  (let ((l (implementation-head s)))
    (if l (first (butlast l)) (underflow s))))

(defmethod predecessor(x (s vector-stack))
  (let* ((v (implementation-vector s))
         (p (position x v)))
    (when (and p (< p (- (cl:length v) 2)))
      (aref v (1+ p)))))

(defmethod predecessor(x (s list-stack))
  (mapl
   #'(lambda(l) (when (eql x (second l)) (return-from predecessor (first l))))
   (implementation-head s))
  nil)

(defmethod successor(x (s vector-stack))
  (let* ((v (implementation-vector s))
         (p (position x v)))
    (when (and p (> p 0))
      (aref v (1- p)))))

(defmethod successor(x (s list-stack))
  (mapl
   #'(lambda(l) (when (eql x (first l)) (return-from successor (second l))))
   (implementation-head s))
  nil)

(defmethod map(f (s list-stack) &rest args)
  (cl:map 'nil (if args #'(lambda(v) (apply f (cons v args))) f)
          (implementation-head s)))

(defmethod map(f (s vector-stack) &rest args)
  (let* ((f (if args #'(lambda(v) (apply f (cons v args))) f))
         (v (implementation-vector s))
         (n (cl:length v)))
    (dotimes(i n)
      (funcall f (aref v (- n i 1))))))

;; interface to standard CL types
(defmethod push(x (s vector))
  (unless (vector-push x s)
    (restart-case
        (error 'overflow :structure s)
      (extend(&optional (extension +standard-heap-extend-size+))
        :report "Extend stack"
        :interactive (lambda() (format t "Entension: ") (read))
        (vector-push-extend x s extension)))))

(defmethod pop((s vector))
  (if (zerop (fill-pointer s))
      (underflow s)
      (vector-pop s)))
