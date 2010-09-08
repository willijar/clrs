
(in-package :clrs)

(defgeneric enqueue(x q)
  (:documentation "Add element x to end of queue q"))

(defgeneric dequeue(q)
  (:documentation "Return next element x from q"))

(defstruct (vector-queue (:include vector-implementation))
  (head 0 :type fixnum)
  (tail 0 :type fixnum))

(defmethod size((q vector-queue))
  (mod (- (vector-queue-tail q) (vector-queue-head q))
       (length (vector-queue-vector q))))

(defmethod empty-p((q vector-queue))
  (= (vector-queue-head q) (vector-queue-tail q)))

(defstruct (list-queue (:include list-implementation))
  (tail nil :type list))

(defun make-queue(&key
                  (initial-size +standard-heap-allocation-size+)
                  (element-type t)
                  (adjustable nil)
                  (implementation (if initial-size 'vector 'list)))
  (ecase implementation
    (vector
     (make-vector-queue
      :vector (make-array initial-size
                          :element-type element-type
                          :adjustable adjustable)))
    (list (make-list-queue))))

(defmethod overflow((q vector-queue))
  (restart-case
      (error 'overflow :structure q)
    (extend(&optional (extension +standard-heap-extend-size+))
        :report "Extend queue"
        :interactive (lambda() (format t "Enter entension: ") (list (read)))
        (let* ((v (implementation-vector q))
               (n (length v))
               (head (vector-queue-head q)))
          (adjust-array v (+ n extension))
          (unless (= 0 head)
            (setf (subseq v (+ head extension)) (subseq v head n)
                  (vector-queue-head q) (+ head extension)))))))

(defmethod enqueue(x (q vector-queue))
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (implementation-vector q))
         (n (length v)))
    (when (= head (mod (1+ tail) n)) (overflow q))
    (prog1
        (setf (aref v tail) x)
      (setf (vector-queue-tail q) (mod (1+ tail) n)))))

(defmethod dequeue((q vector-queue))
  (let ((head (vector-queue-head q))
        (tail (vector-queue-tail q))
        (v (implementation-vector q)))
    (if (= head tail)
        (underflow q)
        (prog1
            (aref v head)
          (setf (vector-queue-head q)
                (mod (1+ head) (length v)))))))

(defmethod enqueue((q list-queue) x)
  (let ((new-cons (cons x nil)))
    (cond ((list-queue-head q)
           (rplacd (list-queue-tail q) new-cons)
           (setf (list-queue-tail q) new-cons))
          (t (setf (list-queue-head q) new-cons)
             (setf (list-queue-tail q) new-cons)))))

(defmethod dequeue((q list-queue))
  (let ((front (list-queue-head q)))
    (if front
        (underflow q)
        (progn
          (setf (list-queue-head q) (rest front))
          (car front)))))

(defmethod push(x (q vector-queue))
  "Add element to front of a queue"
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (implementation-vector q))
         (n (length v)))
    (when (= head (mod (1+ tail) n)) (overflow q))
    (setf (vector-queue-head q) (mod (1- head) n))
    (aref v (vector-queue-head q))))

(defmethod pop((q vector-queue))
  "Pop element off back of queue"
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (implementation-vector q))
         (n (length v)))
    (if (= head tail)
        (underflow q)
        (progn
          (setf (vector-queue-tail q) (mod (1- tail) n))
          (aref v (vector-queue-tail q))))))

;; general interface
(defmethod insert(x (q vector-queue)) (enqueue x q))

(defmethod insert(x (q list-queue)) (enqueue x q))

(defmethod search((k integer) (q vector-queue))
    (let* ((v (implementation-vector q))
           (n (length v))
           (len (size q)))
      (unless (< -1 k len) (invalid-index-error k q))
      (aref v (mod (+ (vector-queue-head q) k) n))))

(defmethod search((k integer) (q list-queue))
  (let ((l (when (>= k 0) (nthcdr k (list-queue-head q)))))
    (if l
      (car l)
      (invalid-index-error k q))))

(defmethod peek((q list-queue))
  (let ((l (implementation-head q)))
    (if l (first l) (underflow q))))

(defmethod peek((q vector-queue))
  (let ((head (vector-queue-head q))
        (tail (vector-queue-tail q)))
    (if (= head tail)
        (underflow q)
        (aref (vector-queue-vector q) head))))

(defun vector-queue-position(x q)
  "Return index in vector of item x in q"
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (vector-queue-vector q))
         (n (length v)))
    (do*((i head (mod (1+ i) n))
         (y (aref v i) (aref v i)))
        ((or (eql x y) (= i tail))
         (unless (= i tail) i)))))

(defmethod rank(x (q vector-queue))
  (let ((p (vector-queue-position x q)))
    (when p
      (mod (- p (vector-queue-head q)) (length (vector-queue-vector q))))))

(defmethod predecessor(x (q vector-queue))
  (let* ((p (vector-queue-position x q))
         (head (vector-queue-head q))
         (v (vector-queue-vector q))
         (n (length v)))
    (when (and p (/= p head)) (aref v (mod (1- p) n)))))

(defmethod predecessor(x (q list-queue))
  (mapl
   #'(lambda(l) (when (eql x (second l)) (return-from predecessor (first l))))
   (implementation-head q))
  nil)

(defmethod successor(x (q vector-queue))
  (let* ((p (vector-queue-position x q))
         (tail (vector-queue-head q))
         (v (vector-queue-vector q))
         (n (length v)))
    (when p
      (let ((p (mod (1+ p) n)))
        (when (< p tail) (aref v p))))))

(defmethod successor(x (q list-queue))
  (mapl
   #'(lambda(l) (when (eql x (first l)) (return-from successor (second l))))
   (implementation-head q))
  nil)q

(defmethod traverse(f (q list-queue) &rest args)
  (cl:map
   'nil
   (if args #'(lambda(v) (apply f (cons v args))) f)
   (implementation-head q)))

(defmethod traverse(f (q vector-queue) &rest args)
  (let* ((f (if args #'(lambda(v) (apply f (cons v args))) f))
         (head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (vector-queue-vector q))
         (n (length v)))
    (do*((i head (mod (1+ i) n))
         (y (aref v i) (aref v i)))
        ((= i tail))
      (funcall f y))))

