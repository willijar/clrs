(in-package :clrs)

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
      :vector (make-array (1+ initial-size)
                          :element-type element-type
                          :adjustable adjustable)))
    (list (make-list-queue))))

(defmethod overflow((q vector-queue))
  (restart-case
      (error 'overflow :structure q)
    (extend(&optional (extension +standard-heap-extend-size+))
        :report "Extend queue"
        :interactive (lambda() (format t "Enter entension: ") (list (read)))
        (let* ((v (vector-queue-vector q))
               (n (length v))
               (head (vector-queue-head q)))
          (setf v
                (setf (vector-queue-vector q)
                      (adjust-array v (+ n extension))))
          (unless (>= (vector-queue-tail q) head)
            (setf (subseq v (+ head extension)) (subseq v head n)
                  (vector-queue-head q) (+ head extension)))))))

(defmethod enqueue(x (q vector-queue))
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (vector-queue-vector q))
         (n (length v)))
    (when (= head (mod (1+ tail) n))
      (overflow q)
      (setf v (vector-queue-vector q)
            head (vector-queue-head q)
            n (length v)))
    (prog1
        (setf (aref v tail) x)
      (setf (vector-queue-tail q) (mod (1+ tail) n)))))

(defmethod dequeue((q vector-queue))
  (let ((head (vector-queue-head q))
        (tail (vector-queue-tail q))
        (v (vector-queue-vector q)))
    (if (= head tail)
        (underflow q)
        (prog1
            (aref v head)
          (setf (vector-queue-head q)
                (mod (1+ head) (length v)))))))

(defmethod enqueue(x (q list-queue))
  (let ((new-cons (cons x nil)))
    (cond ((list-queue-head q)
           (rplacd (list-queue-tail q) new-cons)
           (setf (list-queue-tail q) new-cons))
          (t (setf (list-queue-head q) new-cons)
             (setf (list-queue-tail q) new-cons)))))

(defmethod dequeue((q list-queue))
  (let ((front (list-queue-head q)))
    (if front
        (progn
          (setf (list-queue-head q) (rest front))
          (car front))
        (underflow q))))

(defmethod push(x (q vector-queue))
  (enqueue x q))

;;   "Add element to front of a queue"
;;   (let* ((head (vector-queue-head q))
;;          (tail (vector-queue-tail q))
;;          (v (vector-queue-vector q))
;;          (n (length v)))
;;     (when (= head (mod (1+ tail) n)) (overflow q))
;;     (setf (vector-queue-head q) (mod (1- head) n))
;;     (aref v (vector-queue-head q))))

(defmethod pop((q vector-queue))
  "Pop element off back of queue"
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (vector-queue-vector q))
         (n (length v)))
    (if (= head tail)
        (underflow q)
        (progn
          (setf (vector-queue-tail q) (mod (1- tail) n))
          (aref v (vector-queue-tail q))))))

;; general interface
(defmethod insert(x (q vector-queue)) (enqueue x q))

(defmethod insert(x (q list-queue)) (enqueue x q))

(defmethod rank-lookup((k integer) (q vector-queue))
    (let* ((v (vector-queue-vector q))
           (n (length v))
           (len (size q)))
      (unless (< -1 k len) (invalid-index-error k q))
      (aref v (mod (+ (vector-queue-head q) k) n))))

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

(defmethod successor(x (q vector-queue))
  (let* ((p (vector-queue-position x q))
         (tail (vector-queue-head q))
         (v (vector-queue-vector q))
         (n (length v)))
    (when p
      (let ((p (mod (1+ p) n)))
        (when (< p tail) (aref v p))))))

(defmethod traverse(f (q vector-queue))
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (vector-queue-vector q))
         (n (length v)))
    (do*((i head (mod (1+ i) n))
         (y (aref v i) (aref v i)))
        ((= i tail))
      (funcall f y))))

(defmethod delete(x (q list-queue))
  (setf (list-queue-head q) (cl:delete x (list-queue-head q)))
  (when (eql x (car (list-queue-tail q)))
    (setf (list-queue-tail q) (last (list-queue-head q)))))

(defmethod delete(x (q vector-queue))
  (let* ((head (vector-queue-head q))
         (tail (vector-queue-tail q))
         (v (vector-queue-vector q))
         (n (length v)))
    (do*((i head j)
         (j (mod (1+ i) n) (mod (1+ i) n))
         (y (aref v i) (aref v i))
         (found-p (eql x y) (or found-p (eql x y))))
        ((= i tail) found-p)
      (when found-p (setf (aref v i) (aref v j))))))


