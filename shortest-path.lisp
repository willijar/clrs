;; shortest path algorithms
;; graphs and graphs are represented using
;; vectors with vertices keyed by index in the vertices
;; this makes for an efficient reasonably general implementation


(in-package :clrs)

(defconstant +infinity+ most-positive-short-float "Maximum cost value")

(define-condition not-connected(error)
  ((end :type (integer 0) :reader not-connected-end :initarg :end))
  (:documentation "Network not connected")
  (:report (lambda(c strm)
             (format strm "Not connected: ~D" (not-connected-end c)))))

(define-condition negative-weight-cycle(condition)())

(defun fully-connected-p(n &key (edges-fn #'identity))
  "Given a graph as a sequence of sequences of
outgoing edges indexes determine if network is fully connected."
  (let* ((connected (make-array n :element-type 'bit :initial-element 0))
         (count 1))
    (setf (sbit connected 0) 1)
    (do*((i 0 (first to-check))
         (to-check nil (rest to-check)))
        ((or (not i) (= count n)) (= count n))
      (map 'nil
           #'(lambda(j)
               (when (= (sbit connected j) 0)
                 (setf (sbit connected j) 1)
                 (incf count)
                 (push j to-check)))
           (funcall edges-fn i)))))

(defun dijkstra(source n &key
                (edges-fn #'identity)
                (cost-fn #'(lambda(from to) (declare (ignore from to)) 1.0)))
  "Dijkstra's SPF algorithm - determines shortest path (lowest cost)
from a source index to all other points in a graph of size n.
edges-fn should, given an index, return a sequence of the connected
indices. cost-fn should, given a source and end vertex index, return
the cost as a positive integer.

Returns two vectors, a table of previous vertice indices and the
associated costs. -1 is used in previous to label unconnected vertices
and the source points to itself"
  (let* ((costs (make-array n :element-type 'short-float
                            :initial-element +infinity+))
         (previous (make-array n :element-type `(integer -1 ,n)
                               :initial-element -1)))
    (setf (aref costs source) 0.0
          (aref previous source) source)
    ;; could also allow multiple values for previous and d to allow for
    ;; multiple shortest routes
    (let ((q (make-binary-heap
              :initial-size n
              :element-type 'fixnum
              :key-fn #'(lambda(i) (aref costs i))
              :index 'vector)))
      (dotimes(i n) (enqueue i q))
      (loop
         (let ((u (dequeue q)))
           (map 'nil
                #'(lambda(v)
                    (let ((cost (+ (funcall cost-fn u v) (aref costs u))))
                      (when (< cost (aref costs v))
                        (setf (aref costs v) cost
                              (aref previous v) u)
                        (key-changed v q))))
                (funcall edges-fn u)))
         (when (empty-p q) (return (values previous costs)))))))

(defun extract-route(end previous)
  "Return a list of the route in order to end from the previous output
of dijkstra"
  (let ((route (list end)))
    (do ((n (aref previous end) (aref previous n)))
        ((= n (first route)) route)
      (when (< n 0) (error 'not-connected :end end))
      (push n route))))

(defun extract-first-hops(previous)
  "Return a vector mapping first hop nodes to list of destination nodes
using previous output from dijkstra"
  (let* ((n (length previous))
         (first-hops (make-array n :element-type `(integer -1 ,n) )))
    (dotimes(i n)
      (setf (aref first-hops i)
            (handler-case
                (second (extract-route i previous))
              (not-connected(e) (declare (ignore e))  -1))))
    first-hops))

(defun bellman-ford(source n &key
                    (edges-fn #'identity)
                    (cost-fn #'(lambda(from to)
                                 (declare (ignore from to)) 1.0)))
  "Bellman-Ford algorithm to determing single-source shortest path in a
  weighted digraph - edge weights may be negative. Returns has tables
  keyed by end vertices as follows:

 - the previous vertices on the route
 - the cost to that vertice"
  (let* ((costs (make-array n :element-type 'short-float
                            :initial-element +infinity+))
         (previous (make-array n :element-type `(integer -1 n)
                               :initial-element -1)))
    (setf (aref costs source) 0.0
          (aref previous source) source)
    ;; relax edges repeatedly
    (dotimes(dummy (1- n))
      (dotimes(u n)
        (map 'nil
             #'(lambda(v)
                 (let ((cost (+ (funcall cost-fn u v) (aref costs u))))
                   (when (< cost (aref costs v))
                     (setf (aref costs v) cost
                           (aref previous v) u))))
             (funcall edges-fn u))))
    ;; check for negative weight cycles
    (dotimes(u n)
      (map 'nil
           #'(lambda(v)
               (when (< (+ (funcall cost-fn u v) (aref costs u))
                        (aref costs v))
                 (cerror "Ignore" 'negative-weight-cycle)))
           (funcall edges-fn u)))
    (values previous costs)))

