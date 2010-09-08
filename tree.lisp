(in-package :clrs)

(defstruct node
  (left nil :type (or node null))
  (right nil :type (or node null))
  (parent nil :type (or node null))
  (payload nil :read-only t)
  (size 0 :type fixnum))

(eval-when(:compile-toplevel :load-toplevel)
(defstruct (red-black-tree-node(:include node)
                               (:conc-name rb-))
  (colour 'black :type (member red black))))

(defconstant +rb-nil+
  (if (boundp '+rb-nil+)
      +rb-nil+
      (make-red-black-tree-node :colour 'black))
  "Sentinel node for red black tree implementation")

(declaim (inline null-node-p))
(defun null-node-p(x) (or (null x) (eql x +rb-nil+)))

(defstruct (tree (:copier tree-copy) (:constructor nil))
  (root nil :type (or node null))
  (key-fn #'identity :type function :read-only t)
  (comp-fn #'< :type function :read-only t)
  (eql-fn #'= :type function :read-only t))

(defstruct (binary-tree(:include tree)))

(defmethod empty-p((tree tree)) (null-node-p (tree-root tree)))

(defun inorder-tree-walk(x &optional (function #'print))
  (when x
    (inorder-tree-walk (node-left x) function)
    (funcall function x)
    (inorder-tree-walk (node-right x) function)))

(defmethod traverse(f (tree binary-tree) &rest args)
  (inorder-tree-walk (tree-root tree)
                     (if args #'(lambda(v) (funcall f (cons v args))) f)))

(defmethod size((tree binary-tree))
  (let ((root (tree-root tree)))
    (if root (node-size root) 0)))

(defun tree-search(x k &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (flet ((key(z) (funcall key-fn (node-payload z))))
    (do((x x (if (funcall comp-fn k (key x))
                 (node-left x)
                 (node-right x))))
       ((or (null-node-p x) (funcall eql-fn k (key x)))
        (unless (null-node-p x) x)))))

(defun tree-minimum(x)
  (do((y x (node-left y)))
     ((null-node-p (node-left y)) y)))

(defun tree-maximum(x)
  (do((y x (node-right y)))
     ((null-node-p (node-right y)) y)))

(defun tree-successor(x)
  (if (node-right x)
      (tree-minimum (node-right x))
      (do((y (node-parent x) (node-parent y))
          (x x y))
         ((or (null-node-p y) (not (eql x (node-right y))))
          y))))

(defun tree-predecessor(x)
  (if (node-left x)
      (tree-maximum (node-right x))
      (do((y (node-parent x) (node-parent y))
          (x x y))
         ((or (null-node-p y) (not (eql x (node-left y))))
          y))))

(defun tree-insert(tree z)
  "Insert node z into tree"
  (let ((comp-fn (tree-comp-fn tree))
        (key-fn (tree-key-fn tree)))
    (flet ((key(z) (funcall key-fn (node-payload z))))
      (let*((zkey (key z)))
        (labels((node-insert(x y) ; recursevely add node
           (cond
             ((null-node-p x)
              (cond
                ((null-node-p y); tree was empty
                 (setf (tree-root tree) z))
                ((funcall comp-fn zkey (key y))
                 (setf (node-left y) z))
                 (t
                  (setf (node-right y) z)))
              (setf (node-parent z) y
                    (node-size z) 1))
             (t
              (node-insert (if (funcall comp-fn zkey (key x))
                               (node-left x)
                               (node-right x)) x)
              (incf (node-size x))))))
          (node-insert (tree-root tree) nil))))))

;; (defun tree-insert(tree z)
;;   "Insert node z into tree"
;;   (let ((comp-fn (tree-comp-fn tree))
;;         (key-fn (tree-key-fn tree)))
;;     (flet ((key(z) (funcall key-fn (node-payload z))))
;;       (let*((zkey (key z)))
;;             (y (do((y nil x)
;;                  (x (tree-root tree)
;;                     (if (funcall comp-fn zkey (key x))
;;                         (node-left x)
;;                         (node-right x))))
;;                 ((null-node-p x) y))))
;;       (setf (node-parent z) y)
;;       (cond
;;         ((null-node-p y); tree was empty
;;          (setf (tree-root tree) z))
;;         ((funcall comp-fn zkey (key y))
;;          (setf (node-left y) z))
;;         (t
;;          (setf (node-right y) z)))))))

(defun transplant(tree u v)
  (cond
    ((null-node-p (node-parent u))
     (setf (tree-root tree) v))
    ((eql u (node-left (node-parent u)))
     (setf (node-left (node-parent u)) v))
    (t
     (setf (node-right (node-parent u)) v)))
  (unless (null-node-p v)
    (setf (node-parent v) (node-parent u))))

(defun tree-delete(tree z)
  "Delete node z from tree"
  (cond
    ((null-node-p (node-left z))
     (transplant tree z (node-right z)))
    ((null-node-p (node-right z))
     (transplant tree z (node-left z)))
    ((let ((y (tree-minimum (node-right z))))
       (when (not (eql (node-parent y) z))
         (transplant tree y (node-right y))
         (setf (node-right y) (node-right z)
               (node-parent (node-right y)) y))
       (transplant tree z y)
       (setf (node-left y) (node-left z)
             (node-parent (node-left y)) y)))))

(defmethod search(k (tree binary-tree))
  (let ((x (tree-search (tree-root tree) k
                        :comp-fn (tree-comp-fn tree)
                        :eql-fn (tree-eql-fn tree)
                        :key-fn (tree-key-fn tree))))
    (when x (node-payload x))))

(defmethod minimum((tree binary-tree)) (node-payload (tree-minimum tree)))
(defmethod maximum((tree binary-tree)) (node-payload (tree-maximum tree)))

(defun tree-payload-search
    (x payload &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (do((n (tree-search x (funcall key-fn x)
                      :comp-fn comp-fn
                      :eql-fn eql-fn
                      :key-fn key-fn)
         (tree-successor n)))
     ((or (null-node-p n) (eql (node-payload n) payload)) n)))

(defmethod rank(x (tree binary-tree))
  (let ((n (tree-payload-search (tree-root tree) x
                                :comp-fn (tree-comp-fn tree)
                                :eql-fn (tree-eql-fn tree)
                                :key-fn (tree-key-fn tree))))
    (when n
      (if (node-left n) (node-size (node-left n)) 0))))

(defmethod successor(x (tree binary-tree))
  (let ((n (tree-payload-search (tree-root tree) x
                                :comp-fn (tree-comp-fn tree)
                                :eql-fn (tree-eql-fn tree)
                                :key-fn (tree-key-fn tree))))
    (when n
      (let ((n (tree-successor n)))
        (when n (node-payload n))))))

(defmethod predecessor(x (tree binary-tree))
  (let ((n (tree-payload-search (tree-root tree) x
                                :comp-fn (tree-comp-fn tree)
                                :eql-fn (tree-eql-fn tree)
                                :key-fn (tree-key-fn tree))))
    (when n
      (let ((n (tree-predecessor n)))
        (when n (node-payload n))))))

(defmethod insert(x (tree binary-tree))
  (tree-insert tree (make-instance 'node :payload x)))

(defmethod delete(x (tree binary-tree))
  (let ((n (tree-payload-search (tree-root tree) x
                                :comp-fn (tree-comp-fn tree)
                                :eql-fn (tree-eql-fn tree)
                                :key-fn (tree-key-fn tree))))
    (when n (tree-delete tree n))))

(defstruct (red-black-tree(:include binary-tree)))

(defun make-tree(&key (key-fn #'identity) (comp-fn #'<) (eql-fn #'=))
  (make-red-black-tree
   :key-fn key-fn
   :comp-fn comp-fn
   :eql-fn eql-fn))

(defun left-rotate(tree x)
  (let ((y (node-right x)))
    (setf (node-right x) (node-left y))
    (unless (null-node-p (node-left y))
      (setf (node-parent (node-left y)) x))
    (setf (node-parent y) (node-parent x))
    (cond
      ((null-node-p (node-parent x))
       (setf (tree-root tree) y))
      ((eql x (node-left (node-parent x)))
       (setf (node-left (node-parent x)) y))
      ((setf (node-right (node-parent x)) y)))
    (setf (node-left y) x
          (node-parent x) y)))

(defun right-rotate(tree x)
  (let ((y (node-left x)))
    (setf (node-left x) (node-right y))
    (when (null-node-p (node-right y))
      (setf (node-parent (node-right y)) x))
    (setf (node-parent y) (node-parent x))
    (cond
      ((not (node-parent x))
       (setf (tree-root tree) y))
      ((eql x (node-right (node-parent x)))
       (setf (node-right (node-parent x)) y))
      ((setf (node-left (node-parent x)) y)))
    (setf (node-right y) x
          (node-parent x) y)))

(defun rb-insert(tree z)
  (tree-insert tree z)
  (setf (node-left z) +rb-nil+
        (node-right z) +rb-nil+
        (rb-colour z) 'red)
  (rb-insert-fixup tree z))

(defun rb-insert-fixup(tree z)
  (flet((fixup(side rot1 rot2)
          (let ((y (funcall side (node-parent (node-parent z)))))
            (cond
            ((eql (rb-colour y) 'red)
             (setf (rb-colour (node-parent z)) 'black
                   (rb-colour y) 'black
                   (rb-colour (node-parent (node-parent z))) 'red
                   z (node-parent (node-parent z))))
            (t
             (when (eql z (funcall side (node-parent z)))
               (setf z (node-parent z))
               (funcall rot1 tree z))
             (setf (rb-colour (node-parent z)) 'black
                   (rb-colour (node-parent (node-parent z))) 'red)
             (funcall rot2 tree (node-parent (node-parent z))))))))
  (loop
     (unless (eql (rb-colour (node-parent z)) 'red) (return))
     (if (eql (node-parent z) (node-left (node-parent (node-parent z))))
         (fixup #'node-right #'left-rotate #'right-rotate)
         (fixup #'node-left  #'right-rotate #'left-rotate)))
  (setf (rb-colour (tree-root tree)) 'black)))

(defun rb-transplant(tree u v)
  (cond
    ((null-node-p (node-parent u))
     (setf (tree-root tree) v))
    ((eql u (node-left (node-parent u)))
     (setf (node-left (node-parent u)) v))
    ((setf (node-right (node-parent u)) v)))
  (setf (node-parent v) (node-parent u)))

(defun rb-delete(tree z)
  (let* ((y z)
         (y-original-colour (rb-colour y))
         (x
          (cond
            ((null-node-p (node-left z))
             (prog1 (node-right z)
               (rb-transplant tree z (node-right z))))
            ((null-node-p (node-right z))
             (prog1 (node-left z)
               (rb-transplant tree z (node-left z))))
            ((let ((y (tree-minimum (node-right z))))
               (setf y-original-colour (rb-colour y))
               (prog1 (node-right y)
                 (cond
                   ((eql (node-parent y) z) (setf (node-parent (node-right y)) y))
                   (t
                    (rb-transplant tree y (node-right y))
                    (setf (node-right y) (node-right z)
                          (node-parent (node-right y)) y)))
                 (rb-transplant tree z y)
                 (setf (node-left y) (node-left z)
                       (node-parent (node-left y)) y
                       (rb-colour y) (rb-colour z))))))))
    (when (eql y-original-colour 'black)
      (rb-delete-fixup tree x))))

(defun rb-delete-fixup(tree x)
  (loop
     (unless (or (eql x (tree-root tree)) (eql (rb-colour x) 'black)) (return))
     (if (eql x (node-left (node-parent x)))
        (let ((w (node-right (node-parent x))))
          (when (eql (rb-colour w) 'red)
            (setf (rb-colour w) 'black
                  (rb-colour (node-parent x)) 'red)
            (left-rotate tree (node-parent x))
            (setf w (node-right (node-parent x))))
          (if (and (eql (rb-colour (node-left w)) 'black)
                   (eql (rb-colour (node-right w)) 'black))
             (setf (rb-colour w) 'red
                   x (node-parent x))
             (progn
               (when (eql (rb-colour (node-right x)) 'black)
                 (setf (rb-colour (node-left w)) 'black
                       (rb-colour w) 'red)
                 (right-rotate tree w)
                 (setf w (node-right (node-parent x))))
             (setf (rb-colour w) (rb-colour (node-parent x))
                   (rb-colour (node-parent x)) 'black
                   (rb-colour (node-right w)) 'black)
             (left-rotate tree (node-parent x))
             (setf x (tree-root tree)))))
         (let ((w (node-left (node-parent x))))
           (when (eql (rb-colour w) 'red)
             (setf (rb-colour w) 'black
                   (rb-colour (node-parent x)) 'red)
             (right-rotate tree (node-parent x))
             (setf w (node-left (node-parent x))))
           (if (and (eql (rb-colour (node-right w)) 'black)
                    (eql (rb-colour (node-left w)) 'black))
               (setf (rb-colour w) 'red
                     x (node-parent x))
               (progn
                 (when (eql (rb-colour (node-left x)) 'black)
                   (setf (rb-colour (node-right w)) 'black
                         (rb-colour w) 'red)
                   (left-rotate tree w)
                   (setf w (node-left (node-parent x))))
                 (setf (rb-colour w) (rb-colour (node-parent x))
                       (rb-colour (node-parent x)) 'black
                       (rb-colour (node-left w)) 'black)
                 (right-rotate tree (node-parent x))
                 (setf x (tree-root tree)))))))
  (setf (rb-colour x) 'black))

(defmethod insert(z (tree red-black-tree))
  (rb-insert tree (make-red-black-tree-node :payload z)))

(defmethod delete(x (tree binary-tree))
  (let ((n (tree-payload-search (tree-root tree) x
                                :comp-fn (tree-comp-fn tree)
                                :eql-fn (tree-eql-fn tree)
                                :key-fn (tree-key-fn tree))))
    (when n (rb-delete tree n))))