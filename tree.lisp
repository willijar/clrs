(in-package :clrs)

(defstruct node
  (left nil :type (or node null))
  (right nil :type (or node null))
  (parent nil :type (or node null))
  (payload nil :read-only t))

(defstruct (tree (:copier tree-copy) (:constructor nil))
  (root nil :type (or node null))
  (key-fn #'identity :type function :read-only t)
  (comp-fn #'< :type function :read-only t)
  (eql-fn #'= :type function :read-only t))

(defstruct (binary-tree(:include tree)))

(defmethod empty-p((tree tree)) (null (tree-root tree)))

(defun inorder-tree-walk(x &optional (function #'print))
  (when x
    (inorder-tree-walk (node-left x) function)
    (funcall function x)
    (inorder-tree-walk (node-right x) function)))

(defmethod map(f (tree binary-tree) &rest args)
  (inorder-tree-walk (tree-root tree)
                     (if args #'(lambda(v) (funcall f (cons v args))) f)))

(defmethod length((tree binary-tree))
  (let ((c 0))
    (inorder-tree-walk (tree-root tree) #'(lambda(v) (declare (ignore v)) (incf c)))
    c))

(defun tree-search(x k &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (flet ((key(z) (funcall key-fn (node-payload z))))
    (do((x x (if (funcall comp-fn k (key x))
                 (node-left x)
                 (node-right x))))
       ((or (null x) (funcall eql-fn k (key x)))
        x))))

(defun tree-minimum(x)
  (do((y x (node-left y)))
     ((null (node-left y)) y)))

(defun tree-maximum(x)
  (do((y x (node-right y)))
     ((null (node-right y)) y)))

(defun tree-successor(x)
  (if (node-right x)
      (tree-minimum (node-right x))
      (do((y (node-parent x) (node-parent y))
          (x x y))
         ((or (null y) (not (eql x (node-right y))))
          y))))

(defun tree-predecessor(x)
  (if (node-left x)
      (tree-maximum (node-right x))
      (do((y (node-parent x) (node-parent y))
          (x x y))
         ((or (null y) (not (eql x (node-left y))))
          y))))

(defun tree-insert(tree z)
  "Insert node z into tree"
  (let ((comp-fn (tree-comp-fn tree))
        (key-fn (tree-key-fn tree)))
    (flet ((key(z) (funcall key-fn (node-payload z))))
      (let*((zkey (key z))
            (y (do((y nil x)
                 (x (tree-root tree)
                    (if (funcall comp-fn zkey (key x))
                        (node-left x)
                        (node-right x))))
                ((null x) y))))
      (setf (node-parent z) y)
      (cond
        ((null y); tree was empty
         (setf (tree-root tree) z))
        ((funcall comp-fn zkey (key y))
         (setf (node-left y) z))
        (t
         (setf (node-right y) z)))))))

(defun transplant(tree u v)
  (cond
    ((null (node-parent u))
     (setf (tree-root tree) v))
    ((eql u (node-left (node-parent u)))
     (setf (node-left (node-parent u)) v))
    (t
     (setf (node-right (node-parent u)) v)))
  (unless (null v)
    (setf (node-parent v) (node-parent u))))

(defun tree-delete(tree z)
  "Delete node z from tree"
  (cond
    ((null (node-left z))
     (transplant tree z (node-right z)))
    ((null (node-right z))
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
     ((or (null n) (eql (node-payload n) payload)) n)))

(defmethod successor((tree binary-tree) x)
  (let ((n (tree-payload-search (tree-root tree) x
                                :comp-fn (tree-comp-fn tree)
                                :eql-fn (tree-eql-fn tree)
                                :key-fn (tree-key-fn tree))))
    (when n
      (let ((n (tree-successor n)))
        (when n (node-payload n))))))

(defmethod predecessor((tree binary-tree) x)
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

(defstruct (red-black-tree-node(:include node)
                               (:conc-name rb-))
  (colour 'black :type (member red black)))

(defconstant +rb-nil+
  (if (boundp '+rb-nil+)
      +rb-nil+
      (make-red-black-tree-node :colour 'black))
  "Sentinel node for red black tree implementation")

(defstruct (red-black-tree(:include binary-tree)))

(defun make-tree(&key (key-fn #'identity) (comp-fn #'<) (eql-fn #'=))
  (make-red-black-tree
   :key-fn key-fn
   :comp-fn comp-fn
   :eql-fn eql-fn))

(defun left-rotate(tree x)
  (let ((y (node-right x)))
    (setf (node-right x) (node-left y))
    (unless (eql (node-left y) +rb-nil+)
      (setf (node-parent (node-left y)) x))
    (setf (node-parent y) (node-parent x))
    (cond
      ((eql (node-parent x) +rb-nil+)
       (setf (tree-root tree) y))
      ((eql x (node-left (node-parent x)))
       (setf (node-left (node-parent x)) y))
      ((setf (node-right (node-parent x)) y)))
    (setf (node-left y) x
          (node-parent x) y)))

(defun right-rotate(tree x)
  (let ((y (node-left x)))
    (setf (node-left x) (node-right y))
    (when (eql (node-right y) +rb-nil+)
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
  (let*((comp-fn (tree-comp-fn tree))
        (key-fn (tree-key-fn tree))
        (zkey (funcall key-fn z))
        (y (do((y +rb-nil+ x)
               (x (tree-root tree)
                  (if (funcall comp-fn zkey (funcall key-fn x))
                      (node-left x)
                      (node-right x))))
              ((eql x +rb-nil+) y))))
    (setf (node-parent z) y)
    (cond
      ((eql y +rb-nil+)
       (setf (tree-root tree) z))
      ((funcall comp-fn zkey (funcall key-fn y))
       (setf (node-left y) z))
      (t
       (setf (node-right y) z)))
    (setf (node-left z) +rb-nil+
          (node-right z) +rb-nil+
          (rb-colour z) 'red)
    (rb-insert-fixup tree z)))

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
    ((eql (node-parent u) +rb-nil+)
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
            ((eql (node-left z) +rb-nil+)
             (prog1 (node-right z)
               (rb-transplant tree z (node-right z))))
            ((eql (node-right z) +rb-nil+)
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