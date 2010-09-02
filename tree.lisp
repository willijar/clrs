(in-package :clrs)

(defstruct node
  (left nil :type node)
  (right nil :type node)
  (parent nil :type node)
  (payload nil :read-only t))

(defstruct tree
  (root nil :type node)
  (key-fn #'identity :type function :read-only t)
  (comp-fun #'< :type function :read-only t)
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

(defun tree-search(x k &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (do((x x (if (funcall comp-fn k (funcall key-fn x))
               (node-left x)
               (node-right x))))
     ((or (null x) (funcall eql-fn k (funcall key-fn x)))
      x)))

(defun tree-minimum(x)
  (do((x x (node-left x)))
     ((null (node-left x)) x)))

(defun tree-maximum(x)
  (do((x x (node-right x)))
     ((null (node-right x)) x)))

(defun tree-successor(x &key (eql-fn #'=))
  (if (node-right x)
      (tree-minimum (node-right x))
      (do((y (node-parent x) (node-parent y))
          (x x y))
         ((or (null y) (not (funcall eql-fn x (node-right y))))
          y))))

(defun tree-predecessor(x &key (eql-fn #'=))
  (if (node-left x)
      (tree-maximum (node-right x))
      (do((y (node-parent x) (node-parent y))
          (x x y))
         ((or (null y) (not (funcall eql-fn x (node-left y))))
          y))))

(defun tree-insert(tree z &key (comp-fun #'<) (key-fun #'identity))
  (let*((zkey (funcall key-fun z))
        (y (do((y nil x)
               (x (root tree)
                  (if (funcall comp-fun zkey (funcall key-fun x))
                      (node-left x)
                      (node-right x))))
              ((null x) y))))
    (setf (node-parent z) y)
    (cond
      ((null y); tree was empty
       (setf (root tree) z))
      ((funcall comp-fun zkey (funcall key-fun y))
       (setf (node-left y) z))
      ((setf (node-right y) z)))))

(defun transplant(tree u v)
  (cond
    ((null (node-parent u))
     (setf (root tree) v))
    ((eql u (node-left (node-parent u)))
     (setf (node-left (node-parent u)) v))
    ((setf (node-right (node-parent u)) v)))
  (unless (null v)
    (setf (node-parent v) (node-parent u))))

(defun tree-delete(tree z)
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

(defmethod search((tree binary-tree) k)
  (let ((x (tree-search tree k
                        :comp-fn (comp-fn tree)
                        :eql-fn (eql-fn tree)
                        :key-fn (key-fn tree))))
    (when x (payload x))))

(defmethod minimum((tree binary-tree)) (payload (tree-minimum tree)))
(defmethod maximum((tree binary-tree)) (payload (tree-maximum tree)))

(defun tree-payload-search
    (x payload &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (do((n (tree-search x (funcall key-fn x)
                      :comp-fn comp-fn
                      :eql-fn eql-fn
                      :key-fn key-fn)
         (tree-successor n :eql-fn eql-fn)))
     ((or (null n) (eql (payload n) payload)) n)))


(defmethod successor((tree binary-tree) x)
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n
      (let ((n (tree-successor n :eql-fn (eql-fn tree))))
        (when n (payload n))))))

(defmethod predecessor((tree binary-tree) x)
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n
      (let ((n (tree-predecessor n :eql-fn (eql-fn tree))))
        (when n (payload n))))))

(defmethod insert((tree binary-tree) x)
  (tree-insert tree
               (make-instance 'tree-node :payload x)
               :comp-fun (comp-fn tree)
               :key-fun (key-fn tree)))

(defmethod delete((tree binary-tree) x)
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n (tree-delete tree n))))

#|
(defstruct (red-black-tree-node(:include tree-node) (:nconhc tree-))
  (colour 'black :type '(member red black)))

(defconstant +rb-nil+
  (if (boundp '+rb-nil+)
      +rb-nil+
      (make-red-black-tree-node :colour black))
  "Sentinel node for red black tree implementation")

(defstruct (red-black-tree(:include binary-tree)))

(defun make-rbtree(&key (key-fn #'identity) (comp-fn #'<) (eql-fn #'=))
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
       (setf (root tree) y))
      ((eql x (node-left (node-parent x)))
       (setf (node-left (node-parent x)) y))
      ((setf (node-right (node-parent x)) y)))
    (setf (node-left y) x
          (node-parent x) y)))

(defun left-rotate(tree x)
  (let ((y (node-left x)))
    (setf (node-left x) (node-right y))
    (when (eql (node-right y) +rb-nil+)
      (setf (node-parent (node-right y)) x))
    (setf (node-parent y) (node-parent x))
    (cond
      ((not (node-parent x))
       (setf (root tree) y))
      ((eql x (node-right (node-parent x)))
       (setf (node-right (node-parent x)) y))
      ((setf (node-left (node-parent x)) y)))
    (setf (node-right y) x
          (node-parent x) y)))

(defun rb-insert(tree z &key (comp-fun #'<) (key-fun #'identity))
  (let*((zkey (funcall key-fun z))
        (y (do((y +rb-nil+ x)
               (x (root tree)
                  (if (funcall comp-fun zkey (funcall key-fun x))
                      (node-left x)
                      (node-right x))))
              ((eql x +rb-nil+) y))))
    (setf (node-parent z) y)
    (cond
      ((eql y +rb-nil+)
       (setf (root tree) z))
      ((funcall comp-fun zkey (funcall key-fun y))
       (setf (node-left y) z))
      ((setf (node-right y) z)))
    (setf (node-left z) +rb-nil+
          (node-right z) +rb-nil+
          (colour z) 'red)
    (rb-insert-fixup tree z)))

(defun rb-insert-fixup(tree z)
  (loop
     (unless (eql (colour (node-parent z)) 'red) (return))
     (cond
       ((eql (node-parent z) (node-left (node-parent (node-parent z))))
        (let ((y (node-right (node-parent (node-parent z)))))
          (cond
            ((eql (colour y) 'red)
             (setf (colour (node-parent z)) 'black
                   (colour y) 'black
                   (colour (node-parent (node-parent z))) 'red
                   z (node-parent (node-parent z))))
            (t
             (when (eql z (node-right (node-parent z)))
               (setf z (node-parent z))
               (left-rotate tree z))
             (setf (colour (node-parent z)) 'black
                   (colour (node-parent (node-parent z)) 'red))
             (right-rotate tree (node-parent (node-parent z)))))))
       ((let ((y (node-left (node-parent (node-parent z)))))
          (cond
            ((eql (colour y) 'red)
             (setf (colour (node-parent z)) 'black
                   (colour y) 'black
                   (colour (node-parent (node-parent z))) 'red
                   z (node-parent (node-parent z))))
            (t
             (when (eql z (node-left (node-parent z)))
               (setf z (node-parent z))
               (right-rotate tree z))
             (setf (colour (node-parent z)) 'black
                   (colour (node-parent (node-parent z)) 'red))
             (left-rotate tree (node-parent (node-parent z))))))))
     (setf (colour (root tree)) 'black)))

(defun rb-transplant(tree u v)
  (cond
    ((eql (node-parent u) +rb-nil+)
     (setf (root tree) v))
    ((eql u (node-left (node-parent u)))
     (setf (node-left (node-parent u)) v))
    ((setf (node-right (node-parent u)) v)))
  (setf (node-parent v) (node-parent u)))

(defun rb-delete(tree z)
  (let* ((y z)
         (y-original-colour (colour y))
         (x
          (cond
            ((eql (node-left z) +rb-nil+)
             (prog1 (node-right z)
               (rb-transplant tree z (node-right z))))
            ((eql (node-right z) +rb-nil+)
             (prog1 (node-left z)
               (rb-transplant tree z (node-left z))))
            ((let ((y (tree-minimum (node-right z))))
               (setf y-original-colour (colour y))
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
                       (colour y) (colour z))))))))
    (when (eql y-original-colour 'black)
      (rb-delete-fixup tree x))))

(defun rb-delete-fixup(tree x)
  (loop
     (unless (or (eql x (root tree)) (eql (colour x) 'black)) (return))
     (cond
       ((eql x (node-left (node-parent x)))
        (let ((w (node-right (node-parent x))))
          (when (eql (colour w) 'red)
            (setf (colour w) 'black
                  (colour (node-parent x)) 'red)
            (left-rotate tree (node-parent x)))
          (cond
            ((and (eql (colour (node-left w)) 'black)
                  (eql (colour (node-right w)) 'black))
             (setf (colour w) 'red
                   x (node-parent x)))
            (t
             (when (eql (colour (node-right x)) 'black)
               (setf (colour (node-left w)) 'black
                     (colour w) 'red)
               (right-rotate tree w)
               (setf w (node-right (node-parent x))))
             (setf (colour w) (colour (node-parent x))
                   (colour (node-parent x)) 'black
                   (colour (node-right w)) 'black)
             (left-rotate tree (node-parent x))
             (setf x (root tree)))))
        (t
         (let ((w (node-left (node-parent x))))
           (when (eql (colour w) 'red)
             (setf (colour w) 'black
                   (colour (node-parent x)) 'red)
             (right-rotate tree (node-parent x)))
           (cond
            ((and (eql (colour (node-right w)) 'black)
                  (eql (colour (node-left w)) 'black))
             (setf (colour w) 'red
                   x (node-parent x)))
            (t
             (when (eql (colour (node-left x)) 'black)
               (setf (colour (node-right w)) 'black
                     (colour w) 'red)
               (left-rotate tree w)
               (setf w (node-left (node-parent x))))
             (setf (colour w) (colour (node-parent x))
                   (colour (node-parent x)) 'black
                   (colour (node-left w)) 'black)
             (right-rotate tree (node-parent x))
             (setf x (root tree)))))))))
  (setf (colour x) 'black))
