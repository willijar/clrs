(in-package :clrs)

(defclass node()
  ((left :type (or node null) :reader left)
   (right :type (or node null) :reader right)
   (parent :type (or node null) :reader parent)
   (payload  :reader payload :initarg :payload)))

(defgeneric null-node(node)
  (:documentation "return constant sentinel node")
  (:method((n node)) nil))

(declaim (inline null-node-p))
(defun null-node-p(node)
  (not (slot-boundp node 'payload)))

(defmethod initialize-instance :after ((n node) &key &allow-other-keys)
    (dolist(slot '(parent left right))
      (setf (slot-value n slot) (null-node n))))

(defmethod print-object((n node) os)
  (print-unreadable-object(n os :type t :identity t)
    (unless (null-node-p n)
      (format os "~A" (payload n)))))

(defclass node-size()
  ((size :initform 0 :type fixnum :initarg :size))
   (:documentation "Mixing for handline nodes that maintain size information."))

(defmethod initialize-instance :after ((n node-size) &key (payload nil payload-p) &allow-other-keys)
  (when payload-p (setf (slot-value n 'size) 1)))

(defmethod size((n null)) 0)

(defmethod size((n node-size))
  (if (null-node-p n) 0 (slot-value n 'size)))

(defgeneric (setf size)(size item)
  (:documentation "Set size of item")
  (:method (v (n node-size))
    (unless (null-node-p n)
      (let ((dv (- v (slot-value n 'size))))
        (setf (slot-value n 'size) v)
        (incf (size (parent n)) dv)))))

(defclass red-black-node(node node-size)
  ((colour :initform 'red :type (member red black)
           :initarg :colour :accessor colour)))

(defmethod null-node((node red-black-node))
  (load-time-value
   (make-instance 'red-black-node :colour 'black :size 0)
   t))

(defmethod print-object((n red-black-node) os)
  (print-unreadable-object(n os :type t :identity t)
    (unless (null-node-p n)
      (format os "~A ~A" (colour n) (payload n)))))

(defgeneric (setf left)(child parent)
  (:method(child (parent node))
    (setf (slot-value parent 'left) child)
    (when child
      (setf (slot-value child 'parent) parent)))
  (:method :after(child (parent node-size))
     (setf (size parent) (+ 1 (size child) (size (right parent))))))

(defgeneric (setf right)(child parent)
  (:method(child (parent node))
    (setf (slot-value parent 'right) child)
    (when child
      (setf (slot-value child 'parent) parent)))
  (:method :after(child (parent node-size))
     (setf (size parent) (+ 1 (size child) (size (left parent))))))

(defclass binary-tree()
  ((root :type node :reader root)
   (key-fn :type function :initform #'identity
           :initarg :key-fn :reader key-fn)
   (comp-fn :type function :initform #'<
            :initarg :comp-fn :reader comp-fn)
   (eql-fn :type function :initform #'=
           :initarg :eql-fn :reader eql-fn)
   (node-class :initarg :node-class :initform 'node :reader node-class)))

(defmethod initialize-instance :after((tree binary-tree) &key &allow-other-keys)
  (setf (root tree) (null-node (make-instance (node-class tree)))))

(defmethod empty-p((tree binary-tree)) (null-node-p (root tree)))

(defgeneric (setf root)(node tree)
  (:documentation "Set root of tree to node")
  (:method((n node) (tree binary-tree))
    (setf (slot-value tree 'root) n
          (slot-value n 'parent) (null-node n))))

(defun inorder-tree-walk(x &optional (function #'print))
  (unless (null-node-p x)
    (inorder-tree-walk (left x) function)
    (funcall function x)
    (inorder-tree-walk (right x) function)))

(defmethod traverse(f (tree binary-tree))
  (inorder-tree-walk (root tree)
                     #'(lambda(node) (funcall f (payload node)))))

(defmethod size((tree binary-tree))
  (size (root tree)))

(defun tree-search(x k &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (flet ((key(z) (funcall key-fn (payload z))))
    (do((x x (if (funcall comp-fn k (key x))
                 (left x)
                 (right x))))
       ((or (null-node-p x) (funcall eql-fn k (key x)))
        (unless (null-node-p x) x)))))

(defun tree-left(x)
  (do((y x (left y)))
     ((null-node-p (left y)) y)))

(defun tree-right(x)
  (do((y x (right y)))
     ((null-node-p (right y)) y)))

(defun tree-successor(x)
  (if (right x)
      (tree-left (right x))
      (do((y (parent x) (parent y))
          (x x y))
         ((or (null-node-p y) (not (eql x (right y))))
          y))))

(defun tree-predecessor(x)
  (if (left x)
      (tree-right (left x))
      (do((y (parent x) (parent y))
          (x x y))
         ((or (null-node-p y) (not (eql x (left y))))
          y))))

(defun tree-insert(tree z)
  "Insert node z into tree"
  (let ((comp-fn (comp-fn tree))
        (key-fn (key-fn tree)))
    (if (null-node-p (root tree))
        (setf (root tree) z)
        (flet ((key(z) (funcall key-fn (payload z))))
          (let((zkey (key z)))
              (do((y nil x)
                  (x (root tree)
                     (if (funcall comp-fn zkey (key x))
                         (left x)
                         (right x))))
                 ((null-node-p x)
                  (if (funcall comp-fn zkey (key y))
                      (setf (left y) z)
                      (setf (right y) z))))))) )
  z)

(defun transplant(tree u v)
  (let ((parent (parent u)))
    (cond
      ((null-node-p parent)
       (setf (root tree) v))
      ((eql u (left parent))
       (setf (left parent) v))
      ((setf (right parent) v)))))

(defun tree-delete(tree z)
  "Delete node z from tree"
  (cond
    ((null-node-p (left z))
     (transplant tree z (right z)))
    ((null-node-p (right z))
     (transplant tree z (left z)))
    ((let ((y (tree-left (right z))))
       (when (not (eql (parent y) z))
         (transplant tree y (right y))
         (setf (right y) (right z)))
       (transplant tree z y)
       (setf (left y) (left z))))))

(defmethod search(k (tree binary-tree))
  (let ((x (tree-search (root tree) k
                        :comp-fn (comp-fn tree)
                        :eql-fn (eql-fn tree)
                        :key-fn (key-fn tree))))
    (when x (payload x))))

(defun tree-payload-search
    (x payload &key (comp-fn #'<) (eql-fn #'=) (key-fn #'identity))
  (do((n (tree-search x (funcall key-fn payload)
                      :comp-fn comp-fn
                      :eql-fn eql-fn
                      :key-fn key-fn)
         (tree-successor n)))
     ((or (null-node-p n) (eql (payload n) payload)) n)))

(defun node-rank(n tree)
  (let ((root (root tree))
        (r (size (left n))))
    (do((n n p)
        (p (parent n) (parent n)))
       ((eql n root) r)
      (if (eql n (right p))
          (incf r (- (size p) (size n)))))))

(defmethod rank(x (tree binary-tree))
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n (node-rank n tree))))

(defmethod rank-lookup(r (tree binary-tree))
  (let ((root (root tree)))
    (when root
      (when (or (< r 0) (> r (1- (size root))))
        (invalid-index-error r tree))
      (let* ((y root)
             (rank (size (left y))))
        (loop
           (cond
             ((= r rank) (return (payload y)))
             ((> r rank)
              (setf y (right y)
                    rank (+ rank 1 (size (left y)))))
             ((< r rank)
              (setf y (left y)
                    rank (- rank (size (right y)) 1)))))))))

(defmethod successor(x (tree binary-tree))
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n
      (let ((n (tree-successor n)))
        (when n (payload n))))))

(defmethod predecessor(x (tree binary-tree))
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n
      (let ((n (tree-predecessor n)))
        (when n (payload n))))))

(defmethod insert(x (tree binary-tree))
  (tree-insert tree (make-instance (node-class tree) :payload x)))

(defmethod delete(x (tree binary-tree))
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n (tree-delete tree n))))

(defclass red-black-tree(binary-tree)
  ()
  (:default-initargs :node-class 'red-black-node))

(defun make-tree(&key (key-fn #'identity) (comp-fn #'<) (eql-fn #'=))
  (make-instance 'red-black-tree
                 :key-fn key-fn
                 :comp-fn comp-fn
                 :eql-fn eql-fn))

(defun left-rotate(tree x)
  (let ((y (right x)))
    (setf (right x) (left y))
    (transplant tree x y)
    (setf (left y) x)))

(defun right-rotate(tree y)
  (let ((x (left y)))
    (setf (left y) (right x))
    (transplant tree y x)
    (setf (right x) y)))

(defun rb-insert-fixup(tree z)
  (loop
     (unless (eql (colour (parent z)) 'red) (return))
     (flet((fixup(side rot1 rot2)
             (let ((y (funcall side (parent (parent z)))))
               (cond
                 ((eql (colour y) 'red)
                  (setf (colour (parent z)) 'black
                        (colour y) 'black
                        (colour (parent (parent z))) 'red
                        z (parent (parent z))))
                 (t
                  (when (eql z (funcall side (parent z)))
                    (setf z (parent z))
                    (funcall rot1 tree z))
                  (setf (colour (parent z)) 'black
                        (colour (parent (parent z))) 'red)
                  (funcall rot2 tree (parent (parent z))))))))
       (if (eql (parent z) (left (parent (parent z))))
           (fixup #'right #'left-rotate #'right-rotate)
           (fixup #'left  #'right-rotate #'left-rotate))))
  (setf (colour (root tree)) 'black))

(defun rb-delete(tree z)
  (let*((y z)
        (y-original-colour (colour y))
        (x
         (cond
           ((null-node-p (left z))
            (transplant tree z (right z)))
           ((null-node-p (right z))
            (transplant tree z (left z)))
           ((let ((y (tree-left (right z))))
              (setf y-original-colour (colour y))
              (when (not (eql (parent y) z))
                (transplant tree y (right y))
                (setf (right y) (right z)))
              (transplant tree z y)
              (setf (left y) (left z)
                    (colour y) (colour z))
              (right y))))))
        (when (eql y-original-colour 'black)
          (rb-delete-fixup tree x))))

(defun rb-delete-fixup(tree x)
  (loop
     (unless (or (eql x (root tree)) (eql (colour x) 'black)) (return))
     (if (eql x (left (parent x)))
        (let ((w (right (parent x))))
          (when (eql (colour w) 'red)
            (setf (colour w) 'black
                  (colour (parent x)) 'red)
            (left-rotate tree (parent x))
            (setf w (right (parent x))))
          (if (and (eql (colour (left w)) 'black)
                   (eql (colour (right w)) 'black))
             (setf (colour w) 'red
                   x (parent x))
             (progn
               (when (eql (colour (right x)) 'black)
                 (setf (colour (left w)) 'black
                       (colour w) 'red)
                 (right-rotate tree w)
                 (setf w (right (parent x))))
             (setf (colour w) (colour (parent x))
                   (colour (parent x)) 'black
                   (colour (right w)) 'black)
             (left-rotate tree (parent x))
             (setf x (root tree)))))
         (let ((w (left (parent x))))
           (when (eql (colour w) 'red)
             (setf (colour w) 'black
                   (colour (parent x)) 'red)
             (right-rotate tree (parent x))
             (setf w (left (parent x))))
           (if (and (eql (colour (right w)) 'black)
                    (eql (colour (left w)) 'black))
               (setf (colour w) 'red
                     x (parent x))
               (progn
                 (when (eql (colour (left x)) 'black)
                   (setf (colour (right w)) 'black
                         (colour w) 'red)
                   (left-rotate tree w)
                   (setf w (left (parent x))))
                 (setf (colour w) (colour (parent x))
                       (colour (parent x)) 'black
                       (colour (left w)) 'black)
                 (right-rotate tree (parent x))
                 (setf x (root tree)))))))
  (setf (colour x) 'black))

(defmethod insert(x (tree red-black-tree))
  (rb-insert-fixup tree (call-next-method)))

(defmethod delete(x (tree red-black-tree))
  (let ((n (tree-payload-search (root tree) x
                                :comp-fn (comp-fn tree)
                                :eql-fn (eql-fn tree)
                                :key-fn (key-fn tree))))
    (when n (rb-delete tree n))))