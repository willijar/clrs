;;; Data Structures and Algorithms
;;; Cormen, Leiserson and Rivest and Stein, "Introduction to Algorithms",
;;; Third Edition, MIT Press, 2009.
;;; J.A.R. Williams 2010

(defpackage clrs
  (:use :cl)
  (:nicknames :alg)
  (:shadow  #:search #:delete #:push #:pop #:map #:length)
  (:export
   ;; general interface
   #:search #:delete #:insert #:delete #:minimum #:maximum #:map #:length
   #:successor #:predecessor #:empty-p
   #:overflow #:underflow #:extend
   ;; stack interface
   #:make-stack #:push #:pop
   ;; queue interface
   #:make-queue #:enqueue #:dequeque
   ;; tree interface
   #:make-tree
   ;; heap interface
   #:heapsort #:make-heap #:make-priority-queue #:map))
