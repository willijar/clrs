;;; Data Structures and Algorithms
;;; Cormen, Leiserson and Rivest and Stein, "Introduction to Algorithms",
;;; Third Edition, MIT Press, 2009.
;;; J.A.R. Williams 2010

(defpackage clrs
  (:use :cl)
  (:nicknames :alg)
  (:shadow #:search #:delete #:push #:pop #:delete-if)
  (:export
   ;; general
   #:search #:insert #:delete #:traverse #:delete-if
   #:successor #:predecessor #:empty-p #:rank #:rank-lookup #:size
   #:overflow #:underflow #:extend
   ;; stack
   #:make-stack #:push #:pop #:peek #:back
   ;; queue
   #:make-queue #:list-queue #:vector-queue #:enqueue #:dequeue
   ;; tree
   #:make-tree #:make-binary-tree #:make-red-black-tree
   ;; heap
   #:heapsort #:make-binary-heap #:key-changed
   ;; shortest path
   #:dijkstra #:bellman-ford #:fully-connected-p
   #:extract-route #:extract-first-hops
   #:not-connected #:negative-weight-cycle))
