;;;; -*- Lisp -*-
;;;; System definition for clrs implementations
;;;; Copyright (C) 2010 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
(in-package :cl-user)

(asdf:defsystem clrs
    :description "Implementations as per Cormen, Leiserson and Rivest
    and Stein, Introduction to Algorithms, Third Edition, MIT Press,
    2009"
    :author "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :maintainer "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :licence "GPL v3"
    :version "0.0.1"
    :components ((:file "defpackage")
                 (:file "common" :depends-on ("defpackage"))
                 (:file "stack" :depends-on ("common"))
                 (:file "queue" :depends-on ("common"))
                 (:file "tree" :depends-on ("common"))
                 #+nil(:file "heap" :depends-on ("common")))
)

