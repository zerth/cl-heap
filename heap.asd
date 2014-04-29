; -*- mode: lisp -*-

(asdf:defsystem :heap
  :description "Thread-safe heap implementations."
  :version "0.1"
  :license "MIT"
  :author "Mike Watters <mike@mwatters.net>"
  :depends-on (:alexandria
               :lockable
               :struct-like-classes)
  :serial t
  :components ((:file "packages")
               (:file "heap")
               (:file "tree-based-heap")
               (:file "splay-heap")))
