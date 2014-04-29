; -*- mode: lisp -*-

(asdf:defsystem :heap
  :description "Thread-safe heap implementations."
  :version "0.1"
  :license "MIT"
  :author "Mike Watters <mike@mwatters.net>"
  :depends-on (:alexandria
               :struct-like-classes)
  :components ((:file "heap")))
