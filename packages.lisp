(defpackage :net.mwatters.heap
  (:nicknames :heap)
  (:use :common-lisp)
  (:import-from :struct-like-classes
   :define-struct-like-class)
  (:import-from :lockable
   :lockable
   :with-thing-locked)
  (:import-from :alexandria
   :with-gensyms)
  (:export
   :make-heap
   :heap-push
   :heap-pop
   :heap-next
   :heap-empty-p))