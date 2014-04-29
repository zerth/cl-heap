(in-package :net.mwatters.heap)


(defvar *default-heap-constructor* 'make-splay-heap)

(defun make-heap ()
  (funcall *default-heap-constructor*))


(define-struct-like-class (heap
                           (:constructor make-heap-1)) (lockable)
  (count 0))


(defgeneric heap-pop (heap)
  (:documentation "return as three values the next item from the heap,
its priority, and whether the heap was empty.")
  (:method ((h heap))
   (with-thing-locked h
     (if (heap-empty-p h)
         (values nil nil t)
       (multiple-value-bind (next pri)
           (heap-next h)
         (heap-remove-min h)
         (values next pri nil))))))


(defgeneric heap-push (heap priority item)
  (:documentation "push ITEM onto HEAP with the specified PRIORITY,
returning the heap.")
  (:method ((h heap) priority item)
   (with-thing-locked h
     (heap-insert h priority item))))


(defgeneric heap-next (heap)
  (:documentation "return as values the next item on HEAP, its
priority, and whether the heap was empty.")
  (:method :around ((h heap))
   (with-thing-locked h
     (if (heap-empty-p h)
         (values nil nil t)
       (call-next-method)))))


(defgeneric heap-empty-p (heap)
  (:documentation "return non-nil if HEAP is an empty heap."))


(defgeneric heap-remove-min (heap)
  (:documentation "remove the minimum element in HEAP and return the heap.")
  (:method :after ((h heap))
   (decf (heap-count h))))


(defgeneric heap-insert (heap priority item)
  (:documentation "insert ITEM into HEAP with PRIORITY, returning the
heap.")
  (:method :after ((h heap) priority item)
   (incf (heap-count h))))


(defgeneric check-heap-invariants (heap)
  (:method ((h heap))
   nil))
