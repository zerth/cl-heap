(in-package :net.mwatters.heap)


(defstruct tree-node
  element
  left
  right)


(defstruct pqueue-node
  priority
  data)


(defun pqueue-node< (a b)
  (< (pqueue-node-priority a)
     (pqueue-node-priority b)))


(define-struct-like-class tree-based-heap (heap)
  (predicate #'pqueue-node<)
  root)


(defgeneric tree-based-heap-insert (heap element))


(defmethod heap-empty-p ((h tree-based-heap))
  (not (tree-based-heap-root h)))


(defmethod heap-insert ((h tree-based-heap) prio item)
  (tree-based-heap-insert h (make-pqueue-node :priority prio
                                              :data item)))



(defmethod check-heap-invariants :after ((h tree-based-heap))
  (unless (heap-empty-p h)
    (let ((root (tree-based-heap-root h))
          (pred (tree-based-heap-predicate h)))
      (check-tree-heap-invariants root pred))))


(defun check-tree-heap-invariants (root pred)
  (let ((a (tree-node-left root))
        (x (tree-node-element root))
        (b (tree-node-right root)))
    (when a
      (assert (not (funcall pred x (tree-node-element a))))
      (check-tree-heap-invariants a pred))
    (when b
      (assert (not (funcall pred (tree-node-element b) x)))
      (check-tree-heap-invariants b pred))))
