heap
====

Thread-safe heap implementations for Common Lisp.  Initial
implementation includes splay heaps.

Usage:

```
(let ((heap (heap:make-heap))
      (items (loop for i from 0 below 1024
                   for x = (random 1024)
                   collect x
                   when (zerop (mod i 2)) collect x)))

  (dolist (x items)
    (heap:heap-push heap x x))

  (assert (= (length items) (heap::heap-count heap)))

  (loop for x in (sort items #'<)
        do (multiple-value-bind (next pri empty-p)
               (heap:heap-pop heap)
             (assert (not empty-p))
             (assert (= next pri))
             (assert (= next x))))

  (assert (zerop (heap::heap-count heap))))
```
