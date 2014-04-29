;;;; splay heap implementation translated from okasaki's purely
;;;; functional data structures.

(defpackage :net.mwatters.heap
  (:nicknames :heap)
  (:use :common-lisp)
  (:import-from :struct-like-classes
   :define-struct-like-class)
  (:import-from :lockable
   :lockable
   :with-thing-locked)
  (:import-from :alexandria
   :with-gensyms))

(in-package :heap)


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


(defstruct splay-heap
  (predicate #'pqueue-node<)
  root
  min)


(defun sheap-empty-p (h)
  (not (splay-heap-root h)))


(defmacro with-splay-pivoting ((left elt right) node
                               pred pivot
                               &key empty smaller otherwise)
  "helper macro for expressing splay tree pivoting logic.  if NODE is
empty \(nil\), return the value of EMPTY.  otherwise, bind the vars
denoted by LEFT, ELT, and RIGHT to the corresponding values of NODE.

if the value of ELT is less than the value of PIVOT according to PRED,
return the value of SMALLER.  otherwise return the value of
OTHERWISE."
  (with-gensyms (n p)
    `(let ((,n ,node)
           (,p ,pivot))
       (symbol-macrolet
           ((,left (tree-node-left ,n))
            (,elt (tree-node-element ,n))
            (,right (tree-node-right ,n)))
         (cond
          ((not ,n)
           ,empty)
          ((funcall ,pred ,elt ,p)
           ,smaller)
          (t
           ,otherwise))))))


(defun splay-heap-partition (root pred pivot)
  "return two splay trees containing all elements in the tree-node
ROOT less than and greater than or equal to PIVOT according to PRED,
respectively."
  (with-splay-pivoting (a x b) root
                       pred pivot
    :empty (cons nil nil)
    ;; X is smaller than the pivot (A, X will be in left subtree):
    :smaller (with-splay-pivoting (b1 y b2) b
                                  pred pivot
               :empty (cons root nil)
               ;; Y is smaller than the pivot (B1, Y will be in left
               ;; subtree, split up B2):
               :smaller (destructuring-bind (smaller . bigger)
                            (splay-heap-partition b2 pred pivot)
                          (cons (make-tree-node
                                 :left (make-tree-node
                                        :left a
                                        :element x
                                        :right b1)
                                 :element y
                                 :right smaller)
                                bigger))
               ;; Y is gte the pivot (Y, B2 will be in right subtree,
               ;; split up B1):
               :otherwise (destructuring-bind (smaller . bigger)
                              (splay-heap-partition b1 pred pivot)
                            (cons (make-tree-node
                                   :left a
                                   :element x
                                   :right smaller)
                                  (make-tree-node
                                   :left bigger
                                   :element y
                                   :right b2))))
    ;; X is gte the pivot (X, B will be in right subtree):
    :otherwise (with-splay-pivoting (a1 y a2) a
                                    pred pivot
                 :empty (cons nil root)
                 ;; Y is smaller than the pivot (A1, Y will be in left
                 ;; subtree, split up A2):
                 :smaller (destructuring-bind (smaller . bigger)
                              (splay-heap-partition a2 pred pivot)
                            (cons (make-tree-node
                                   :left a1
                                   :element y
                                   :right smaller)
                                  (make-tree-node
                                   :left bigger
                                   :element x
                                   :right b)))
                 ;; Y is gte the pivot (Y, A2 will be in right
                 ;; subtree, split up A1):
                 :otherwise (destructuring-bind (smaller . bigger)
                                (splay-heap-partition a1 pred pivot)
                              (cons smaller
                                    (make-tree-node
                                     :left bigger
                                     :element y
                                     :right (make-tree-node
                                             :left a2
                                             :element x
                                             :right b)))))))

(defun splay-heap-insert (h e)
  (let ((r (splay-heap-root h))
        (p (splay-heap-predicate h)))
    (destructuring-bind (smaller . bigger)
        (splay-heap-partition r p e)
      ;; fixme; return new splay heap instance?
      (setf (splay-heap-root h) (make-tree-node
                                 :left smaller
                                 :element e
                                 :right bigger))
      ;; record new minimum element:
      (when (or (not (splay-heap-min h))
                (funcall p e (splay-heap-min h))
                ;; if the new element has the same priority, it will
                ;; be closer to the root and will be the actual new
                ;; min elt:
                (and (not (funcall p e (splay-heap-min h)))
                     (not (funcall p (splay-heap-min h) e))))
        (setf (splay-heap-min h) e))
      h)))


(defun splay-heap-find-min (n)
  (when n
    (let ((left (tree-node-left n)))
      (if left
          (splay-heap-find-min left)
        (tree-node-element n)))))


(defun splay-heap-remove-min (n)
  (when n
    (let ((left (tree-node-left n)))
      (if (not left)
          (tree-node-right n)
        (symbol-macrolet
            ((a (tree-node-left left))
             (x (tree-node-element left))
             (b (tree-node-right left))
             (y (tree-node-element n))
             (c (tree-node-right n)))
          (if (not a)
              (make-tree-node :left b
                              :element y
                              :right c)
            (make-tree-node :left (splay-heap-remove-min a)
                            :element x
                            :right (make-tree-node :left b
                                                   :element y
                                                   :right c))))))))



(defun sheap-next (h)
  (when (sheap-empty-p h)
    (error "empty heap: ~S" h))
  (let* ((saved-min (splay-heap-min h))
         (n (if saved-min
                saved-min
              (splay-heap-find-min (splay-heap-root h)))))
    (unless saved-min
      (setf (splay-heap-min h) n))
    (values (pqueue-node-data n)
            (pqueue-node-priority n))))


(defun sheap-remove-min (h)
  (when (sheap-empty-p h)
    (error "empty heap: ~S" h))
  (setf
   (splay-heap-root h) (splay-heap-remove-min (splay-heap-root h))
   (splay-heap-min h) (unless (sheap-empty-p h)
                        (splay-heap-find-min (splay-heap-root h))))
  h)


(defun sheap-insert (h prio item)
  (splay-heap-insert h (make-pqueue-node :priority prio
                                         :data item)))


(defun check-heap-invariants (h)
  (check-splay-heap-invariants (splay-heap-root h)
                               (splay-heap-predicate h)))


(defun check-splay-heap-invariants (root pred)
  (when root
    (let ((a (tree-node-left root))
          (x (tree-node-element root))
          (b (tree-node-right root)))
      (when a
        (assert (not (funcall pred x (tree-node-element a))))
        (check-splay-heap-invariants a pred))
      (when b
        (assert (not (funcall pred (tree-node-element b) x)))
        (check-splay-heap-invariants b pred)))))



;;;; thread-safe implementation


(define-struct-like-class heap (lockable)
  (actual (make-splay-heap))
  (count 0))


(defmacro heap-pop (place)
  "return as three values the next item from the heap, its priority,
and whether the heap was empty."
  (with-gensyms (next pri hv)
    `(let ((,hv ,place))
       (with-thing-locked ,hv
         (if (heap-empty-p ,hv)
             (values nil nil t)
           (multiple-value-bind (,next ,pri)
               (sheap-next (heap-actual ,hv))
             (values-list (prog1 (list ,next ,pri nil)
                            (decf (heap-count ,hv))
                            (setf (heap-actual ,hv)
                                  (sheap-remove-min (heap-actual ,hv)))))))))))


(defmacro heap-push (heap priority item)
  (with-gensyms (hv)
    `(let ((,hv ,heap))
       (with-thing-locked ,hv
         (setf (heap-actual ,hv)
               (sheap-insert (heap-actual ,hv) ,priority ,item))
         (incf (heap-count ,hv))
         ,hv))))


(defmacro heap-next (heap)
  (with-gensyms (hv)
    `(let ((,hv ,heap))
       (with-thing-locked ,hv
         (if (heap-empty-p ,hv)
             (values nil nil t)
           (sheap-next (heap-actual ,hv)))))))


(defun heap-empty-p (heap)
  (sheap-empty-p (heap-actual heap)))
