;;;; splay heap implementation translated from Okasaki's Purely
;;;; Functional Data Structures.

(in-package :net.mwatters.heap)


(define-struct-like-class splay-heap (tree-based-heap)
  ;; cached element with minimum priority:
  min)


;; note: want these as auto-defined accessors, but need to extend
;; struct-like-classes impl (or just use structs).
(defmacro splay-heap-root (h)
  `(tree-based-heap-root ,h))
(defmacro splay-heap-predicate (h)
  `(tree-based-heap-predicate ,h))


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


(defmethod tree-based-heap-insert ((h splay-heap) e)
  (let ((r (splay-heap-root h))
        (p (splay-heap-predicate h)))
    (destructuring-bind (smaller . bigger)
        (splay-heap-partition r p e)
      (setf (splay-heap-root h) (make-tree-node
                                 :left smaller
                                 :element e
                                 :right bigger))
      ;; record new minimum element:
      (when (or (not (splay-heap-min h))
                (funcall p e (splay-heap-min h))
                ;; if the new element has the same priority as the
                ;; current minimum, it will be the new root element:
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


(defmethod heap-next ((h splay-heap))
  (let* ((saved-min (splay-heap-min h))
         (n (if saved-min
                saved-min
              (splay-heap-find-min (splay-heap-root h)))))
    (unless saved-min
      (setf (splay-heap-min h) n))
    (values (pqueue-node-data n)
            (pqueue-node-priority n)
            nil)))


(defmethod heap-remove-min ((h splay-heap))
  (setf
   (splay-heap-root h) (splay-heap-remove-min (splay-heap-root h))
   (splay-heap-min h) (splay-heap-find-min (splay-heap-root h)))
  h)
