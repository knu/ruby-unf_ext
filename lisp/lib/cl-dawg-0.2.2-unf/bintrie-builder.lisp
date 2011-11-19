(defpackage dawg.bintrie-builder
  (:use :common-lisp :dawg.global)
  (:export build-from-file
           build-from-list
           collect-children
           node-label
           node-terminal?
           node-sibling-total
           node-child
           node-options
           element-count))
(in-package :dawg.bintrie-builder)

(package-alias :dawg.octet-stream :stream)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*
         (inline make-node collect-children calc-child-total calc-sibling-total 
                 node-options element-count))

;;;;;;;;
;;; node
(defstruct node
  (label         0 :type octet)
  (terminal?   nil :type boolean)
  (child       nil :type (or null node))
  (sibling     nil :type (or null node))
  (child-total   0 :type positive-fixnum) ; amount of child side nodes
  (sibling-total 0 :type positive-fixnum) ; amount of sibling side nodes
  (value -1 :type fixnum)
  (hash         -1 :type fixnum))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(macrolet ((calc-xxx-total (node slot)
             `(with-slots (,slot) (the node ,node)
                (if (null ,slot)
                    0
                  (the positive-fixnum
                       (+ (if (node-terminal? ,slot) 1 0)
                          (node-child-total ,slot) (node-sibling-total ,slot)))))))
  (defun calc-child-total (node) (calc-xxx-total node child))
  (defun calc-sibling-total (node) (calc-xxx-total node sibling)))

;;;;;;;;;;;;;;;;;
;;; hash function
(defun node= (n1 n2)
  (and (eq (node-child n1) (node-child n2))
       (eq (node-sibling n1) (node-sibling n2))
       (= (node-value n1) (node-value n2))
       (= (node-label n1) (node-label n2))
       (eq (node-terminal? n1) (node-terminal? n2))))

(defun sxhash-node (node)
  (if (null node)
      #.(sxhash nil)
    (with-slots (hash child-total sibling-total) (the node node)
      (when (= -1 hash)
        (setf hash (logxor (sxhash (node-label node))
                           (sxhash (node-value node))
                           (sxhash (node-terminal? node))
                           (fixnumize (* (sxhash-node (node-child node)) 7))
                           (fixnumize (* (sxhash-node (node-sibling node)) 13))))
        (setf child-total (calc-child-total node)
              sibling-total (calc-sibling-total node)))
      hash)))

;;;;;;;;;;;;;;;;;;
;;; build function
(defun share (node memo)
  (if (null node)
      nil
    (or (dict:get node memo)
        (progn 
          (setf (node-child node) (share (node-child node) memo)
                (node-sibling node) (share (node-sibling node) memo))
          (dict:get node memo))
        (setf (dict:get node memo) node))))

(defun push-child (in parent value)
  (if (stream:eos? in)
      (setf (node-terminal? parent) t
            (node-value parent) value)
    (let ((new-node (make-node :label (stream:read in))))
      (shiftf (node-sibling new-node) (node-child parent) new-node)
      (push-child in new-node value))))

(defun insert (in parent memo value)
  (let ((node (node-child parent)))
    (if (or (null node)
            (stream:eos? in)
            (/= (stream:peek in) (node-label node)))
        (progn
          (setf (node-child parent) (share node memo))
          (push-child in parent value))
      (insert (stream:eat in) node memo value))))

(defun build-impl (key-generator show-progress)
    (loop WITH trie = (make-node)
          WITH memo = (dict:make :test #'node= :hash #'sxhash-node)
          FOR num fixnum FROM 0
          FOR (key . value) = (funcall key-generator)
          WHILE key
      DO
      (when (and show-progress (zerop (mod num 100000)))
        (format t "~&;  ~A~%" num))
      (let ((in (stream:make key)))
        (declare (dynamic-extent in))
        (insert in trie memo value))

      FINALLY
      (return (share trie memo))))

(defun build-from-list (keyset &key show-progress)
  (when show-progress
    (format t "~&; build trie list (size ~A):~%" (length keyset)))
  (build-impl (lambda () (prog1 (car keyset)
                           (setf keyset (cdr keyset))))
              show-progress))
  
(defun build-from-file (filepath &key show-progress)
  (when show-progress
    (format t "~&; build trie from ~A:~%" filepath))
  (with-open-file (in filepath)
    (build-impl (lambda () (read-line in nil nil)) 
                show-progress)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; other external function
(defun node-options (node)
  "Encode terminal? and sibling-total fields into fixnum"
  (with-slots (terminal? sibling-total) (the node node)
    (fixnumize
     (+ (if terminal? 1 0)
        (ash sibling-total 1)))))

(defun element-count (node)
  (with-slots (terminal? child-total) (the node node)
    (the fixnum (+ (if terminal? 1 0) child-total))))
        
(defun collect-children (node)
  (loop WITH acc = '()
        FOR child = (node-child node)
               THEN (node-sibling child)
        WHILE child
    DO
    (push child acc)
    FINALLY
    (return acc)))

;;;;;;;;;;;;;
;;; for debug
(defun member? (key trie)
  (declare #.*interface*
           (simple-characters key)
           (node trie))
  (let ((in (stream:make key)))
    (declare (dynamic-extent in))
    (nlet recur ((in in) (node (node-child trie)) (parent trie))
      (cond ((stream:eos? in) (node-terminal? parent))
            ((null node) nil)
            ((= (stream:peek in) (node-label node))
             (recur (stream:eat in) (node-child node) node))
            ((< (stream:peek in) (node-label node))
             (recur in (node-sibling node) parent))))))

(package-alias :dawg.octet-stream)
