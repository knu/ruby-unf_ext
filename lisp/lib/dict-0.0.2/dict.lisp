(in-package :dict)

(declaim (inline index make make-dict count))

(defstruct dict
  (count               0 :type positive-fixnum)
  (next-resize-trigger 0 :type positive-fixnum)
  (root-bitlen         0 :type fixnum-length)
  (root              #() :type simple-vector)
  (test            #'eql :type function)
  (hash         #'sxhash :type hash-function))

(defun make (&key (test #'eql) (hash #'sxhash))
  (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note)
           (function test)
           (hash-function hash))
  (make-dict :next-resize-trigger (* 16 4)
             :root-bitlen 4
             :test test
             :hash hash
             :root (make-array 16 :initial-element '())))

(defun count (dict)
  (dict-count dict))

(defun index (len hash-code)
  (ldb (byte len 0) hash-code))

(defun get (key dict)
  (declare #.*interface* (dict dict))
  (with-slots (root root-bitlen hash test) dict
    (declare #.*fastest*)
    (let ((entries (aref root (index root-bitlen (funcall hash key)))))
      (a.if (assoc key entries :test test)
            (values (cdr it) t)
        (values nil nil)))))

(defun resize (dict)
  (declare #.*fastest* (dict dict))
  (with-slots (root root-bitlen next-resize-trigger hash) dict
    (let ((new-root (make-array (* (length root) 16) :initial-element nil))
          (new-root-bitlen (+ root-bitlen 4)))
      (declare (fixnum-length new-root-bitlen))
      (loop FOR entries ACROSS root
        DO
        (loop FOR e IN entries
              FOR index = (index new-root-bitlen (funcall hash (car e)))
          DO
          (push e (aref new-root index))))
    (setf next-resize-trigger (the positive-fixnum (* next-resize-trigger 16))
          root new-root
          root-bitlen new-root-bitlen))))

(defun set (key value dict)
  (declare #.*interface* (dict dict))
  (with-slots (root root-bitlen count next-resize-trigger hash test) dict
    (let ((index (index root-bitlen (funcall hash key))))
      (multiple-value-bind (new-list added?) (acons! key value (aref root index) :test test)
        (when added?
          (setf (aref root index) new-list)
          (incf count)
          (when (= count next-resize-trigger)
            (resize dict))))))
  value)

(defsetf get (key dict) (new-value)
  `(set ,key ,new-value ,dict))

(defun remove (key dict)
  (declare #.*interface* (dict dict))
  (with-slots (root root-bitlen hash test count) dict
    (declare #.*fastest*)
    (let ((index (index root-bitlen (funcall hash key)))
          (exists? nil))
      (setf #1=(aref root index) 
            (delete-if (lambda (e)
                         (and (funcall test key (car e))
                              (decf count)
                              (setf exists? t)))
                       (the list #1#)))
      exists?)))

(defmacro each ((entry dict &optional result-form) &body body)
  (let ((entries (gensym)))
    `(loop FOR ,entries ACROSS (dict-root ,dict)
       DO
       (loop FOR ,entry IN ,entries
         DO
         (locally ,@body))
       FINALLY
       (return ,result-form))))

(defun map (fn dict)
  (declare #.*interface*
           (dict dict)
           (function fn))
  (let ((acc '()))
    (each (e dict (nreverse acc))
      (push (funcall fn (car e) (cdr e)) acc))))
