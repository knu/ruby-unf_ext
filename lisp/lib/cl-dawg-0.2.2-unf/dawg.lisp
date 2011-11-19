(defpackage dawg
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export dawg
           build
           load
           member?
           get-id))
(in-package :dawg)

(package-alias :dawg.octet-stream :stream)

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(eval-when (:compile-toplevel)
  (defvar *args-type* '(simple-characters dawg &key (:start positive-fixnum)
                                                    (:end positive-fixnum))))
(defconstant +ARC_LIMIT+ #x100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dawg (double-array format)
(defstruct dawg
  (node #() :type (simple-array uint4)))

(defmethod print-object ((o dawg) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A:~A" :node-count (length (dawg-node o)))))

;;;;;;;;;;;;;;;
;;; declamation
(declaim (inline check-encoded-children get-node 
                 base chck terminal? sibling-total inc-id 
                 get-id-impl member?-impl
                 each-common-prefix-impl each-predictive-impl)
         (ftype (function #.*args-type* boolean) member?)
         (ftype (function #.*args-type* (or null positive-fixnum)) get-id))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(1)
(defun read-array (index-path &key size element-type offset byte-order)
  (declare ((member uint4 uint8) element-type))
  (with-open-file (in index-path :element-type element-type)
    (file-position in offset)
    (let ((ary (make-array size :element-type element-type)))
      (read-sequence ary in)
      (unless (or (eq byte-order :native)
                  (eq byte-order +NATIVE_ORDER+))
        (let ((byte-size (ecase element-type
                           (uint4 4)
                           (uint8 8))))
          (dotimes (i size)
            (setf (aref ary i) (byte-reverse (aref ary i) byte-size)))))
      ary)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(1)
(defun build (&key input output (byte-order :native) show-progress) 
  (declare ((or string pathname list) input)
           ((or string pathname) output)
           ((member :native :little :big) byte-order))
  (let ((trie (if (listp input)
                  (dawg.bintrie-builder:build-from-list input :show-progress show-progress)
                (dawg.bintrie-builder:build-from-file input :show-progress show-progress))))
    (dawg.double-array-builder:build-from-bintrie 
     trie :output-file output :byte-order byte-order :show-progress show-progress))
  t)

(defun load (index-path &key (byte-order :native))
  (declare ((or string pathname file-stream) index-path)
           ((member :native :little :big) byte-order))
  (let ((sizes (read-array index-path :size 2 :element-type 'uint4 :offset 0 
                                      :byte-order byte-order)))
    (make-dawg
     :node (read-array index-path :element-type 'uint4
                                  :size (/ (aref sizes 0) 4)
                                  :offset 2
                                  :byte-order byte-order))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(2)
(defun base (node) (ldb (byte 24  0) (the uint8 node)))
(defun chck (node) (ldb (byte  8 24) (the uint8 node)))

(defun get-node (dawg index)
  (declare (dawg dawg)
           (positive-fixnum index))
  (aref (dawg-node dawg) index))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(2)
(defun member?-impl (in dawg)
  (nlet recur ((node (get-node dawg 0)))
    (if (stream:eos? in)
        t 
      (let* ((arc (stream:read in))
             (next (get-node dawg (+ (base node) arc))))
        (when (= (chck next) arc)
          (recur next))))))

(defun get-id-impl (in dawg)
  (nlet recur ((node (get-node dawg 0)))
    (print (list :in #1=(+ (base node) (stream:peek in)) (get-node dawg #1#)
                 (stream:peek in)))
    (if (stream:eos? in)
        (ldb (byte 24 0) node) 
      (let* ((arc (stream:read in))
             (next (get-node dawg (+ (base node) arc))))
        (when (= (chck next) arc)
          (recur next))))))


(defmacro with-key-stream ((in key &key start end) &body body)
  (let ((k (gensym))
        (s (gensym))
        (e (gensym)))
    `(let ((,k ,key)
           (,s ,start)
           (,e ,end))
       (declare #.*interface*
                (simple-characters ,k)
                (positive-fixnum ,s ,e))
       (locally
        (declare #.*fastest*)
        (let ((,in (stream:make ,k :start ,s :end ,e)))
          (declare (dynamic-extent ,in))
          ,@body)))))

(defun member? (key dawg &key (start 0) (end (length key)))
  (with-key-stream (in key :start start :end end)
    (member?-impl in dawg)))
  
(defun get-id (key dawg &key (start 0) (end (length key)))
  (with-key-stream (in key :start start :end end)
    (get-id-impl in dawg)))

(package-alias :dawg.octet-stream)
