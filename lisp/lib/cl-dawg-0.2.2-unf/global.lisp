(defpackage dawg.global
  (:use :common-lisp)
  (:export ;; special variable
           *fastest*
           *interface*

           ;; type
           array-index
           positive-fixnum
           octet
           simple-characters
           unicode
           uint8
           uint4
           uint1

           ;; byte order
           +NATIVE_ORDER+
           byte-reverse

           ;; utility function
           fixnumize
           package-alias
           muffle
           a.if
           nlet
           with-open-output-file
           
           ;; symbol for anaphoric macro
           it))
(in-package :dawg.global)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; special variable for optimize declaration
(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
(defvar *interface* '(optimize (speed 3) (safety 2) (debug 1) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;
;;; type definition
(deftype array-index () `(mod ,array-dimension-limit))
(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype octet () '(unsigned-byte 8))
(deftype simple-characters () '(simple-array character))
(deftype unicode () `(mod ,char-code-limit))
(deftype uint8 () '(unsigned-byte 64))
(deftype uint4 () '(unsigned-byte 32))
(deftype uint1 () '(unsigned-byte 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unility function and macro
(declaim (inline fixnumize))
(defun fixnumize (n)
  (ldb (byte #.(integer-length most-positive-fixnum) 0) n))

(defmacro package-alias (package &rest alias-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package ',alias-list)))

(defmacro muffle (&body body)
  `(locally
    (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it
         ,then
       ,else)))

(defmacro nlet (fn-name letargs &body body)
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

(defmacro with-open-output-file ((stream path element-type &key (if-exists :supersede)) &body body)
  `(with-open-file (,stream ,path :direction :output
                                  :if-exists ,if-exists
                                  :element-type ,element-type)
     ,@body))

(declaim (inline byte-reverse))
(defun byte-reverse (n size)
  (declare ((member 2 4 8) size))
  (muffle
   (loop FOR u fixnum FROM (1- size) DOWNTO 0
         FOR l fixnum FROM 0 TO (1- size)
         WHILE (> u l)
     DO
     (rotatef (ldb (byte 8 (* u 8)) n)
              (ldb (byte 8 (* l 8)) n)))
   n))
