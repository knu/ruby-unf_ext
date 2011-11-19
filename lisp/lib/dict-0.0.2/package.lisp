(defpackage dict
  (:use :common-lisp)
  (:shadow :common-lisp get set remove count map)
  (:export dict
           make
           count
           get
           remove
           each
           map))
(in-package :dict)

(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype fixnum-length () `(integer 0 ,(integer-length most-positive-fixnum)))
(deftype hash-function () #+SBCL `(function (t) (values positive-fixnum))
                          #-SBCL 'function)
(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 2) (debug 1)))
