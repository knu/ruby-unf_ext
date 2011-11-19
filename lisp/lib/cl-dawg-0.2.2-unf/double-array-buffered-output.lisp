(defpackage dawg.double-array.buffered-output
  (:use :common-lisp :dawg.global)
  (:export buffered-output
           with-output
           write-uint))
(in-package :dawg.double-array.buffered-output)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*)

;;;;;;;;;;;;
;;; constant
(defconstant +BUFFER_SIZE+ 819200)

;;;;;;;;;;;;;;;;;;;
;; buffered-output
(defstruct buffered-output
  (binary-output nil :type file-stream)
  (buffer        #() :type simple-array)
  (offset          0 :type array-index))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defmacro with-output ((out path &key (byte-width 1)) &body body)
  (declare ((member 1 2 4 8) byte-width))
  `(with-open-file (,out ,path :element-type #1='(unsigned-byte ,(* 8 byte-width))
                               :direction :output
                               :if-exists :supersede)
     (let ((,out (make-buffered-output 
                  :binary-output ,out
                  :buffer (make-array ,+BUFFER_SIZE+ :element-type #1#
                                                     :initial-element #xFF000000))))
       (unwind-protect
           (locally ,@body)
         (flush ,out :final t)))))

(defun write-uint (uint out &key (position 0))
  (declare (buffered-output out)
           (positive-fixnum position))
  (with-slots (binary-output buffer offset) out
    (cond ((< position offset)
           (file-position binary-output position)
           (write-byte uint binary-output))
          ((< position (+ offset +BUFFER_SIZE+))
           (muffle
            (setf (aref buffer (- position offset)) uint)))
          (t
           (flush out)
           (incf offset +BUFFER_SIZE+)
           (fill buffer #xFF000000)
           (write-uint uint out :position position)))))

(defun flush (out &key final)
  (declare (buffered-output out))
  (with-slots (binary-output buffer offset) out
    (file-position binary-output offset)
    (if (null final)
        (write-sequence buffer binary-output)
      (let ((end (muffle
                  (or (position-if-not (lambda (x) (= x #xFF000000)) buffer :from-end t)
                      (1- +BUFFER_SIZE+)))))
        (write-sequence buffer binary-output :end (1+ end))
        (loop REPEAT #x100 DO (write-byte #xFF000000 binary-output))))))
