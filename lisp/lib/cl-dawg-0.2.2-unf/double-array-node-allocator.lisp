(defpackage dawg.double-array.node-allocator
  (:use :common-lisp :dawg.global)
  (:export make
           allocate))
(in-package :dawg.double-array.node-allocator)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*
         (inline get-next can-allocate?))

;;;;;;;;;;;;
;;; constant
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +BUFFER_SIZE+ 89120))

;;;;;;;;;;;;;;;;;;
;;; node-allocator
(defstruct node-allocator 
  (head #x100 :type array-index)
  (bits   #*  :type (simple-bit-vector #.+BUFFER_SIZE+))
  (nexts #()  :type (simple-array fixnum (#.+BUFFER_SIZE+)))
  (prevs #()  :type (simple-array fixnum (#.+BUFFER_SIZE+)))
  (offset  0  :type array-index))

;;;;;;;;;;;;;;;
;;; constructor
(defun make ()
  (let ((bits  (make-array +BUFFER_SIZE+ :element-type 'bit :initial-element 0))
        (nexts (make-array +BUFFER_SIZE+ :element-type 'fixnum))
        (prevs (make-array +BUFFER_SIZE+ :element-type 'fixnum)))
    (loop FOR i FROM 0 BELOW +BUFFER_SIZE+ 
      DO
      (setf (aref nexts i) (1+ i)
            (aref prevs i) (1- i)))
    (make-node-allocator :nexts nexts :prevs prevs :bits bits)))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(defun shift (alloca)
  (with-slots (bits nexts prevs offset head) (the node-allocator alloca)
    (let ((new-offset head))
      (loop WHILE (< new-offset (+ offset (- +BUFFER_SIZE+ (* #x100 2))))
        DO
        (setf new-offset (aref nexts (- new-offset offset))))
      (let* ((delta (- new-offset offset))
             (use-len (- +BUFFER_SIZE+ delta)))
        (shiftf (subseq bits 0 use-len) (subseq bits delta))
        (fill bits 0 :start use-len)

        (setf offset new-offset)
        
        (shiftf (subseq nexts 0 use-len) (subseq nexts delta))
        (shiftf (subseq prevs 0 use-len) (subseq prevs delta))
        (loop FOR i FROM (+ offset use-len) BELOW (+ offset +BUFFER_SIZE+)
          DO
          (setf (aref nexts (- i offset)) (1+ i)
                (aref prevs (- i offset)) (1- i)))

        (setf head offset)
        (loop WHILE (< head (+ offset #x100))
          DO
          (setf head (aref nexts (- head offset)))))))
  alloca)

(defun ref (alloca index)
  (declare (array-index index))
  (with-slots (offset nexts) (the node-allocator alloca)
    (if (<= (+ offset +BUFFER_SIZE+) index)
        (ref (shift alloca) index) 
      (aref nexts (- index offset)))))

(defun bref (alloca index)
  (declare (array-index index))
  (with-slots (bits offset) (the node-allocator alloca)
    (if (> offset index)
        1
      (if (<= (+ offset +BUFFER_SIZE+) index)
          (bref (shift alloca) index)
        (bit bits (- index offset))))))

(defun get-next (alloca index)
  (ref alloca index))

(defun can-allocate? (alloca index arcs)
  (declare (list arcs)
           (array-index index))
  (and (zerop (bref alloca index))
       (every (lambda (arc)
                (declare (octet arc))
                (/= -1 (ref alloca (+ index arc))))
              arcs)))

(defun allocate-impl (alloca index arcs)
  (declare (array-index index))
  (with-slots (bits head prevs nexts offset) (the node-allocator alloca)
    (when (<= offset index)
      (setf (bit bits (- index offset)) 1))
    (loop WITH base = index
          FOR arc OF-TYPE (mod #x100) IN arcs
          FOR index OF-TYPE fixnum = (+ base arc)
      DO
      (when (<= offset index)
        (ref alloca index)

        (let ((prev (aref prevs (- index offset)))
              (next (aref nexts (- index offset))))
          (setf (aref prevs (- index offset)) -1
                (aref nexts (- index offset)) -1)
          
          (when (= head index)
            (setf head next))

          (when (<= offset prev)
            (setf (aref nexts (- prev offset)) next))

          (when (<= offset next)
            (ref alloca next)
            (setf (aref prevs (- next offset)) prev)))))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun allocate (alloca arcs)
  (with-slots (head) (the node-allocator alloca)
    (loop WITH front OF-TYPE (mod #x100) = (car arcs)
          FOR cur = (get-next alloca head) THEN (get-next alloca cur)
          FOR base OF-TYPE fixnum = (- cur front)
          UNTIL (and (plusp base) (can-allocate? alloca base (cdr arcs)))
      FINALLY
      (allocate-impl alloca base arcs)
      (return base))))
