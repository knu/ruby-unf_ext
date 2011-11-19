(defpackage dawg.double-array-builder
  (:use :common-lisp :dawg.global)
  (:export build-from-bintrie))
(in-package :dawg.double-array-builder)

(package-alias :dawg.double-array.node-allocator :node-allocator)
(package-alias :dawg.double-array.buffered-output :output)
(package-alias :dawg.bintrie-builder :bintrie)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*
         (inline set-base set-chck set-opts))

;;;;;;;;;;;;
;;; constant
(defconstant +BUFFER_SIZE+ 819200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; da (abbreviation of "double array")
(defstruct da
  (node t :type output:buffered-output)
  (exts t :type stream)
  (done-count 0 :type positive-fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
(defstruct node
  (index         0 :type positive-fixnum)
  (base          0 :type positive-fixnum)
  (terminal?     t :type boolean)
  (sibling-total 0 :type positive-fixnum)
  (chck          0 :type uint1)
  (children    '() :type list))

(defun new-node (parent-base-idx trie)
  (declare (positive-fixnum parent-base-idx))
  (make-node :index (+ parent-base-idx (bintrie:node-label trie))
             :sibling-total #1=(bintrie:node-sibling-total trie)
             :terminal? (bintrie:node-terminal? trie)
             :base (if (bintrie:node-terminal? trie) (bintrie::node-value trie) 0)
             :chck (bintrie:node-label trie)))

(defun child-acceptable-p (node)
  nil)

(defun add-child (node child)
  (with-slots (children) (the node node)
    (setf children (nconc children (list (bintrie:node-label child))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function

(defun merge-files-native-order (destination files)
  ;; write each file size
  (with-open-output-file (out destination 'uint4)
    (loop FOR (file) IN files
          DO (with-open-file (in file :element-type 'uint1)
               (write-byte (file-length in) out))))
  
  ;; write each file content
  (with-open-output-file (out destination 'uint1 :if-exists :append)
    (loop FOR (file) IN files
          DO (with-open-file (in file :element-type 'uint1)
               (loop FOR b = (read-byte in nil nil)
                     WHILE b
                     DO (write-byte b out))))))

(defun merge-files-reverse-order (destination files)
  ;; write each file size
  (with-open-output-file (out destination 'uint4)
    (loop FOR (file) IN files
      DO (with-open-file (in file :element-type 'uint1)
           (write-byte (byte-reverse (file-length in) 4) out))))
  
  ;; write each file content
  (loop FOR (file type) IN files
        FOR byte-size = (ecase type (uint4 4) (uint4 4))
    DO
    (with-open-output-file (out destination type :if-exists :append)
      (with-open-file (in file :element-type type)
        (loop FOR b = (read-byte in nil nil)
              WHILE b
          DO (write-byte (byte-reverse b byte-size) out))))))

(defun merge-files (destination byte-order files)
  (if (or (eq byte-order :native)
          (eq byte-order +NATIVE_ORDER+))
      (merge-files-native-order destination files)
    (merge-files-reverse-order destination files))
  (mapc #'delete-file (mapcar #'first files)))

(defmacro show (fmt &rest args)
  `(when show-progress
     (format t ,fmt ,@args)))

;;;;;;;;;;;;;;;;;;
;;; build function
(defun write-node-impl (node da)
  (with-slots (index type base terminal? sibling-total chck children) (the node node)
    (let ((n 0))
      (declare ((unsigned-byte 32) n))
      (setf (ldb (byte 24  0) n) base
            (ldb (byte  8 24) n) chck)
      (output:write-uint n (da-node da) :position index))))

(defun write-node (node da &key base)
  (when base
    (setf (node-base node) base))
  (write-node-impl node da))

(defmacro show-and-write-node (node da &key base)
  `(progn 
     (incf #1=(da-done-count ,da))
     (when (and show-progress (zerop (mod #1# 100000)))
       (show ";  ~a nodes~%" #1#))
     (write-node ,node ,da :base ,base)))

(defun build-impl (trie alloca da node memo &optional show-progress)
  (let ((children (bintrie:collect-children trie)))
    (loop WHILE (and (not #1=(gethash (bintrie:node-child trie) memo))
                     (null (cdr children))
                     (not (bintrie::node-terminal? (car children)))
                     (child-acceptable-p node))
      DO
      (add-child node (car children))
      (setf trie (car children))
      (setf children (bintrie:collect-children trie)))
  
    (a.if #1#
          (show-and-write-node node da :base it)
      (if (null children)
          (show-and-write-node node da)
        (let ((base-idx (node-allocator:allocate
                         alloca 
                         (mapcar #'bintrie:node-label children))))
          (setf #1# base-idx)
          (show-and-write-node node da :base base-idx)
            
          (dolist (child children)
            (build-impl child alloca da (new-node base-idx child) memo show-progress)))))))
                        

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun build-from-bintrie (trie &key output-file byte-order show-progress)
  (show "~2&; build double array from trie:~%")
  (let ((node-file (format nil "~a.node" output-file))
        (exts-file (format nil "~a.ext" output-file)))
    (show ";  create tmpfiles: ~a, ~a~%" node-file exts-file)

    (show "; build:~%")
    (output:with-output (node node-file :byte-width 4)
      (with-open-output-file (exts exts-file 'uint4)
        (let ((da (make-da :node node :exts exts)))
          (build-impl trie (node-allocator:make) da 
                      (new-node 0 trie)
                      (make-hash-table :test #'eq)
                      show-progress))))
    (show "; concatenate tempfiles to ~A~%"  output-file)
    (merge-files output-file byte-order `((,node-file uint4) (,exts-file uint4))))
  'done)

(package-alias :dawg.double-array.node-allocator)
(package-alias :dawg.double-array.buffered-output)
(package-alias :dawg.bintrie-builder)
