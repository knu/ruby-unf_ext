;; TODO: 整理
(require :asdf)

;;
(unless (= (length sb-ext:*posix-argv*) 3)
  (format *error-output* "~&Usage: sbcl --script gen-table.lisp DATA_DIR OUTPUT_TABLE_FILE~%")
  (sb-ext:quit))

(defparameter *data-dir* (pathname (second sb-ext:*posix-argv*)))
(defparameter *table-hh* (pathname (third sb-ext:*posix-argv*)))

;;
(defun load-local-system (package &optional (package-directory #P"./"))
  (let #.`((asdf:*central-registry* (directory package-directory))
           ;; or #+ASDF2
           ,@(when #.#1=(find-symbol "*DEFAULT-SOURCE-REGISTRIES*" :asdf)
                   `((,#1# nil))))
       (asdf:load-system package)))

(defmacro each-file-line ((line filepath &rest keys) &body body)
  `(with-open-file (#1=#:in ,filepath ,@keys)
     (let (,line)
       (loop while (setf ,line (read-line #1# nil nil nil))
             DO (locally ,@body)))))

(defun s (&rest args)
  "ARGSを連接した文字列に変換する"
  (with-output-to-string (s)
    (dolist (a args)
      (typecase a
        (string    (write-string a s))
        (character (write-char a s))
        (otherwise (princ a s))))))

(defun flatten (lst &aux acc)
  (labels ((self (x)
             (if (consp x)
                 (progn (self (car x)) (self (cdr x)))
               (when x
                 (push x acc)))))
    (self lst)
    (nreverse acc)))

(load-local-system :dict #P"lib/dict-0.0.2/")
(load-local-system :dawg #P"lib/cl-dawg-0.2.2-unf/")

;;
(defun read-attr-def (path &aux acc)
  (each-file-line (line path)
    (push (list (subseq line 3) (parse-integer line :end 2 :radix 16)) acc))
  (sort (nreverse acc) #'string< :key #'first))

(defun read-map-def (path &aux acc)
  (each-file-line (line path)
    (let ((p (position #\Tab line)))
      (push (list (subseq line 0 p) (subseq line (1+ p))) acc)))
  (sort (nreverse acc) #'string< :key #'first))

(let ((*default-pathname-defaults* (probe-file *data-dir*)))
  (defparameter *cac*
    (read-map-def "canonical-composition.def"))

  (defparameter *cad*
    (read-map-def "canonical-decomposition.def"))
  
  (defparameter *cod*
    (read-map-def "compatibility-decomposition.def"))
  
  (defparameter *ccc*
    (read-attr-def "canonical-combining-class.def"))
  
  (defparameter *nic*
    (read-attr-def "nfc-illegal-char.def"))
  
  (defparameter *nfic*
    (read-attr-def "nfkc-illegal-char.def")))

;;
(defun add-prefix (prefix)
  (lambda (s)
    (s prefix (car s))))

(defun cat (strs)
  (reduce (lambda (acc s)
            (declare (simple-string s acc))
            (let ((p (search s acc)))
              (if (null p)
                  (concatenate 'string s acc)
                acc)))
          strs
          :initial-value ""))

(defparameter *keys*
  (flatten 
   (list (mapcar (add-prefix "0") *cac*)
         (mapcar (add-prefix "1") *cad*)
         (mapcar (add-prefix "2") *cod*)
         (mapcar (add-prefix "3") *ccc*)
         (mapcar (add-prefix "4") *nic*)
         (mapcar (add-prefix "5") *nfic*))))

(defparameter *strs*
  (cat
   (sort
    (flatten 
     (list (mapcar #'second *cac*)
           (mapcar #'second *cad*)
           (mapcar #'second *cod*)))
    #'> :key #'length)))

(defparameter *octets* (sb-ext:string-to-octets *strs*))

(with-open-file (out "/tmp/unf.str.dat" :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
  (write-sequence *octets* out)
  'done)

(defparameter *vals*   
  (flatten
   (list 
    (loop FOR as IN (list *cac* *cad* *cod*)
      COLLECT
      (loop FOR (_ v) IN as
            FOR bv = (string-to-octets v)
            FOR p = (search bv *octets*)
        COLLECT (progn
                  (assert (and (<= (integer-length p) 18)
                               (<= (integer-length (length bv)) 6)))
                  (dpb (length bv) (byte 6 18) p))))

    (loop FOR (_ attr) IN *ccc* COLLECT attr)

    (loop REPEAT (+ (length *nic*) (length *nfic*)) COLLECT 0))))

(defparameter *kvs* (mapcar (lambda (x y)
                              (cons (s x (code-char 0)) y))
                            *keys* *vals*))

;;
(dawg:build :input *kvs* :output "/tmp/unf.key.idx")

(defun gen-source (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "#ifndef UNF_TABLE_HH~%")
    (format out "#define UNF_TABLE_HH~%")
    (format out "namespace UNF {~%")
    (format out "namespace TABLE {~%")
    
    (with-open-file (in "/tmp/unf.key.idx" :element-type '(unsigned-byte 32))
      (let ((base (ldb (byte 24 0) (progn #1=(read-byte in nil nil) #1# #1#))))
        (format out "const unsigned CANONICAL_COM_ROOT = ~d;~%" (+ base (char-code #\0)))
        (format out "const unsigned CANONICAL_DECOM_ROOT = ~d;~%" (+ base (char-code #\1)))
        (format out "const unsigned COMPATIBILITY_DECOM_ROOT = ~d;~%" (+ base (char-code #\2)))
        (format out "const unsigned CANONICAL_CLASS_ROOT = ~d;~%" (+ base (char-code #\3)))
        (format out "const unsigned NFC_ILLEGAL_ROOT = ~d;~%" (+ base (char-code #\4)))
        (format out "const unsigned NFKC_ILLEGAL_ROOT = ~d;~%" (+ base (char-code #\5)))))
        
    (with-open-file (in "/tmp/unf.key.idx" :element-type '(unsigned-byte 32))
      (format out "~%const unsigned NODES[]={")
      (read-byte in nil nil)
      (read-byte in nil nil)
      (loop FOR v = (read-byte in nil nil)
            WHILE v
            FOR i FROM 0
        DO
        (when (zerop (mod i 10))
          (terpri out))
        (format out "0x~8,'0x" v)
        (when (listen in)
          (format out ",")))
      (format out "};~%"))

    (with-open-file (in "/tmp/unf.str.dat" :element-type '(signed-byte 8))
      (format out "~%const char STRINGS[]={")
      (loop FOR c = (read-byte in nil nil)
            WHILE c
            FOR i FROM 0
        DO
        (when (zerop (mod i 20))
          (terpri out))
        (format out "~4d" c)
        (when (listen in)
          (format out ",")))
      (format out "};~%"))
    
    (format out "}~%")
    (format out "}~%")
    (format out "#endif~%")))

;;
(gen-source *table-hh*)
