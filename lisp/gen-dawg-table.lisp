(defun read-attr-def (path &aux acc)
  (each-file-line (line path)
    (push (list (subseq line 3) (parse-integer line :end 2 :radix 16)) acc))
  (sort (nreverse acc) #'string< :key #'first))

(defun read-map-def (path &aux acc)
  (each-file-line (line path)
    (let ((p (position #\Tab line)))
      (push (list (subseq line 0 p) (subseq line (1+ p))) acc)))
  (sort (nreverse acc) #'string< :key #'first))

(defparameter *cac*
  (read-map-def "data/canonical-composition.def"))

(defparameter *cad*
  (read-map-def "data/canonical-decomposition.def"))

(defparameter *cod*
  (read-map-def "data/compatibility-decomposition.def"))

(defparameter *ccc*
  (read-attr-def "data/canonical-combining-class.def"))

(defparameter *nic*
  (read-attr-def "data/nfc-illegal-char.def"))

(defparameter *nfic*
  (read-attr-def "data/nfkc-illegal-char.def"))

(defun add-prefix (prefix)
  (lambda (s)
    (s prefix (car s))))

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

(defparameter *octets* (string-to-octets *strs*))

(defun cat (strs)
  (reduce (lambda (acc s)
            (declare (simple-string s acc))
            (let ((p (search s acc)))
              (if (null p)
                  (concatenate 'string s acc)
                acc)))
          strs
          :initial-value ""))
  
(dawg:build :input *keys* :output "/tmp/key.idx")

(with-open-file (out "/tmp/str.dat" :direction :output
                     :if-exists :supersede
                     :element-type 'octet)
  (write-sequence (string-to-octets *strs*) out)
  'done)

(with-open-file (out "/tmp/val.dat" :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 32))
  (loop FOR as IN (list *cac* *cad* *cod*)
    DO
    (loop FOR (_ v) IN as
          FOR bv = (string-to-octets v)
          FOR p = (search (the simple-octets bv) (the simple-octets *octets*))
      DO
      (write-byte (dpb (length bv) (byte 8 24) p) out)))

  (loop FOR (_ attr) IN *ccc*
    DO
    (write-byte attr out))
  'done)

(defun gen-source (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "#ifndef UNF_TABLE2_HH~%")
    (format out "#define UNF_TABLE2_HH~%")
    (format out "namespace UNF {~%")
    (format out "namespace TABLE {~%")
    
    (with-open-file (in "/tmp/key.idx" :element-type '(unsigned-byte 64))
      (let ((base (ldb (byte 29 0) (progn #1=(read-byte in nil nil) #1#))))
        (format out "const unsigned CANONICAL_COM_ROOT = ~d;~%" (+ base (char-code #\0)))
        (format out "const unsigned CANONICAL_DECOM_ROOT = ~d;~%" (+ base (char-code #\1)))
        (format out "const unsigned COMPATIBILITY_DECOM_ROOT = ~d;~%" (+ base (char-code #\2)))
        (format out "const unsigned CANONICAL_CLASS_ROOT = ~d;~%" (+ base (char-code #\3)))
        (format out "const unsigned NFC_ILLEGAL_ROOT = ~d;~%" (+ base (char-code #\4)))
        (format out "const unsigned NFKC_ILLEGAL_ROOT = ~d;~%" (+ base (char-code #\5)))))
        
    (with-open-file (in "/tmp/key.idx" :element-type '(unsigned-byte 64))
      (format out "~%const unsigned NODES[]={")
      (read-byte in nil nil)
      (loop FOR v = (read-byte in nil nil)
            WHILE v
            FOR i FROM 0
        DO
        (when (zerop (mod i 5))
          (terpri out))
        (format out "0x~8,'0x," (ldb (byte 32 32) v))
        (format out "0x~8,'0x" (ldb (byte 32 0) v))
        (when (listen in)
          (format out ",")))
      (format out "};~%"))

    (with-open-file (in "/tmp/val.dat" :element-type '(unsigned-byte 32))
      (format out "~%const unsigned VALUES[]={")
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

    (with-open-file (in "/tmp/str.dat" :element-type '(signed-byte 8))
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