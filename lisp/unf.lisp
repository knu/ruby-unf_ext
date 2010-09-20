(defpackage unf
  (:use :common-lisp)
  (:export normalize
           get-canonical-combining-class-map
           get-mapping
           get-illegal-char-list))
(in-package :unf)

;;;;;;;;;;;;;;
;;;; libraries 
(require :cl-ppcre)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; auxiliary functions (1)
(defun parse-hex      (s) (parse-integer s :radix 16))
(defun parse-hex-char (s) (code-char (parse-hex s)))
(defmacro nconcf (list1 list2) `(setf ,list1 (nconc ,list1 ,list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parse UnicodeData.txt
(let ((*default-pathname-defaults* *load-pathname*))
  (defvar *unicode-data*
    (with-open-file (in "UnicodeData.txt")
      (loop For line = (read-line in nil nil)
            WHILE line
        COLLECT (cl-ppcre:split ";" line)))))

(let ((canonical-decomp-map (make-hash-table))
      (compatible-decomp-map (make-hash-table))
      (canonical-combining-class (make-hash-table)))
  (loop FOR (1st _ __ 4th ___ 6th) IN *unicode-data*
        FOR char = (parse-hex-char 1st)
        FOR ccc  = (parse-integer 4th)
        FOR decomp-chars =
        (let ((tmp (cl-ppcre:split " " 6th)))
          (when tmp
            (if (char= #\< (char (first tmp) 0))
                (cons :compatible (mapcar #'parse-hex-char (cdr tmp))) ; 互換分解
              (cons :canonical (mapcar #'parse-hex-char tmp)))))       ; 正規分解
    DO
    (when (plusp ccc)
      (setf (gethash char canonical-combining-class) ccc))

    (when decomp-chars
      (if (eq (car decomp-chars) :canonical)
          (setf (gethash char canonical-decomp-map) (cdr decomp-chars))   ; 正規分解
        (setf (gethash char compatible-decomp-map) (cdr decomp-chars))))) ; 互換分解

  (defvar *canonical-decomp-map* canonical-decomp-map)
  (defvar *compatible-decomp-map* compatible-decomp-map)
  (defvar *canonical-combining-class* canonical-combining-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parse CompositionExclusions.txt
(let ((*default-pathname-defaults* *load-pathname*)
      (comp-exclusion-set (make-hash-table)))
  (with-open-file (in "CompositionExclusions.txt")
    (loop FOR line = (read-line in nil nil)
          WHILE line
          WHEN (and (plusp (length line))
                    (char/= (char line 0) #\#))
      DO
      (setf (gethash (parse-hex-char (subseq line 0 (position #\Space line)))
                     comp-exclusion-set)
            t)))
  (defparameter *comp-exclusion-set* comp-exclusion-set))

(let ((canonical-comp-map (make-hash-table :test #'equal)))
  (maphash 
   (lambda (src-char decomped-chars)
     (when (and (= 2 (length decomped-chars))
                (not (gethash src-char *comp-exclusion-set*)))
       (setf (gethash (coerce decomped-chars 'list)
                      canonical-comp-map)
             src-char)))
   *canonical-decomp-map*)
  (defparameter *canonical-comp-map* canonical-comp-map))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; auxiliary functions (2)
(defun get-canonical-combining-class (ch)
  (gethash ch *canonical-combining-class* 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; decomposition/ordering/composition
(defun decompose-char (char &optional (type :canonical))
  (let ((decomped-chars (or (gethash char *canonical-decomp-map*)
                            (and (eq type :compatible) 
                                 (gethash char *compatible-decomp-map*)))))
    (if decomped-chars
        (mapcan (lambda (c) (decompose-char c type)) decomped-chars)
      (list char))))

(let* ((s-base #xAC00)
       (l-base #x1100)
       (v-base #x1161)
       (t-base #x11A7)
       (l-count 19)
       (v-count 21)
       (t-count 28)
       (n-count (* v-count t-count))
       (s-count (* l-count n-count)))
  ;; 分割
  (defun decompose-hangul-char (ch &aux (cd (char-code ch)))
    (let ((s-index (- cd s-base)))
      (unless (<= 0 s-index (1- s-count))
        (return-from decompose-hangul-char (list ch)))
      
      (let ((lc (+ l-base (floor s-index n-count)))
            (vc (+ v-base (floor (mod s-index n-count) t-count)))
            (tc (+ t-base (mod s-index t-count))))
        (if (/= tc t-base)
            (list (code-char lc) (code-char vc) (code-char tc))
          (list (code-char lc) (code-char vc))))))
  
  ;; 合成
  (defun compose-hangul (str &aux (len (length str)))
    (if (zerop len)
        str
      (let* ((last (char str 0))
             (new-chars (list last)))
        (loop FOR i FROM 1 BELOW len
              FOR ch = (char str i)
              FOR l-index = (- (char-code last) l-base)
              FOR s-index = (- (char-code last) s-base)
          DO
          (tagbody
           ;; 1. check to see if two current characters are L and V
           (when (<= 0 l-index (1- l-count))
             (let ((v-index (- (char-code ch) v-base)))
               (when (<= 0 v-index (1- v-count))
                 ;; make syllable of form LV
                 (setf last 
                       (code-char (+ s-base (* (+ (* l-index v-count) v-index) t-count))))
		 (setf (car new-chars) last) ; reset last
                 (go :end))))                ; discard ch
           
           ;; 2. check to see if two current characters are LV and T
           (when (and (<= 0 s-index (1- s-count))
                      (zerop (mod s-index t-count)))
             (let ((t-index (- (char-code ch) t-base)))
               (when (< 0 t-index t-count)
                 ;; make syllable of form LVT
                 (setf last (code-char (+ (char-code last) t-index)))
                 (setf (car new-chars) last) ; reset last
                 (go :end))))                ; discard ch

           ;; if neigher case was true, just add the character
           (push (setf last ch) new-chars)
           :end))
        (coerce (nreverse new-chars) 'string)))))

(defun decompose (s type)
  (loop FOR c ACROSS s
    APPEND
      (mapcan #'decompose-hangul-char (decompose-char c type))
      INTO new-s
    FINALLY
      (return (coerce new-s 'string))))

(defun canonical-ordering (decomposed-string &aux (s decomposed-string))
  (let ((starter-indices
         (loop FOR i FROM 1 BELOW (length s)
               FOR ccc = (get-canonical-combining-class (aref s i))
               WHEN (zerop ccc)
           COLLECT i)))
    (loop FOR (beg end) ON (cons 0 starter-indices) DO
      (setf #1=(subseq s beg end) 
            (stable-sort #1# #'< :key #'get-canonical-combining-class))))
  s)

(defun compose (decomposed-string)
  (let* ((s decomposed-string) 
         (to-cs (coerce s 'simple-vector)))
    (loop FOR i FROM 1 BELOW (length s)
          FOR ch-right  = (char s i)      ; 右側の文字
          FOR ccc-right = (get-canonical-combining-class ch-right)
      DO
      (loop FOR j FROM (1- i) DOWNTO 0
            FOR ch-left  = (aref to-cs j) ; 左側の文字
            FOR ccc-left = (and ch-left (get-canonical-combining-class ch-left))
            WHEN ch-left
        DO
        (when (zerop ccc-left)
          ;; ch-left + ch-right の合成文字が存在するなら、それでch-leftを置換する
          (let ((comped-char (gethash (list ch-left ch-right) *canonical-comp-map*)))
            (when comped-char
              (setf (aref to-cs j) comped-char
                    (aref to-cs i) nil)))
          (return))
       
        (unless (< ccc-left ccc-right)
          (return))))
    (compose-hangul (coerce (remove nil to-cs) 'string))))

;;;;;;;;;;;;;;;;;;;;;;
;;;; NFD/NFKD/NFC/NFKC
(defun nfd (s)
  (canonical-ordering (decompose s :canonical)))

(defun nfkd (s)
  (canonical-ordering (decompose s :compatible)))

(defun nfc (s)
  (compose (nfd s)))

(defun nfkc (s)
  (compose (nfkd s)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; external functions
(defun normalize (str normalization-form)
  (ecase normalization-form
    (:nfd  (nfd str))
    (:nfkd (nfkd str))
    (:nfc  (nfc str))
    (:nfkc (nfkc str))))

  (defvar *canonical-decomp-map* canonical-decomp-map)
  (defvar *compatible-decomp-map* compatible-decomp-map)

(defun get-mapping (normalization-form &aux (mapping '()))
  (dolist (map (ecase normalization-form
                 (:nfd  (list *canonical-decomp-map*))
                 (:nfkd (list *canonical-decomp-map* *compatible-decomp-map*))
                 (:nfc  (list *canonical-comp-map*))))
    (maphash 
     (lambda (from to)
       (flet ((to-str (x)
                (if (listp x) (coerce x 'string) (string x))))
             (case normalization-form
               (:nfd  (push (list (to-str from) (decompose (to-str to) :canonical)) mapping))
               (:nfkd (push (list (to-str from) (decompose (to-str to) :compatible)) mapping))
               (:nfc  (when (string= (compose (to-str from)) (to-str to))
                        (push (list (decompose (to-str from) :canonical) (to-str to)) mapping))))))
     map))

  ;; hangul
  (loop FOR code FROM #xAC00 BELOW (+ #xAC00 11172) 
        FOR char = (string (code-char code))
    DO 
    (case normalization-form
      ((:nfd :nfkd) (push (list char (decompose char :canonical)) mapping))
      ((:nfc)       (push (list (decompose char :canonical) char) mapping))))

  (nreverse mapping))

(defun get-canonical-combining-class-map ()
  *canonical-combining-class*)

(let ((nfd-illegal-list '())
      (nfkd-illegal-list '())
      (nfc-illegal-list '())
      (nfkc-illegal-list '())
      (*default-pathname-defaults* *load-pathname*))

  (flet ((parse-line (maybe-key line)
           (let* ((fst (string-trim " " (car (cl-ppcre:split ";" line))))
                  (range (mapcar #'parse-hex (cl-ppcre:split "\\.\\." fst)))
                  (maybe? (and maybe-key (not (null (search maybe-key line))))))
             (loop FOR code FROM (first range) TO (or (second range) (first range)) 
                   FOR char = (code-char code) 
               COLLECT (list char maybe?)))))
    (with-open-file (in "DerivedNormalizationProps.txt")
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
        (cond ((or (search "NFKC_QC; N" line) (search "NFKC_QC; M" line))
               (nconcf nfkc-illegal-list (parse-line "NFKC_QC; M" line)))
              ((or (search "NFC_QC; N" line) (search "NFC_QC; M" line))
               (nconcf nfc-illegal-list (parse-line "NFC_QC; M" line)))
              ((search "NFKD_QC; N" line)
               (nconcf nfkd-illegal-list (parse-line nil line)))
              ((search "NFD_QC; N" line)
               (nconcf nfd-illegal-list (parse-line nil line)))))))
  
  (defun get-illegal-char-list (normalization-form)
    (ecase normalization-form
      (:nfd  nfd-illegal-list)
      (:nfkd nfkd-illegal-list)
      (:nfc  nfc-illegal-list)
      (:nfkc nfkc-illegal-list))))