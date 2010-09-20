(load "unf")

(defmacro with-output-file ((out path) &body body)
  `(with-open-file (,out ,path :direction :output :if-exists :supersede)
     ,@body))

(defun generate-definition-files (output-dir &aux (output-dir (pathname output-dir)))
  (ensure-directories-exist output-dir)
  (let ((*default-pathname-defaults* (probe-file output-dir)))
    
    ;; canonical-combining-class
    (format *error-output* "; generate 'canonical-combining-class.def'~%")
    (with-output-file (out "canonical-combining-class.def")
      (maphash 
       (lambda (char class)
         (format out "~2,'0X~C~C~%" class #\Tab char))
       (unf:get-canonical-combining-class-map)))

    ;; decomposition/composition
    (loop FOR (filename mapping) IN `(("canonical-decomposition.def"     ,(unf:get-mapping :nfd))
                                      ("compatibility-decomposition.def" ,(unf:get-mapping :nfkd))
                                      ("canonical-composition.def"       ,(unf:get-mapping :nfc)))
      DO
      (format *error-output* "; generate '~A'~%" filename)
      (with-output-file (out filename)
        (loop FOR (from to) IN mapping
          DO (format out "~A~C~A~%" from #\Tab to))))

    ;; illegals
    (loop FOR (filename list) IN `(("nfc-illegal-char.def"  ,(unf:get-illegal-char-list :nfc))
                                  ("nfkc-illegal-char.def" ,(unf:get-illegal-char-list :nfkc)))
      DO
      (format *error-output* "; generate '~A'~%" filename)
      (with-output-file (out filename)
        (loop FOR (char maybe?) IN list
          DO
          (format out "~:[00~;01~] ~C~%" maybe? char)))))
  'done)
