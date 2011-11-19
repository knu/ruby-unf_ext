;# <- バイトオーダー判定用文字列
;; ファイル名: byte-order.lisp
(in-package :dawg.global)

(eval-when (:compile-toplevel :load-toplevel)
  (defun guess-byte-order (sample-file)
    (with-open-file (1byte sample-file :element-type '(unsigned-byte 8))
      (with-open-file (2byte sample-file :element-type '(unsigned-byte 16))
        (if (= (read-byte 2byte)
               (+ (ash (read-byte 1byte) 8) (read-byte 1byte)))
            :big
          :little)))))

(defconstant +NATIVE_ORDER+ (guess-byte-order (or *COMPILE-FILE-PATHNAME*
                                                  *LOAD-PATHNAME*)))

