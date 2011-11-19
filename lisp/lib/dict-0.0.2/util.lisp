(in-package :dict)

(declaim (inline acons!))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it
         ,then
       ,else)))

(defun acons! (key value list &key test)
  (loop FOR x IN list
        WHEN (funcall test key (car x))
    DO (setf (cdr x) value)
       (return (values list nil))
    FINALLY
       (return (values `(,(cons key value) . ,list) t))))
