(defvar sakura-packages nil)

(defmacro sakura/pkg (name &rest body)
  (declare (indent defun))
  (let ((id (if (listp name) (car name) name)))
    `(progn
       (defun ,(intern (format "sakura/init-%s" id)) ()
	 (use-package ,id ,@body))
       (push ',name sakura-packages))))
