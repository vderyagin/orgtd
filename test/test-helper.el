(defmacro with-org (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     ,@body))
