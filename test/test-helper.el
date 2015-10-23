(require 'orgtd)

(defmacro with-org (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     (search-forward "<POINT>" nil t)
     ,@body))
