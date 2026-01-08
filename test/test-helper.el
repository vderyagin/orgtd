;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'orgtd)

(defmacro with-org (text &rest body)
  (declare (indent 1))
  (let ((position (gensym)))
    `(with-temp-buffer
       (org-mode)
       (insert ,text)
       (goto-char (point-min))
       (search-forward "<POINT>" nil t)
       (let ((,position (point)))
         (with-org-todo-keywords '("TODO" "NEXT" "DONE")
           ,@body)
         (unless (= ,position (point))
           (error "Point moved from %d to %d when it should not have"
                  ,position
                  (point)))))))

(defmacro with-org-todo-keywords (keywords &rest body)
  (declare (indent 1))
  `(let ((org-todo-keywords ,keywords))
     (org-set-regexps-and-options)
     ,@body))

(provide 'test/test-helper)
