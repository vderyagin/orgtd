(require 'orgtd)
(require 'org-agenda)
(require 'org-capture)

(defgroup orgtd-capture nil
  "Org-capture-related orgtd things"
  :group 'orgtd)

(defcustom orgtd-capture-subtask-key "s"
  "A key to select a subtask capture template"
  :group 'orgtd-capture
  :type 'string)

(defcustom orgtd-capture-sibling-key "S"
  "A key to select a sibling capture template"
  :group 'orgtd-capture
  :type 'string)

(defun orgtd-capture-target-sibling ()
  (org-goto-marker-or-bmk (orgtd-get-location))
  (org-back-to-heading)
  (unless (orgtd-at-todo-p)
    (user-error "Not at task"))
  (org-goto-marker-or-bmk (orgtd-parent-subproject-or-project-location)))

(defun orgtd-capture-target-subtask ()
  (org-goto-marker-or-bmk (orgtd-get-location))
  (org-back-to-heading)
  (unless (orgtd-at-todo-p)
    (user-error "Not at task"))
  (org-goto-marker-or-bmk (point-marker)))

(defun orgtd-capture-subtask-p ()
  (when-let (location (orgtd-get-location 'noerror))
    (org-with-point-at location
      (unless (zerop (org-outline-level))
        (org-back-to-heading 'invisible-ok)
        (and (orgtd-get-project-at-point)
             (orgtd-at-todo-p))))))

(defun orgtd-capture-sibling-p ()
  (when-let (location (orgtd-get-location 'noerror))
    (org-with-point-at location
      (unless (zerop (org-outline-level))
        (org-back-to-heading 'invisible-ok)
        (and (orgtd-get-project-at-point)
             (not (orgtd-at-project-p))
             (orgtd-at-todo-p))))))

;;;###autoload
(defun orgtd-capture-setup ()
  (seq-each
   (lambda (template) (add-to-list 'org-capture-templates template 'append))
   `((,orgtd-capture-subtask-key "subtask" entry
      (function orgtd-capture-target-subtask)
      "* NEXT %?\n:PROPERTIES:\n:Captured_at: %U\n:END:")
     (,orgtd-capture-sibling-key "sibling" entry
      (function orgtd-capture-target-sibling)
      "* NEXT %?\n:PROPERTIES:\n:Captured_at: %U\n:END:")))

  (seq-each
   (lambda (context) (add-to-list 'org-capture-templates-contexts context 'append))
   `((,orgtd-capture-subtask-key (orgtd-capture-subtask-p))
     (,orgtd-capture-sibling-key (orgtd-capture-sibling-p)))))

(provide 'orgtd-capture)
