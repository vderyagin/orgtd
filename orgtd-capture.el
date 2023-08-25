(require 'orgtd)
(require 'org-agenda)

(defgroup orgtd-capture nil
  "Org-capture-related orgtd things"
  :group 'orgtd)

(defcustom orgtd-capture-project-task-key "p"
  "A key to select a subtask capture template"
  :group 'orgtd-capture
  :type 'string)

(defun orgtd-capture-target-project-task ()
  (org-goto-marker-or-bmk (orgtd-get-location))
  (if-let (project (orgtd-get-project-at-point))
      (org-goto-marker-or-bmk project)
    (user-error "Not at project")))

(defun orgtd-capture-project-p ()
  (when-let (location (orgtd-get-location 'noerror))
    (org-with-point-at location
      (orgtd-get-project-at-point))))

(defvar org-capture-templates)
(defvar org-capture-templates-contexts)

;;;###autoload
(defun orgtd-capture-setup ()
  (require 'org-capture)

  (add-to-list 'org-capture-templates
               `(,orgtd-capture-project-task-key
                 "project task" entry
                 (function orgtd-capture-target-project-task)
                 "* TODO %?\n:PROPERTIES:\n:Captured_at: %U\n:END:")'append)

  (add-to-list 'org-capture-templates-contexts
               `(,orgtd-capture-project-task-key (orgtd-capture-project-p))
               'append))

(provide 'orgtd-capture)
