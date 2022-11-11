(require 'orgtd)
(require 'org-agenda)

(defgroup orgtd-capture nil
  "Org-capture-related orgtd things"
  :group 'orgtd)

(defcustom orgtd-capture-project-task-key "p"
  "A key to select a subtask capture template"
  :group 'orgtd-capture
  :type 'string)

(defcustom orgtd-capture-note-key "n"
  "A key to select a note capture template"
  :group 'orgtd-capture
  :type 'string)

(defun orgtd-capture-target-project-task ()
  (org-goto-marker-or-bmk (orgtd-get-location))
  (if-let (project (orgtd-get-project-at-point))
      (org-goto-marker-or-bmk project)
    (user-error "Not at project")))

(defun orgtd-capture-target-note ()
  (org-goto-marker-or-bmk (orgtd-get-location))
  (outline-next-heading)
  (org-show-context)
  (unless (eobp)
    (backward-char))
  (when (zerop (org-outline-level))
    ;; mark sequence of notes outside of any headings
    ;; (before headings or when file has no headings):
    (or (save-excursion (search-backward "\nNotes:\n" nil 'noerror))
        (insert "\nNotes:\n")))
  ;; get rid of extra newlines:
  (while (looking-back "\n\n" (- (point) 2))
    (delete-char -1))
  (org-goto-marker-or-bmk (point-marker)))

(defun orgtd-capture-project-p ()
  (when-let (location (orgtd-get-location 'noerror))
    (org-with-point-at location
      (orgtd-get-project-at-point))))

(defun orgtd-capture-note-p ()
  (orgtd-get-location 'noerror))

(defvar org-capture-templates)
(defvar org-capture-templates-contexts)

;;;###autoload
(defun orgtd-capture-setup ()
  (require 'org-capture)
  (seq-each
   (lambda (template) (add-to-list 'org-capture-templates template 'append))
   `((,orgtd-capture-project-task-key "project task" entry
      (function orgtd-capture-target-project-task)
      "* TODO %?\n:PROPERTIES:\n:Captured_at: %U\n:END:")
     (,orgtd-capture-note-key "note" plain
      (function orgtd-capture-target-note)
      "- Note taken on %U \\\\\n  %?")))

  (seq-each
   (lambda (context) (add-to-list 'org-capture-templates-contexts context 'append))
   `((,orgtd-capture-project-task-key (orgtd-capture-project-p))
     (,orgtd-capture-note-key (orgtd-capture-note-p)))))

(provide 'orgtd-capture)
