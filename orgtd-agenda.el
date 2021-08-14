(require 'helm)
(require 'map)
(require 'org-agenda)
(require 'org-clock)
(require 'orgtd)
(require 'orgtd-capture)
(require 'subr-x)

(defgroup orgtd-agenda nil
  "Org-agenda-related orgtd things"
  :group 'orgtd)

(defcustom orgtd-agenda-project-key "p"
  "Key to select project-specific agenda view from org-agenda dispatcher"
  :group 'orgtd-agenda
  :type 'string)

(defcustom orgtd-agenda-all-projects-key "P"
  "Key to select agenda view with list of all projects from org-agenda dispatcher"
  :group 'orgtd-agenda
  :type 'string)

(defcustom orgtd-agenda-review-key "R"
  "Key to select review agenda view from org-agenda dispatcher"
  :group 'orgtd-agenda
  :type 'string)

(defcustom orgtd-agenda-project-custom-command-variables
  '((org-agenda-prefix-format '((tags . "  ")
                                (todo . "  "))))
  "Variables for Project custom agenda command"
  :group 'orgtd-agenda
  :type org-agenda-custom-commands-local-options)

(defcustom orgtd-agenda-all-projects-custom-command-variables
  nil
  "Variables for Projects custom agenda command"
  :group 'orgtd-agenda
  :type org-agenda-custom-commands-local-options)

(defcustom orgtd-agenda-review-custom-command-variables
  nil
  "Variables for Review custom agenda command"
  :group 'orgtd-agenda
  :type org-agenda-custom-commands-local-options)

(defvar orgtd-agenda-custom-command-project
  (list orgtd-agenda-project-key
        "Project (DON'T INVOKE THIS DIRECTLY)"
        '((todo "*"
                ((org-agenda-overriding-header "Project heading")
                 (org-agenda-skip-function '(orgtd-keep-project-headings))))
          (todo "*"
                ((org-agenda-overriding-header "Active subprojects")
                 (org-agenda-skip-function
                  '(orgtd-keep-subprojects-with-status :active))))
          (todo "*"
                ((org-agenda-overriding-header "Stuck subprojects")
                 (org-agenda-skip-function
                  '(orgtd-keep-subprojects-with-status :stuck))))
          (todo "*"
                ((org-agenda-overriding-header "Suspended subprojects")
                 (org-agenda-skip-function
                  '(orgtd-keep-subprojects-with-status :suspended))))
          (todo "*"
                ((org-agenda-overriding-header "Finished subprojects")
                 (org-agenda-skip-function
                  '(orgtd-keep-subprojects-with-status :finished))))
          (todo "TODO|STARTED|NEXT"
                ((org-agenda-overriding-header "Relevant")
                 (org-agenda-skip-function '(orgtd-keep-tasks))))
          (tags "-TODO=\"STARTED\"-TODO=\"NEXT\"-TODO=\"TODO\""
                ((org-agenda-overriding-header "Rest of tasks")
                 (org-agenda-skip-function
                  '(or (orgtd-keep-tasks)
                       (orgtd-skip-everything-under-done-headings)
                       (org-agenda-skip-entry-if 'nottodo '("*")))))))
        orgtd-agenda-project-custom-command-variables))

(defvar orgtd-agenda-custom-command-all-projects
  (list orgtd-agenda-all-projects-key
        "Projects"
        '((todo "*"
                ((org-agenda-overriding-header "Finished projects")
                 (org-agenda-skip-function
                  '(orgtd-keep-projects-with-status :finished))))
          (todo "*"
                ((org-agenda-overriding-header "Active projects")
                 (org-agenda-skip-function
                  '(orgtd-keep-projects-with-status :active))))
          (todo "*"
                ((org-agenda-overriding-header "Stuck projects")
                 (org-agenda-skip-function
                  '(orgtd-keep-projects-with-status :stuck))))
          (todo "*"
                ((org-agenda-overriding-header "Suspended projects")
                 (org-agenda-skip-function
                  '(orgtd-keep-projects-with-status :suspended)))))
        orgtd-agenda-all-projects-custom-command-variables))

(defvar orgtd-agenda-custom-command-review
  (list orgtd-agenda-review-key
        "Review"
        '((todo "*"
                ((org-agenda-overriding-header "Stuck projects")
                 (org-agenda-skip-function
                  '(orgtd-keep-projects-with-status :stuck))))
          (todo "DONE|CANCELED"
                ((org-agenda-overriding-header "Stuff to archive")
                 (org-agenda-skip-function '(orgtd-skip-everything-under-done-headings)))))
        orgtd-agenda-review-custom-command-variables))

(defun orgtd-agenda-invoke-for-project-at-marker (marker)
  (org-with-point-at marker
    (org-agenda nil orgtd-agenda-project-key 'subtree)))

(defclass orgtd-project-source (helm-source)
  ((nomark :initform t)
   (candidate-transformer
    :initform
    '((lambda (candidates)
        (seq-sort (lambda (a b)
                    (> (or (orgtd-project-last-active-at a) 0)
                       (or (orgtd-project-last-active-at b) 0)))
                  candidates))
      (lambda (candidates)
        (seq-map (lambda (project)
                   (let ((title (orgtd-project-title project)))
                     (cons
                      (if (orgtd-project-currently-clocked-p project)
                          (propertize title 'face 'bold)
                        title)
                      (orgtd-project-location project))))
                 candidates))))
   (persistent-action
    :initform
    'orgtd-agenda-invoke-for-project-at-marker)
   (persistent-help
    :initform
    "Show project agenda")
   (mode-line
    :initform
    '("Project(s)" "f1:Show agenda f2:Show agenda+clock in f3:Go to heading f4:Follow link f5:Capture"))
   (action
    :initform
    '(("Show agenda" . orgtd-agenda-invoke-for-project-at-marker)
      ("Show agenda + clock in" . (lambda (marker)
                                    (org-with-point-at marker (org-clock-in))
                                    (orgtd-agenda-invoke-for-project-at-marker marker)))
      ("Go to heading" . org-goto-marker-or-bmk)
      ("Follow link under heading" . (lambda (marker)
                                       (org-with-point-at marker
                                         (beginning-of-line)
                                         (call-interactively #'org-open-at-point))))
      ("Capture a task at heading" . (lambda (marker)
                                       (org-with-point-at marker
                                         (org-capture nil orgtd-capture-subtask-key))))))))

;;;###autoload
(defun orgtd-agenda-projects ()
  (interactive)
  (let ((projects (seq-group-by #'orgtd-project-status (orgtd-projects))))
    (helm :prompt "Project: "
          :buffer " *helm org projects*"
          :sources
          (list
           (helm-make-source "Stuck Projects" #'orgtd-project-source
             :candidates (map-elt projects :stuck))
           (helm-make-source "Active Projects" #'orgtd-project-source
             :candidates (map-elt projects :active))
           (helm-make-source "Finished Projects" #'orgtd-project-source
             :candidates (map-elt projects :finished))
           (helm-make-source "Suspended Projects" #'orgtd-project-source
             :candidates (map-elt projects :suspended))))))

;;;###autoload
(defun orgtd-agenda-for-project-at-point ()
  "Show agenda view for project at point"
  (interactive)
  (if-let ((starting-marker (orgtd-get-location))
           (project-marker (org-with-point-at starting-marker
                             (orgtd-get-project-at-point))))
      (orgtd-agenda-invoke-for-project-at-marker project-marker)
    (error "Not at project currently")))

;;;###autoload
(defun orgtd-agenda-for-currently-clocked-project ()
  "Show agenda view for currently clocked project"
  (interactive)

  (unless org-clock-loaded
    (org-clock-load))

  (if-let (project-marker (when (org-clocking-p)
                            (org-with-point-at org-clock-marker
                              (orgtd-get-project-at-point))))
      (orgtd-agenda-invoke-for-project-at-marker project-marker)
    (error "No project is currently clocked")))

;;;###autoload
(defun orgtd-agenda-for-most-recently-active-project ()
  "Show agenda view for project with most recent recorded activity"
  (interactive)

  (unless org-clock-loaded
    (org-clock-load))

  (if-let (project (thread-last (orgtd-projects)
                     (seq-filter #'orgtd-project-last-active-at)
                     (seq-sort-by #'orgtd-project-last-active-at #'>)
                     car))
      (orgtd-agenda-invoke-for-project-at-marker (orgtd-project-location project))
    (error "Did not find any projects with recorded activity")))

;;;###autoload
(defun orgtd-agenda-narrow-to-subproject-at-point ()
  "Narrow to subproject at point"
  (interactive)
  (unless (eq major-mode 'org-agenda-mode)
    (error "Can only be used in agenda view"))
  (if-let (marker (orgtd-get-location))
      (if (org-with-point-at marker (orgtd-at-subproject-p))
          (orgtd-agenda-invoke-for-project-at-marker marker)
        (error "Not a subproject"))
    (org-agenda-error)))

(defun orgtd-agenda-maybe-update-restrictions ()
  "Update restriction (if any), in case tree grown past initial size"
  (when (and org-agenda-restrict
             (marker-buffer org-agenda-restrict-begin))
    (setq org-agenda-restrict-end
          (org-with-point-at org-agenda-restrict-begin
            (org-end-of-subtree t)
            (point-marker)))))

(defun orgtd-agenda--block-header-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (eq (plist-get (text-properties-at (point)) 'face)
        'org-agenda-structure)))

(defun orgtd-agenda--empty-block-header-p ()
  ;; Empty block header is followed by another block
  ;; header (or nothing), not list of tasks.
  (let ((next-line-start (1+ (line-end-position))))
    (and (orgtd-agenda--block-header-p)
         (or (equal (point-max) next-line-start)
             (orgtd-agenda--block-header-p next-line-start)))))

(defun orgtd-agenda-remove-empty-block-headers ()
  "Remove headers of empty blocks in block agenda."
  (if org-agenda-compact-blocks
      (cl-loop initially (goto-char (point-min))
               with buffer-read-only = nil
               for next-line-start = (1+ (line-end-position))
               until (eobp)
               if (orgtd-agenda--empty-block-header-p)
               do (delete-region (line-beginning-position) next-line-start)
               else do (forward-line))
    (message "Can only remove compact block headers")))

(defun orgtd-agenda-setup ()
  (add-hook 'org-agenda-mode-hook #'orgtd-agenda-maybe-update-restrictions)
  (add-hook 'org-agenda-finalize-hook #'orgtd-agenda-remove-empty-block-headers)

  (seq-each (lambda (custom-command)
              (add-to-list 'org-agenda-custom-commands custom-command 'append))
            (list
             orgtd-agenda-custom-command-project
             orgtd-agenda-custom-command-all-projects
             orgtd-agenda-custom-command-review))

  (add-to-list 'org-agenda-custom-commands-contexts
               `(,orgtd-agenda-project-key (orgtd-get-project-at-point))))

(provide 'orgtd-agenda)
