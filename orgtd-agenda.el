(require 'helm)
(require 'helm-plugin)
(require 'map)
(require 'org-agenda)
(require 'orgtd)

(defvar orgtd-agenda-project-key "p")
(defvar orgtd-agenda-all-projects-key "P")
(defvar orgtd-agenda-review-key "R")

(defvar orgtd-agenda-custom-command-project
  (list orgtd-agenda-project-key
        "Project"
        '((todo "*"
                ((org-agenda-overriding-header "Project heading")
                 (org-agenda-skip-function '(orgtd-keep-project-headings))
                 (org-agenda-prefix-format '((todo . "  %c: ")))))
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
                 (org-agenda-skip-function '(orgtd-keep-tasks))
                 (org-agenda-sorting-strategy '(todo-state-down))))
          (tags "-TODO=\"STARTED\"-TODO=\"NEXT\"-TODO=\"TODO\""
                ((org-agenda-overriding-header "Rest of tasks")
                 (org-agenda-skip-function
                  '(or (orgtd-keep-tasks)
                       (orgtd-skip-everything-under-done-headings)
                       (org-agenda-skip-entry-if 'nottodo '("*")))))))
        '((org-agenda-prefix-format '((tags . "  ")
                                      (todo . "  "))))))

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
                  '(orgtd-keep-projects-with-status :suspended)))))))

(defvar orgtd-agenda-custom-command-review
  (list orgtd-agenda-review-key
        "Review"
        '((todo "*"
                ((org-agenda-overriding-header "Stuck projects")
                 (org-agenda-skip-function
                  '(orgtd-keep-projects-with-status :stuck))))
          (todo "DONE|CANCELED"
                ((org-agenda-overriding-header "Stuff to archive")
                 (org-agenda-skip-function '(orgtd-skip-everything-under-done-headings)))))))

(defun orgtd-agenda-invoke-for-project-at-marker (marker)
  (org-with-point-at marker
    (org-agenda nil orgtd-agenda-project-key 'subtree)))

(defclass orgtd-project-source (helm-source)
  ((nomark :initform t)
   (candidate-transformer
    :initform
    ((lambda (candidates)
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
    orgtd-agenda-invoke-for-project-at-marker)
   (persistent-help
    :initform
    "Show project agenda")
   (mode-line
    :initform
    ("Project(s)" "f1:Show agenda f2:Show agenda+clock in f3:Go to heading f4:Follow link f5:Capture"))
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
                                         (org-capture nil "s"))))))))

;;;###autoload
(defun orgtd-agenda-projects ()
  (interactive)
  (let ((projects (seq-group-by #'orgtd-project-status (orgtd-projects))))
    (helm :prompt "Project: "
          :buffer "*helm org projects*"
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
  (require 'org-clock)
  (org-clock-load)
  (if-let (project-marker (when (org-clocking-p)
                            (org-with-point-at org-clock-marker
                              (orgtd-get-project-at-point))))
      (orgtd-agenda-invoke-for-project-at-marker project-marker)
    (error "No project is currently clocked")))

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

(defun orgtd-agenda-set-appropriate-project-todo-keyword ()
  "Update project todo keyword to match it's content. Intended to
be invoked after change took place (from hooks or from code that
changes stuff within project)"
  (unless (orgtd-at-project-p) ; do nothing if on project heading (avoid recursion)
    (when-let (project-marker (orgtd-get-project-at-point))
      (org-with-point-at project-marker
        (org-todo (if (orgtd-contains-next-p) "TODO" "HOLD"))))))

(defun orgtd-agenda-setup ()
  (add-hook 'org-agenda-mode-hook #'orgtd-agenda-maybe-update-restrictions)
  (add-hook 'org-agenda-finalize-hook #'orgtd-agenda-remove-empty-block-headers)

  (add-hook 'org-after-refile-insert-hook #'orgtd-agenda-set-appropriate-project-todo-keyword)
  (add-hook 'org-after-todo-state-change-hook #'orgtd-agenda-set-appropriate-project-todo-keyword)
  (add-hook 'org-capture-before-finalize-hook #'orgtd-agenda-set-appropriate-project-todo-keyword)

  (seq-each (lambda (custom-command)
              (add-to-list 'org-agenda-custom-commands custom-command 'append))
            (list
             orgtd-agenda-custom-command-project
             orgtd-agenda-custom-command-all-projects
             orgtd-agenda-custom-command-review))

  (add-to-list 'org-agenda-custom-commands-contexts
               `(,orgtd-agenda-project-key ((in-mode . "org-mode")))))

(provide 'orgtd-agenda)
