;;; -*- lexical-binding: t -*-

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

(defcustom orgtd-agenda-all-projects-key "P"
  "Key to select agenda view with list of all projects from org-agenda dispatcher"
  :group 'orgtd-agenda
  :type 'string)

(defcustom orgtd-agenda-all-projects-custom-command-variables
  nil
  "Variables for Projects custom agenda command"
  :group 'orgtd-agenda
  :type org-agenda-custom-commands-local-options)

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
    'org-goto-marker-or-bmk)
   (persistent-help
    :initform
    "Show project")
   (mode-line
    :initform
    '("Project(s)" "f1:Show project f2:Show project+clock in f3:Follow link f4:Capture"))
   (action
    :initform
    '(("Show project" . org-goto-marker-or-bmk)
      ("Show project + clock in" . (lambda (marker)
                                     (org-with-point-at marker (org-clock-in))
                                     (org-goto-marker-or-bmk marker)))
      ("Follow link under heading" . (lambda (marker)
                                       (org-with-point-at marker
                                         (beginning-of-line)
                                         (call-interactively #'org-open-at-point))))
      ("Capture a task at heading" . (lambda (marker)
                                       (org-with-point-at marker
                                         (org-capture nil orgtd-capture-project-task-key))))))))

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

(defun orgtd-agenda--block-header-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (eq (plist-get (text-properties-at (point)) 'face)
        'org-agenda-structure)))

(defun orgtd-agenda--empty-block-header-p ()
  ;; Empty block header is followed by another block
  ;; header (or nothing), not list of tasks.
  (let ((next-line-start (1+ (pos-eol))))
    (and (orgtd-agenda--block-header-p)
         (or (equal (point-max) next-line-start)
             (orgtd-agenda--block-header-p next-line-start)))))

(defun orgtd-agenda-remove-empty-block-headers ()
  "Remove headers of empty blocks in block agenda."
  (if org-agenda-compact-blocks
      (cl-loop initially (goto-char (point-min))
               with buffer-read-only = nil
               for next-line-start = (1+ (pos-eol))
               until (eobp)
               if (orgtd-agenda--empty-block-header-p)
               do (delete-region (pos-bol) next-line-start)
               else do (forward-line))
    (message "Can only remove compact block headers")))

(defun orgtd-agenda-setup ()
  (add-hook 'org-agenda-finalize-hook #'orgtd-agenda-remove-empty-block-headers)
  (org-add-agenda-custom-command orgtd-agenda-custom-command-all-projects))

(provide 'orgtd-agenda)
