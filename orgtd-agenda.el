;;; -*- lexical-binding: t -*-

(require 'consult)
(require 'embark)
(require 'embark-consult)
(require 'map)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'orgtd)
(require 'orgtd-capture)
(require 'subr-x)

(declare-function consult-org-heading "consult-org")

(defvar orgtd-capture--project-marker)

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

(defvar-keymap embark-orgtd-project-map
  :doc "Keymap for actions on orgtd projects"
  "RET" #'orgtd-agenda--project-show
  "I" #'orgtd-agenda--project-clock-in
  "o" #'orgtd-agenda--project-follow-link
  "p" #'orgtd-agenda--project-capture-task
  ">" #'orgtd-agenda--project-refile-here
  "j" #'orgtd-agenda--project-goto-heading)

(add-to-list 'embark-keymap-alist '(orgtd-project . embark-orgtd-project-map))

(defun orgtd-agenda--project-show (marker)
  (org-goto-marker-or-bmk marker))

(defun orgtd-agenda--project-clock-in (marker)
  (org-with-point-at marker (org-clock-in))
  (org-goto-marker-or-bmk marker))

(defun orgtd-agenda--project-follow-link (marker)
  (org-with-point-at marker
    (beginning-of-line)
    (call-interactively #'org-open-at-point)))

(defun orgtd-agenda--project-capture-task (marker)
  (setq orgtd-capture--project-marker marker)
  (run-at-time 0 nil (lambda ()
                       (unwind-protect
                           (org-capture nil orgtd-capture-project-task-key)
                         (setq orgtd-capture--project-marker nil)))))

(defun orgtd-agenda--project-refile-here (marker)
  (let ((rfloc (org-with-point-at marker
                 (list (nth 4 (org-heading-components))
                       (buffer-file-name)
                       nil
                       marker))))
    (with-current-buffer (embark--target-buffer)
      (org-refile nil nil rfloc)
      (when (and org-capture-mode
                 (buffer-base-buffer (current-buffer)))
        (org-capture-kill)))))

(defun orgtd-agenda--project-goto-heading (marker)
  (with-current-buffer (marker-buffer marker)
    (consult-org-heading)))

(defun orgtd-agenda--format-project (project)
  (let* ((title (orgtd-project-title project))
         (display (if (orgtd-project-currently-clocked-p project)
                      (propertize title 'face 'bold)
                    title))
         (marker (orgtd-project-location project)))
    (propertize display
                'orgtd-project-marker marker
                'consult--candidate marker)))

(defun orgtd-agenda--project-candidates (projects)
  (seq-map #'orgtd-agenda--format-project
           (seq-sort (lambda (a b)
                       (> (or (orgtd-project-last-active-at a) 0)
                          (or (orgtd-project-last-active-at b) 0)))
                     projects)))

(defun orgtd-agenda--project-state ()
  (let ((jump-state (consult--jump-preview)))
    (lambda (action cand)
      (let ((marker (and cand (get-text-property 0 'consult--candidate cand))))
        (funcall jump-state action marker)))))

(defun orgtd-agenda--get-marker (cand)
  (when cand
    (get-text-property 0 'consult--candidate cand)))

(defun orgtd-agenda--embark-target ()
  (when-let* ((cand (run-hook-with-args-until-success 'consult--completion-candidate-hook))
              (marker (orgtd-agenda--get-marker cand)))
    (cons 'orgtd-project marker)))

;;;###autoload
(defun orgtd-agenda-projects ()
  (interactive)
  (let* ((projects (seq-group-by #'orgtd-project-status (orgtd-projects)))
         (sources
          `((:name "Stuck"
             :narrow ?s
             :category orgtd-project
             :state ,#'orgtd-agenda--project-state
             :items ,(orgtd-agenda--project-candidates (map-elt projects :stuck)))
            (:name "Active"
             :narrow ?a
             :category orgtd-project
             :state ,#'orgtd-agenda--project-state
             :items ,(orgtd-agenda--project-candidates (map-elt projects :active)))
            (:name "Finished"
             :narrow ?f
             :category orgtd-project
             :state ,#'orgtd-agenda--project-state
             :items ,(orgtd-agenda--project-candidates (map-elt projects :finished)))
            (:name "Suspended"
             :narrow ?u
             :category orgtd-project
             :state ,#'orgtd-agenda--project-state
             :items ,(orgtd-agenda--project-candidates (map-elt projects :suspended))))))
    (add-hook 'embark-target-finders #'orgtd-agenda--embark-target)
    (unwind-protect
        (when-let* ((selected (consult--multi sources
                                              :prompt "Project: "
                                              :preview-key "C-j"))
                    (marker (orgtd-agenda--get-marker (car selected))))
          (org-goto-marker-or-bmk marker))
      (remove-hook 'embark-target-finders #'orgtd-agenda--embark-target))))

(defun orgtd-agenda--block-header-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (eq (plist-get (text-properties-at (point)) 'face)
        'org-agenda-structure)))

(defun orgtd-agenda--empty-block-header-p ()
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
