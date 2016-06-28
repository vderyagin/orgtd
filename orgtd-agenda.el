(require 'helm)
(require 'helm-plugin)
(require 'map)
(require 'org-agenda)
(require 'orgtd)

(defvar orgtd-agenda-custom-command-project
  '("p" "Project"
    ((todo "*"
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
    ((org-agenda-prefix-format '((tags . "  ")
                                 (todo . "  "))))))

(defclass orgtd-project-source (helm-source)
  ((nomark :initform t)
   (candidate-transformer
    :initform
    ((lambda (candidates)
       (seq-sort (lambda (a b)
                   (< (or (orgtd-project-last-active-at a) 0)
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
    (lambda (marker)
      (org-with-point-at marker
        (org-agenda nil "p" 'subtree))))
   (persistent-help
    :initform
    "Show project agenda")
   (mode-line
    :initform
    ("Project(s)" "f1:Show agenda f2:Show agenda+clock in f3:Go to heading f4:Follow link f5:Capture"))
   (action
    :initform
    '(("Show agenda" . (lambda (marker)
                         (org-with-point-at marker
                           (org-agenda nil "p" 'subtree))))
      ("Show agenda + clock in" . (lambda (marker)
                                    (org-with-point-at marker
                                      (org-clock-in)
                                      (org-agenda nil "p" 'subtree))))
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
  ;; (org-clone-repo-if-missing)
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

(defun orgtd-agenda-setup ()
  (add-to-list 'org-agenda-custom-commands
               orgtd-agenda-custom-command-project
               'append))

(provide 'orgtd-agenda)
