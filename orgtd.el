;;; orgtd.el --- Org functions facilitating GTD -*- lexical-binding: t -*-

;; Copyright (C) 2015-2024  Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 22 Oct 2015
;; Version: 0.4.0

;; Package-Requires: ((helm))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'org)
(require 'org-clock)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup orgtd nil
  "Org functions facilitating GTD"
  :group 'org)

(defcustom orgtd-next-task-keywords '("NEXT" "STARTED")
  "List of keywords of next tasks"
  :group 'orgtd
  :type '(set string))

(defcustom orgtd-project-latest-activity-property-name "ORGTD_PROJECT_LAST_ACTIVE"
  "Name of property for storing timestamp corresponding to last activity in project"
  :group 'orgtd
  :type 'string)

(defcustom orgtd-project-property-name "ORGTD_IS_PROJECT"
  "Name of property for explicitly marking a heading as project root"
  :group 'orgtd
  :type 'string)

;;;###autoload
(defun orgtd-at-todo-p ()
  "Predicate determining if heading at point is a todo item.
Every headline with a valid todo keyword is considered a todo item."
  (and (eq major-mode 'org-mode)
       (org-at-heading-p)
       (org-get-todo-state)))

;;;###autoload
(defun orgtd-contains-todo-p ()
  "Predicate determining if subtree of heading at point contains a todo item.
Heading itself is excluded from search."
  (and (eq major-mode 'org-mode)
       (org-at-heading-p)
       (save-excursion
         (cl-loop initially (outline-next-heading)
                  with subtree-end = (save-excursion (org-end-of-subtree 'invisible-ok))
                  while (< (point) subtree-end)
                  thereis (orgtd-at-todo-p)
                  do (outline-next-heading)))))

;;;###autoload
(defun orgtd-contained-in-todo-p ()
  "Predicate determining if heading at point is within heading w/ todo keyword."
  (and (eq major-mode 'org-mode)
       (org-at-heading-p)
       (org-with-wide-buffer
        (cl-loop while (org-up-heading-safe)
                 thereis (orgtd-at-todo-p)))))

;;;###autoload
(defun orgtd-at-task-p ()
  "Predicate determining if heading at point is a task.
Task is a todo item (fulfilling `orgtd-at-todo-p' predicate) that
does not contain any other todo items."
  (and (eq major-mode 'org-mode)
       (orgtd-at-todo-p)
       (not (orgtd-contains-todo-p))
       (not (org-entry-get nil orgtd-project-property-name))
       (not (org-entry-get nil orgtd-project-latest-activity-property-name))))

;;;###autoload
(defun orgtd-contains-scheduled-task-p ()
  "Predicate determining if subtree of heading at point contains a scheduled task.
Heading itself is excluded from search."
  (and (eq major-mode 'org-mode)
       (org-at-heading-p)
       (save-excursion
         (cl-loop initially (outline-next-heading)
                  with subtree-end = (save-excursion (org-end-of-subtree 'invisible-ok))
                  for here = (point)
                  while (< here subtree-end)
                  thereis (and (orgtd-at-task-p)
                               (not (org-entry-is-done-p))
                               (or (org-get-deadline-time here)
                                   (org-get-scheduled-time here)))
                  do (outline-next-heading)))))
;;;###autoload
(defun orgtd-at-project-p ()
  "Predicate determining if heading at point is a root of a project.
Project is a todo item (fulfilling `orgtd-at-todo-p' predicate) that
contains other todo items and is not itself contained under
higher level todo item."
  (and (eq major-mode 'org-mode)
       (orgtd-at-todo-p)
       (not (orgtd-contained-in-todo-p))
       (or (orgtd-contains-todo-p)
           (org-entry-get nil orgtd-project-property-name)
           (org-entry-get nil orgtd-project-latest-activity-property-name))))

;;;###autoload
(defun orgtd-buffer-contains-project-p (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (org-with-wide-buffer
     (cl-loop initially (goto-char (point-min))
              thereis (orgtd-at-project-p)
              until (eobp)
              do (outline-next-heading)))))

;;;###autoload
(defun orgtd-contains-next-p ()
  "Predicate determining if heading at point contains a next item.
Next item is a heading with keyword in `orgtd-next-task-keywords'."
  (and (eq major-mode 'org-mode)
       (org-at-heading-p)
       (save-excursion
         (cl-loop initially (outline-next-heading)
                  with subtree-end = (save-excursion (org-end-of-subtree 'invisible-ok))
                  while (< (point) subtree-end)
                  for state = (org-get-todo-state)
                  thereis (member state orgtd-next-task-keywords)
                  do (outline-next-heading)))))

;;;###autoload
(defun orgtd-get-project-at-point ()
  "Return marker pointing to heading of project cointaining position at point.
Return nil if position at point is not under any project."
  (when (eq major-mode 'org-mode)
    (org-with-wide-buffer
     (cl-loop initially (unless (zerop (org-outline-level))
                          (org-back-to-heading 'invisible-ok))
              if (orgtd-at-project-p) return (point-marker)
              unless (org-up-heading-safe) return nil))))

(defun orgtd-project-at-point-status ()
  "Get a symbol representing a status of project at point.
Can be either of :active, :suspended, :finished, or :stuck.
Raise error if not applicable."
  (if (orgtd-at-project-p)
      (let ((todo-state (org-get-todo-state)))
        (cond
         ((string= "HOLD" todo-state) :suspended)
         ((member todo-state org-done-keywords) :finished)
         ((or (orgtd-contains-next-p)
              (orgtd-contains-scheduled-task-p)) :active)
         (t :stuck)))
    (error "Point is not at project heading at the moment")))

(defclass orgtd-project ()
  ((location :reader orgtd-project-location)
   (last-active-at :reader orgtd-project-last-active-at)
   (title :reader orgtd-project-title)
   (status :type (member :active :suspended :stuck :finished)
           :reader orgtd-project-status)))

(cl-defmethod initialize-instance :after
  ((project orgtd-project) &rest _)
  (with-slots (location last-active-at title status) project
    (setq location (point-marker)
          last-active-at (when-let (time-string
                                    (org-entry-get nil orgtd-project-latest-activity-property-name))
                           (thread-last time-string
                             org-parse-time-string
                             (apply #'encode-time)
                             float-time))
          title (org-with-point-at location
                  (org-link-display-format (nth 4 (org-heading-components))))
          status (orgtd-project-at-point-status))))

(cl-defmethod orgtd-project-currently-clocked-p ((project orgtd-project))
  "Return `t' when PROJECT is being clocked currently, `nil' otherwise."
  (equal (oref project location)
         (when (org-clocking-p)
           (org-with-point-at org-clock-marker
             (orgtd-get-project-at-point)))))

;;;###autoload
(defun orgtd-projects ()
  (seq-mapcat
   (lambda (org-file)
     (with-current-buffer (find-file-noselect org-file)
       (org-with-wide-buffer
        (cl-loop initially (goto-char (point-min))
                 until (eobp)
                 if (orgtd-at-project-p)
                 collect (orgtd-project)
                 and do (org-end-of-subtree 'invisible-ok)
                 end
                 do (outline-next-heading)))))
   (org-agenda-files 'no-restrictions)))

;;;###autoload
(defun orgtd-skip-over-projects ()
  (when-let (prj (orgtd-get-project-at-point))
    (org-with-point-at prj
      (org-end-of-subtree 'invisible-ok))))

;;;###autoload
(defun orgtd-filter-projects (predicate)
  (save-excursion
    (cond
     ((orgtd-at-project-p)
      (unless (funcall predicate (orgtd-project))
        (org-end-of-subtree 'invisible-ok)))
     ((orgtd-at-todo-p)
      (org-end-of-subtree 'invisible-ok))
     (t
      (or (outline-next-heading)
          (point-max))))))

;;;###autoload
(defun orgtd-keep-projects-with-status (status)
  (orgtd-filter-projects
   (lambda (project)
     (eq status (orgtd-project-status project)))))

;;;###autoload
(defun orgtd-keep-tasks ()
  "Skip function for org agenda that keeps only tasks, skipping
project headings."
  (unless (orgtd-at-task-p)
    (save-excursion (or (outline-next-heading)
                        (point-max)))))

;;;###autoload
(defun orgtd-skip-project-headings ()
  "Skip function for org agenda that skips everything other then
project headings."
  (when (orgtd-at-project-p)
    (save-excursion (or (outline-next-heading)
                        (point-max)))))

;;;###autoload
(defun orgtd-keep-project-headings ()
  "Skip function for org agenda that skips everything other then
project headings."
  (unless (orgtd-at-project-p)
    (save-excursion (or (outline-next-heading)
                        (point-max)))))

;;;###autoload
(defun orgtd-keep-projects ()
  "Skip function for org agenda that skips everything outside projects."
  (unless (orgtd-get-project-at-point)
    (save-excursion
      (or (outline-next-heading)
          (point-max)))))

;;;###autoload
(defun orgtd-skip-everything-under-done-headings ()
  "Skip function for org agenda that skips everything under done
headings."
  (org-with-wide-buffer
   (cl-loop while (org-up-heading-safe)
            if (member (org-get-todo-state) org-done-keywords)
            return (org-end-of-subtree 'invisible-ok))))

;;;###autoload
(defun orgtd-parent-project-location ()
  (org-with-wide-buffer
   (unless (zerop (org-outline-level))
     (org-back-to-heading t))
   (when (and (orgtd-at-todo-p)
              (org-up-heading-safe))
     (cl-loop until (zerop (org-outline-level))
              if (orgtd-at-project-p)
              return (point-marker)
              unless (org-up-heading-safe) return nil))))

;;;###autoload
(defun orgtd-last-clock-out-time ()
  "Return timestamp corresponding to a last time any item under
current heading clocked out."
  (when (org-at-heading-p)
    (save-excursion
      (cl-loop with end = (save-excursion (org-end-of-subtree 'invisible-ok))
               with re = (concat org-clock-string ".*\\]--\\(\\[[^]]+\\]\\)")
               while (re-search-forward re end 'noerror)
               maximize (float-time (org-time-string-to-time (match-string 1)))))))

(defun orgtd-set-project-last-active-timestamp ()
  "Set a property on project heading indicating activity. Intended for use in hooks"
  (when-let (project (orgtd-get-project-at-point))
    (orgtd-bump-latest-activity-timestamp project)
    (org-entry-delete project orgtd-project-property-name)))

(defun orgtd-bump-latest-activity-timestamp (location)
  (org-entry-put location
                 orgtd-project-latest-activity-property-name
                 (format-time-string "[%Y-%m-%d %a %H:%M]" (float-time))))

(defun orgtd-set-appropriate-project-todo-keyword ()
  "Update project todo keyword to match it's content. Intended to
be invoked after change took place (from hooks or from code that
changes stuff within project)"
  (unless (orgtd-at-project-p) ; do nothing if on project heading (avoid recursion)
    (when-let (project-marker (orgtd-get-project-at-point))
      (org-with-point-at project-marker
        (org-todo (if (or (orgtd-contains-next-p)
                          (eq (orgtd-project-at-point-status) :stuck)
                          (orgtd-contains-scheduled-task-p))
                      "TODO"
                    "HOLD"))))))

;;;###autoload
(defun orgtd-narrow-to-project ()
  (interactive)
  (if-let (project-marker (orgtd-get-project-at-point))
      (progn
        (org-goto-marker-or-bmk project-marker)
        (org-narrow-to-subtree))
    (error "Point is not positioned in project")))

(declare-function org-agenda-error "org-agenda")
(defun orgtd-get-location (&optional noerror)
  (pcase major-mode
    (`org-mode
     (point-marker))
    (`org-agenda-mode
     (or (org-get-at-bol 'org-hd-marker)
         (unless noerror
           (org-agenda-error))))
    (_ (unless noerror
         (user-error "Called from the wrong mode")))))

;;;###autoload
(defun orgtd-mark-as-project ()
  "Mark heading at point as a root of the project. "
  (interactive)
  (unless (org-get-todo-state)
    (org-todo "TODO"))
  (orgtd-bump-latest-activity-timestamp (orgtd-get-location)))

(autoload #'orgtd-capture-setup "orgtd-capture")
(autoload #'orgtd-agenda-setup "orgtd-agenda")

;;;###autoload
(defun orgtd-setup ()
  (add-hook 'org-clock-in-hook #'orgtd-set-project-last-active-timestamp)
  (add-hook 'org-clock-out-hook #'orgtd-set-project-last-active-timestamp)
  (add-hook 'org-after-todo-state-change-hook #'orgtd-set-project-last-active-timestamp)

  (add-hook 'org-after-refile-insert-hook #'orgtd-set-appropriate-project-todo-keyword)
  (add-hook 'org-after-todo-state-change-hook #'orgtd-set-appropriate-project-todo-keyword)
  (add-hook 'org-capture-before-finalize-hook #'orgtd-set-appropriate-project-todo-keyword)

  (with-eval-after-load 'org-capture
    (orgtd-capture-setup))

  (with-eval-after-load 'org-agenda
    (orgtd-agenda-setup)))

(provide 'orgtd)

;;; orgtd.el ends here
