;;; orgtd.el --- Org functions facilitating GTD -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 22 Oct 2015
;; Version: 0.2.0

;; Package-Requires: ()

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
  (and (org-at-heading-p)
       (org-get-todo-state)))

;;;###autoload
(defun orgtd-contains-todo-p ()
  "Predicate determining if subtree of heading at point contains a todo item.
Heading itself is excluded from search."
  (and (org-at-heading-p)
       (save-excursion
         (cl-loop initially (outline-next-heading)
                  with subtree-end = (save-excursion (org-end-of-subtree 'invisible-ok))
                  while (< (point) subtree-end)
                  thereis (orgtd-at-todo-p)
                  do (outline-next-heading)))))

;;;###autoload
(defun orgtd-contained-in-todo-p ()
  "Predicate determining if heading at point is contained within a heading with a todo keyword."
  (and (org-at-heading-p)
       (org-with-wide-buffer
        (cl-loop while (org-up-heading-safe)
                 thereis (orgtd-at-todo-p)))))

;;;###autoload
(defun orgtd-at-task-p ()
  "Predicate determining if heading at point is a task.
Task is a todo item (fulfilling `orgtd-at-todo-p' predicate) that
does not contain any other todo items."
  (and (orgtd-at-todo-p)
       (not (orgtd-contains-todo-p))
       (not (org-entry-get nil orgtd-project-property-name))))

;;;###autoload
(defun orgtd-at-project-p ()
  "Predicate determining if heading at point is a root of a project.
Project is a todo item (fulfilling `orgtd-at-todo-p' predicate) that
contains other todo items and is not itself contained under
higher level todo item."
  (and (orgtd-at-todo-p)
       (not (orgtd-contained-in-todo-p))
       (or (orgtd-contains-todo-p)
           (org-entry-get nil orgtd-project-property-name)
           (org-entry-get nil orgtd-project-latest-activity-property-name))))

;;;###autoload
(defun orgtd-at-subproject-p ()
  "Predicate determining if heading at point is a root of subproject.
Subproject is a todo item (fulfilling `orgtd-at-todo-p' predicate)
that contains other todo items and is itself contained under
higher level todo item."
  (and (orgtd-at-todo-p)
       (orgtd-contained-in-todo-p)
       (orgtd-contains-todo-p)))

;;;###autoload
(defun orgtd-contains-next-p ()
  "Predicate determining if heading at point contains a next item.
Next item is a heading with keyword in `orgtd-next-task-keywords'."
  (and (org-at-heading-p)
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
  (org-with-wide-buffer
   (cl-loop initially (unless (zerop (org-outline-level))
                        (org-back-to-heading 'invisible-ok))
            if (orgtd-at-project-p) return (point-marker)
            unless (org-up-heading-safe) return nil)))

(defun orgtd-project-at-point-status ()
  "Get a symbol representing a status of (sub)project at point.
Can be either of :active, :suspended, :finished, or :stuck.
Raise error if not applicable."
  (if (or (orgtd-at-project-p)
          (orgtd-at-subproject-p))
      (if (orgtd-contains-next-p)
          :active
        (let ((todo-state (org-get-todo-state)))
          (cond
           ((string= "HOLD" todo-state) :suspended)
           ((member todo-state org-done-keywords) :finished)
           (t :stuck))))
    (error "Point is not at (sub)project heading at the moment")))

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
                           (float-time (apply #'encode-time
                                              (org-parse-time-string time-string))))
          title (org-with-point-at location
                  (nth 4 (org-heading-components)))
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
  (when (orgtd-at-project-p)
    (save-excursion (org-end-of-subtree 'invisible-ok))))

;;;###autoload
(defun orgtd-keep-projects-with-status (status)
  (save-excursion
    (if (orgtd-at-project-p)
        (unless (eq (orgtd-project-status (orgtd-project)) status)
          (org-end-of-subtree 'invisible-ok))
      (if (orgtd-at-todo-p)
          (org-end-of-subtree 'invisible-ok)
        (or (outline-next-heading)
            (point-max))))))

;;;###autoload
(defun orgtd-keep-tasks ()
  "Skip function for org agenda that keeps only tasks, skipping
project and subproject headings."
  (unless (orgtd-at-task-p)
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
(defun orgtd-skip-everything-under-done-headings ()
  "Skip function for org agenda that skips everything under done
headings."
  (org-with-wide-buffer
   (cl-loop while (org-up-heading-safe)
            if (member (org-get-todo-state) org-done-keywords)
            return (org-end-of-subtree 'invisible-ok))))

;;;###autoload
(defun orgtd-keep-subprojects-with-status (status)
  (if (orgtd-at-subproject-p)
      (unless (eq status (orgtd-project-at-point-status))
        (org-end-of-subtree 'invisible-ok))
    (or (outline-next-heading)
        (point-max))))

;;;###autoload
(defun orgtd-parent-subproject-or-project-location ()
  (org-with-wide-buffer
   (unless (zerop (org-outline-level)) (org-back-to-heading t))
   (when (and (orgtd-at-todo-p) (org-up-heading-safe))
     (cl-loop until (zerop (org-outline-level))
              if (or (orgtd-at-project-p)
                     (orgtd-at-subproject-p))
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
    (org-with-point-at project
      (org-entry-put nil
                     orgtd-project-latest-activity-property-name
                     (format-time-string "[%Y-%m-%d %a %H:%M]" (float-time))))))

;;;###autoload
(defun orgtd-install-hooks ()
  (add-hook 'org-clock-in-hook #'orgtd-set-project-last-active-timestamp)
  (add-hook 'org-clock-out-hook #'orgtd-set-project-last-active-timestamp)
  (add-hook 'org-after-todo-state-change-hook #'orgtd-set-project-last-active-timestamp))

(provide 'orgtd)

;;; orgtd.el ends here
