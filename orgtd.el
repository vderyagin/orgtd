;;; orgtd.el --- Org functions facilitating GTD -*- lexical-binding: t -*-

;; Copyright (C) 2015 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 22 Oct 2015
;; Version: 0.0.1

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

(defgroup orgtd nil
  "Org functions facilitating GTD"
  :group 'org)

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
       (let ((subtree-end (save-excursion (org-end-of-subtree 'invisible-ok))))
         (save-excursion
           (cl-loop initially do (outline-next-heading)
                    while (< (point) subtree-end)
                    thereis (orgtd-at-todo-p)
                    do (outline-next-heading))))))

;;;###autoload
(defun orgtd-contained-in-todo-p ()
  "Predicate determining if heading at point is contained within a heading with a todo keyword."
  (and (org-at-heading-p)
       (save-excursion
         (cl-loop while (org-up-heading-safe)
                  thereis (orgtd-at-todo-p)))))

;;;###autoload
(defun orgtd-at-task-p ()
  "Predicate determining if heading at point is a task.
Task is a todo item (fulfilling `orgtd-at-todo-p' predicate) that
does not contain any other todo items."
  (and (orgtd-at-todo-p)
       (not (orgtd-contains-todo-p))))

;;;###autoload
(defun orgtd-at-project-p ()
  "Predicate determining if heading at point is a root of a project.
Project is a todo item (fulfilling `orgtd-at-todo-p' predicate) that
contains other todo items and is not itself contained under
higher level todo item."
  (and (orgtd-at-todo-p)
       (not (orgtd-contained-in-todo-p))
       (orgtd-contains-todo-p)))

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
Next item is a heading with NEXT todo keyword."
  (and (org-at-heading-p)
       (let ((subtree-end (save-excursion (org-end-of-subtree 'invisible-ok))))
         (save-excursion
           (cl-loop initially do (outline-next-heading)
                    while (< (point) subtree-end)
                    thereis (looking-at "^\*\*+ NEXT ")
                    do (outline-next-heading))))))

;;;###autoload
(defun orgtd-get-project-at-point ()
  "Return marker pointing to heading of project cointaining position at point.
Return nil if position at point is not under any project."
  (save-excursion
    (cl-loop initially (unless (zerop (org-outline-level)) (org-back-to-heading t))
             if (orgtd-at-project-p) return (point-marker)
             unless (org-up-heading-safe) return nil)))

(defclass orgtd-project ()
  ((location :reader orgtd-project-location)
   (status :type (member :active :suspended :stuck :finished)
           :reader orgtd-project-status)))

(cl-defmethod initialize-instance :after
  ((project orgtd-project) &rest _)
  (with-slots (location status) project
    (setq location (point-marker)
          status
          (if (orgtd-contains-next-p)
              :active
            (let ((todo-state (org-get-todo-state)))
              (cond
               ((string= "HOLD" todo-state) :suspended)
               ((member todo-state org-done-keywords) :finished)
               (t :stuck)))))))

(cl-defmethod orgtd-project-title ((project orgtd-project))
  (org-with-point-at (oref project location)
    (nth 4 (org-heading-components))))

(cl-defmethod orgtd-project-currently-clocked-p ((project orgtd-project))
  "Return `t' when PROJECT is being clocked currently, `nil' otherwise."
  (equal (oref project location)
         (when (marker-buffer org-clock-marker)
           (org-with-point-at org-clock-marker
             (orgtd-get-project-at-point)))))

;;;###autoload
(defun orgtd-projects ()
  (seq-mapcat
   (lambda (org-file)
     (with-current-buffer (find-file-noselect org-file)
       (save-excursion
         (goto-char (point-min))
         (cl-loop until (eobp)
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
(defun orgtd-skip-over-projects-unless (status)
  (when (and (orgtd-at-project-p)
             (not (eq (orgtd-project-status (orgtd-project)) status)))
    (save-excursion (org-end-of-subtree 'invisible-ok))))

;;;###autoload
(defun orgtd-keep-projects-with-status (status)
  (save-excursion
    (if (orgtd-at-project-p)
        (when (not (eq (orgtd-project-status (orgtd-project)) status))
          (org-end-of-subtree 'invisible-ok))
      (if (orgtd-at-todo-p)
          (org-end-of-subtree 'invisible-ok)
        (or (outline-next-heading)
            (point-max))))))

;;;###autoload
(defun orgtd-parent-subproject-or-project-location ()
  (save-excursion
    (unless (zerop (org-outline-level)) (org-back-to-heading t))
    (when (and (orgtd-at-todo-p) (org-up-heading-safe))
      (cl-loop until (zerop (org-outline-level))
               if (or (orgtd-at-project-p)
                      (orgtd-at-subproject-p))
               return (point-marker)
               unless (org-up-heading-safe) return nil))))

(provide 'orgtd)

;;; orgtd.el ends here
