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

(require 'org)

(defgroup orgtd nil
  "Org functions facilitating GTD"
  :group 'org)

;;;###autoload
(defun orgtd-todo-p ()
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
                    thereis (orgtd-todo-p)
                    do (outline-next-heading))))))

;;;###autoload
(defun orgtd-task-p ()
  "Predicate determining if heading at point is a task.
Task is a todo item (fulfilling `orgtd-todo-p' predicate) that
does contain any other todo items."
  (and (orgtd-todo-p)
       (not (orgtd-contains-todo-p))))

(provide 'orgtd)

;;; orgtd.el ends here
