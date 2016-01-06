(ert-deftest just-returns-nil-when-there-is-nothing-there ()
  (with-org ""
    (should-not (orgtd-parent-subproject-or-project-location) )))

(ert-deftest just-returns-nil-when-called-outside-of-any-project ()
  (with-org "* TODO a standalone task"
    (should-not (orgtd-parent-subproject-or-project-location))))

(ert-deftest just-returns-nil-when-called-at-project-heading ()
  (with-org "* TODO <POINT> a project
** TODO some task"
    (should-not (orgtd-parent-subproject-or-project-location))))

(ert-deftest finds-project-if-it-is-an-immediate-parent ()
  (with-org "* TODO project heading
** TODO <POINT> task heading"
    (should (equal (orgtd-parent-subproject-or-project-location)
                   (save-excursion
                     (goto-char (point-min))
                     (point-marker))))))


(ert-deftest finds-project-if-it-is-itself-nested ()
  (with-org "* just some heading
** TODO project heading
*** TODO <POINT> task heading"
    (should (equal (orgtd-parent-subproject-or-project-location)
                   (save-excursion
                     (search-backward "project")
                     (move-beginning-of-line 1)
                     (point-marker))))))

(ert-deftest finds-project-location-for-deeply-nested-todo-items ()
  (with-org "* TODO project
** foo
*** bar
**** TODO <POINT> a task"
    (should (equal (orgtd-parent-subproject-or-project-location)
                   (save-excursion
                     (goto-char (point-min))
                     (point-marker))))))

(ert-deftest finds-subproject-if-it-is-an-immediata-parent ()
  (with-org "* TODO project heading
** TODO subproject heading
*** TODO task heading <POINT>"
    (should (equal (orgtd-parent-subproject-or-project-location)
                   (save-excursion
                     (search-backward "subproject")
                     (move-beginning-of-line 1)
                     (point-marker))))))

(ert-deftest finds-subproject-location-for-deeply-nested-todo-items ()
  (with-org "* TODO project
** foo
*** TODO subproject
**** bar
***** baz
****** TODO <POINT> a task"
    (should (equal (orgtd-parent-subproject-or-project-location)
                   (save-excursion
                     (search-backward "subproject")
                     (move-beginning-of-line 1)
                     (point-marker))))))
