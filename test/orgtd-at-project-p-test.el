(ert-deftest rejects-non-project-headings ()
  (with-org "* plain heading"
    (should-not (orgtd-at-project-p))))

(ert-deftest accepts-empty-todo-item-with-set-property ()
  (let ((orgtd-project-property-name "IS_PROJECT"))
    (with-org "* TODO empty project heading
:PROPERTIES:
:IS_PROJECT: true
:END:
"
              (should (orgtd-at-project-p)))))

(ert-deftest accepts-empty-todo-item-with-project-latest-activity-timestamp ()
  (let ((orgtd-project-latest-activity-property-name "LAST_ACTIVE"))
    (with-org "* TODO empty project heading
:PROPERTIES:
:LAST_ACTIVE: [2016-06-27 Mon 13:29]
:END:
"
      (should (orgtd-at-project-p)))))

(ert-deftest rejects-empty-todo-items-plain-headings ()
  (with-org "* TODO plain todo item"
    (should-not (orgtd-at-project-p))))

(ert-deftest accepts-todo-item-with-immediately-nested-todo-item ()
  (with-org "* TODO project
** TODO task"
    (should (orgtd-at-project-p))))

(ert-deftest accepts-todo-item-with-deeply-nested-todo-item ()
  (with-org "* TODO foo
** bar
*** baz
**** quux
***** corge
****** TODO finally another todo"
    (should (orgtd-at-project-p))))

(ert-deftest rejects-any-heading-if-it-is-under-todo-item ()
  (with-org "* TODO top-level-todo
** TODO <POINT>second-level-todo
*** TODO third-level todo"
    (should-not (orgtd-at-project-p))))
