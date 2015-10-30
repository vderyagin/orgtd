(ert-deftest rejects-non-project-headings ()
  (with-org "* plain heading"
    (should-not (orgtd-project-p))))

(ert-deftest rejects-empty-todo-items-plain-headings ()
  (with-org "* TODO plain todo item"
    (should-not (orgtd-project-p))))

(ert-deftest accepts-todo-item-with-immediately-nested-todo-item ()
  (with-org "* TODO project
** TODO task"
      (should (orgtd-project-p))))

(ert-deftest accepts-todo-item-with-deeply-nested-todo-item ()
  (with-org "* TODO foo
** bar
*** baz
**** quux
***** corge
****** TODO finally another todo"
    (should (orgtd-project-p))))

(ert-deftest rejects-any-heading-if-it-is-under-todo-item ()
  (with-org "* TODO top-level-todo
** TODO <POINT>second-level-todo
*** TODO third-level todo"
    (should-not (orgtd-project-p))))
