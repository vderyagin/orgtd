(ert-deftest rejects-project-headings ()
  (with-org "* TODO project
** TODO project task"
    (should-not (orgtd-subproject-p))))

(ert-deftest rejects-plain-headings ()
  (with-org "* plain heading"
    (should-not (orgtd-subproject-p))))

(ert-deftest rejects-bare-todo-items ()
  (with-org "* TODO bare todo item"
    (should-not (orgtd-subproject-p))))

(ert-deftest accepts-subprojects ()
  (with-org "* TODO project
** TODO <POINT>subproject
*** TODO task"
    (should (orgtd-subproject-p))))

(ert-deftest rejects-subproject-tasks ()
  (with-org "* TODO project
** TODO subproject
*** TODO <POINT>task"
    (should-not (orgtd-subproject-p))))
