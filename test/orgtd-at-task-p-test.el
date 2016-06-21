(ert-deftest handles-simple-todo-items ()
  (with-org "* TODO do a thing"
    (should (orgtd-at-task-p))))

(ert-deftest rejects-plain-non-task-headings ()
  (with-org "* do a thing"
    (should-not (orgtd-at-task-p))))

(ert-deftest rejects-headings-containing-other-todos ()
  (with-org "* TODO do things
** TODO do first thing
** TODO do second thing"
    (should-not (orgtd-at-task-p))))

(ert-deftest handles-plain-child-headings ()
  (with-org "* TODO a
** b
** c"
    (should (orgtd-at-task-p))))

(ert-deftest rejects-empty-explicitly-marked-projects ()
  (let ((orgtd-project-property-name "IS_PROJECT"))
    (with-org "* TODO empty project
:PROPERTIES:
:IS_PROJECT: true
:END:
"
      (should-not (orgtd-at-task-p)))))
