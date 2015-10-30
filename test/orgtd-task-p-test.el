(ert-deftest handles-simple-todo-items ()
  (with-org "* TODO do a thing"
    (should (orgtd-task-p))))

(ert-deftest rejects-plain-non-task-headings ()
  (with-org "* do a thing"
    (should-not (orgtd-task-p))))

(ert-deftest rejects-headings-containing-other-todos ()
  (with-org "* TODO do things
** TODO do first thing
** TODO do second thing"
    (should-not (orgtd-task-p))))

(ert-deftest handles-plain-child-headings ()
  (with-org "* TODO a
** b
** c"
    (should (orgtd-task-p))))
