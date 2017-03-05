(ert-deftest handles-headings-without-scheduled-tasks ()
  (with-org "* heading"
    (should-not (orgtd-contains-scheduled-task-p))))

(ert-deftest ignores-sheduled-status-of-heading-itself ()
  (with-org "* TODO heading
SCHEDULED: <2017-02-26 Sun>"
    (should-not (orgtd-contains-scheduled-task-p) )))

(ert-deftest gracefully-handles-invocation-outside-of-any-headings ()
  (with-org "some text"
    (should-not (orgtd-contains-scheduled-task-p))))

(ert-deftest handles-headings-containing-a-non-scheduled-todo-item ()
  (with-org "* heading
** DONE second-level heading"
    (should-not (orgtd-contains-scheduled-task-p))))

(ert-deftest handles-headings-containing-a-scheduled-todo-item ()
  (with-org "* heading
** TODO second-level heading
SCHEDULED: <2017-02-26 Sun>"
    (should (orgtd-contains-scheduled-task-p))))

(ert-deftest does-not-accept-done-scheduled-items ()
  (with-org "* heading
** DONE second-level heading
SCHEDULED: <2017-02-26 Sun>"
    (should-not (orgtd-contains-scheduled-task-p))))

(ert-deftest also-accepts-items-with-deadlines ()
  (with-org "* heading
** TODO second-level heading
DEADLINE: <2017-02-26 Sun>"
    (should (orgtd-contains-scheduled-task-p))))

(ert-deftest finds-deeply-nested-scheduled-todo-items ()
  (with-org "* level 1
** level 2
*** level 3
**** level 4
***** TODO level 5
SCHEDULED: <2017-02-26 Sun>"
    (should (orgtd-contains-scheduled-task-p))))

(ert-deftest limits-itself-to-contents-of-tree-at-point ()
  (with-org "* heading a
** subheading
* heading b
** TODO subheading
SCHEDULED: <2017-02-26 Sun>"
    (should-not (orgtd-contains-scheduled-task-p))))
