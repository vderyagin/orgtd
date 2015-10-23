(ert-deftest handles-top-level-headings ()
  (with-org "* foo"
    (should-not (orgtd-contained-in-todo-p))))

(ert-deftest handles-headings-with-todo-parent ()
  (with-org "* TODO foo
** <POINT> bar"
    (should (orgtd-contained-in-todo-p))))

(ert-deftest handles-funny-nesting ()
  (with-org "* a
** TODO b
*** c
**** <POINT> d"
    (should (orgtd-contained-in-todo-p))))

(ert-deftest rejects-todo-items-in-hierarchy-of-plain-headings ()
  (with-org "* a
** b
*** c
**** d
***** TODO <POINT> e"
    (should-not (orgtd-contained-in-todo-p))))

(ert-deftest gracefully-returns-when-invoked-from-top-level ()
  (with-org ""
    (should-not (orgtd-contained-in-todo-p))))
