(ert-deftest handles-top-level-headings ()
  (with-org "* foo"
    (should-not (orgtd-contained-in-todo-p))))

(ert-deftest handles-headings-with-todo-parent ()
  (with-org "* TODO foo
** bar"
    (search-forward "bar")
    (should (orgtd-contained-in-todo-p))))

(ert-deftest handles-funny-nesting ()
  (with-org "* a
** TODO b
*** c
**** bar"
    (search-forward "bar")
    (should (orgtd-contained-in-todo-p))))

(ert-deftest rejects-todo-items-in-hierarchy-of-plain-headings ()
  (with-org "* a
** b
*** c
**** d
***** TODO bar"
    (search-forward "bar")
    (should-not (orgtd-contained-in-todo-p))))

(ert-deftest gracefully-returns-when-invoked-from-top-level ()
  (with-org ""
    (should-not (orgtd-contained-in-todo-p))))
