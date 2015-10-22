(ert-deftest handles-headings-with-nothing-inside ()
  (with-org "* heading"
    (should-not (orgtd-contains-todo-p))))

(ert-deftest handles-headings-containing-a-todo-item ()
  (with-org "* heading
** DONE second-level heading")
  (should (orgtd-contains-todo-p)))
