(ert-deftest handles-headings-with-nothing-inside ()
  (with-org "* heading"
    (should-not (orgtd-contains-todo-p))))

(ert-deftest ignores-todo-keyword-on-headline-itself ()
  (with-org "* TODO heading"
    (should-not (orgtd-contains-todo-p) )))

(ert-deftest handles-headings-containing-a-todo-item ()
  (with-org "* heading
** DONE second-level heading"
    (should (orgtd-contains-todo-p))))

(ert-deftest finds-deeply-tested-todo-items ()
  (with-org "* level 1
** level 2
*** level 3
**** level 4
***** TODO level 5"
   (should (orgtd-contains-todo-p))))

(ert-deftest limits-itself-to-tree-at-point ()
  (with-org "* heading a
** subheading
* heading b
** TODO subheading"
    (should-not (orgtd-contains-todo-p))))
