(ert-deftest rejects-headings-withoug-todo-keywords ()
  (with-org "* plain heading"
    (should-not (orgtd-contains-next-p))))

(ert-deftest rejects-bare-next-items ()
  (with-org "* NEXT foo"
    (should-not (orgtd-contains-next-p))))

(ert-deftest accepts-heading-with-nested-next-item ()
  (with-org "* plain heading
** NEXT next thing"
    (should (orgtd-contains-next-p))))

(ert-deftest accepts-heading-with-deeply-nested-next-item ()
  (with-org "* TODO project
** TODO subproject
*** TODO subsubproject
**** NEXT next task"
    (should (orgtd-contains-next-p))))

(ert-deftest is-not-fooled-by-non-heading-containing-NEXT ()
  (with-org "* heading
foo bar * NEXT baz")
  (should-not (orgtd-contains-next-p)))

(ert-deftest accepts-other-keywords-if-set ()
  (with-org "* TODO project
** TODO subproject
*** TODO subsubproject
**** FOOBAR next task"
    (let ((orgtd-next-task-keywords '("FOOBAR")))
      (with-org-todo-keywords '("TODO" "FOOBAR" "DONE")
        (should (orgtd-contains-next-p))))))
