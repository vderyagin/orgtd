(ert-deftest bare-headings-are-not-within-any-project ()
  (with-org "* foo"
    (should-not (orgtd-get-project-at-point))))

(ert-deftest detects-top-level-projects-at-point ()
  (with-org "* TODO a
** TODO b
** TODO c"
    (should (equal (point-marker)
                   (orgtd-get-project-at-point)))))

(ert-deftest detects-projects-from-within ()
  (with-org "* TODO a
** TODO b
** TODO <POINT>c"
    (should (equal (orgtd-get-project-at-point)
                   (save-excursion
                     (goto-char (point-min))
                     (point-marker))))))

(ert-deftest detects-non-top-level-projects-from-within ()
  (with-org "* just a heading
** TODO project
*** TODO <POINT>c"
    (should (equal (orgtd-get-project-at-point)
                   (save-excursion
                     (search-backward "project")
                     (move-beginning-of-line 1)
                     (point-marker))))))

(ert-deftest is-not-confused-by-todo-item-with-bunch-of-subheadings ()
  (with-org "* TODO a
** b
** c
*** d"
    (should-not (orgtd-get-project-at-point))))
