(describe "orgtd-get-project-at-point"
  (it "bare headings are not within any project"
    (with-org "* foo"
      (expect (orgtd-get-project-at-point) :to-be nil)))

  (it "detects top level projects at point"
    (with-org "* TODO a
** TODO b
** TODO c"
      (expect (point-marker)
              :to-equal
              (orgtd-get-project-at-point))))

  (it "detects projects from within"
    (with-org "* TODO a
** TODO b
** TODO <POINT>c"
      (expect (orgtd-get-project-at-point)
              :to-equal
              (save-excursion
                (goto-char (point-min))
                (point-marker)))))

  (it "detects non-top-level projects from within"
    (with-org "* just a heading
** TODO project
*** TODO <POINT>c"
      (expect  (orgtd-get-project-at-point)
               :to-equal
               (save-excursion
                 (search-backward "project")
                 (move-beginning-of-line 1)
                 (point-marker)))))

  (it "is not confused by todo item with bunch of subheadings"
    (with-org "* TODO a
** b
** c
*** d"
      (expect (orgtd-get-project-at-point) :to-be nil))))
