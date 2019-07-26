(describe "orgtd-at-subproject-p"
  (it "rejects project headings"
    (with-org "* TODO project
** TODO project task"
      (expect (orgtd-at-subproject-p) :to-be nil)))

  (it "rejects non-subproject headings"
    (with-org "* plain heading"
      (expect (orgtd-at-subproject-p) :to-be nil)))

  (it "rejects bare todo items"
    (with-org "* TODO bare todo item"
      (expect (orgtd-at-subproject-p) :to-be nil)))

  (it "accepts subprojects"
    (with-org "* TODO project
** TODO <POINT>subproject
*** TODO task"
      (should (orgtd-at-subproject-p))))

  (it "rejects subproject tasks"
    (with-org "* TODO project
** TODO subproject
*** TODO <POINT>task"
      (expect (orgtd-at-subproject-p) :to-be nil))))
