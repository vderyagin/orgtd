;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-at-project-p"
  (it "rejects non-project headings"
    (with-org "* plain heading"
      (expect (orgtd-at-project-p) :to-be nil)))

  (it "accepts empty todo item with IS_PROJECT property set"
    (let ((orgtd-project-property-name "IS_PROJECT"))
      (with-org "* TODO empty project heading
:PROPERTIES:
:IS_PROJECT: true
:END:
"
        (expect (orgtd-at-project-p) :to-be-truthy))))

  (it "accepts empty todo item with project LAST_ACTIVE property"
    (let ((orgtd-project-latest-activity-property-name "LAST_ACTIVE"))
      (with-org "* TODO empty project heading
:PROPERTIES:
:LAST_ACTIVE: [2016-06-27 Mon 13:29]
:END:
"
        (expect (orgtd-at-project-p) :to-be-truthy))))

  (it "rejects empty todo items with plain headings"
    (with-org "* TODO plain todo item"
      (expect (orgtd-at-project-p) :to-be nil)))

  (it "accepts todo item with immediately nested todo item"
    (with-org "* TODO project
** TODO task"
      (expect (orgtd-at-project-p) :to-be-truthy)))

  (it "accepts todo item with deeply nested todo item"
    (with-org "* TODO foo
** bar
*** baz
**** quux
***** corge
****** TODO finally another todo"
      (expect (orgtd-at-project-p) :to-be-truthy)))

  (it "rejects any heading if it is under todo item"
    (with-org "* TODO top-level-todo
** TODO <POINT>second-level-todo
*** TODO third-level todo"
      (expect (orgtd-at-project-p) :to-be nil))))
