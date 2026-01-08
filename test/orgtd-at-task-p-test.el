;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-at-task-p"
  (it "handles simple todo items"
    (with-org "* TODO do a thing"
      (expect (orgtd-at-task-p) :to-be-truthy)))

  (it "rejects plain non task headings"
    (with-org "* do a thing"
      (expect (orgtd-at-task-p) :to-be nil)))

  (it "rejects headings containing other todos"
    (with-org "* TODO do things
** TODO do first thing
** TODO do second thing"
      (expect (orgtd-at-task-p) :to-be nil)))

  (it "handles plain child headings"
    (with-org "* TODO a
** b
** c"
      (expect (orgtd-at-task-p) :to-be-truthy)))

  (it "rejects empty explicitly marked projects"
    (let ((orgtd-project-property-name "IS_PROJECT"))
      (with-org "* TODO empty project
:PROPERTIES:
:IS_PROJECT: true
:END:
"
        (expect (orgtd-at-task-p) :to-be nil))))

  (it "accepts tasks nested within projects"
    (with-org "* TODO project
** TODO <POINT>task within project"
      (expect (orgtd-at-task-p) :to-be-truthy)))

  (it "rejects point in body text"
    (with-org "* TODO task
<POINT>body text"
      (expect (orgtd-at-task-p) :to-be nil))))
