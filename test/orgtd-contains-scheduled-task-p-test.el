;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-contains-scheduled-task-p"
  (it "handles headings without scheduled tasks"
    (with-org "* heading"
      (expect (orgtd-contains-scheduled-task-p) :to-be nil)))

  (it "ignores sheduled status of heading itself"
    (with-org "* TODO heading
SCHEDULED: <2017-02-26 Sun>"
      (expect (orgtd-contains-scheduled-task-p) :to-be nil )))

  (it "gracefully handles invocation outside of non-headings"
    (with-org "some text"
      (expect (orgtd-contains-scheduled-task-p) :to-be nil)))

  (it "handles headingsd -containing a non-scheduled todo item"
    (with-org "* heading
** DONE second-level heading"
      (expect (orgtd-contains-scheduled-task-p) :to-be nil)))

  (it "handles headings containing a scheduled todo item"
    (with-org "* heading
** TODO second-level heading
SCHEDULED: <2017-02-26 Sun>"
      (expect (orgtd-contains-scheduled-task-p) :to-be-truthy)))

  (it "does not accept DONE scheduled items"
    (with-org "* heading
** DONE second-level heading
SCHEDULED: <2017-02-26 Sun>"
      (expect (orgtd-contains-scheduled-task-p) :to-be nil)))

  (it "also accepts items with deadlines"
    (with-org "* heading
** TODO second-level heading
DEADLINE: <2017-02-26 Sun>"
      (expect (orgtd-contains-scheduled-task-p) :to-be-truthy)))

  (it "finds deeply nested scheduled todo items"
    (with-org "* level 1
** level 2
*** level 3
**** level 4
***** TODO level 5
SCHEDULED: <2017-02-26 Sun>"
      (expect (orgtd-contains-scheduled-task-p) :to-be-truthy)))

  (it "limits itself to contents of tree at point"
    (with-org "* heading a
** subheading
* heading b
** TODO subheading
SCHEDULED: <2017-02-26 Sun>"
      (expect (orgtd-contains-scheduled-task-p) :to-be nil))))
