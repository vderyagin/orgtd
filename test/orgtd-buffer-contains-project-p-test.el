;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-buffer-contains-project-p"
  (it "returns nil for empty buffer"
    (with-org ""
      (expect (orgtd-buffer-contains-project-p) :to-be nil)))

  (it "returns nil for buffer with only plain headings"
    (with-org "* heading
** subheading"
      (expect (orgtd-buffer-contains-project-p) :to-be nil)))

  (it "returns nil for buffer with only standalone tasks"
    (with-org "* TODO task one
* TODO task two"
      (expect (orgtd-buffer-contains-project-p) :to-be nil)))

  (it "returns truthy for buffer containing a project"
    (with-org "* TODO project
** TODO task"
      (expect (orgtd-buffer-contains-project-p) :to-be-truthy)))

  (it "finds project even when point is elsewhere"
    (with-org "* plain heading
** <POINT> subheading
* TODO project
** TODO task"
      (expect (orgtd-buffer-contains-project-p) :to-be-truthy)))

  (it "finds explicitly marked project"
    (let ((orgtd-project-property-name "IS_PROJECT"))
      (with-org "* TODO empty project
:PROPERTIES:
:IS_PROJECT: true
:END:
"
        (expect (orgtd-buffer-contains-project-p) :to-be-truthy)))))
